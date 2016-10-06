{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


Functions for inferring (and simplifying) the context for derived instances.
-}

{-# LANGUAGE CPP #-}

module TcDerivInfer (inferConstraints, simplifyInstanceContexts) where

#include "HsVersions.h"

import Bag
import Class
import DataCon
import DynFlags
import ErrUtils
import Inst
import Outputable
import PrelNames
import TcDerivUtils
import TcEnv
import TcErrors (reportAllUnsolved)
import TcGenFunctor
import TcGenGenerics
import TcMType
import TcRnMonad
import TcType
import TyCon
import Type
import TcSimplify
import TcValidity (validDerivPred)
import TcUnify (buildImplicationFor)
import Unify (tcUnifyTy)
import Util
import VarSet

import Control.Monad
import Data.List
import Data.Maybe

----------------------

inferConstraints :: [TyVar] -> Class -> [TcType] -> TcType
                 -> TyCon -> [TcType]
                 -> (ThetaOrigin -> [TyVar] -> [TcType] -> TcM a)
                 -> TcM a
-- inferConstraints figures out the constraints needed for the
-- instance declaration generated by a 'deriving' clause on a
-- data type declaration. It also returns the new in-scope type
-- variables and instance types, in case they were changed due to
-- the presence of functor-like constraints.
-- See Note [Inferring the instance context]

-- e.g. inferConstraints
--        C Int (T [a])    -- Class and inst_tys
--        :RTList a        -- Rep tycon and its arg tys
-- where T [a] ~R :RTList a
--
-- Generate a sufficiently large set of constraints that typechecking the
-- generated method definitions should succeed.   This set will be simplified
-- before being used in the instance declaration
inferConstraints tvs main_cls cls_tys inst_ty rep_tc rep_tc_args mkTheta
  | is_generic                        -- Generic constraints are easy
  = mkTheta [] tvs inst_tys

  | is_generic1                       -- Generic1 needs Functor
  = ASSERT( length rep_tc_tvs > 0 )   -- See Note [Getting base classes]
    ASSERT( length cls_tys   == 1 )   -- Generic1 has a single kind variable
    do { functorClass <- tcLookupClass functorClassName
       ; con_arg_constraints (get_gen1_constraints functorClass) mkTheta }

  | otherwise  -- The others are a bit more complicated
  = ASSERT2( equalLength rep_tc_tvs all_rep_tc_args
           , ppr main_cls <+> ppr rep_tc
             $$ ppr rep_tc_tvs $$ ppr all_rep_tc_args )
    con_arg_constraints get_std_constrained_tys
      $ \arg_constraints tvs' inst_tys' ->
      do { traceTc "inferConstraints" $ vcat
                [ ppr main_cls <+> ppr inst_tys'
                , ppr arg_constraints
                ]
         ; mkTheta (stupid_constraints ++ extra_constraints
                     ++ sc_constraints ++ arg_constraints)
                   tvs' inst_tys' }
  where
    tc_binders = tyConBinders rep_tc
    choose_level bndr
      | isNamedTyConBinder bndr = KindLevel
      | otherwise               = TypeLevel
    t_or_ks = map choose_level tc_binders ++ repeat TypeLevel
       -- want to report *kind* errors when possible

       -- Constraints arising from the arguments of each constructor
    con_arg_constraints :: (CtOrigin -> TypeOrKind
                                     -> Type
                                     -> [(ThetaOrigin, Maybe TCvSubst)])
                        -> (ThetaOrigin -> [TyVar] -> [TcType] -> TcM a)
                        -> TcM a
    con_arg_constraints get_arg_constraints mkTheta
      = let (predss, mbSubsts) = unzip
              [ preds_and_mbSubst
              | data_con <- tyConDataCons rep_tc
              , (arg_n, arg_t_or_k, arg_ty)
                  <- zip3 [1..] t_or_ks $
                     dataConInstOrigArgTys data_con all_rep_tc_args
                -- No constraints for unlifted types
                -- See Note [Deriving and unboxed types]
              , not (isUnliftedType arg_ty)
              , let orig = DerivOriginDC data_con arg_n
              , preds_and_mbSubst <- get_arg_constraints orig arg_t_or_k arg_ty
              ]
            preds = concat predss
            -- If the constraints require a subtype to be of kind (* -> *)
            -- (which is the case for functor-like constraints), then we
            -- explicitly unify the subtype's kinds with (* -> *).
            -- See Note [Inferring the instance context]
            subst        = foldl' composeTCvSubst
                                  emptyTCvSubst (catMaybes mbSubsts)
            unmapped_tvs = filter (\v -> v `notElemTCvSubst` subst
                                      && not (v `isInScope` subst)) tvs
            (subst', _)  = mapAccumL substTyVarBndr subst unmapped_tvs
            preds'       = substThetaOrigin subst' preds
            inst_tys'    = substTys subst' inst_tys
            tvs'         = tyCoVarsOfTypesWellScoped inst_tys'
        in mkTheta preds' tvs' inst_tys'

    is_generic  = main_cls `hasKey` genClassKey
    is_generic1 = main_cls `hasKey` gen1ClassKey
    -- is_functor_like: see Note [Inferring the instance context]
    is_functor_like = typeKind inst_ty `tcEqKind` typeToTypeKind
                   || is_generic1 -- Technically, Generic1 requires a type of
                                  -- kind (k -> *), not (* -> *), but we still
                                  -- label it "functor-like" to make sure
                                  -- all_rep_tc_args has all the necessary type
                                  -- variables it needs to function.

    get_gen1_constraints :: Class -> CtOrigin -> TypeOrKind -> Type
                         -> [(ThetaOrigin, Maybe TCvSubst)]
    get_gen1_constraints functor_cls orig t_or_k ty
       = mk_functor_like_constraints orig t_or_k functor_cls $
         get_gen1_constrained_tys last_tv ty

    get_std_constrained_tys :: CtOrigin -> TypeOrKind -> Type
                            -> [(ThetaOrigin, Maybe TCvSubst)]
    get_std_constrained_tys orig t_or_k ty
        | is_functor_like = mk_functor_like_constraints orig t_or_k main_cls $
                            deepSubtypesContaining last_tv ty
        | otherwise       = [( [mk_cls_pred orig t_or_k main_cls ty]
                             , Nothing )]

    mk_functor_like_constraints :: CtOrigin -> TypeOrKind
                                -> Class -> [Type]
                                -> [(ThetaOrigin, Maybe TCvSubst)]
    -- 'cls' is usually main_cls (Functor or Traversable etc), but if
    -- main_cls = Generic1, then 'cls' can be Functor; see get_gen1_constraints
    --
    -- For each type, generate two constraints, [cls ty, kind(ty) ~ (*->*)],
    -- and a kind substitution that results from unifying kind(ty) with * -> *.
    -- If the unification is successful, it will ensure that the resulting
    -- instance is well kinded. If not, the second constraint will result
    -- in an error message which points out the kind mismatch.
    -- See Note [Inferring the instance context]
    mk_functor_like_constraints orig t_or_k cls
       = map $ \ty -> let ki = typeKind ty in
                      ( [ mk_cls_pred orig t_or_k cls ty
                        , mkPredOrigin orig KindLevel
                            (mkPrimEqPred ki typeToTypeKind) ]
                      , tcUnifyTy ki typeToTypeKind
                      )

    rep_tc_tvs      = tyConTyVars rep_tc
    last_tv         = last rep_tc_tvs
    all_rep_tc_args | is_functor_like = rep_tc_args ++ [mkTyVarTy last_tv]
                    | otherwise       = rep_tc_args

        -- Constraints arising from superclasses
        -- See Note [Superclasses of derived instance]
    cls_tvs  = classTyVars main_cls
    inst_tys = cls_tys ++ [inst_ty]
    sc_constraints = ASSERT2( equalLength cls_tvs inst_tys, ppr main_cls <+> ppr rep_tc)
                     mkThetaOrigin DerivOrigin TypeLevel $
                     substTheta cls_subst (classSCTheta main_cls)
    cls_subst = ASSERT( equalLength cls_tvs inst_tys )
                zipTvSubst cls_tvs inst_tys

        -- Stupid constraints
    stupid_constraints = mkThetaOrigin DerivOrigin TypeLevel $
                         substTheta tc_subst (tyConStupidTheta rep_tc)
    tc_subst = ASSERT( equalLength rep_tc_tvs all_rep_tc_args )
               zipTvSubst rep_tc_tvs all_rep_tc_args

        -- Extra Data constraints
        -- The Data class (only) requires that for
        --    instance (...) => Data (T t1 t2)
        -- IF   t1:*, t2:*
        -- THEN (Data t1, Data t2) are among the (...) constraints
        -- Reason: when the IF holds, we generate a method
        --             dataCast2 f = gcast2 f
        --         and we need the Data constraints to typecheck the method
    extra_constraints
      | main_cls `hasKey` dataClassKey
      , all (isLiftedTypeKind . typeKind) rep_tc_args
      = [ mk_cls_pred DerivOrigin t_or_k main_cls ty
        | (t_or_k, ty) <- zip t_or_ks rep_tc_args]
      | otherwise
      = []

    mk_cls_pred orig t_or_k cls ty   -- Don't forget to apply to cls_tys' too
       = mkPredOrigin orig t_or_k (mkClassPred cls (cls_tys' ++ [ty]))
    cls_tys' | is_generic1 = [] -- In the awkward Generic1 case, cls_tys'
                                -- should be empty, since we are applying the
                                -- class Functor.
             | otherwise   = cls_tys

typeToTypeKind :: Kind
typeToTypeKind = liftedTypeKind `mkFunTy` liftedTypeKind

{-
Note [Inferring the instance context]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There are two sorts of 'deriving':

  * InferTheta: the deriving clause for a data type
      data T a = T1 a deriving( Eq )
    Here we must infer an instance context,
    and generate instance declaration
      instance Eq a => Eq (T a) where ...

  * CheckTheta: standalone deriving
      deriving instance Eq a => Eq (T a)
    Here we only need to fill in the bindings;
    the instance context is user-supplied

For a deriving clause (InferTheta) we must figure out the
instance context (inferConstraints). Suppose we are inferring
the instance context for
    C t1 .. tn (T s1 .. sm)
There are two cases

  * (T s1 .. sm) :: *         (the normal case)
    Then we behave like Eq and guess (C t1 .. tn t)
    for each data constructor arg of type t.  More
    details below.

  * (T s1 .. sm) :: * -> *    (the functor-like case)
    Then we behave like Functor.

In both cases we produce a bunch of un-simplified constraints
and them simplify them in simplifyInstanceContexts; see
Note [Simplifying the instance context].

In the functor-like case, we may need to unify some kind variables with * in
order for the generated instance to be well-kinded. An example from
Trac #10524:

  newtype Compose (f :: k2 -> *) (g :: k1 -> k2) (a :: k1)
    = Compose (f (g a)) deriving Functor

Earlier in the deriving pipeline, GHC unifies the kind of Compose f g
(k1 -> *) with the kind of Functor's argument (* -> *), so k1 := *. But this
alone isn't enough, since k2 wasn't unified with *:

  instance (Functor (f :: k2 -> *), Functor (g :: * -> k2)) =>
    Functor (Compose f g) where ...

The two Functor constraints are ill-kinded. To ensure this doesn't happen, we:

  1. Collect all of a datatype's subtypes which require functor-like
     constraints.
  2. For each subtype, create a substitution by unifying the subtype's kind
     with (* -> *).
  3. Compose all the substitutions into one, then apply that substitution to
     all of the in-scope type variables and the instance types.

Note [Getting base classes]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Functor and Typeable are defined in package 'base', and that is not available
when compiling 'ghc-prim'.  So we must be careful that 'deriving' for stuff in
ghc-prim does not use Functor or Typeable implicitly via these lookups.

Note [Deriving and unboxed types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We have some special hacks to support things like
   data T = MkT Int# deriving ( Show )

Specifically, we use TcGenDeriv.box to box the Int# into an Int
(which we know how to show), and append a '#'. Parenthesis are not required
for unboxed values (`MkT -3#` is a valid expression).

Note [Superclasses of derived instance]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In general, a derived instance decl needs the superclasses of the derived
class too.  So if we have
        data T a = ...deriving( Ord )
then the initial context for Ord (T a) should include Eq (T a).  Often this is
redundant; we'll also generate an Ord constraint for each constructor argument,
and that will probably generate enough constraints to make the Eq (T a) constraint
be satisfied too.  But not always; consider:

 data S a = S
 instance Eq (S a)
 instance Ord (S a)

 data T a = MkT (S a) deriving( Ord )
 instance Num a => Eq (T a)

The derived instance for (Ord (T a)) must have a (Num a) constraint!
Similarly consider:
        data T a = MkT deriving( Data )
Here there *is* no argument field, but we must nevertheless generate
a context for the Data instances:
        instance Typeable a => Data (T a) where ...

************************************************************************
*                                                                      *
         Finding the fixed point of deriving equations
*                                                                      *
************************************************************************

Note [Simplifying the instance context]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider

        data T a b = C1 (Foo a) (Bar b)
                   | C2 Int (T b a)
                   | C3 (T a a)
                   deriving (Eq)

We want to come up with an instance declaration of the form

        instance (Ping a, Pong b, ...) => Eq (T a b) where
                x == y = ...

It is pretty easy, albeit tedious, to fill in the code "...".  The
trick is to figure out what the context for the instance decl is,
namely Ping, Pong and friends.

Let's call the context reqd for the T instance of class C at types
(a,b, ...)  C (T a b).  Thus:

        Eq (T a b) = (Ping a, Pong b, ...)

Now we can get a (recursive) equation from the data decl.  This part
is done by inferConstraints.

        Eq (T a b) = Eq (Foo a) u Eq (Bar b)    -- From C1
                   u Eq (T b a) u Eq Int        -- From C2
                   u Eq (T a a)                 -- From C3


Foo and Bar may have explicit instances for Eq, in which case we can
just substitute for them.  Alternatively, either or both may have
their Eq instances given by deriving clauses, in which case they
form part of the system of equations.

Now all we need do is simplify and solve the equations, iterating to
find the least fixpoint.  This is done by simplifyInstanceConstraints.
Notice that the order of the arguments can
switch around, as here in the recursive calls to T.

Let's suppose Eq (Foo a) = Eq a, and Eq (Bar b) = Ping b.

We start with:

        Eq (T a b) = {}         -- The empty set

Next iteration:
        Eq (T a b) = Eq (Foo a) u Eq (Bar b)    -- From C1
                   u Eq (T b a) u Eq Int        -- From C2
                   u Eq (T a a)                 -- From C3

        After simplification:
                   = Eq a u Ping b u {} u {} u {}
                   = Eq a u Ping b

Next iteration:

        Eq (T a b) = Eq (Foo a) u Eq (Bar b)    -- From C1
                   u Eq (T b a) u Eq Int        -- From C2
                   u Eq (T a a)                 -- From C3

        After simplification:
                   = Eq a u Ping b
                   u (Eq b u Ping a)
                   u (Eq a u Ping a)

                   = Eq a u Ping b u Eq b u Ping a

The next iteration gives the same result, so this is the fixpoint.  We
need to make a canonical form of the RHS to ensure convergence.  We do
this by simplifying the RHS to a form in which

        - the classes constrain only tyvars
        - the list is sorted by tyvar (major key) and then class (minor key)
        - no duplicates, of course

Note [Deterministic simplifyInstanceContexts]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Canonicalisation uses nonDetCmpType which is nondeterministic. Sorting
with nonDetCmpType puts the returned lists in a nondeterministic order.
If we were to return them, we'd get class constraints in
nondeterministic order.

Consider:

  data ADT a b = Z a b deriving Eq

The generated code could be either:

  instance (Eq a, Eq b) => Eq (Z a b) where

Or:

  instance (Eq b, Eq a) => Eq (Z a b) where

To prevent the order from being nondeterministic we only
canonicalize when comparing and return them in the same order as
simplifyDeriv returned them.
See also Note [nonDetCmpType nondeterminism]
-}


simplifyInstanceContexts :: [DerivSpec ThetaOrigin] -> TcM [DerivSpec ThetaType]
-- Used only for deriving clauses (InferTheta)
-- not for standalone deriving
-- See Note [Simplifying the instance context]

simplifyInstanceContexts [] = return []

simplifyInstanceContexts infer_specs
  = do  { traceTc "simplifyInstanceContexts" $ vcat (map pprDerivSpec infer_specs)
        ; iterate_deriv 1 initial_solutions }
  where
    ------------------------------------------------------------------
        -- The initial solutions for the equations claim that each
        -- instance has an empty context; this solution is certainly
        -- in canonical form.
    initial_solutions :: [ThetaType]
    initial_solutions = [ [] | _ <- infer_specs ]

    ------------------------------------------------------------------
        -- iterate_deriv calculates the next batch of solutions,
        -- compares it with the current one; finishes if they are the
        -- same, otherwise recurses with the new solutions.
        -- It fails if any iteration fails
    iterate_deriv :: Int -> [ThetaType] -> TcM [DerivSpec ThetaType]
    iterate_deriv n current_solns
      | n > 20  -- Looks as if we are in an infinite loop
                -- This can happen if we have -XUndecidableInstances
                -- (See TcSimplify.tcSimplifyDeriv.)
      = pprPanic "solveDerivEqns: probable loop"
                 (vcat (map pprDerivSpec infer_specs) $$ ppr current_solns)
      | otherwise
      = do {      -- Extend the inst info from the explicit instance decls
                  -- with the current set of solutions, and simplify each RHS
             inst_specs <- zipWithM newDerivClsInst current_solns infer_specs
           ; new_solns <- checkNoErrs $
                          extendLocalInstEnv inst_specs $
                          mapM gen_soln infer_specs

           ; if (current_solns `eqSolution` new_solns) then
                return [ spec { ds_theta = soln }
                       | (spec, soln) <- zip infer_specs current_solns ]
             else
                iterate_deriv (n+1) new_solns }

    eqSolution a b = eqListBy (eqListBy eqType) (canSolution a) (canSolution b)
       -- Canonicalise for comparison
       -- See Note [Deterministic simplifyInstanceContexts]
    canSolution = map (sortBy nonDetCmpType)
    ------------------------------------------------------------------
    gen_soln :: DerivSpec ThetaOrigin -> TcM ThetaType
    gen_soln (DS { ds_loc = loc, ds_tvs = tyvars
                 , ds_cls = clas, ds_tys = inst_tys, ds_theta = deriv_rhs })
      = setSrcSpan loc  $
        addErrCtxt (derivInstCtxt the_pred) $
        do { theta <- simplifyDeriv the_pred tyvars deriv_rhs
                -- checkValidInstance tyvars theta clas inst_tys
                -- Not necessary; see Note [Exotic derived instance contexts]

           ; traceTc "TcDeriv" (ppr deriv_rhs $$ ppr theta)
                -- Claim: the result instance declaration is guaranteed valid
                -- Hence no need to call:
                --   checkValidInstance tyvars theta clas inst_tys
           ; return theta }
      where
        the_pred = mkClassPred clas inst_tys

derivInstCtxt :: PredType -> MsgDoc
derivInstCtxt pred
  = text "When deriving the instance for" <+> parens (ppr pred)

{-
***********************************************************************************
*                                                                                 *
*            Simplify derived constraints
*                                                                                 *
***********************************************************************************
-}

-- | Given @instance (wanted) => C inst_ty@, simplify 'wanted' as much
-- as possible. Fail if not possible.
simplifyDeriv :: PredType -- ^ @C inst_ty@, head of the instance we are
                          -- deriving.  Only used for SkolemInfo.
              -> [TyVar]  -- ^ The tyvars bound by @inst_ty@.
              -> ThetaOrigin   -- ^ @wanted@ constraints, i.e. @['PredOrigin']@.
              -> TcM ThetaType -- ^ Needed constraints (after simplification),
                               -- i.e. @['PredType']@.
simplifyDeriv pred tvs theta
  = do { (skol_subst, tvs_skols) <- tcInstSkolTyVars tvs -- Skolemize
                -- The constraint solving machinery
                -- expects *TcTyVars* not TyVars.
                -- We use *non-overlappable* (vanilla) skolems
                -- See Note [Overlap and deriving]

       ; let skol_set  = mkVarSet tvs_skols
             skol_info = DerivSkol pred
             doc = text "deriving" <+> parens (ppr pred)
             mk_ct (PredOrigin t o t_or_k)
                 = newWanted o (Just t_or_k) (substTy skol_subst t)

       -- Generate the wanted constraints with the skolemized variables
       ; (wanted, tclvl) <- pushTcLevelM (mapM mk_ct theta)

       ; traceTc "simplifyDeriv inputs" $
         vcat [ pprTvBndrs tvs $$ ppr theta $$ ppr wanted, doc ]
       -- Simplify the constraints
       ; residual_wanted <- simplifyWantedsTcM wanted
            -- Result is zonked

       -- Split the resulting constraints into bad and good constraints,
       -- building an @unsolved :: WantedConstraints@ representing all
       -- the constraints we can't just shunt to the predicates.
       -- See Note [Exotic derived instance contexts]
       ; let residual_simple = wc_simple residual_wanted
             (bad, good) = partitionBagWith get_good residual_simple
             unsolved    = residual_wanted { wc_simple = bad }

                         -- See Note [Exotic derived instance contexts]

             get_good :: Ct -> Either Ct PredType
             get_good ct | validDerivPred skol_set p
                         , isWantedCt ct
                         = Right p
                          -- NB re 'isWantedCt': residual_wanted may contain
                          -- unsolved CtDerived and we stick them into the
                          -- bad set so that reportUnsolved may decide what
                          -- to do with them
                         | otherwise
                         = Left ct
                           where p = ctPred ct

       ; traceTc "simplifyDeriv outputs" $
         vcat [ ppr tvs_skols, ppr residual_simple, ppr good, ppr bad ]

       -- If we are deferring type errors, simply ignore any insoluble
       -- constraints.  They'll come up again when we typecheck the
       -- generated instance declaration
       ; defer <- goptM Opt_DeferTypeErrors
       ; (implic, _) <- buildImplicationFor tclvl skol_info tvs_skols [] unsolved
                   -- The buildImplicationFor is just to bind the skolems,
                   -- in case they are mentioned in error messages
                   -- See Trac #11347
       -- Report the (bad) unsolved constraints
       ; unless defer (reportAllUnsolved (mkImplicWC implic))


       -- Return the good unsolved constraints (unskolemizing on the way out.)
       ; let min_theta  = mkMinimalBySCs (bagToList good)
             subst_skol = zipTvSubst tvs_skols $ mkTyVarTys tvs
                          -- The reverse substitution (sigh)
       ; return (substTheta subst_skol min_theta) }

{-
Note [Overlap and deriving]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider some overlapping instances:
  data Show a => Show [a] where ..
  data Show [Char] where ...

Now a data type with deriving:
  data T a = MkT [a] deriving( Show )

We want to get the derived instance
  instance Show [a] => Show (T a) where...
and NOT
  instance Show a => Show (T a) where...
so that the (Show (T Char)) instance does the Right Thing

It's very like the situation when we're inferring the type
of a function
   f x = show [x]
and we want to infer
   f :: Show [a] => a -> String

BOTTOM LINE: use vanilla, non-overlappable skolems when inferring
             the context for the derived instance.
             Hence tcInstSkolTyVars not tcInstSuperSkolTyVars

Note [Exotic derived instance contexts]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In a 'derived' instance declaration, we *infer* the context.  It's a
bit unclear what rules we should apply for this; the Haskell report is
silent.  Obviously, constraints like (Eq a) are fine, but what about
        data T f a = MkT (f a) deriving( Eq )
where we'd get an Eq (f a) constraint.  That's probably fine too.

One could go further: consider
        data T a b c = MkT (Foo a b c) deriving( Eq )
        instance (C Int a, Eq b, Eq c) => Eq (Foo a b c)

Notice that this instance (just) satisfies the Paterson termination
conditions.  Then we *could* derive an instance decl like this:

        instance (C Int a, Eq b, Eq c) => Eq (T a b c)
even though there is no instance for (C Int a), because there just
*might* be an instance for, say, (C Int Bool) at a site where we
need the equality instance for T's.

However, this seems pretty exotic, and it's quite tricky to allow
this, and yet give sensible error messages in the (much more common)
case where we really want that instance decl for C.

So for now we simply require that the derived instance context
should have only type-variable constraints.

Here is another example:
        data Fix f = In (f (Fix f)) deriving( Eq )
Here, if we are prepared to allow -XUndecidableInstances we
could derive the instance
        instance Eq (f (Fix f)) => Eq (Fix f)
but this is so delicate that I don't think it should happen inside
'deriving'. If you want this, write it yourself!

NB: if you want to lift this condition, make sure you still meet the
termination conditions!  If not, the deriving mechanism generates
larger and larger constraints.  Example:
  data Succ a = S a
  data Seq a = Cons a (Seq (Succ a)) | Nil deriving Show

Note the lack of a Show instance for Succ.  First we'll generate
  instance (Show (Succ a), Show a) => Show (Seq a)
and then
  instance (Show (Succ (Succ a)), Show (Succ a), Show a) => Show (Seq a)
and so on.  Instead we want to complain of no instance for (Show (Succ a)).

The bottom line
~~~~~~~~~~~~~~~
Allow constraints which consist only of type variables, with no repeats.
-}
