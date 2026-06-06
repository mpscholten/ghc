section: template-haskell
synopsis:
  ``addTopDecls`` now accepts more top-level declaration forms.
description: {
  Declarations queued with Template Haskell's ``addTopDecls`` were previously
  restricted to value bindings, type signatures, annotations, and foreign
  imports; any other form was rejected with the ``InvalidTopDecl``
  (``GHC-52886``) diagnostic.

  That restriction has been relaxed. Splices may now queue type, class,
  instance, data, newtype, type synonym, type/data family, and data family
  instance declarations, including records. Names introduced by these
  declarations – data constructors, record field selectors, class methods,
  and so on – are available to the splice result when the corresponding
  binders use exact ``Name``\ s built with ``newName``. This is particularly
  useful for quasi-quoters that want to generate a fresh data type and then
  return an expression of that type.

  The ``GHC-52886`` diagnostic code has been retired.
}
