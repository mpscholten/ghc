#!/bin/bash
# Benchmark script to compare two-phase vs three-phase interface generation
# Run from the ghc6 directory

set -e

echo "=== GHC Three-Phase Interface Generation Benchmark ==="
echo ""

# Check that we have a built stage0 GHC (which includes our three-phase changes)
if [ ! -f "_build/stage0/bin/ghc" ]; then
    echo "Error: Stage0 GHC not found. Please build with ./hadrian/build stage1 first."
    exit 1
fi

# stage0 GHC is the compiler built from our modified sources
GHC="_build/stage0/bin/ghc"
TMPDIR=$(mktemp -d)

echo "Creating test modules in $TMPDIR..."

# Create a chain of modules that benefit from parallel compilation
# A <- B <- C <- D (C and D can potentially start earlier with three-phase)
cat > "$TMPDIR/A.hs" << 'EOF'
module A where

data Foo = Foo Int String
  deriving (Show, Eq)

fooValue :: Foo
fooValue = Foo 42 "hello"

class MyClass a where
  myMethod :: a -> Int

instance MyClass Foo where
  myMethod (Foo n _) = n

processFoo :: Foo -> String
processFoo (Foo n s) = s ++ show n
EOF

cat > "$TMPDIR/B.hs" << 'EOF'
module B where

import A

data Bar = Bar Foo Foo
  deriving (Show, Eq)

makeBar :: Bar
makeBar = Bar fooValue fooValue

extractFirst :: Bar -> Foo
extractFirst (Bar f _) = f

instance MyClass Bar where
  myMethod (Bar f1 f2) = myMethod f1 + myMethod f2
EOF

cat > "$TMPDIR/C.hs" << 'EOF'
module C where

import A
import B

data Baz = Baz Bar [Foo]
  deriving (Show, Eq)

makeBaz :: Int -> Baz
makeBaz n = Baz makeBar (replicate n fooValue)

bazSize :: Baz -> Int
bazSize (Baz bar foos) = myMethod bar + length foos
EOF

cat > "$TMPDIR/D.hs" << 'EOF'
module D where

import A
import B
import C

data Qux = Qux Baz Baz
  deriving (Show, Eq)

makeQux :: Qux
makeQux = Qux (makeBaz 3) (makeBaz 5)

totalSize :: Qux -> Int
totalSize (Qux b1 b2) = bazSize b1 + bazSize b2
EOF

cat > "$TMPDIR/Main.hs" << 'EOF'
module Main where

import A
import B
import C
import D

main :: IO ()
main = do
  putStrLn $ "Foo: " ++ show fooValue
  putStrLn $ "Bar: " ++ show makeBar
  putStrLn $ "Baz: " ++ show (makeBaz 2)
  putStrLn $ "Qux: " ++ show makeQux
  putStrLn $ "Total: " ++ show (totalSize makeQux)
EOF

echo ""
echo "Compiling with -j1 (baseline - no parallelism)..."
rm -f "$TMPDIR"/*.hi "$TMPDIR"/*.o "$TMPDIR/Main"
time $GHC -j1 -i"$TMPDIR" -outputdir "$TMPDIR" -o "$TMPDIR/Main" "$TMPDIR/Main.hs" 2>&1

echo ""
echo "Compiling with -j4 (parallel compilation)..."
rm -f "$TMPDIR"/*.hi "$TMPDIR"/*.o "$TMPDIR/Main"
time $GHC -j4 -i"$TMPDIR" -outputdir "$TMPDIR" -o "$TMPDIR/Main" "$TMPDIR/Main.hs" 2>&1

echo ""
echo "Running the compiled program to verify correctness..."
"$TMPDIR/Main"

echo ""
echo "Cleaning up..."
rm -rf "$TMPDIR"

echo ""
echo "=== Benchmark Complete ==="
echo ""
echo "Note: The three-phase optimization benefits are most visible in larger projects"
echo "with many interdependent modules. For a more comprehensive benchmark, try:"
echo "  time ./hadrian/build -j8 stage1:lib:ghc-internal"
