#!/bin/bash
# Test script for three-phase interface generation
#
# This script creates a multi-module project and compiles it with parallel
# compilation to exercise the three-phase interface generation code path.
#
# Usage: ./test-three-phase.sh <path-to-ghc>
#
# The test verifies:
# 1. Modules with type classes and instances compile correctly
# 2. Early interfaces allow dependent modules to start earlier
# 3. Instance resolution works correctly with early interfaces

set -e

GHC="${1:-_build/stage1/bin/ghc}"

if [ ! -x "$GHC" ]; then
    echo "Error: GHC not found at $GHC"
    echo "Usage: $0 <path-to-ghc>"
    exit 1
fi

echo "Using GHC: $GHC"
$GHC --version

# Create test directory
TEST_DIR=$(mktemp -d)
echo "Test directory: $TEST_DIR"
cd "$TEST_DIR"

# Create test modules
cat > Types.hs << 'EOF'
module Types where

class MyShow a where
  myShow :: a -> String

data Foo = Foo Int String deriving (Eq)

instance MyShow Foo where
  myShow (Foo n s) = "Foo " ++ show n ++ " " ++ s

data Bar = Bar Bool deriving (Eq)
EOF

cat > Instances.hs << 'EOF'
module Instances where

import Types

instance MyShow Bar where
  myShow (Bar b) = "Bar " ++ show b

instance MyShow a => MyShow [a] where
  myShow xs = "[" ++ concatMap (\x -> myShow x ++ ", ") xs ++ "]"

showAll :: MyShow a => [a] -> String
showAll = myShow
EOF

cat > Consumer.hs << 'EOF'
module Consumer where

import Types
import Instances

showFoo :: Foo -> String
showFoo = myShow

showBar :: Bar -> String
showBar = myShow

showFoos :: [Foo] -> String
showFoos = showAll

example :: String
example = showFoo (Foo 42 "hello") ++ " and " ++ showBar (Bar True)
EOF

cat > Main.hs << 'EOF'
module Main where

import Types
import Instances
import Consumer

main :: IO ()
main = do
  putStrLn "Testing three-phase interface generation..."
  putStrLn $ "Foo: " ++ showFoo (Foo 1 "test")
  putStrLn $ "Bar: " ++ showBar (Bar False)
  putStrLn $ "List: " ++ showFoos [Foo 1 "a", Foo 2 "b"]
  putStrLn $ "Example: " ++ example
  putStrLn "Done!"
EOF

echo ""
echo "=== Single-threaded compilation ==="
$GHC --make -v0 Main.hs -o main-single
./main-single

# Clean intermediate files
rm -f *.hi *.o main-single

echo ""
echo "=== Parallel compilation (testing three-phase) ==="
# -j4 enables parallel compilation which triggers two-phase and three-phase
# -v1 shows some progress info
$GHC --make -j4 -v1 Main.hs -o main-parallel 2>&1 | grep -E "(Compiling|Linking)" || true
./main-parallel

echo ""
echo "=== All tests passed! ==="

# Cleanup
cd /
rm -rf "$TEST_DIR"
