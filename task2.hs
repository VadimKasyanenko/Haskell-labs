import Data.Function (fix)

infiniteFibonacciHelper = \f -> 1 : 1 : zipWith (+) f (tail f)

infiniteFibonacci = fix infiniteFibonacciHelper

