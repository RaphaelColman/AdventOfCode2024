module Common.MathUtils where
import Data.Ratio (denominator, numerator)

triangleX :: (Integral a) => a -> a
triangleX x = x * (x + 1) `div` 2

isInteger :: Rational -> Bool
isInteger x = denominator x == 1

rationalToInteger :: Rational -> Maybe Integer
rationalToInteger x = if isInteger x then Just $ numerator x else Nothing
