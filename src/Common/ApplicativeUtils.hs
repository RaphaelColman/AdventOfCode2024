module Common.ApplicativeUtils where

liftResult :: (Applicative f) => (a -> b) -> (a -> f b)
liftResult fun = pure . fun
