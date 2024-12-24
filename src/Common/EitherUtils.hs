module Common.EitherUtils where

withLeft :: b -> Maybe a -> Either b a
withLeft _ (Just a) = Right a
withLeft b Nothing = Left b
