module Utils ( justOr ) where

justOr :: a -> Maybe a -> a
justOr _ (Just x) = x
justOr fault Nothing = fault
