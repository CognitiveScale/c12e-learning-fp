module HW1.WhileM
    ( whileM, whileMFoldL
    ) where

whileM :: Monad m => m () -> m Bool -> m ()
whileM action condition = whileMFoldL () (\_ -> ()) action condition

whileMSum :: (Monad m, Monoid b) => m b -> m Bool -> m b
whileMSum action condition = whileMFoldL mempty (\(b, a) -> mappend a b) action condition

whileMFoldL :: Monad m => b -> ((b, a) -> b) -> m a -> m Bool -> m b
whileMFoldL zero append action condition = iteratee zero where
  iteratee acc = condition >>= (executeIf ((fmap accumulate action) >>= iteratee) (pure acc)) where
    accumulate value = append (acc, value)
  executeIf ifTrue ifFalse predicate =
    if predicate then ifTrue else ifFalse