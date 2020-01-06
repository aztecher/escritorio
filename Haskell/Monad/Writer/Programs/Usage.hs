import Control.Monad.Writer

main = do
  print $ sumW [1..10]

sumW :: (Num a) => [a] -> (a, [a])
sumW xs = runWriter $ foldM addW 0 xs

addW :: (Num a) => a -> a -> Writer [a] a
addW p1 p2 = tell [r] >> return r
  where
    r = p1 + p2
