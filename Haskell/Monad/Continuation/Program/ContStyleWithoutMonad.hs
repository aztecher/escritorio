import Prelude hiding (div)

add :: Num a => a -> a -> a
add x y = x + y

div :: Fractional a => a -> a -> a
div x y = x / y

average3 :: Fractional a => a -> a -> a -> a
average3 x y z =
  let xPlusY = add x y
      xPlusYPlusZ = add xPlusY z
      average = div xPlusYPlusZ 3
  in average

-- 型を見てみる.
-- 通常2つの値をとり, さらに関数実行後の処理をとり,
-- 通常の2つの値から関数を実行したのち, その結果を引数で受け取る処理に渡し, 最終的な結果を得る.
--
-- * usage example : 加算演算後, 文字列にする
-- ghci> addCPS 1 2 (\x -> show x)
-- "3"
addCPS :: Num a => a -> a -> (a -> result) -> result
addCPS x y cont = cont (x + y)

divCPS :: Fractional a => a -> a -> (a -> result) -> result
divCPS x y cont = cont (x / y)


average3CPS :: Fractional a => a -> a -> a -> (a -> result) -> result
average3CPS x y z cont =
    addCPS x y $ \xPlusY ->
    addCPS xPlusY z $ \xPlusYPlusZ ->
    divCPS xPlusYPlusZ 3 $ \average ->
    cont average
