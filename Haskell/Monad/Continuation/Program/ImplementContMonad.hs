import Control.Monad (liftM, ap)

newtype Cont result a = Cont {
  runCont :: (a -> result) -> result
}

returnCont :: a -> Cont result a
returnCont a = Cont $ \cont -> cont a

bindCont :: Cont result a -> (a -> Cont result b) -> Cont result b
bindCont (Cont xCPS) f = Cont $ \cont ->
  xCPS $ \xVal ->
  let Cont yCPS = f xVal
  in yCPS $ \yVal ->
    cont yVal


instance Monad (Cont result) where
  return = returnCont
  (>>=) = bindCont

instance Applicative (Cont result) where
  pure = return
  (<*>) = ap

instance Functor (Cont result) where
  fmap = liftM

-- define average3CPS by using Cont Monad

addCont :: Num a => a -> a -> Cont result a
addCont x y = return $ x + y

divCont :: Fractional a => a -> a -> Cont result a
divCont x y = return $ x / y

average3Cont :: Fractional a => a -> a -> a -> Cont result a
average3Cont x y z = do
  xPlusY <- addCont x y
  xPlusYPlusZ <- addCont xPlusY z
  let average = xPlusYPlusZ / 3
  return average
