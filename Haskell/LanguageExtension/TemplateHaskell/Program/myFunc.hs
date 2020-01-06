{-# LANGUAGE TemplateHaskell #-}
-- You have to need in ghci, you can set TemplateHaskell like this.
-- >:set -XTemplateHaskell
--
-- and use this program.
--
-- > $(myFunc) 2
-- 3
-- > $myFunc 3
-- 4
--
import Language.Haskell.TH

myFunc :: Q Exp
myFunc = do
  x <- newName "x"
  return $ LamE
    [VarP x]
    (InfixE (Just (VarE x)) (VarE '(+)) (Just (LitE (IntegerL 1))))
