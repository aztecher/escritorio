{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH

myFunc :: Q Exp
myFunc = [| \x -> x + 1 |]

add2 :: Q Exp
add2 = [| $myFunc . $myFunc |]
