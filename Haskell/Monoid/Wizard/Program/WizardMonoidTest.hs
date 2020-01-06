import Data.Monoid ((<>))

prompt :: String -> IO (IO ())
prompt attribute = do
  putStrLn (attribute ++ "は?")
  x <- getLine
  return (putStrLn (attribute ++ ": " ++ x))

runWizard :: IO (IO a) -> IO a
runWizard request = do
  respond <- request
  respond

main :: IO ()
main = runWizard (prompt "名前" <> prompt "年齢")

mainPlus :: IO ()
mainPlus = runWizard (prompt "名前" <> prompt "年齢" <> prompt "好きな食べ物")

