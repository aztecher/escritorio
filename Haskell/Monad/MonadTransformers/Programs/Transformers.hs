module Transformers where

import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import Data.Maybe
import qualified Data.Map as Map

type Name = String
data Exp = Lit Integer
         | Var Name
         | Plus Exp Exp
         | Abs Name Exp
         | App Exp Exp
         deriving (Show)
data Value = IntVal Integer
           | FunVal Env Name Exp
           deriving (Show)
type Env = Map.Map Name Value

eval0 :: Env -> Exp -> Value
eval0 env (Lit i) = IntVal i
eval0 env (Var n) = fromJust (Map.lookup n env)
eval0 env (Plus e1 e2) = let IntVal i1 = eval0 env e1
                             IntVal i2 = eval0 env e2
                         in IntVal (i1 + i2)
eval0 env (Abs n e) = FunVal env n e
eval0 env (App e1 e2) = let val1 = eval0 env e1
                            val2 = eval0 env e2
                        in case val1 of
                          FunVal env' n body -> eval0 (Map.insert n val2 env') body

exampleExp :: Exp
exampleExp = Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))

failureExampleExp :: Exp
failureExampleExp = Lit 12 `Plus` (App (Abs "x" (Var "y")) (Lit 4 `Plus` Lit 2))

testEval0 :: IO ()
testEval0 = putStrLn . show $ eval0 Map.empty exampleExp

--

type Eval1 a = Identity a

runEval1 :: Eval1 a -> a
runEval1 ev = runIdentity ev

eval1 :: Env -> Exp -> Eval1 Value
eval1 env (Lit i) = return $ IntVal i
eval1 env (Var n) = maybe (fail ("[Catch Error] undefined variable: " ++ n)) return $ Map.lookup n env
eval1 env (Plus e1 e2) = do IntVal i1 <- eval1 env e1
                            IntVal i2 <- eval1 env e2
                            return $ IntVal (i1 + i2)
eval1 env (Abs n e) = return $ FunVal env n e
eval1 env (App e1 e2) = do val1 <- eval1 env e1
                           val2 <- eval1 env e2
                           case val1 of
                             FunVal env' n body ->
                               eval1 (Map.insert n val2 env') body

testEval1 :: IO ()
testEval1 = putStrLn . show $ runEval1 (eval1 Map.empty exampleExp)

failureTestEval1 :: IO ()
failureTestEval1 = putStrLn . show $ runEval1 (eval1 Map.empty failureExampleExp)

--

-- eval1 の型を一般化してみる.
-- これは, return と do記法に隠された (>>=) 以外には,
-- どのようなモナド操作も行っていないことにより可能である.
-- つまり, これはどのようなモナドの文脈でも使用できる.
--
-- 試しに, ghciで
--
-- runEval1 (eval1 Map.empty exampleExp)
--
-- の代わりに
--
-- eval1' Map.empty exampleExp
--
-- を実行すると, 実行できる.
-- これはインタープリタは内部的にprint関数を使っているが,
-- IOモナドの内部でこの式が実装できるためである.
eval1' :: Monad m => Env -> Exp -> m Value
eval1' env (Lit i) = return $ IntVal i
eval1' env (Var n) = maybe (fail ("[Catch Error] undefined variable: " ++ n)) return $ Map.lookup n env
eval1' env (Plus e1 e2) = do IntVal i1 <- eval1' env e1
                             IntVal i2 <- eval1' env e2
                             return $ IntVal (i1 + i2)
eval1' env (Abs n e) = return $ FunVal env n e
eval1' env (App e1 e2) = do val1 <- eval1' env e1
                            val2 <- eval1' env e2
                            case val1 of
                              FunVal env' n body ->
                                eval1' (Map.insert n val2 env') body

--

type Eval2 a = ErrorT String Identity a

runEval2 :: Eval2 a -> Either String a
runEval2 ev = runIdentity (runErrorT ev)

eval2a :: Env -> Exp -> Eval2 Value
eval2a env (Lit i) = return $ IntVal i
eval2a env (Var n) = maybe (fail ("[Catch Error] undefined variable: " ++ n)) return $ Map.lookup n env
eval2a env (Plus e1 e2) = do IntVal i1 <- eval2a env e1
                             IntVal i2 <- eval2a env e2
                             return $ IntVal (i1 + i2)
eval2a env (Abs n e) = return $ FunVal env n e
eval2a env (App e1 e2) = do val1 <- eval2a env e1
                            val2 <- eval2a env e2
                            case val1 of
                              FunVal env' n body
                                -> eval2a (Map.insert n val2 env') body

testEval2a :: IO ()
testEval2a = putStrLn . show $ runEval2 (eval2a Map.empty exampleExp)

--

eval2b :: Env -> Exp -> Eval2 Value
eval2b env (Lit i) = return $ IntVal i
eval2b env (Var n) = maybe (fail ("[Catch Error] undefined variable: " ++ n)) return $ Map.lookup n env
eval2b env (Plus e1 e2) = do e1' <- eval2b env e1
                             e2' <- eval2b env e2
                             case (e1', e2') of
                               (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
                               _ -> throwError "type error"
eval2b env (Abs n e) = return $ FunVal env n e
eval2b env (App e1 e2) = do val1 <- eval2b env e1
                            val2 <- eval2b env e2
                            case val1 of
                              FunVal env' n body -> eval2b (Map.insert n val2 env') body
                              _ -> throwError "type error"

testEval2b :: IO ()
testEval2b = putStrLn . show $ runEval2 (eval2b Map.empty exampleExp)

testEval2bFailure :: IO ()
testEval2bFailure = putStrLn . show $ runEval2 (eval2b Map.empty failureExampleExp)

testEval2bFailure2 :: IO ()
testEval2bFailure2 = putStrLn . show $ runEval2 (eval2b Map.empty (Plus (Lit 1) (Abs "x" (Var "x"))))

--

eval2c :: Env -> Exp -> Eval2 Value
eval2c env (Lit i) = return $ IntVal i
eval2c env (Var n) = maybe (fail ("undefined variable: " ++ n)) return $ Map.lookup n env
eval2c env (Plus e1 e2) = do IntVal i1 <- eval2c env e1
                             IntVal i2 <- eval2c env e2
                             return $ IntVal (i1 + i2)
eval2c env (Abs n e) = return $ FunVal env n e
eval2c env (App e1 e2) = do FunVal env' n body <- eval2c env e1
                            val2 <- eval2c env e2
                            eval2c (Map.insert n val2 env') body

testEval2c :: IO ()
testEval2c = putStrLn . show $ runEval2 (eval2c Map.empty exampleExp)

testEval2cFailure :: IO ()
testEval2cFailure = putStrLn . show $ runEval2 (eval2c Map.empty failureExampleExp)

testEval2cFailure2 :: IO ()
testEval2cFailure2 = putStrLn . show $ runEval2 (eval2c Map.empty (Plus (Lit 1) (Abs "x" (Var "x"))))

--

eval2 :: Env -> Exp -> Eval2 Value
eval2 env (Lit i) = return $ IntVal i
eval2 env (Var n) = case Map.lookup n env of
                      Nothing -> throwError ("unbounded variable: " ++ n)
                      Just val -> return val
eval2 env (Plus e1 e2) = do e1' <- eval2 env e1
                            e2' <- eval2 env e2
                            case (e1', e2') of
                              (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
                              _ -> throwError "type error in addition"
eval2 env (Abs n e) = return $ FunVal env n e
eval2 env (App e1 e2) = do val1 <- eval2 env e1
                           val2 <- eval2 env e2
                           case val1 of
                             FunVal env' n body -> eval2 (Map.insert n val2 env') body
                             _ -> throwError "type error in application"

testEval2 :: IO ()
testEval2 = putStrLn . show $ runEval2 (eval2 Map.empty exampleExp)

testEval2Failure :: IO ()
testEval2Failure = putStrLn . show $ runEval2 (eval2 Map.empty failureExampleExp)

testEval2Failure2 :: IO ()
testEval2Failure2 = putStrLn . show $ runEval2 (eval2 Map.empty (Plus (Lit 1) (Abs "x" (Var "x"))))

--

type Eval3 a = ReaderT Env (ErrorT String Identity) a

runEval3 :: Env -> Eval3 a -> Either String a
runEval3 env ev = runIdentity (runErrorT (runReaderT ev env))

eval3 :: Exp -> Eval3 Value
eval3 (Lit i) = return $ IntVal i
eval3 (Var n) = do env <- ask
                   case Map.lookup n env of
                     Nothing -> throwError ("unbounded variables: " ++ n)
                     Just val -> return val
eval3 (Plus e1 e2) = do e1' <- eval3 e1
                        e2' <- eval3 e2
                        case (e1', e2') of
                          (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
                          _ -> throwError "type error in addition"
eval3 (Abs n e) = do env <- ask
                     return $ FunVal env n e
eval3 (App e1 e2) = do val1 <- eval3 e1
                       val2 <- eval3 e2
                       case val1 of
                         FunVal env' n body -> local (const (Map.insert n val2 env')) (eval3 body)
                         _ -> throwError "type error in application"


testEval3 :: IO ()
testEval3 = putStrLn . show $ runEval3 Map.empty (eval3 exampleExp)

testEval3Failure :: IO ()
testEval3Failure = putStrLn . show $ runEval3 Map.empty (eval3 failureExampleExp)

testEval3Failure2 :: IO ()
testEval3Failure2 = putStrLn . show $ runEval3 Map.empty (eval3 (Plus (Lit 1) (Abs "x" (Var "x"))))

--

type Eval4 a = ReaderT Env (ErrorT String (StateT Integer Identity)) a

runEval4 :: Env -> Integer -> Eval4 a -> (Either String a, Integer)
runEval4 env st ev = runIdentity (runStateT (runErrorT (runReaderT ev env)) st)

tick :: (Num s, MonadState s m) => m ()
tick = do st <- get
          put (st + 1)

eval4 :: Exp -> Eval4 Value
eval4 (Lit i) = do tick
                   return $ IntVal i
eval4 (Var n) = do tick
                   env <- ask
                   case Map.lookup n env of
                     Nothing -> throwError ("unbound variable: " ++ n)
                     Just val -> return val
eval4 (Plus e1 e2) = do tick
                        e1' <- eval4 e1
                        e2' <- eval4 e2
                        case (e1', e2') of
                          (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
                          _ -> throwError "type error in addition"
eval4 (Abs n e) = do tick
                     env <- ask
                     return $ FunVal env n e
eval4 (App e1 e2) = do tick
                       val1 <- eval4 e1
                       val2 <- eval4 e2
                       case val1 of
                         FunVal env' n body -> local (const (Map.insert n val2 env')) (eval4 body)
                         _ -> throwError "type error in application"

testEval4 :: IO ()
testEval4 = putStrLn . show $ runEval4 Map.empty 0 (eval4 exampleExp)

testEval4Failure :: IO ()
testEval4Failure = putStrLn . show $ runEval4 Map.empty 0 (eval4 failureExampleExp)

testEval4Failure2 :: IO ()
testEval4Failure2 = putStrLn . show $ runEval4 Map.empty 0 (eval4 (Plus (Lit 1) (Abs "x" (Var "x"))))

--

type Eval5 a = ReaderT Env (ErrorT String (WriterT [String] (StateT Integer Identity))) a


runEval5 :: Env -> Integer -> Eval5 a -> ((Either String a, [String]), Integer)
runEval5 env st ev = runIdentity (runStateT (runWriterT (runErrorT (runReaderT ev env))) st)


eval5 :: Exp -> Eval5 Value
eval5 (Lit i) = do tick
                   return $ IntVal i
eval5 (Var n) = do tick
                   tell [n]
                   env <- ask
                   case Map.lookup n env of
                     Nothing -> throwError ("unbound variable: " ++ n)
                     Just val -> return val
eval5 (Plus e1 e2) = do tick
                        e1' <- eval5 e1
                        e2' <- eval5 e2
                        case (e1', e2') of
                          (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
                          _ -> throwError "type error in additin"
eval5 (Abs n e) = do tick
                     env <- ask
                     return $ FunVal env n e
eval5 (App e1 e2) = do tick
                       val1 <- eval5 e1
                       val2 <- eval5 e2
                       case val1 of
                         FunVal env' n body -> local (const (Map.insert n val2 env')) (eval5 body)
                         _ -> throwError "type error in application"

testEval5 :: IO ()
testEval5 = putStrLn . show $ runEval5 Map.empty 0 (eval5 exampleExp)

testEval5Failure :: IO ()
testEval5Failure = putStrLn . show $ runEval5 Map.empty 0 (eval5 failureExampleExp)

testEval5Failure2 :: IO ()
testEval5Failure2 = putStrLn . show $ runEval5 Map.empty 0 (eval5 (Plus (Lit 1) (Abs "x" (Var "x"))))


-- 

type Eval6 a = ReaderT Env (ErrorT String (WriterT [String] (StateT Integer IO))) a


runEval6 :: Env -> Integer -> Eval6 a -> IO ((Either String a, [String]), Integer)
runEval6 env st ev = runStateT (runWriterT (runErrorT (runReaderT ev env))) st


eval6 :: Exp -> Eval6 Value
eval6 (Lit i) = do tick
                   liftIO $ print i
                   return $ IntVal i
eval6 (Var n) = do tick
                   tell [n]
                   env <- ask
                   case Map.lookup n env of
                     Nothing -> throwError ("unbound variable: " ++ n)
                     Just val -> return val
eval6 (Plus e1 e2) = do tick
                        e1' <- eval6 e1
                        e2' <- eval6 e2
                        case (e1', e2') of
                          (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
                          _ -> throwError "type error in addition"
eval6 (Abs n e) = do tick
                     env <- ask
                     return $ FunVal env n e
eval6 (App e1 e2) = do tick
                       val1 <- eval6 e1
                       val2 <- eval6 e2
                       case val1 of
                         FunVal env' n body -> local (const (Map.insert n val2 env')) (eval6 body)
                         _ -> throwError "type error in application"


testEval6 :: IO ()
testEval6 = putStrLn . show =<< runEval6 Map.empty 0 (eval6 exampleExp)

testEval6Failure :: IO ()
testEval6Failure = putStrLn . show =<< runEval6 Map.empty 0 (eval6 failureExampleExp)

testEval6Failure2 :: IO ()
testEval6Failure2 = putStrLn . show =<< runEval6 Map.empty 0 (eval6 (Plus (Lit 1) (Abs "x" (Var "x"))))
