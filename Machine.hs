module Machine (execute) where

import Types
import qualified Data.Map as Map

lookup :: String -> Env -> Value
lookup s e = Map.findWithDefault (error $ "Unbound name: " ++ s ++ " in " ++ show e) s e

bind :: String -> Value -> Env -> Env
bind = Map.insert

makeClosure :: Env -> Code -> Value
makeClosure e cs = VClosure (e, cs)

execute :: Monad m => (Stack -> m Stack) -> (Env, Code) -> Stack -> m Stack
execute render (e, []) s = return s
execute render (e, c:cs) s = do (e2, s2) <- step (e, s, c)
                                execute render (e2, cs) s2
   where
      step (e, s, (PushConstant v)) = return (e, v:s)
      step (e, s, (PushClosure c)) = return (e, (makeClosure e c):s)
      step (e, s, (PushArray c)) = do s2 <- execute render (e, c) []
                                      return (e, (VArray s2):s)
      step (e, s, (Lookup name)) = return (e, (Machine.lookup name e):s)
      step (e, v:s, (Bind name)) = return ((bind name v e), s)
      step (e, s, (Invoke name f)) = return (e, f s)
      step (e, (VClosure c):s, Apply) = do s2 <- execute render c s
                                           return (e, s2)
      step (e, (VClosure c2):(VClosure c1):(VBool b):s, If) =
         do ss <- execute render (if b then c1 else c2) s
            return (e, ss)
      step (e, s, Render) = do s2 <- render s
                               return (e, s2)
      step (e, s, i) = error $ "Error in step:" ++ show s ++ show i