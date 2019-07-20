{-# LANGUAGE TupleSections #-}
module Ref.Infer where

import           Control.Monad.IO.Class
import           Control.Monad.State
import           Data.IORef
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Ref.Type
import           Syntax

type Env = Map String Type

type InferM a = StateT Env IO a

runInfer :: InferM Type -> IO (Type, Env)
runInfer m = do
  (a, env) <- runStateT m mempty
  (,) <$> derefType a <*> deref env

infer :: Exp -> InferM Type
infer (Var x)       = do
  mt <- gets (Map.lookup x)
  case mt of
    Nothing -> error "undefined variable"
    Just t  -> return t
infer (Const IntC{}) = return (TyApp IntCon [])
infer (Const BoolC{}) = return (TyApp BoolCon [])
infer (App e1 e2)   = do
  t1 <- infer e1
  t2 <- infer e2
  retTy <- newTyMeta
  unify t1 (TyApp ArrowC [t2, retTy])
  return retTy
infer (Lam x e)     = do
  xTy <- newTyMeta

  env <- get
  modify (Map.insert x xTy)
  eTy <- infer e
  put env

  return (TyApp ArrowC [xTy, eTy])
infer (Let x e1 e2) = do
  env <- get
  t1 <- infer e1
  put env

  modify (Map.insert x t1)
  infer e2

newTyMeta :: MonadIO m => m Type
newTyMeta = TyMeta <$> liftIO (newIORef Nothing)

unify :: MonadIO m => Type -> Type -> m ()
unify (TyApp c1 ts1) (TyApp c2 ts2)
  | c1 == c2 = mapM_ (uncurry unify) (zip ts1 ts2)
unify (TyMeta r1) (TyMeta r2)
  | r1 == r2 = return ()
unify (TyMeta r) t2 = do
  mt <- liftIO $ readIORef r
  case mt of
    Just t1 -> unify t1 t2
    Nothing -> do
      isOccur <- occurCheck r t2
      if isOccur
        then error "unify error"
        else liftIO $ writeIORef r (Just t2)
unify t1 (TyMeta r) = unify (TyMeta r) t1
unify _ _ = error "unify error"

occurCheck :: MonadIO m => IORef (Maybe Type) -> Type -> m Bool
occurCheck r1 (TyApp _ ts) = or <$> mapM (occurCheck r1) ts
occurCheck r1 (TyMeta r2)
  | r1 == r2 = return True
  | otherwise = do
      mt <- liftIO $ readIORef r2
      case mt of
        Nothing -> return False
        Just t  -> occurCheck r1 t

derefType :: MonadIO m => Type -> m Type
derefType (TyMeta r) = do
  mt <- liftIO $ readIORef r
  case mt of
    Nothing -> return (TyMeta r)
    Just t  -> derefType t
derefType (TyApp c ts) = TyApp c <$> mapM derefType ts

deref :: MonadIO m => Env -> m Env
deref = mapM derefType
