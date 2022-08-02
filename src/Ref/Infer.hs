{-# LANGUAGE FlexibleContexts #-}

module Ref.Infer where

import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.IORef
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Ref.Type
import Syntax

type Env = Map String Scheme

type Error = String

type InferM a = ReaderT Env (ExceptT Error IO) a

runInfer :: InferM Type -> IO (Either Error Scheme)
runInfer m = do
  a <- runExceptT (runReaderT m mempty)
  case a of
    Right t -> fmap Right $ generalize mempty =<< derefType t
    Left err -> pure $ Left err

infer :: Exp -> InferM Type
infer (Var x) = do
  env <- ask
  case Map.lookup x env of
    Nothing -> throwError "undefined variable"
    Just t -> instantiate t
infer (Const Int {}) = return (TyApp IntC [])
infer (Const Bool {}) = return (TyApp BoolC [])
infer (App e1 e2) = do
  t1 <- infer e1
  t2 <- infer e2
  retTy <- newTyMeta
  unify t1 (TyApp ArrowC [t2, retTy])
  return retTy
infer (Lam x e) = do
  xTy <- newTyMeta
  eTy <- local (Map.insert x (Forall [] xTy)) $ infer e
  return (TyApp ArrowC [xTy, eTy])
infer (Let x e1 e2) = do
  t1 <- infer e1
  env <- ask
  scheme <- generalize env t1
  local (Map.insert x scheme) $ infer e2

newTyMeta :: MonadIO m => m Type
newTyMeta = TyMeta <$> liftIO (newIORef Nothing)

unify :: (MonadIO m, MonadError Error m) => Type -> Type -> m ()
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
        then throwError "unify error"
        else liftIO $ writeIORef r (Just t2)
unify t1 (TyMeta r) = unify (TyMeta r) t1
unify _ _ = throwError "unify error"

occurCheck :: MonadIO m => IORef (Maybe Type) -> Type -> m Bool
occurCheck r1 (TyApp _ ts) = or <$> mapM (occurCheck r1) ts
occurCheck r1 (TyMeta r2)
  | r1 == r2 = return True
  | otherwise = do
    mt <- liftIO $ readIORef r2
    case mt of
      Nothing -> return False
      Just t -> occurCheck r1 t

derefType :: MonadIO m => Type -> m Type
derefType (TyMeta r) = do
  mt <- liftIO $ readIORef r
  case mt of
    Nothing -> return (TyMeta r)
    Just t -> derefType t
derefType (TyApp c ts) = TyApp c <$> mapM derefType ts

derefScheme :: MonadIO f => Scheme -> f Scheme
derefScheme (Forall as t) = Forall as <$> derefType t

deref :: MonadIO m => Env -> m Env
deref = mapM derefScheme

generalize :: MonadIO m => Env -> Type -> m Scheme
generalize env t = do
  fvs <- freevars env t
  Forall fvs <$> derefType t

instantiate :: MonadIO m => Scheme -> m Type
instantiate (Forall fvs t) = do
  as <- mapM (const newTyMeta) fvs
  forM_ (zip fvs as) $ \(f, a) ->
    liftIO $ writeIORef f (Just a)
  t' <- derefType t
  forM_ fvs $ \f ->
    liftIO $ writeIORef f Nothing
  return t'

freevars :: MonadIO m => Env -> Type -> m [IORef (Maybe Type)]
freevars env t = do
  env' <- deref env
  t' <- derefType t
  return $ fv t' \\ nub (foldr (union . fv') [] (Map.elems env'))

fv :: Type -> [IORef (Maybe Type)]
fv (TyMeta r) = [r]
fv (TyApp _ ts) = nub (foldr (union . fv) [] ts)

fv' :: Scheme -> [IORef (Maybe Type)]
fv' (Forall as t) = fv t \\ as
