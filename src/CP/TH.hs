{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module CP.TH
  ( deriveAll
  , deriveAllT
  )
where

import           Control.Monad.Logger           ( MonadLogger )
import qualified Data.Set                      as Set
import           Language.Haskell.TH

import           CP.Class

nameList :: [Name]
nameList =
  [ ''MonadGetArgs
  , ''MonadLogger
  , ''MonadReadFile
  , ''MonadWriteFile
  ]

declareDeriveT :: Type -> Q Type -> Q [Dec]
declareDeriveT klass transformer = [d|
    deriving instance $(pure klass) m => $(pure klass) ($transformer m)
  |]

deriveAllT :: Name -> [Name] -> Q [Dec]
deriveAllT = applyAll declareDeriveT

declareDerive :: Type -> Q Type -> Q [Dec]
declareDerive klass typ = [d|
    deriving instance $(pure klass) $typ
  |]

deriveAll :: Name -> [Name] -> Q [Dec]
deriveAll = applyAll declareDerive

applyAll :: (Type -> Q Type -> Q [Dec]) -> Name -> [Name] -> Q [Dec]
applyAll fn typ (Set.fromList -> excluding) =
  concat
    <$> mapM
          (\klass -> if klass `Set.member` excluding
            then pure []
            else fn (ConT klass) (pure $ ConT typ)
          )
          nameList
