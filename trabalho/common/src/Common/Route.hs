{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Common.Route where

{- -- You will probably want these imports for composing Encoders.
import Prelude hiding (id, (.))
import Control.Category
-}
import Data.Function

import Data.Text (Text, unpack)
import Data.Functor.Identity

import Obelisk.Route
import Obelisk.Route.TH

data BackendRoute :: * -> * where
  -- | Used to handle unparseable routes.
  BackendRoute_Produto :: BackendRoute ()
  BackendRoute_Cliente :: BackendRoute ()
  BackendRoute_Armazem :: BackendRoute ()
  BackendRoute_EditarP :: BackendRoute Int
  BackendRoute_BuscarP :: BackendRoute Int
  BackendRoute_ListarP :: BackendRoute ()
  BackendRoute_EditarC :: BackendRoute Int
  BackendRoute_BuscarC :: BackendRoute Int
  BackendRoute_ListarC :: BackendRoute ()
  BackendRoute_EditarA :: BackendRoute Int
  BackendRoute_BuscarA :: BackendRoute Int
  BackendRoute_ListarA :: BackendRoute () 
  
  
  
  BackendRoute_Missing :: BackendRoute ()

  -- You can define any routes that will be handled specially by the backend here.
  -- i.e. These do not serve the frontend, but do something different, such as serving static files.

data FrontendRoute :: * -> * where
  FrontendRoute_Main :: FrontendRoute ()
  -- This type is used to define frontend routes, i.e. ones for which the backend will serve the frontend.

checkedFullRouteEncoder
  :: Encoder Identity Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
checkedFullRouteEncoder = checkEncoder fullRouteEncoder & \case
  Left err -> error $ unpack err
  Right encoder -> encoder  
  
fullRouteEncoder
  :: Encoder (Either Text) Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
fullRouteEncoder = mkFullRouteEncoder
  (FullRoute_Backend BackendRoute_Missing :/ ())
  (\case
      BackendRoute_Missing -> PathSegment "missing" $ unitEncoder mempty
      BackendRoute_Produto -> PathSegment "produto" $ unitEncoder mempty
      BackendRoute_Cliente -> PathSegment "cliente" $ unitEncoder mempty
      BackendRoute_Armazem -> PathSegment "armazem" $ unitEncoder mempty
      BackendRoute_BuscarC -> PathSegment "buscarc" $ readShowEncoder
      BackendRoute_EditarC -> PathSegment "editarc" $ readShowEncoder
      BackendRoute_ListarC -> PathSegment "listarc" $ unitEncoder mempty
      BackendRoute_BuscarA -> PathSegment "buscara" $ readShowEncoder
      BackendRoute_EditarA -> PathSegment "editara" $ readShowEncoder
      BackendRoute_ListarA -> PathSegment "listara" $ unitEncoder mempty
      BackendRoute_BuscarP -> PathSegment "buscarp" $ readShowEncoder
      BackendRoute_EditarP -> PathSegment "editarp" $ readShowEncoder
      BackendRoute_ListarP -> PathSegment "listarp" $ unitEncoder mempty)
      
      
      
  (\case
      FrontendRoute_Main -> PathEnd $ unitEncoder mempty)

concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRoute
  ]
