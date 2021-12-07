{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BlockArguments #-}
module Common.Api where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)
import Database.PostgreSQL.Simple 

newtype Nome = Nome Text deriving (Generic, ToJSON, FromJSON)

data Produto = Produto {
    nomeProduto :: Text,
    valorProduto :: Double,
    qtProduto :: Int
} deriving (Generic, ToJSON, FromJSON, ToRow, FromRow, Eq, Show)

data Cliente = Cliente {
    cpfCliente :: Int,
    nomeCliente :: Text,
    telCliente :: Int,
    estCliente :: Text,
    cidCliente :: Text,
    endCliente :: Text,
    endnumCliente :: Int
} deriving (Generic, ToJSON, FromJSON, ToRow, FromRow, Eq, Show)

data Armazem = Armazem {
    nomeArmazem :: Text,
    estArmazem :: Text,
    cidArmazem :: Text,
    tamArmazem :: Double
} deriving (Generic, ToJSON, FromJSON, ToRow, FromRow, Eq, Show)

