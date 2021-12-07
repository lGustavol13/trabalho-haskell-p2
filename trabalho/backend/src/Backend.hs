{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs, ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
module Backend where

import Common.Route
import Common.Api
import Obelisk.Backend
import Obelisk.Route
import Database.PostgreSQL.Simple 
import Snap.Core
import qualified Data.Aeson as A
import Data.Text
import Control.Monad.IO.Class (liftIO)
import Data.Aeson.Text

migrationCli :: Query
migrationCli = "CREATE TABLE IF NOT EXISTS cliente\
  \ (cpf PRIMARY KEY, nome TEXT NOT NULL, tel INTEGER NOT NULL, est TEXT NOT NULL, cid TEXT NOT NULL, end TEXT NOT NULL, endnum TEXT NOT NULL)" 

migrationProd :: Query
migrationProd = "CREATE TABLE IF NOT EXISTS produto\
  \ (id SERIAL PRIMARY KEY, nome TEXT NOT NULL, valor REAL NOT NULL, qt INTEGER NOT NULL)"

migrationArm :: Query
migrationArm = "CREATE TABLE INF NOT EXIST armazem\
  \ (id SERIAL PRIMARY KEY, nome TEXT NOT NULL, est TEXT NOT NULL, cid TEXT NOT NULL, tam REAL NOT NULL)"  
  
getConn :: ConnectInfo
getConn = ConnectInfo "ec2-18-235-192-50.compute-1.amazonaws.com" 
                      5432 
                      "zwuyirhfsdlpqo" 
                      "69771ee7c7af52a1ba248410715059544a34523171c67db8c2079b92b4a62bfc" 
                      "dd14efur7h7hbu"

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      dbcon <- connect getConn
      serve $ do
        \case
                BackendRoute_EditarA :/ pid -> do
                    armazem <- A.decode <$> readRequestBody 2000
                    case armazem of
                         Just arm -> do
                             liftIO $ do
                                 execute_ dbcon migrationArm
                                 execute dbcon "UPDATE armazem SET nome =?,est=?,cid=?,tam=? WHERE id=?" (nomeArmazem arm, estArmazem arm, cidArmazem arm, tamArmazem arm, pid)
                                 modifyResponse $ setResponseStatus 200 "OK"
                                 Nothing -> modifyResponse $ setResponseStatus 500 "ERROR"
                    BackendRoute_ListarA :/ () -> method GET do
                        res :: [Armazem] <- liftIO $ do
                                execute_ dbcon migrationArm
                                query_ dbcon "SELECT nome,est,cid,tam from armazem"
                                modifyResponse $ setResponseStatus 200 "OK"
                                writeLazyText (encodeToLazyText res)
                    BackendRoute_BuscarA :/ pid -> do
                        res :: [Armazem] <- liftIO $ do
                            execute_ dbcon migrationArm
                            query dbcon "SELECT nome,est,cid,tam from armazem where id=?" (Only (pid :: Int))
                        if res /= [] then do
                            modifyResponse $ setResponseStatus 200 "OK"
                            writeLazyText (encodeToLazyText (Prelude.head res))
                        else
                            modifyResponse $ setResponseStatus 404 "NOT FOUND"
                    BackendRoute_Armazem :/ () -> method POST do
                        armazem <- A.decode <$> readRequestBody 2000
                        case armazem of
                             Just arm -> do
                                 liftIO $ do
                                     execute_ dbcon migrationArm
                                     execute dbcon "INSERT INTO armazem(nome,est,cid,tam) VALUES (?,?,?,?)" (nomeArmazem arm, estArmazem arm, cidArmazem arm, tamArmazem arm)
                                     modifyResponse $ setResponseStatus 200 "OK"
                                     Nothing -> modifyResponse $ setResponseStatus 500 "ERRO"                    
                    BackendRoute_EditarC :/ pid -> do
                        cliente <- A.decode <$> readRequestBody 2000
                        case cliente of
                             Just cli -> do
                                 liftIO $ do
                                     execute_ dbcon migrationCli
                                     execute dbcon "UPDATE cliente SET nome=?,tel=?,est=?,cid=?,end=?,endnum=? WHERE cpf=?" (nomeCliente cli, telCliente cli, estCliente cli, cidCliente cli, endCliente cli, endnumCliente cli, pid)
                                     modifyResponse $ setResponseStatus 200 "OK"
                                     Nothing -> modifyResponse $ setResponseStatus 500 "ERROR"
                    BackendRoute_ListarC :/ () -> method GET do
                        res :: [Cliente] <- liftIO $ do
                                execute_ dbcon migrationCli
                                query_ dbcon "SELECT cpf,nome,tel,est,cid,end,endnum from cliente"
                                modifyResponse $ setResponseStatus 200 "OK"
                                writeLazyText (encodeToLazyText res)                        
                    BackendRoute_BuscarC :/ pid -> do
                        res :: [Cliente] <- liftIO $ do
                            execute_ dbcon migrationCli
                            query dbcon "SELECT cpf,nome,tel,est,cid,end,endnum from cliente where cpf=?" (Only (pid :: Int))
                        if res /= [] then do
                            modifyResponse $ setResponseStatus 200 "OK"
                            writeLazyText (encodeToLazyText (Prelude.head res))
                        else
                            modifyResponse $ setResponseStatus 404 "NOT FOUND"                        
                    BackendRoute_Cliente :/ () -> method POST do
                        cliente <- A.decode <$> readRequestBody 2000
                        case cliente of
                             Just cli -> do
                                 liftIO $ do
                                     execute_ dbcon migrationCli
                                     execute dbcon "INSERT INTO cliente(cpf,nome,tel,est,cid,end,endnum) VALUES (?,?,?,?,?,?,?)" (cpfCliente cli, nomeCliente cli, telCliente cli, estCliente cli, cidCliente cli, endCliente cli, endnumCliente cli)
                                     modifyResponse $ setResponseStatus 200 "OK"
                                     Nothing -> modifyResponse $ setResponseStatus 500 "ERRO"
                    BackendRoute_EditarP :/ pid -> method POST do
                        produto <- A.decode <$> readRequestBody 2000
                        case produto of
                             Just prod -> do
                                 liftIO $ do
                                     execute_ dbcon migrationProd
                                     execute dbcon "UPDATE produto SET nome =?,valor=?,qt=? WHERE id=?" (nomeProduto prod, valorProduto prod, qtProduto prod, pid)
                                     modifyResponse $ setResponseStatus 200 "OK"
                                     Nothing -> modifyResponse $ setResponseStatus 500 "ERRO"
                    BackendRoute_ListarP :/ () method GET -> do
                        res :: [Produto] <- liftIO $ do
                                execute_ dbcon migrationProd
                                query_ dbcon "SELECT nome,valor,qt from produto"
                                modifyResponse $ setResponseStatus 200 "OK"
                                writeLazyText (encodeToLazyText res)
                    BackendRoute_BuscarP :/ pid -> method GET do
                        res :: [Produto] <- liftIO $ do
                            execute_ dbcon migrationProd
                            query dbcon "SELECT nome, valor,qt from produto where id=?" (Only (pid :: Int))
                        if res /= [] then do
                            modifyResponse $ setResponseStatus 200 "OK"
                            writeLazyText (encodeToLazyText (Prelude.head res))
                        else
                            modifyResponse $ setResponseStatus 404 "NOT FOUND"
                    BackendRoute_Produto :/ () -> method POST do
                        produto <- A.decode <$> readRequestBody 2000
                        case produto of
                             Just prod -> do
                                 liftIO $ do
                                     execute_ dbcon migrationProd
                                     execute dbcon "INSERT INTO produto(nome,valor,qt) VALUES (?,?,?)" (nomeProduto prod, valorProduto prod, qtProduto prod)
                                     modifyResponse $ setResponseStatus 200 "OK"
                                     Nothing -> modifyResponse $ setResponseStatus 500 "ERRO"
            _ -> return ()
, _backend_routeEncoder = fullRouteEncoder
}
