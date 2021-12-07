{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecursiveDo, BlockArguments #-}

module Frontend where

import Control.Monad
import Data.Maybe
import Text.Read (readMaybe)
import qualified Data.Text as T

import Control.Monad.Fix (MonadFix)
import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Route
import Obelisk.Generated.Static

import Reflex.Dom.Core

import Common.Api
import Common.Route

----------------

getPath :: R BackendRoute -> T.Text
getPath rt = renderBackendRoute checkedFullRouteEncoder rt


prodRequest :: Produto -> XhrRequest T.Text
prodRequest p = postJson (getPath (BackendRoute_Produto :/ ())) p

cliRequest :: Cliente -> XhrRequest T.Text
cliRequest c = postJson (getPath (BackendRoute_Cliente :/ ())) c

armRequest :: Armazem -> XhrRequest T.Text
armRequest a = postJson (getPath (BackendRoute_Armazem :/ ())) a

getListprodReq :: XhrRequest ()
getListprodReq = xhrRequest "GET" (getPath (BackendRoute_ListarP :/ ())) def

getListcliReq :: XhrRequest ()
getListcliReq = xhrRequest "GET" (getPath (BackendRoute_ListarC :/ ())) def

getListarmReq :: XhrRequest ()
getListarmReq = xhrRequest "GET" (getPath (BackendRoute_ListarA :/ ())) def


tabCliente :: DomBuilder t m => Cliente -> m ()
tabCliente cl = do
    el "tr" $ do
        el "td" (text $ nomeCliente cl)
        el "td" (text $ T.pack $ show $ cpfCliente cl)
        el "td" (text $ T.pack $ show $ telClient cl)
        el "td" (text $ estCliente cl)
        el "td" (text $ cidCliente cl)
        el "td" (text $ endCliente cl)
        el "td" (text $ T.pack $ show $ endnumCliente cl)
        
tabArmazem :: DomBuilder t m => Armazem -> m ()
tabArmazem ar = do
    el "tr" $ do
        el "td" (text $ nomeArmazem ar)
        el "td" (text $ estArmazem ar)
        el "td" (text $ cidArmazem ar)
        el "td" (text $ T.pack $ show $ tamArmazem ar)

tabProduto :: DomBuilder t m => Produto -> m ()
tabProduto pr = do 
    el "tr" $ do
        el "td" (text $ nomeProduto pr)
        el "td" (text $ T.pack $ show $ valorProduto pr)
        el "td" (text $ T.pack $ show $ qtProduto pr)
    
reqprodLista :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => m ()
reqprodLista = do
    (btn, _) <- el' "button" (text "Listar")
    let click = domEvent Click btn
    prods :: Dynamic t (Event t (Maybe [Produto])) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> performRequestAsync (const getListprodReq <$> click))
    dynP <- foldDyn (\ps d -> case ps of
                            Nothing -> []
                            Just p -> d++p) [] (switchDyn prods)
    el "table" $ do
        el "thead" $ do
            el "tr" $ do
                el "th" (text "Nome")
                el "th" (text "Valor")
                el "th" (text "Qt")
        
        el "tbody" $ do
            dyn_ (fmap sequence (ffor dynP (fmap tabProduto)))
            
reqarmLista :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => m ()
reqarmLista = do
    (btn, _) <- el' "button" (text "Listar")
    let click = domEvent Click btn
    arms :: Dynamic t (Event t (Maybe [Armazem])) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> performRequestAsync (const getListarmReq <$> click))
    dynP <- foldDyn (\ps d -> case ps of
                            Nothing -> []
                            Just p -> d++p) [] (switchDyn arms)
    el "table" $ do
        el "thead" $ do
            el "tr" $ do
                el "th" (text "Nome")
                el "th" (text "Estado")
                el "th" (text "Cidade")
                el "th" (text "Tamanho")
        
        el "tbody" $ do
            dyn_ (fmap sequence (ffor dynP (fmap tabArmazem)))
    

reqcliLista :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => m ()
reqcliLista = do
    (btn, _) <- el' "button" (text "Listar")
    let click = domEvent Click btn
    clis :: Dynamic t (Event t (Maybe [Cliente])) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> performRequestAsync (const getListcliReq <$> click))
    dynP <- foldDyn (\ps d -> case ps of
                            Nothing -> []
                            Just p -> d++p) [] (switchDyn clis)
    el "table" $ do
        el "thead" $ do
            el "tr" $ do
                el "th" (text "CPF")
                el "th" (text "Nome")
                el "th" (text "Telefone")
                el "th" (text "Estado")
                el "th" (text "Cidade")
                el "th" (text "Endereco")
                el "th" (text "Num.Casa")
        
        el "tbody" $ do
            dyn_ (fmap sequence (ffor dynP (fmap tabCliente)))
    
    
reqProd :: ( DomBuilder t m
       , Prerender js t m
       ) => m ()
reqProd = do
    nome <- inputElement def
    vl <- numberInput
    qt <- numberInput
    let prod = fmap (\((n,v),i) -> Produto n v i) (zipDyn (zipDyn (_inputElement_value nome) vl) qt)
    (submitBtn,_) <- el' "button" (text "Inserir")
    let click = domEvent Click submitBtn
    let prodEvt = tag (current prod) click
    _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> performRequestAsync (prodRequest <$> prodEvt))
    return () 
-----------
reqCli :: ( DomBuilder t m
       , Prerender js t m
       ) => m ()
reqCli = do
    cpf <- numberInput
    nome <- inputElement def
    tel <- numberInput
    est <- inputElement def
    cid <- inputElement def
    end <- inputElement def
    endnum <- numberInput
    let cli = fmap (\((cp,nm),(te,es)(ci,en),eu) -> Cliente cp nm te es ci en eu (zipDyn (zipDyn cpf (_inputElement_value nome)(zipDyn te (_inputElement_value es) (zipDyn (_inputElement_value ci)(_inputElement_value en)))) eu)
    (submitBtn,_) <- el' "button" (text "Inserir")
    let click = domEvent Click submitBtn
    let cliEvt = tag (current cli) click
    _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
    (pure never)
    (fmap decodeXhrResponse <$> performRequestAsync (cliRequest <$> cliEvt))
    return ()
    
reqArm :: ( DomBuilder t m
       , Prerender js t m
       ) => m ()
reqArm = do
    nome <- inputElement def
    est <- numberInput
    end <- numberInput
    tam <- numberInput
    let arm = fmap (\((n,es),(en,t)) -> Armazem n es en t) (zipDyn (zipDyn (_inputElement_value nome) est)(zipDyn en t))
    (submitBtn,_) <- el' "button" (text "Inserir")
    let click = domEvent Click submitBtn
    let armEvt = tag (current arm) click
    _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> performRequestAsync (armRequest <$> armEvt))
    return () 

btns :: (DomBuilder t m, PostBuild t m) => m ()
btns = el "div" $ do
  t <- inputElement def
  s <- inputElement def
  text " "
  dynText $ zipDynWith (<>) (_inputElement_value t)  (_inputElement_value s) 


   
buttonClick :: DomBuilder t m => m (Event t T.Text)
buttonClick = do
    t <- inputElement def
    (e,_) <- el' "button" (text "OK")
    return $ tagPromptlyDyn (fmap revText (_inputElement_value t)) (domEvent Click e)
   
numberInput :: (Read a, Num a) => DomBuilder t m => m (Dynamic t a)
numberInput = do
      n <- inputElement $ def
        & inputElementConfig_initialValue .~ "0"
        & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("type" =: "number")
      return $ fmap (fromMaybe 0 . readMaybe . T.unpack) $ _inputElement_value n
   
sumButton :: (DomBuilder t m) => m (Event t Double)
sumButton = do
    n1 <- numberInput
    text " "
    n2 <- numberInput
    text " "
    (e,_) <- el' "button" (text "OK")
    let dynDouble = zipDynWith (+) n1  n2
    return $ attachPromptlyDynWith const dynDouble (domEvent Click e)

sumEvt :: (DomBuilder t m, PostBuild t m, MonadHold t m) => m ()
sumEvt = do
    evt <- sumButton
    s <- holdDyn 0 evt 
    el "div" (dynText $ fmap (T.pack . show) s)
    
bttnEvt :: (DomBuilder t m, PostBuild t m, MonadHold t m) => m ()
bttnEvt = do
    evt <- buttonClick
    hl <-  holdDyn "" evt
    el "div" (dynText hl)

data Pagina = Pagina0 | Pagina1 | Pagina2 | Pagina3 | Pagina4 | Pagina5 | Pagina6
    
clickLi :: DomBuilder t m => Pagina -> T.Text -> m (Event t Pagina)
clickLi p t = do
    (ev, _) <- el' "li" (elAttr "a" ("href" =: "#") (text t))
    return ((\_ -> p) <$> domEvent Click ev)
    
menuLi :: (DomBuilder t m, MonadHold t m) => m (Dynamic t Pagina)
menuLi = do
    evs <- el "ul" $ do
        p1 <- clickLi Pagina1 "Cadastro: Cliente"
        p2 <- clickLi Pagina2 "Cadastro: Produto"
        p3 <- clickLi Pagina3 "Cadastro: Armazem"
        p4 <- clickLi Pagina4 "Lista de Clientes"
        p5 <- clickLi Pagina5 "Lista de Produtos"
        p6 <- clickLi Pagina6 "LIsta de Armazens"
        return (leftmost [p1,p2,p3,p4,p5,p6)
    holdDyn Pagina0 evs    
    
currPag :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m, Prerender js t m) => Pagina -> m ()
currPag p = 
    case p of
         Pagina0 -> blank
         Pagina1 -> reqCli
         Pagina2 -> reqProd
         Pagina3 -> reqArm
         Pagina4 -> reqcliLista
         Pagina5 -> reqprodLista
         Pagina6 -> reqarmLista
         
mainPag :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m, Prerender js t m) => m ()
mainPag = do
    pag <- el "div" menuLi
    dyn_ $ fmap currPag pag
    
    
pagClick :: (MonadHold t m, 
             PostBuild t m,
             DomBuilder t m, 
             MonadFix m) => m ()
pagClick = do
        evt <- countClick 
        st <- accumDyn (+) 0 evt 
        el "div" (dynText (fmap (T.pack . show) st))
    
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Mercado ATACADISTA"
      elAttr "link" ("href" =: static @"main.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
  , _frontend_body = mainPag
  }
