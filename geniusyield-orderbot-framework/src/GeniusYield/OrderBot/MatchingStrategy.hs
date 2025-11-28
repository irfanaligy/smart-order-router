{- |
Module      : GeniusYield.OrderBot.MatchingStrategy
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.OrderBot.MatchingStrategy (
  IndependentStrategy,
  FillType (..),
  MatchExecutionInfo (..),
  MatchResult,
  completeFill,
  partialFill,
  executionSkeleton,
  matchExecutionInfoUtxoRef,
) where

import Data.Maybe (fromJust)
import Data.Either (lefts, rights)
import GeniusYield.Api.DEX.PartialOrder (
  PORefs,
  PartialOrderInfo (..)
 )
import GeniusYield.Api.DEX.TwoWayOrder (TwoWayOrderInfo (..), TWFillSpec (..), fillTwoWayAndLegacyPartialOrders)
-- import GeniusYield.Api.DEX.TwoWayOrderConfig (TWORef (..), RefTWOCD (..), fetchTwoWayOrderConfig)
import GeniusYield.Api.DEX.TwoWayOrderConfig (TWORef (..), RefTWOCD (..))
-- import GeniusYield.Scripts (GYCompiledScripts, readCompiledScripts)
-- import GeniusYield.Api.DEX.Constants (DEXInfo (..), twoRefsPreprod, twoRefsMainnet)
import GeniusYield.Api.Types (GYApiMonad)
-- import Control.Monad.Reader (runReaderT)
import GeniusYield.TxBuilder.Class (GYTxQueryMonad, GYTxUserQueryMonad)
import GeniusYield.OrderBot.Strategies (
  IndependentStrategy,
  MatchResult,
 )
import GeniusYield.OrderBot.Types
import GeniusYield.TxBuilder (GYTxSkeleton)
import GeniusYield.Types.Address (GYAddress)
import GeniusYield.Types.PlutusVersion (PlutusVersion (PlutusV3))
import GeniusYield.Types.TxOutRef (GYTxOutRef)

executionSkeleton ::
  (GYApiMonad m, GYTxQueryMonad m, GYTxUserQueryMonad m) =>
  (TWORef, PORefs) ->
  GYAddress ->
  RefTWOCD ->
  MatchResult ->
  m (GYTxSkeleton 'PlutusV3)
executionSkeleton (twor, pors) recpAddr refTwo mr = do
  fillTwoWayAndLegacyPartialOrders twor tspecs refTwo pors pspecs Nothing
  where
    (pspecs, tspecs) = let specs = (map f mr) in (lefts specs, rights specs)
    getAmount CompleteFill    o = orderAmount o
    getAmount (PartialFill n) o = if isBuyOrder o then floor $ fromIntegral n * getPrice (orderPrice o) else n
    f (OrderExecutionInfo ft o@(OrderInfo { orderData = Just (DEXOrderDataPartial poi) })) = Left (poiRef poi, getAmount ft o)
    f (OrderExecutionInfo ft o@OrderInfo { orderData = Just (DEXOrderDataTwoWay twoi) })   = Right $
      TWFillSpec
        { twfsOrderRef = (twoiRef twoi)
        , twfsDirection = fromJust (orderDir o)
        , twfsAmount = getAmount ft o
        , twfsOracleCertificate = orderCert o
        , twfsRecipient = recpAddr
        }

matchExecutionInfoUtxoRef :: MatchExecutionInfo -> GYTxOutRef
matchExecutionInfoUtxoRef (OrderExecutionInfo _ (OrderInfo { orderData = Just (DEXOrderDataPartial p) })) = poiRef  p
matchExecutionInfoUtxoRef (OrderExecutionInfo _ (OrderInfo { orderData = Just (DEXOrderDataTwoWay  t) })) = twoiRef t
