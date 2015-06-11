{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Plots.Types.Bar
  ( BarPlot
  , BarOpt
  , simpleBarPlot
    -- * Prism
  , _BarPlot

    -- * Lenses
  , barWidth
  , barSpacing
  , verticleBars
  , barBaseLine
  , barOrientation
  , bars
  ) where

import Control.Lens     hiding (transform, ( # ))
import Data.Default
import Data.Typeable
import Data.Foldable (Foldable, foldMap, toList)
import Data.Maybe
import Diagrams.Prelude

-- import Plots.Themes
import Plots.Types

data BarPlot n = BarPlot
  { barData       :: [(n,[n])] -- data for bars
  , _barWidth     :: n         -- total width of bars for one 'bit'
  , _barSpacing   :: n         -- gap between multibars in same value
  , _verticleBars :: Bool    -- whether the bars are verticle
  , _stacked      :: Bool    -- whether the bars stacked (or side by side)
  } deriving Typeable

type instance V (BarPlot n) = V2
type instance N (BarPlot n) = n

makeLenses ''BarPlot

data BarOpt = BarOpt
    { _barWidth :: Double  -- ^ from 0 to 1
    , _barBaseLine :: Maybe Double
    , _barOrientation :: Char
    }

makeLenses ''BarOpt

instance Default BarOpt where
    def = BarOpt
        { _barWidth = 0.8
        , _barBaseLine = Nothing
        , _barOrientation = '^'
        }

instance OrderedField n => Enveloped (BarPlot n) where
  getEnvelope = mempty

instance (Typeable b, TypeableFloat n, Renderable (Path V2 n) b)
    => Plotable (BarPlot n) b where
  renderPlotable _ t d _pp = drawBarPlot d # transform t

instance Fractional n => Default (BarPlot n) where
  def = BarPlot
          { barData       = []
          , _barWidth     = 0.5
          , _barSpacing   = 0.1
          , _verticleBars = True
          }

-- TODO: work out a nice way to get different colours for multi-bar plots.

drawBarPlot :: (TypeableFloat n, Renderable (Path V2 n) b) => BarPlot n -> QDiagram b V2 n Any
drawBarPlot bp = foldMap makeBar (barData bp)
  where
    tW = bp^.barWidth
    δ  = bp^.barSpacing
    --
    makeBar (_,[]) = mempty
    makeBar (x,bs) = ifoldMap mkBar bs
      where
        mkBar i h = rect w h
                      # alignBL
                      # translateX (x + fromIntegral i * (δ + w))
        n = fromIntegral $ length bs
        w = recip n * (tW - δ * (n - 1))

-- instance (Typeable b, Renderable (Path R2) b) => Plotable (BarPlot b) b R2 where
--   plot _r _ t = transform t . drawBarPlot

simpleBarPlot :: (TypeableFloat n, Foldable f) => f n -> BarPlot n
simpleBarPlot (toList -> xs) = def { barData = imap f xs }
  where
    f i h = (fromIntegral i + 1, [h])


_BarPlot :: Plotable (BarPlot n) b => Prism' (Plot b V2 n) (BarPlot n)
_BarPlot = _Plot

bars :: (PlotData m1 a1, PlotData m2 a2) => m1 a1 -> m2 a2 -> BarOpt -> PlotFn
bars xs ys opt m = case opt^.barOrientation of
                       '^' -> upBars xs ys opt m
                       '>' -> rightBars xs ys opt m
                       'V' -> downBars xs ys opt m
                       _ -> upBars xs ys opt m

upBars :: (PlotData m1 a1, PlotData m2 a2) => m1 a1 -> m2 a2 -> BarOpt -> PlotFn
{-# INLINE upBars #-}
upBars xs ys opt mapX mapY = map (uncurry moveTo) [ (x ^& ((y+bl)/2), rect w (y-bl)) | (x, y) <- xy ]
  where
    xy = mapMaybe (runMap pMap) $ zip (getValues xs) $ getValues ys
    w = (opt^.barWidth) * gap'
    gap' = (fromJust.runMap mapX) 2 - (fromJust.runMap mapX) 1
    pMap = compose mapX mapY
    bl = fromMaybe 0 $ do b <- opt^.barBaseLine
                          runMap mapY b

rightBars :: (PlotData m1 a1, PlotData m2 a2) => m1 a1 -> m2 a2 -> BarOpt -> PlotFn
rightBars xs ys opt mapX mapY = map (uncurry moveTo) [ ( ((x+bl)/2) ^& y, rect (x-bl) h) | (x, y) <- xy ]
  where
    xy = mapMaybe (runMap pMap) $ zip (getValues xs) $ getValues ys
    h = (opt^.barWidth) * gap'
    gap' = (fromJust.runMap mapY) 2 - (fromJust.runMap mapY) 1
    pMap = compose mapX mapY
    bl = fromMaybe 0 $ do b <- opt^.barBaseLine
                          runMap mapX b
{-# INLINE rightBars #-}

downBars :: (PlotData m1 a1, PlotData m2 a2) => m1 a1 -> m2 a2 -> BarOpt -> PlotFn
downBars xs ys opt mapX mapY = map (uncurry moveTo) [ (x ^& ((areaHeight+y-bl)/2), rect w (areaHeight-y-bl) ) | (x, y) <- xy ]
  where
    xy = mapMaybe (runMap pMap) $ zip (getValues xs) $ getValues ys
    w = (opt^.barWidth) * gap'
    gap' = (fromJust.runMap mapX) 2 - (fromJust.runMap mapX) 1
    pMap = compose mapX mapY
    areaHeight = l' + u'
    (l', u') = both %~ fromJust . runMap mapY $ domain mapY
    bl = fromMaybe 0 $ do b <- opt^.barBaseLine
                          runMap mapY b
{-# INLINE downBars #-}

------------------------------------------------------------------------
-- Histogram
------------------------------------------------------------------------


-- data Histogram n a = forall s. Histogram
--   { histogramData  :: s
--   , histogramFold  :: Fold s a
--   , histogramLowerLimit :: Maybe n
--   , histogramUpperLimit :: Maybe n
--   }
--
-- mkHistogramOf :: Fold s n -> s -> BarPlot n
-- mkHistogramOf f as =


