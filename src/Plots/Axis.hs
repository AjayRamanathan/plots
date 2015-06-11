{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Plots.Axis
    ( AxisFn(..)
    , LabelOpt
    , Axis(..)
    , axisMap
    , axisLabels
    , axisDiag
    , axisLabelOpt
    , offsetX
    , offsetY
    , rotation
    , size
    , realAxis
    , indexAxis
    , emptyAxis
    , axis
    , tickLen
    , minorTickLen
    , labelOpt
    ) where

import           Control.Lens          hiding (lmap, transform, ( # ))
import           Data.Default
import           Data.Monoid.Recommend
import           Data.Typeable

import           Diagrams.Prelude      as D hiding (under, view)
import           Diagrams.TwoD.Text

import           Plots.Axis.ColourBar
import           Plots.Axis.Grid
import           Plots.Axis.Labels
import           Plots.Axis.Ticks
import           Plots.Legend
import           Plots.Themes
import           Plots.Types
import           Plots.Utils

-- | Where axis line for coordinate should be drawn.
data AxisLineType
  = BoxAxisLine
  | LeftAxisLine
  | MiddleAxisLine
  | RightAxisLine
  | NoAxisLine
  deriving (Show, Eq, Typeable)

instance Default AxisLineType where
  def = BoxAxisLine

-- | Information about position and style of axis lines.
data AxisLine n = AxisLine
  { _axisLineType  :: AxisLineType
  , _axisArrowOpts :: Maybe (ArrowOpts n)
  } deriving Typeable

makeLenses ''AxisLine

type AxisLines v n = v (AxisLine n)

instance Default (AxisLine n) where
  def = AxisLine
          { _axisLineType  = def
          , _axisArrowOpts = def
          }

-- Scaling

type AspectRatio v n = v n

data ScaleMode
  = AutoScale
  | NoScale
  | Stretch
  | UniformScale UniformScaleStrategy
  deriving (Show, Read)

data UniformScaleStrategy
  = AutoUniformScale
  | UnitOnly
  | ChangeVerticalLimits
  | ChangeHorizontalLimits
  deriving (Show, Read)

data Scaling n = Scaling
  { _aspectRatio       :: Recommend n
  , _axisPostScale     :: Maybe n
  , _axisScaleMode     :: ScaleMode
  , _enlargeAxisLimits :: Maybe (Recommend n)
  }
  deriving Show

makeLenses ''Scaling

type AxisScaling v n = v (Scaling n)

instance Fractional n => Default (Scaling n) where
  def = Scaling
    { _aspectRatio       = Recommend 1
    , _axisPostScale     = Nothing
    , _axisScaleMode     = AutoScale
    , _enlargeAxisLimits = Just $ Recommend 0.1
    }

type PropertyAdjust b v n = PlotProperties b v n -> PlotProperties b v n

-- axis data type

-- | Axis is the data type that holds all the nessessary information to render
--   a plot. The idea is to use one of the default axis, customise, add plots
--   and render using @drawAxis@.
data Axis b v n = Axis
  { -- These lenses are not being exported, they're just here for instances.
    _axisAxisBounds :: Bounds v n

  -- These lenses are exported.
  , _axisColourBar  :: ColourBarOpts b n
  , _axisGridLines  :: AxisGridLines v n
  , _axisLabels     :: AxisLabels b v n
  , _axisLegend     :: Legend b n
  , _axisLines      :: AxisLines v n
  , _axisPlots      :: [Plot' b v n]
  , _axisScaling    :: AxisScaling v n
  , _axisSize       :: SizeSpec v n
  , _axisTheme      :: Theme b v n
  , _axisTickLabels :: AxisTickLabels b v n
  , _axisTicks      :: AxisTicks v n
  , _axisTitle      :: Maybe String
  , _axisScale      :: v AxisScale
  , _defProperties  :: PlotProperties b v n
  } deriving Typeable

makeLenses ''Axis

type instance V (Axis b v n) = v
type instance N (Axis b v n) = n
type instance B (Axis b v n) = b

axisLine :: E v -> Lens' (Axis b v n) (AxisLine n)
axisLine (E l) = axisLines . l

instance HasBounds (Axis b v n) where
  bounds = axisAxisBounds

-- R2 axis

instance (TypeableFloat n, Enum n, Renderable (Text n) b, Renderable (Path V2 n) b)
    => Default (Axis b V2 n) where
  def = Axis
    { _axisTitle      = Nothing
    , _axisSize       = mkWidth 300
    , _axisPlots      = []
    , _axisLegend     = def
    , _axisColourBar  = defColourBar
    , _axisTheme      = coolTheme
    , _axisAxisBounds = Bounds $ pure def
    , _axisGridLines  = pure def
    , _axisLabels     = V2 def (def & axisLabelFunction %~ (fmap . fmap $ rotateBy (1/4))
                                    & axisLabelGap .~ 40)
    , _axisScaling    = pure def
    , _axisTickLabels = pure def
    , _axisTicks      = pure def
    , _axisLines      = pure def
    , _axisScale      = pure def
    , _defProperties  = def
    }

-- | control the rendering of labels
data LabelOpt = LabelOpt
    { _labelOptOffsetX :: !Double
    , _labelOptOffsetY :: !Double
    , _labelOptRotation :: !Double
    , _labelOptSize :: !Double
    } deriving (Show)

makeFields ''LabelOpt

instance Default LabelOpt where
    def = LabelOpt
        { _labelOptOffsetX = 0
        , _labelOptOffsetY = -0.1
        , _labelOptRotation = 0
        , _labelOptSize = 0.2
        }

data AxisOpt = AxisOpt
    { _nTick :: !Int
    , _nMinorTick :: !Int
    , _tickLen :: !Double
    , _minorTickLen :: !Double
    , _labelOpt :: !LabelOpt
    }

makeLenses ''AxisOpt

instance Default AxisOpt where
    def = AxisOpt
        { _nTick = 5
        , _nMinorTick = 4
        , _tickLen = 0.1
        , _minorTickLen = 0.05
        , _labelOpt = def
        }

-- | axis data type
data Axis = Axis
    { _axisMap :: !(PointMap Double)
    , _axisDiag :: !DiaR2
    , _axisLabels :: ![((Double, Double), String)]
    , _axisLabelOpt :: !LabelOpt
    }

makeLenses ''Axis

-- | given the length, draw axis
newtype AxisFn = AxisFn { makeAxis :: Double -> Axis }

instance Default AxisFn where
    def = emptyAxis 

{-
flipAxisFn :: AxisFn -> AxisFn
flipAxisFn axisF = AxisFn $ do (Axis m labels diag) <- makeAxis axisF
                               let (labelP, label) = unzip labels
                                   newLabels = zip labelP $ reverse label
                               return $ Axis (flipMap m) newLabels diag
                               -}


realAxis :: (Double, Double) -> Double -> AxisOpt -> AxisFn
realAxis r pad' opt = AxisFn ( \len ->
    let pMap = linearMap (fromRational l, fromRational u) (pad', len-pad')
        (l, u, step) = autoSteps ((opt^.nTick)-1) r
        axis' = lwO 1 $ axis len pad' $ opt & nTick .~ tickN'
        labels = zip labelP
            $ map ((show :: Float -> String) . fromRational) [l, l+step .. u]
        tickN' = truncate ((u - l) / step) + 1
        labelP = zip (enumFromThenTo pad' (pad'+stepLabel) (len-pad')) $ repeat 0
        stepLabel = (len - 2*pad') / fromIntegral (tickN' - 1)
    in Axis pMap axis' labels (opt^.labelOpt) )

indexAxis :: Int -> [String] -> Double -> AxisOpt -> AxisFn
indexAxis num labels pad' opt = AxisFn
    ( \len -> let axis' = axis len pad' $ opt & nTick .~ num
                                              & nMinorTick .~ 0
                                              & minorTickLen .~ 0
                  pMap = linearMap (1, fromIntegral num) (pad', len-pad')
                  labels' = zip labelP labels
                  labelP = zip (enumFromThenTo pad' (pad'+stepLabel) (len-pad')) $ repeat 0
                  stepLabel = (len - 2*pad') / fromIntegral (num - 1)
              in Axis pMap axis' labels' (opt^.labelOpt)
    )

emptyAxis :: AxisFn
emptyAxis = AxisFn $ const $ Axis pMap mempty [] def
  where 
    pMap = PointMap (const Nothing) (0, -1)

axis :: Double -> Double -> AxisOpt -> DiaR2
axis len pad opt = l <> translateX pad (majorTicks <> minorTicks)
  where
    l = fromVertices [ 0 ^& 0, len ^& 0 ]
    majorTicks = ticks (len - 2*pad) (opt^.nTick) (opt^.tickLen)
    minorTicks = ticks (len - 2*pad) minorN (opt^.minorTickLen)
    minorN = ((opt^.nMinorTick) + 1) * ((opt^.nTick) - 1) + 1

ticks :: Double -> Int -> Double -> DiaR2
ticks len tickNum tickL = mconcat [ fromVertices [ x ^& 0, x ^& tickL ] | x <- ticksPos ] 
  where
    ticksPos = enumFromThenTo 0 step len
    step = len / (fromIntegral tickNum - 1)
-- R3 Axis

-- instance (TypeableFloat n, Enum n, Renderable (Text n) b, Renderable (Path V2 n) b)
--     => Default (Axis b V3 n) where
--   def = Axis
--           { _axisTitle      = Nothing
--           , _axisSize       = mkWidth 300
--           , _axisPlots      = []
--           , _axisLegend     = def
--           , _axisTheme      = coolTheme
--           , _axisLinearMap  = isometricProjection
--           , _axisAxisBounds = Bounds $ pure def
--           , _axisGridLines  = pure def
--           , _axisLabels     = pure def
--           , _axisScaling    = pure def
--           , _axisTickLabels = pure def
--           , _axisTicks      = pure def
--           , _axisLines      = pure def
--           }

-- Drawing the axis

