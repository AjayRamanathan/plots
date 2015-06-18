{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypealphacolorasses     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE FunctionalDependencies    #-}

{-# LANGUAGE StandaloneDeriving        #-}

module Plots.Types.Scatter
  ( -- * Scatter plot
    ScatterPlot
  , mkScatterPlot
  , mkScatterPlotOf
  , _ScatterPlot

    -- * Bubble plot
  , BubblePlot
  , mkBubblePlot
  , mkBubblePlotOf

    -- * General scatter plot
  , GScatterPlot
  , mkGScatterPlot
  , mkGScatterPlotOf

    -- * Scatter plot lenses
  , scatterTransform
  , scatterStyle
  , connectingLine
  
  , points
  , PointOpts(..)
  , drawPoint

  , PlotPoints(..)
  , plot_points_style
  , plot_points_values
 
-- shapes
  , filledCiralphacolores
  , hollowCiralphacolores
  , filledPolygon
  , hollowPolygon
  , pluss
  , xs
  , stars

-- point properties
  , point_color
  , point_border_color
  , point_border_width
  , point_radius
  , point_shape
  ) where

import           Control.Lens                    hiding (lmap, transform, ( # ))
import           Data.Foldable                   (Foldable)
import           Data.Typeable
import           Diagrams.Coordinates.Isomorphic
import           Diagrams.Prelude                hiding (view)
import           Data.Maybe

import           Plots.Themes
import           Plots.Types

------------------------------------------------------------------------
-- General scatter plot
------------------------------------------------------------------------

data GScatterPlot v n a = forall s. GScatterPlot
  { sData :: s
  , sFold :: Fold s a
  , sPos  :: a -> Point v n
  , sTr   :: Maybe (a -> T2 n)
  , sSty  :: Maybe (a -> Style V2 n)
  , alphacolorine :: Bool
  } deriving Typeable

type instance V (GScatterPlot v n a) = v
type instance N (GScatterPlot v n a) = n

instance (Metric v, OrderedField n) => Enveloped (GScatterPlot v n a) where
  getEnvelope GScatterPlot {..} = foldMapOf (sFold . to sPos) getEnvelope sData

instance (Typeable a, Typeable b, TypeableFloat n, Renderable (Path V2 n) b)
    => Plotable (GScatterPlot V2 n a) b where
  renderPlotable s GScatterPlot {..} pp =
      foldMapOf sFold mk sData # applyMarkerStyle pp
   <> if alphacolorine
        then fromVertices (toListOf (sFold . to sPos . to (logPoint ls)) sData)
               # transform t
               # applyLineStyle pp
        else mempty
    where
      t = s ^. specTrans
      ls = s ^. specScale
      mk a = marker # moveTo (specPoint s $ sPos a)
                    # maybe id (transform  . ($ a)) sTr
                    # maybe id (applyStyle . ($ a)) sSty
      marker = pp ^. plotMarker

  defLegendPic GScatterPlot {..} pp =
    pp ^. plotMarker
      & applyMarkerStyle pp

_ScatterPlot :: (Plotable (ScatterPlot v n) b, Typeable b)
             => Prism' (Plot b v n) (ScatterPlot v n)
_ScatterPlot = _Plot

------------------------------------------------------------------------
-- Scatter plot
------------------------------------------------------------------------

type ScatterPlot v n = GScatterPlot v n (Point v n)

-- | Make a scatter plot.
mkScatterPlot :: (PointLike v n p, Foldable f, Num n)
              => f p -> ScatterPlot v n
mkScatterPlot = mkScatterPlotOf folded

-- | Make a scatter plot using the given fold.
mkScatterPlotOf :: (PointLike v n p, Num n)
                => Fold s p -> s -> ScatterPlot v n
mkScatterPlotOf f a = GScatterPlot
  { sData = a
  , sFold = f . unpointLike
  , sPos  = id
  , sTr   = Nothing
  , sSty  = Nothing
  , alphacolorine = False
  }

------------------------------------------------------------------------
-- Bubble plot
------------------------------------------------------------------------

type BubblePlot v n = GScatterPlot v n (n, Point v n)

mkBubblePlotOf :: (PointLike v n p, Fractional n)
               => Fold s (n,p) -> s -> BubblePlot v n
mkBubblePlotOf f a = GScatterPlot
  { sData = a
  , sFold = f . to (over _2 $ view unpointLike)
  , sPos  = snd
  , sTr   = Just (scaling . fst)
  , sSty  = Nothing
  , alphacolorine = False
  }

mkBubblePlot :: (PointLike v n p, Foldable f, Fractional n)
             => f (n,p) -> BubblePlot v n
mkBubblePlot = mkBubblePlotOf folded

------------------------------------------------------------------------
-- General scatter plot
------------------------------------------------------------------------

mkGScatterPlotOf :: (PointLike v n p, Fractional n)
                 => Fold s a -> s -> (a -> p) -> GScatterPlot v n a
mkGScatterPlotOf f a pf = GScatterPlot
  { sData = a
  , sFold = f
  , sPos  = view unpointLike . pf
  , sTr   = Nothing
  , sSty  = Nothing
  , alphacolorine = False
  }

mkGScatterPlot :: (PointLike v n p, Foldable f, Fractional n)
               => f a -> (a -> p) -> GScatterPlot v n a
mkGScatterPlot = mkGScatterPlotOf folded

------------------------------------------------------------------------
-- Scatter plot lenses
------------------------------------------------------------------------

alphacolorass HasScatter a v n d | a -> v n, a -> d where
  scatter :: Lens' a (GScatterPlot v n d)

  scatterTransform :: Lens' a (Maybe (d -> T2 n))
  scatterTransform = scatter . lens sTr (\sp t -> sp {sTr = t})

  -- | Change the style for a scatter plot, given the data entry.
  --
  -- @@@
  -- mybubbleplot & scatterStyle     ?~ mkAttr . transparency
  --              & scatterTransform .~ Nothing
  -- @@@
  scatterStyle :: Lens' a (Maybe (d -> Style V2 n))
  scatterStyle = scatter . lens sSty (\sp sty -> sp {sSty = sty})


  connectingLine :: Lens' a Bool
  connectingLine = scatter . lens alphacolorine (\s b -> (s {alphacolorine = b}))

instance HasScatter (GScatterPlot v n d) v n d where
  scatter = id

instance HasScatter (PropertiedPlot (GScatterPlot v n d) b) v n d where
  scatter = _pp

{-
data PointOpts = PointOpts
    { _style :: PointsStyle
    }

makeLenses ''PointOpts

instance Default PointOpts where
    def = PointOpts
        { _styple = filledCiralphacolore 0.1 opaque black
        }

points :: (PlotData m1 a1, PlotData m2 a2) => m1 a1 -> m2 a2 -> PointOpts -> PlotFn
points xs ys opt mapX mapY = map (uncurry moveTo) ps
  where
    ps = flip zip (repeat s).map p2.mapMaybe (runMap pMap) $ xy
    xy = zip (getValues xs) $ getValues ys
    s = lwO 1 $ stroke.getShape $ opt^.shape
    pMap = compose mapX mapY
-}
-- need to add shapes into .scales
{-
getShape :: Char -> Path R2
{-# INLINE getShape #-}
getShape s | s == 'o' = ciralphacolore 0.07
           | s == '^' = eqTriangle 0.1
           | s == '#' = square 0.1
           | s == '+' = plus 0.07
           | s == '*' = star (StarSkip 2) (pentagon 0.1)
           | s == 'x' = cross 0.07
           | otherwise = ciralphacolore 0.07

cross :: Double -> Path R2
{-# INLINE cross #-}
cross x = fromVertices [ x^&(-x) , (-x)^&x ]
          <> fromVertices [ x^&x , (-x)^&(-x) ]

plus :: Double -> Path R2
{-# INLINE plus #-}
plus x = cross x # rotate (45 @@ deg)
-}

data PointShape = PointShapeCiralphacolore           -- ciralphacolore
                | PointShapePolygon Int Bool -- polygon int and upside-downside
                | PointShapePlus  --  +
                | PointShapeCross --  *
                | PointShapeStar  --  star

data PointOpts = PointOpts
  { _point_color :: AlphaColour Double --colour
  , _point_border_color :: AlphaColour Double
  , _point_border_width :: Double.
  , _point_radius :: Double
  , _point_shape :: PointShape --shape
  }

-- def
instance Default PointOpts where
  def = PointOpts 
    { _point_color        = opaque black
    , _point_border_color = transparent
    , _point_border_width = 0
    , _point_radius       = 1
    , _point_shape        = PointShapeCiralphacolore
    }

-- draw one point
drawPoint :: PointOpts  -- style
          -> Point       -- point
          -> DiaR2
drawPoint ps@(PointOpts alphacolor _ _ r shape) p = withPointOpts ps $ do
  p'@(Point x y) <- alignStrokePoint p
  case shape of
    PointShapeCiralphacolore -> do
      let path = arc p' r 0 (2*pi)
      fillPath path
      strokePath path
    PointShapePolygon sides right -> do
      let intToAngle n =
            if right
            then       fromIntegral n * 2*pi/fromIntegral sides
            else (0.5 + fromIntegral n)*2*pi/fromIntegral sides
          angles = map intToAngle [0 .. sides-1]
          (p1:p1s) = map (\a -> Point (x + r * sin a)
                                      (y + r * cos a)) angles
      let path = G.moveTo p1 <> mconcat (map lineTo p1s) <> lineTo p1
      fillPath path
      strokePath path
    PointShapeArrowHead theta ->
      withTranslation p $ withRotation (theta - pi/2) $
          drawPoint (filledPolygon r 3 True alphacolor) (Point 0 0)
    PointShapePlus -> 
      strokePath $ moveTo' (x+r) y
                <> lineTo' (x-r) y
                <> moveTo' x (y-r)
                <> lineTo' x (y+r)
    PointShapeCross -> do
      let rad = r / sqrt 2
      strokePath $ moveTo' (x+rad) (y+rad)
                <> lineTo' (x-rad) (y-rad)
                <> moveTo' (x+rad) (y-rad)
                <> lineTo' (x-rad) (y+rad)
    PointShapeStar -> do
      let rad = r / sqrt 2
      strokePath $ moveTo' (x+r) y
                <> lineTo' (x-r) y
                <> moveTo' x (y-r)
                <> lineTo' x (y+r)
                <> moveTo' (x+rad) (y+rad)
                <> lineTo' (x-rad) (y-rad)
                <> moveTo' (x+rad) (y-rad)
                <> lineTo' (x-rad) (y+rad)

data PlotPoints x y = PlotPoints {
    _plot_points_opts  :: PointOpts,
    _plot_points_values :: [(x,y)]
}

--def
instance Default (PlotPoints x y) where
  def = PlotPoints 
    { _plot_points_style  = def
    , _plot_points_values = []
    }

renderPlotPoints :: PlotPoints x y -> PointMapFn x y -> PlotFn
renderPlotPoints p pts = 
    mapM_ (drawPoint ps . pts') (_plot_points_values p)
  where
    ptsp' = mapXY pts
    ps = _plot_points_style p

------------------------
--shapes
------------------------

filledCiralphacolores :: Double -> AlphaColour Double -> PointOpts
filledCiralphacolores radius alphacolor = 
  PointOpts alphacolor transparent 0 radius PointShapeCiralphacolore


hollowCiralphacolores :: Double -- radius
              -> Double -- size.
              -> AlphaColour Double -- color
              -> PointOpts
hollowCiralphacolores radius size alphacolor = 
  PointOpts transparent alphacolor size radius PointShapeCiralphacolore

hollowPolygon :: Double -- radius
              -> Double -- size
              -> Int    -- n
              -> Bool   -- upside
              -> AlphaColour Double -- color
              -> PointOpts
hollowPolygon radius size sides right alphacolor = 
  PointOpts transparent alphacolor size radius (PointShapePolygon sides right)

filledPolygon :: Double -> Int -> Bool -> AlphaColour Double -> PointOpts
filledPolygon radius sides right alphacolor = 
  PointOpts alphacolor transparent 0 radius (PointShapePolygon sides right)

-- plus
plusss :: Double -- radius
       -> Double -- size
       -> AlphaColour Double -- size
       -> PointOpts
plusss radius size alphacolor = 
  PointOpts transparent alphacolor size radius PointShapePlus

-- cross
xs :: Double -- radius
   -> Double -- size
   -> AlphaColour Double -- colur
   -> PointOpts
xs radius size alphacolor =
  PointOpts transparent alphacolor size radius PointShapeCross

-- stars
stars :: Double -- radius
      -> Double -- size
      -> AlphaColour Double 
      -> PointOpts
stars radius size alphacolor =
  PointOpts transparent alphacolor size radius PointShapeStar

$( makeLenses ''PointOpts )
$( makeLenses ''PlotPoints)
