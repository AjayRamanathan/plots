
{-# LANGUAGE TemplateHaskell #-}

module Plots.Types.Ribbon(
    PlotRibbon(..),

    plot_ribbon_style,
    plot_ribbon_values,
) where

import Control.Lens hiding (transform, ( # ), lmap)
-- import Data.Typeable
import Diagrams.Prelude
-- import Diagrams.LinearMap

import Data.Colour (opaque)
import Data.Colour.SRGB (sRGB)
import Data.Default.Class

import Plots.Types
import Plots.Utils (hasNaN)

-- | value specific plot filling the area between two sets of y
--   , and a x.

data PlotRibbon x y = PlotRibbon {
    _plot_fillbetween_style  :: FillStyle,
    _plot_fillbetween_values :: [ (x, (y,y))]
}

renderPlotRibbon :: (PlotData m1 a1) => PlotRibbon x y -> m2 a2 -> PlotFn
renderPlotRibbon p =
    renderPlotRibbon' p (_plot_Ribbon_values p)

renderPlotRibbon' :: PlotRibbon x y -> [(a, (b, b))] -> ((Limit a, Limit b) -> Point) -> PlotFn
renderPlotRibbon' _ [] _     = return ()
renderPlotRibbon' p vs pts  = 
  withFillStyle (_plot_Ribbon_style p) $ do
    ps <- alignFillPoints $ [a] ++ b1 ++ reverse b2 ++ [a]
    fillPointPath ps
  where
    pts'    = mapXY pts
    (a:b1) = map pts' [ (x,y1) | (x,(y1,_)) <- vs ]
    b2      = map pts' [ (x,y2) | (x,(_,y2)) <- vs ]

-----------------------------------------------------------------------------------------
--PlotLegend :: PlotRibbon x y -> Rect -> 
--PlotLegend p r = 
-------------------------------------------------------

plotPointsRibbon :: PlotRibbon x y -> ([x],[y])
plotPointsRibbon p = ( [ x | (x,(_,_)) <- pts ]
                             , concat [ [y1,y2] | (_,(y1,y2)) <- pts ] )
  where
    pts = _plot_ribbon_values p

instance Default (PlotRibbon x y) where
  def = PlotRibbon 
    { _plot_Ribbon_style  = solidFillStyle (opaque $ sRGB 0.5 0.5 1.0)
    , _plot_Ribbon_values = []
    }

$( makeLenses ''PlotRibbon )

