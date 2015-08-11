{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE AllowAmbiguousTypes          #-}


{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

module Plots.API.Scatter
  (      -- ** Scatter plot
    ScatterPlot
  , scatterPlot
  , scatterPlot'
  , scatterPlotL
  , scatterPlotOf
  , scatterPlotOf'
  , scatterPlotLOf

  , gscatterPlot
  , gscatterPlot'
  , gscatterPlotL
  -- **fold variant
  --, gscatterPlotOf
  --, gscatterPlotOf'
  --, gscatterPlotLOf
  ) where

import           Control.Lens                    hiding (( # ))
import           Control.Monad.State.Lazy
import           Data.Default
import           Data.Monoid.Recommend
import           Data.Typeable
import qualified Data.Foldable as F
import           Data.List
import           Data.Function

import           Diagrams.Coordinates.Isomorphic
import           Diagrams.Prelude
import           Diagrams.TwoD.Text
import           Linear

import           Plots.Axis
import           Plots.Axis.Grid
import           Plots.Axis.Labels
import           Plots.Axis.Render
import           Plots.Axis.Ticks
import           Plots.Axis.ColourBar

import           Plots.Types
import           Plots.Themes

import           Plots.Types.Scatter
import           Plots.API

------------------------------------------------------------------------
-- Scatter plot
------------------------------------------------------------------------

-- $ scatter
-- Scatter plots display data as dots. There are several representations
-- for scatter plots for extra parameters. Scatter plots have the
-- following lenses:
--
-- @
-- * 'connectingLine' :: 'Lens'' ('ScatterPlot' v n) 'Bool' - False
-- * 'scatterTransform' :: 'Lens'' ('ScatterPlot' v n) ('Maybe' ('Point' v n -> 'T2' n)) - Nothing
-- * 'scatterStyle': 'Maybe' ('Point' v n -> 'Style' 'V2' n) - Nothing
-- @
--

-- | Add a 'ScatterPlot' to the 'AxisState' from a data set.
--
-- @
--   myaxis = r2Axis ~&
--     scatterPlot data1
-- @

scatterPlot
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (ScatterPlot v n) b,
      F.Foldable f)
  => f p -> m ()
scatterPlot d = addPlotable (mkScatterPlot d)

-- | Make a 'ScatterPlot' and take a 'State' on the plot to alter it's
--   options
--
-- @
--   myaxis = r2Axis &~ do
--     scatterPlot' pointData1 $ do
--       connectingLine .= True
--       addLegendEntry "data 1"
-- @
scatterPlot'
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (ScatterPlot v n) b,
      F.Foldable f)
  => f p -> PlotState (ScatterPlot v n) b -> m ()
scatterPlot' d = addPlotable' (mkScatterPlot d)

-- | Add a 'ScatterPlot' with the given name for the legend entry.
--
-- @
--   myaxis = r2Axis &~ do
--     scatterPlotL "blue team" pointData1
--     scatterPlotL "red team" pointData2
-- @
scatterPlotL
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (ScatterPlot v n) b,
      F.Foldable f)
  => String -> f p -> m ()
scatterPlotL l d = addPlotableL l (mkScatterPlot d)

-- Fold variants

scatterPlotOf
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (ScatterPlot v n) b)
  => Fold s p -> s -> m ()
scatterPlotOf f s = addPlotable (mkScatterPlotOf f s)

scatterPlotOf'
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (ScatterPlot v n) b)
  => Fold s p -> s -> PlotState (ScatterPlot v n) b -> m ()
scatterPlotOf' f s = addPlotable' (mkScatterPlotOf f s)

scatterPlotLOf
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (ScatterPlot v n) b)
  => String -> Fold s p -> s -> m ()
scatterPlotLOf l f s = addPlotableL l (mkScatterPlotOf f s)

------------------------------------------------------------------------
-- Bubble plot -- ??
------------------------------------------------------------------------

-- $ bubble
-- Scatter plots with extra numeric parameter. By default the extra
-- parameter is the scale of the marker but this can be changed.

-- bubblePlot :: (PointLike (BaseSpace v) n p, R2Backend b n, Plotable (P.ScatterPlot v n) b, F.Foldable f)
--             => f (n,p) -> AxisState b v n
-- bubblePlot d = axisPlots <>= [P.Plot (P.mkBubblePlot d) def]

-- bubblePlot' :: (PointLike (BaseSpace v) n p, R2Backend b n, Plotable (P.ScatterPlot v n) b, F.Foldable f)
--             => f (n,p) -> AxisState b v n
-- bubblePlot' d s = axisPlots <>= [P.Plot (execState s $ P.mkBubblePlot d) def]

------------------------------------------------------------------------
-- GScatterPlot
------------------------------------------------------------------------

gscatterPlot
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (GScatterPlot v n a) b,
      F.Foldable f)
  => f a -> (a -> p) -> m ()
gscatterPlot d pf = addPlotable (mkGScatterPlot d pf)

gscatterPlot'
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (GScatterPlot v n a) b,
      F.Foldable f)
  => f a -> (a -> p) -> PlotState (GScatterPlot v n a) b -> m ()
gscatterPlot' d pf = addPlotable' (mkGScatterPlot d pf)


gscatterPlotL
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (GScatterPlot v n a) b,
      F.Foldable f)
  => String -> f a -> (a -> p) -> m ()
gscatterPlotL l d pf = addPlotableL l (mkGScatterPlot d pf)