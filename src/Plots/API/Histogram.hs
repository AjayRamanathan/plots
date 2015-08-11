{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE AllowAmbiguousTypes          #-}


{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

module Plots.API.Histogram
  ( histogramPlot
  , histogramPlot'
  , histogramPlotL
  , histogramPlotOf
  , histogramPlotOf'
  , histogramPlotLOf
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

import           Plots.Types.Histogram
import           Plots.API

------------------------------------------------------------------------
-- Histogram
------------------------------------------------------------------------

histogramPlot
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (HistogramPlot v n) b,
      F.Foldable f, Enum n)
  => f p -> m ()
histogramPlot d = addPlotable (mkHistogramPlot d)

histogramPlot'
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (HistogramPlot v n) b,
      F.Foldable f, Enum n)
  => f p -> PlotState (HistogramPlot v n) b -> m ()
histogramPlot' d = addPlotable' (mkHistogramPlot d)

histogramPlotL
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (HistogramPlot v n) b,
      F.Foldable f, Enum n)
  => String -> f p -> m ()
histogramPlotL l d = addPlotableL l (mkHistogramPlot d)

-- Fold variants

histogramPlotOf
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (HistogramPlot v n) b, Enum n)
  => Fold s p -> s -> m ()
histogramPlotOf f s = addPlotable (mkHistogramPlotOf f s)

histogramPlotOf'
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (HistogramPlot v n) b, Enum n)
  => Fold s p -> s -> PlotState (HistogramPlot v n) b -> m ()
histogramPlotOf' f s = addPlotable' (mkHistogramPlotOf f s)

histogramPlotLOf
  :: (v ~ BaseSpace c,
      PointLike v n p,
      MonadState (Axis b c n) m,
      Plotable (HistogramPlot v n) b, Enum n)
  => String -> Fold s p -> s -> m ()
histogramPlotLOf l f s = addPlotableL l (mkHistogramPlotOf f s)