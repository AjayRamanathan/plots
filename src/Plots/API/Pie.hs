{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE AllowAmbiguousTypes          #-}


{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

module Plots.API.Ribbon
  ( ribbonPlot
  , ribbonPlot'
  , ribbonPlotL
  , ribbonPlotOf
  , ribbonPlotOf'
  , ribbonPlotLOf
  , barPlotRatioMultiC
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

import           Plots.Types.Ribbon
import           Plots.API