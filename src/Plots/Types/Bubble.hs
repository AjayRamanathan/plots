{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TypeFamilies              #-}

module Plots.Types.Scatter
  ( -- * Scatter plot
    ScatterPlot
  , mkScatterPlot
  , mkScatterPlotOf

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
  ) where

import           Control.Lens                    hiding (lmap, transform, ( # ))
import           Data.Foldable                   (Foldable)
import           Data.Typeable
import           Diagrams.Coordinates.Isomorphic
import           Diagrams.Prelude                hiding (view)

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
  } deriving Typeable

type instance V (GScatterPlot v n a) = v
type instance N (GScatterPlot v n a) = n

instance (Typeable a, Typeable b, TypeableFloat n, Renderable (Path V2 n) b)
    => Plotable (GScatterPlot V2 n a) b where
  renderPlotable pp s GScatterPlot {..} =
      foldMapOf sFold mk sData # applyStyle gSty
    where
      mk a = marker # moveTo p
                    # maybe id (transform  . ($ a)) sTr
                    # maybe id (applyStyle . ($ a)) sSty
        where
          p = transform (s^.specTrans) $ sPos a
      marker = pp ^. themeMarker
      gSty   = pp ^. themeMarkerStyle

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
  }

mkGScatterPlot :: (PointLike v n p, Foldable f, Fractional n)
               => f a -> (a -> p) -> GScatterPlot v n a
mkGScatterPlot = mkGScatterPlotOf folded

------------------------------------------------------------------------
-- Scatter plot lenses
------------------------------------------------------------------------

scatterTransform :: Lens' (GScatterPlot v n a) (Maybe (a -> T2 n))
scatterTransform = lens sTr (\sp t -> sp {sTr = t})

-- | Change the style for a scatter plot, given the data entry.
--
-- @@@
-- mybubbleplot & scatterStyle     ?~ mkAttr . transparency
--              & scatterTransform .~ Nothing
-- @@@
scatterStyle :: Lens' (GScatterPlot v n a) (Maybe (a -> Style V2 n))
scatterStyle = lens sSty (\sp sty -> sp {sSty = sty})

