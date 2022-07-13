-----------------------------------------------------------------------------
--
-- Module      :   a more elaborate sub
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications     #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.TryDelaunay 
     where

import UniformBase
-- import Algorithms.Geometry.DelaunayTriangulation.Naive

import Data.Geometry

{-# LANGUAGE ScopedTypeVariab#-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PolyKinds #-}
-- module Demo.Delaunay where

import           Algorithms.Geometry.DelaunayTriangulation.DivideAndConquer
import           Algorithms.Geometry.DelaunayTriangulation.Types
import           Algorithms.Geometry.EuclideanMST
import           Control.Lens
import           Data.Data
import           Data.Ext
-- import           Geometry
-- import           Geometry.PlanarSubdivision
-- import           Geometry.PlanarSubdivision.Draw
import qualified Data.List.NonEmpty as NonEmpty
import           Data.RealNumber.Rational
import           Data.Semigroup
-- import           Data.Tree.Draw
-- import           Ipe
-- import           Ipe.Color
-- import           Options.Applicative

import Linear.V2
import Data.List.NonEmpty



p1 :: V2 Int 
p1 = V2 1 2 ^+^ V2 3 4 -- works! needs Linear.V2

ps = [V2 0 0, V2 1 1, V2 0 2, V2 2 2]
psne = fromList ps :: NonEmpty (V2 Int)

-- q :: Point 2 Real 
q1 = Point2 4.5 3.3 :+ 'a'


qs :: [Point 2 Float :+ Char]
qs = [(Point2  0 0) :+ 'a' , Point2  1 1 :+ 'b' , Point2  0 2  :+ 'c', Point2  2 2  :+ 'd']

-- qsne = fmap (:+()) $ fromList qs
qsne =  fromList qs


t1 = delaunayTriangulation qsne 
-- NonEmpty.fromList [4, 5]
-- p2 :: Point 2 Int
mainDel1:: IO ()
mainDel1 = do 
    putIOwords ["mainDel"]
    putIOwords ["p1", showT p1]
    putIOwords ["ps", showT ps]
    putIOwords ["qs", showT qs]
    putIOwords ["t1", showT t1]

    return ()