-----------------------------------------------------------------------------
--
-- Module      :   an example how to use the triangulation from hgeometry 

-- dependencies:
--  - hgeometry 
--   - hgeometry-combinatorial
--   - linear
-- all language extensions are explicitely in the file

-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}
-- {-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.TryDelaunay 
     where

import UniformBase
-- import Algorithms.Geometry.DelaunayTriangulation.Naive

import Data.Geometry 
-- ( Point(Point2), Additive((^+^)) )


-- module Demo.Delaunay where

import Algorithms.Geometry.DelaunayTriangulation.DivideAndConquer
    ( delaunayTriangulation )
-- import           Algorithms.Geometry.DelaunayTriangulation.Types
-- import           Algorithms.Geometry.EuclideanMST
-- import           Control.Lens
-- import           Data.Data
import Data.Ext ( type (:+)(..) )
-- import           Geometry
-- import           Geometry.PlanarSubdivision
-- import           Geometry.PlanarSubdivision.Draw
-- import qualified Data.List.NonEmpty as NonEmpty
-- import           Data.RealNumber.Rational
-- import           Data.Semigroup
-- import           Data.Tree.Draw
-- import           Ipe
-- import           Ipe.Color
-- import           Options.Applicative

import Linear.V2 ( V2(..) )
import Data.List.NonEmpty ( NonEmpty, fromList )
import Algorithms.Geometry.DelaunayTriangulation.Types
    ( Triangulation, toPlanarSubdivision, toPlaneGraph )
-- import Data.PlaneGraph ( PlaneGraph )
-- import Data.PlaneGraph ( PlaneGraph )
import Data.Geometry.PlanarSubdivision
    -- ( PlanarSubdivision, PlaneGraph, edgeSegment )

import Data.Aeson.Encode.Pretty (encodePretty)


p1 :: V2 Int 
p1 = V2 1 2 ^+^ V2 3 4 -- works! needs Linear.V2

ps = [V2 0 0, V2 1 1, V2 0 2, V2 2 2]
psne = fromList ps :: NonEmpty (V2 Int)

-- q :: Point 2 Real 
q1 = Point2 4.5 3.3 :+ 'a'


qs :: [Point 2 Float :+ Char]
qs = [(Point2  0 0) :+ 'a' , Point2  1.5 1.5 :+ 'b' , Point2  0 2  :+ 'c', Point2  2 0  :+ 'd']
qs2 :: [Point 2 Float]
qs2 = [(Point2  0 0) , Point2  1.5 1.5  , Point2  0 2  , Point2  2 0  ]
qsColl :: [Point 2 Integer :+ Char]
qsColl = [(Point2  0 0) :+ 'a' , Point2  1 1 :+ 'b' , Point2  0 2  :+ 'c', Point2  2 0  :+ 'd']-- fails with b 1 1 ??

qsne1 = fmap (:+()) $ fromList qs
qsne2 :: NonEmpty (Point 2 Float :+ Char)
qsne2 =  fromList qs
qsne3 :: NonEmpty (Point 2 Float :+ Int)
qsne3 = fromList . zipWith (:+) qs2 $ [1..]

t1 :: Triangulation Char Float
t1 = delaunayTriangulation qsne2 

g1 :: PlanarSubdivision s Char () () Float
g1 = toPlanarSubdivision t1
pg1 :: forall k (s :: k). PlaneGraph s Char () () Float
pg1 = toPlaneGraph t1  --

pg2 = toPlaneGraph . delaunayTriangulation $ qsne3
-- pg1json = toJSON pg1 
pg1pretty = encodePretty pg1 
pg2pretty = encodePretty  pg2
-- planeg1 = writePlaneGraph pg1pretty
-- NonEmpty.fromList [4, 5]
-- p2 :: Point 2 Int

-- sg2 = edgeSegments pg2
v2s = vertices g1
mainDel1:: IO ()
mainDel1 = do 
    putIOwords ["mainDel"]
    putIOwords ["p1", showT p1]
    putIOwords ["ps", showT ps]
    putIOwords ["qs", showT qs]
    -- putIOwords ["qsFails", showT . delaunayTriangulation . fromList $ qsColl]
    putIOwords ["t1", showT t1]
    putIOwords ["g1", showT g1]
    putIOwords ["the json format of the planeGraph", showT pg1]
    putIOwords ["the json format of the planeGraph",   bl2t pg1pretty]
    putIOwords ["the json format of the planeGraph pg2",   bl2t pg2pretty]
    -- putIOwords ["sg2", showT sg2]
    return ()