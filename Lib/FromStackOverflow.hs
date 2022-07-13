-----------------------------------------------------------------------------
--
-- Module      :    a sub 
--------------------------------------------------------------------------- 
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Lib.DirTree   
     where
-- https://stackoverflow.com/questions/49443539/haskell-to-prolog-or-just-prolog-delaunay-triangulation


-- The type for a single point.
    type Point a = (a,a)

-- The type for a pair of points.
    type Pair a = (Point a, Point a)

-- The type for a triple of points.
    type Triple a = (Point a, Point a, Point a)

-- Predicate for a triple of 3 points is in CCW order or not
    isCCW :: Real a => Triple a -> Bool
    isCCW ((x1, y1), (x2, y2), (x3, y3)) = (x2-x1)*(y3-y1)-(x3-x1)*(y2-y1) > 0

-- Convert a triple to a CCW triple 
    toCCW :: Real a => Triple a -> Triple a
    toCCW (p1, p2, p3) = if (isCCW ((( p1, p2, p3 )))) then (p1, p2, p3)
                 else (p1, p3, p2) 

-- Generate all pairs of points from a list of points.
-- Each pair should appear exactly once in the result.
    pairsFromPoints :: Real a => [Point a] -> [Pair a]
    pairsFromPoints [] = []
    pairsFromPoints (x:xs) = map makePair xs ++ (pairsFromPoints xs)
where makePair y = (x,y)

-- Generate all unique CCW triples of points from a list of points
-- Each triple should appear exactly once in the result and be
-- CCW ordered.
    triplesFromPoints :: Real a => [Point a] -> [Triple a]
    triplesFromPoints [] = []
    triplesFromPoints (x:xs) = map makeTriple (pairsFromPoints xs) ++ (triplesFromPoints xs)
        where makeTriple (y,z) = toCCW(x,y,z)


-- import Uniform.Strings
 





