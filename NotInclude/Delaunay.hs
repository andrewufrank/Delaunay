-----------------------------------------------------------------------------
--
-- Module      :    a test for delaunay from hackage
-- dependencies cannot be satisfied by cabal 
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

module Lib.Delaunay 
     where

import Graphics.Triangulation.Delaunay 
-- git://github.com/mruegenberg/Delaunay.git
-- import Uniform.Strings

trianl = triangulate []
dirMain :: IO ()
dirMain = do
    putStrLn "Lib.DirTree here"
    return ()





