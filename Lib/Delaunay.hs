-----------------------------------------------------------------------------
--
-- Module      :    a test for delaunay from hackage
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

import Graphics.Triangulation.Delaunay
-- import Uniform.Strings

trianl = triangulate []
dirMain :: IO ()
dirMain = do
    putStrLn "Lib.DirTree here"
    return ()





