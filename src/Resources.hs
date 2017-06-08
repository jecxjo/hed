{-# LANGUAGE TemplateHaskell #-}

module Resources ( getResource, listResources ) where

import qualified Data.ByteString as BS
import Data.FileEmbed
import Data.List (find)

resources :: [(FilePath, BS.ByteString)]
resources = $(embedDir "res")

getResource :: FilePath -> Maybe BS.ByteString
getResource path = findResource resources
  where
    findResource [] = Nothing
    findResource ((p,cont):xs) = if p == path
                                 then (Just cont)
                                 else findResource xs

listResources :: [String]
listResources = map fst resources
