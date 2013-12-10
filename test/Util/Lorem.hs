module Util.Lorem(
  generate
  ) where

import Data.Char

generate :: [String]
generate =  words $ map toLower raw 
  where
    raw = "Lorem ipsum dolor sit amet consectetur adipiscing elit Etiam tempor volutpat enim nec sagittis" ++
          " Nunc suscipit quam mi in posuere ipsum rhoncus a Praesent non lorem libero Etiam ultricies felis" ++
          " eget volutpat posuere Nulla molestie eros vel purus dictum vitae ullamcorper arcu tempor phasellus" ++
          " mollis libero quis risus vulputate a semper erat laoreet etiam faucibus leo ac lectus commodo" ++
          " sit amet tempus est suscipit Donec sit amet accumsan erat viverra lacinia lorem Etiam hendrerit" ++
          " aliquet lorem tincidunt gravida Integer urna nisi tempus vitae pretium quis porttitor varius ligula"++
          " Quisque eu tincidunt felis ac pellentesque mauris Nunc urna tellus tristique a diam at mollis fermentum" ++
          " ipsum Sed quis lectus iaculis ultricies sapien nec vulputate nibh Praesent fringilla mauris nec ultrices" ++
          " rhoncus Mauris vitae pretium sapien Sed purus sapien ornare vel bibendum nec mollis quis leo Pellentesque"
