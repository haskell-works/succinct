module Data.Succinct.Xml.Lens where

import Control.Lens
import Data.Succinct.Xml.Value
import Data.Text                   (Text)

isTagNamed :: Text -> Value -> Bool
isTagNamed a (XmlElement b _ _) | a == b  = True
isTagNamed _     _              = False

tagNamed :: (Applicative f, Choice p) => Text -> Optic' p f Value Value
tagNamed = filtered . isTagNamed
