module WikimediaCommonsApi (getRandomImageResources, Error(..), ImageResource, pUrlTemplate, UrlTemplate) where


import Data.Maybe
import Prelude

import Foreign.Object as Object
import Affjax as Affjax
import Affjax.ResponseFormat as AffjaxResponseFormat
import Data.Argonaut (decodeJson, class DecodeJson)
import Data.Argonaut as Argonaut
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..), hush)
import Data.Map (Map, values)
import Data.Map as Map
import Data.String.CodeUnits (fromCharArray)
import Data.Tuple.Nested (type (/\))
import Data.Typelevel.Num.Reps (D2)
import Data.Vec (Vec, vec2)
import Effect.Aff (Aff)
import Foreign.Object (Object)
import Text.Parsing.StringParser (Parser, runParser, try)
import Text.Parsing.StringParser.CodeUnits (alphaNum, anyDigit, char, noneOf, string, anyChar, eof, upperCaseChar, whiteSpace)
import Text.Parsing.StringParser.Combinators (between, many, sepBy1, many1Till, manyTill, lookAhead)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

data ImageResource = ImageResource
    { maxSize :: Vec D2 Int
    , urlTemplate :: UrlTemplate
    }

data UrlTemplate = UrlTemplate { prefix :: String, suffix :: String }

derive instance genericUrlTemplate :: Generic UrlTemplate _
derive instance genericImageResource :: Generic ImageResource _
derive instance genericError :: Generic Error _

instance showUrlTemplate :: Show UrlTemplate where
  show = genericShow

instance showImageResource :: Show ImageResource where
  show = genericShow

instance showError :: Show Error where
  show = genericShow

pUrlTemplate :: Parser UrlTemplate
pUrlTemplate = ado
  prefix <- many1Till anyChar (try $ many1Till anyDigit (lookAhead $ string "px"))
  suffix <- many1Till anyChar eof
  in UrlTemplate
     { prefix : fromCharArray $ Array.fromFoldable prefix
     , suffix : fromCharArray $ Array.fromFoldable suffix
     }

mkImageResource :: ApiResponseImageInfo -> Maybe ImageResource
mkImageResource { width, height, thumburl } = ado
  urlTemplate <- runParser pUrlTemplate thumburl # hush
  in ImageResource { maxSize : vec2 (width-1) (height-1)
                   , urlTemplate
                   }

type ApiResponseImageInfo =
  { width :: Int
  , height :: Int
  , thumburl :: String
  }

type ApiResponse =
  { query :: { pages :: Object { imageinfo :: Array ApiResponseImageInfo } } }

filterApiResponse :: ApiResponse -> Array ImageResource
filterApiResponse resp =
  resp.query.pages
    # Object.values
    # Array.fromFoldable
    >>= _.imageinfo # Array.mapMaybe mkImageResource

data Error
  = ErrJson String
  | ErrParse String

getRandomImageResources :: Int -> Aff (Either Error (Array ImageResource))
getRandomImageResources limit = do
  { body } <- Affjax.request $ Affjax.defaultRequest
      { url = "https://commons.wikimedia.org/w/api.php?origin=*&action=query&format=json&prop=imageinfo&iiprop=url|size|sha1&generator=random&iiurlwidth=100&grnnamespace=6&grnlimit=" <> show limit
      , responseFormat = AffjaxResponseFormat.json
      }
  case body of
    Left formatErr -> pure $ Left $ ErrJson $ Affjax.printResponseFormatError formatErr
    Right value -> case decodeJson value of
                     Left parseErr -> pure $ Left $ ErrParse parseErr
                     Right resp -> pure $ Right $ filterApiResponse resp
