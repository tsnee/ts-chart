module Plotly.TraceData where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Test.QuickCheck.Arbitrary (class Arbitrary)

import Plotly.ColorScale (ColorScale)
import Plotly.Line (Line)
import Plotly.Marker (Marker)
import SparseEncoder (field, sparseRecordToJson)

newtype TraceData a b c = TraceData
  { x :: Array a
  , xaxis :: Maybe String
  , y :: Array b
  , yaxis :: Maybe String
  , z :: Maybe (Array c)
  , zaxis :: Maybe String
  , colorscale :: Maybe ColorScale
  , fill :: Maybe String
  , fillcolor :: Maybe String
  , hovertext :: Maybe (Array String)
  , legendgroup :: Maybe String
  , line :: Maybe Line
  , marker :: Maybe Marker
  , mode :: Maybe String
  , name :: Maybe String
  , opacity :: Maybe Number
  , showlegend :: Maybe Boolean
  , showscale :: Maybe Boolean
  , text :: Maybe (Array String)
  , type :: String
  }
derive newtype instance arbitraryTraceData :: Arbitrary (TraceData Number Number Number)
derive newtype instance decodeJsonTraceData
  :: (DecodeJson a, DecodeJson b, DecodeJson c)
  => DecodeJson (TraceData a b c)
instance encodeJsonTraceData
  :: (EncodeJson a, EncodeJson b, EncodeJson c)
  => EncodeJson (TraceData a b c) where
  encodeJson (TraceData d) = sparseRecordToJson
    [ field "x" (Just d.x)
    , field "xaxis" d.xaxis
    , field "y" (Just d.y)
    , field "yaxis" d.yaxis
    , field "z" d.z
    , field "zaxis" d.zaxis
    , field "colorscale" d.colorscale
    , field "fill" d.fill
    , field "fillcolor" d.fillcolor
    , field "hovertext" d.hovertext
    , field "legendgroup" d.legendgroup
    , field "line" d.line
    , field "marker" d.marker
    , field "mode" d.mode
    , field "name" d.name
    , field "opacity" d.opacity
    , field "showlegend" d.showlegend
    , field "showscale" d.showscale
    , field "text" d.text
    , field "type" (Just d.type)
    ]
derive newtype instance eqTraceData
  :: (Eq a, Eq b, Eq c)
  => Eq (TraceData a b c)

smartTraceData
  :: forall a b
  . Array a -> Array b -> String -> TraceData a b Unit
smartTraceData x y typ =
  TraceData
  { x
  , xaxis: Nothing
  , y
  , yaxis: Nothing
  , z: Nothing
  , zaxis: Nothing
  , colorscale: Nothing
  , fill: Nothing
  , fillcolor: Nothing
  , hovertext: Nothing
  , legendgroup: Nothing
  , line: Nothing
  , marker: Nothing
  , mode: Nothing
  , name: Nothing
  , opacity: Nothing
  , showlegend: Nothing
  , showscale: Nothing
  , text: Nothing
  , type: typ
  }

withXAxis :: forall a b c. String -> TraceData a b c -> TraceData a b c
withXAxis xaxis (TraceData d) = TraceData $ d { xaxis = Just xaxis }

withYAxis :: forall a b c. String -> TraceData a b c -> TraceData a b c
withYAxis yaxis (TraceData d) = TraceData $ d { yaxis = Just yaxis }

withZ :: forall a b c d. Array d -> TraceData a b c -> TraceData a b d
withZ z (TraceData d) = TraceData $ d { z = Just z }

withZAxis :: forall a b c. String -> TraceData a b c -> TraceData a b c
withZAxis zaxis (TraceData d) = TraceData $ d { zaxis = Just zaxis }

withColorscale :: forall a b c. ColorScale -> TraceData a b c -> TraceData a b c
withColorscale cs (TraceData d) = TraceData $ d { colorscale = Just cs }

withFill :: forall a b c. String -> TraceData a b c -> TraceData a b c
withFill fill (TraceData d) = TraceData $ d { fill = Just fill }

withFillcolor :: forall a b c. String -> TraceData a b c -> TraceData a b c
withFillcolor fillcolor (TraceData d) = TraceData $ d { fillcolor = Just fillcolor }

withLine :: forall a b c. Line -> TraceData a b c -> TraceData a b c
withLine line (TraceData d) = TraceData $ d { line = Just line }

withMarker :: forall a b c. Marker -> TraceData a b c -> TraceData a b c
withMarker marker (TraceData d) = TraceData $ d { marker = Just marker }

withMode :: forall a b c. String -> TraceData a b c -> TraceData a b c
withMode mode (TraceData d) = TraceData $ d { mode = Just mode }

withName :: forall a b c. String -> TraceData a b c -> TraceData a b c
withName name (TraceData d) = TraceData $ d { name = Just name }

withShowscale :: forall a b c. Boolean -> TraceData a b c -> TraceData a b c
withShowscale b (TraceData d) = TraceData $ d { showscale = Just b }
