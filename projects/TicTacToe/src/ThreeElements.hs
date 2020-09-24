module ThreeElements (ThreeElements, render) where

import Element (Element)
import qualified Element

type ThreeElements = (Element, Element, Element)

render :: ThreeElements -> String
render (e1, e2, e3) =
    "|" <> Element.render e1
        <> "|"
        <> Element.render e2
        <> "|"
        <> Element.render e3
        <> "|"
