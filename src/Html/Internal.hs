module Html.Internal where
import Numeric.Natural (Natural)

newtype Html = Html String
newtype Structure = Structure String

type Title = String

html_ :: Title -> Structure -> Html
html_ title (Structure body) =
    Html (el "head" (el "title" (escape title)) <> el "body" body)

p_ :: String -> Structure
p_ = Structure . el "p" . escape

h_ :: Natural -> String -> Structure
h_ n = Structure . el ("h" <> show n) . escape

el :: String -> String -> String
el tag content =
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

ul_ :: [Structure] -> Structure
ul_ = Structure . el "ul" . concatMap (el "li" . getStructureString)

ol_ :: [Structure] -> Structure
ol_ = Structure . el "ol" . concatMap (el "li" . getStructureString)

code_ :: String -> Structure
code_ = Structure . el "pre" . escape

instance Semigroup Structure where
    (<>) (Structure a) (Structure b) =
        Structure (a <> b)

instance Monoid Structure where
    mempty = empty_

getStructureString :: Structure -> String
getStructureString struct =
    case struct of
    Structure str -> str

render :: Html -> String
render html =
    case html of
    Html str -> str

escape :: String -> String
escape =
    let
        escapeChar c =
            case c of
            '<' -> "&lt;"
            '>' -> "&gt;"
            '&' -> "&amp;"
            '"' -> "&quot;"
            '\'' -> "&#39;"
            _   -> [c]
    in concatMap escapeChar

empty_ :: Structure
empty_ = Structure ""

concatStructure :: [Structure] -> Structure
concatStructure ss =
    case ss of
    [] -> empty_
    s:rest -> s <> concatStructure rest
