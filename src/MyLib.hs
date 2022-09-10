newtype Html = Html String
newtype Structure = Structure String

getStructureString :: Structure -> String
getStructureString struct =
    case struct of
    Structure str -> str

body_ :: String -> Structure
body_ = Structure . el "body"

head_ :: String -> Structure
head_ = Structure . el "head"

title_ :: String -> Structure
title_ = Structure . el "title"

p_ :: String -> Structure
p_ = Structure . el "p"

h1_ :: String -> Structure
h1_ = Structure . el "h1"

append_ :: Structure -> Structure -> Structure
append_ (Structure a) (Structure b) =
    Structure (a <> b)

render :: Html -> String
render html =
    case html of
    Html str -> str

type Title = String

html_ :: Title -> Structure -> Html
html_ title (Structure body) =
    Html (el "head" (el "title" title) <> el "body" body)

myhtml :: Html
myhtml =
    html_
    "My title"
    (append_
        (h1_ "Heading")
        (append_
            (p_ "Paragraph #1")
            (p_ "Paragraph #2")
        )
    )

el :: String -> String -> String
el tag content =
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"
