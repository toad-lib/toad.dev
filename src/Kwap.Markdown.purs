module Kwap.Markdown where

import Prelude
import Control.Alternative ((<|>))
import Data.Foldable (foldl)
import Data.Array as Array
import Data.String.NonEmpty.CodeUnits (fromNonEmptyCharArray)
import Data.Array.NonEmpty as NEA
import Data.String.NonEmpty as NES
import Data.Maybe (maybe, Maybe(..))
import Data.Tuple (fst, snd)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Parsing (Parser, fail)
import Parsing.Combinators
  ( advance
  , choice
  , endBy1
  , lookAhead
  , manyTill
  , many1Till_
  , manyTill_
  , optional
  , between
  , many
  , many1
  , skipMany
  , try
  , notFollowedBy
  )
import Parsing.Combinators.Array as CombinatorArray
import Parsing.String (anyTill, char, eof, string, rest, match, anyChar)
import Parsing.String.Basic (space)
import Effect.Console (log)
import Effect.Unsafe (unsafePerformEffect)

derive instance eqCodeFenceFileType :: Eq CodeFenceFileType

derive instance eqCodeFence :: Eq CodeFence

derive instance eqSpan :: Eq Span

derive instance eqToken :: Eq Token

derive instance eqText :: Eq Text

derive instance eqHeading :: Eq Heading

derive instance eqElement :: Eq Element

derive instance eqDocument :: Eq Document

derive instance eqAnchor :: Eq Anchor

derive instance genericCodeFenceFileType :: Generic CodeFenceFileType _

derive instance genericCodeFence :: Generic CodeFence _

derive instance genericSpan :: Generic Span _

derive instance genericToken :: Generic Token _

derive instance genericText :: Generic Text _

derive instance genericHeading :: Generic Heading _

derive instance genericElement :: Generic Element _

derive instance genericDocument :: Generic Document _

derive instance genericAnchor :: Generic Anchor _

instance showCodeFenceFileType :: Show CodeFenceFileType where
  show = genericShow

instance showCodeFence :: Show CodeFence where
  show = genericShow

instance showSpan :: Show Span where
  show = genericShow

instance showText :: Show Text where
  show = genericShow

instance showHeading :: Show Heading where
  show = genericShow

instance showElement :: Show Element where
  show = genericShow

instance showDocument :: Show Document where
  show = genericShow

instance showAnchor :: Show Anchor where
  show = genericShow

instance showToken :: Show Token where
  show (AnchorToken an) = "AnchorToken (" <> show an <> ")"
  show (TextToken text) = "TextToken (" <> show text <> ")"

data Text
  = Unstyled String
  | Bold String
  | Italic String
  | BoldItalic String
  | InlineCode String

data Span
  = Span (NEA.NonEmptyArray Token)

data Anchor
  = Anchor (NEA.NonEmptyArray Text) String

data Token
  = AnchorToken Anchor
  | TextToken Text

data CodeFenceFileType
  = CodeFenceFileType NES.NonEmptyString

data CodeFence
  = CodeFence (Maybe CodeFenceFileType) String

data Heading
  = H1 Span
  | H2 Span
  | H3 Span
  | H4 Span
  | H5 Span
  | H6 Span

data Element
  = ElementHeading Heading
  | ElementCodeFence CodeFence
  | ElementSpan Span

data Document
  = Document (Array Element)

newtype Stop
  = Stop (Parser String String)

tokenText :: Token -> Maybe Text
tokenText (TextToken t) = Just t

tokenText _ = Nothing

stop :: Stop -> Parser String String
stop (Stop p) = p

tokenStop :: Stop
tokenStop = Stop (choice [ eof <#> const "", string "\n\n" ])

untilTokenStopOr :: Array Stop -> Parser String String
untilTokenStopOr others =
  choice (([ tokenStop ] <> others) <#> stop)
    # many1Till_ anyChar
    <#> (fst >>> NEA.fromFoldable1 >>> fromNonEmptyCharArray >>> NES.toString)

combineUnstyled :: ∀ a. (a -> Maybe Text) -> (Text -> a) -> NEA.NonEmptyArray a -> NEA.NonEmptyArray a
combineUnstyled text ofText tokens =
  let
    collapse soFar token = case [ text $ NEA.last soFar, text token ] of
      [ Just (Unstyled a), Just (Unstyled b) ] ->
        maybe
          (NEA.singleton $ ofText $ Unstyled $ a <> b)
          (_ <> (NEA.singleton $ ofText $ Unstyled $ a <> b))
          (NEA.init >>> NEA.fromArray $ soFar)
      _ -> soFar <> (NEA.singleton token)
  in
    foldl collapse (NEA.singleton $ NEA.head tokens) (NEA.tail tokens)

documentP :: Parser String Document
documentP = CombinatorArray.many elementP <#> Document

elementP :: Parser String Element
elementP =
  (headingP <#> ElementHeading)
    <|> (codeFenceP <#> ElementCodeFence)
    <|> ((spanP []) <#> ElementSpan)

codeFenceP :: Parser String CodeFence
codeFenceP = do
  _ <- string "```"
  fileType <- anyTill (string "\n") <#> fst
  contents <- anyTill (optional (string "\n") *> string "```") <#> fst
  pure $ CodeFence (NES.fromString fileType <#> CodeFenceFileType) contents

headingP :: Parser String Heading
headingP =
  let
    h pre ctor = (string pre *> many (char ' ') *> spanP [ Stop $ string "\n" ] <#> ctor)
  in
    h "######" H6
      <|> h "#####" H5
      <|> h "####" H4
      <|> h "###" H3
      <|> h "##" H2
      <|> h "#" H1

spanP :: Array Stop -> Parser String Span
spanP stops = many1Till_ (tokenP stops) (choice $ [ tokenStop ] <> stops <#> stop) <#> (fst >>> NEA.fromFoldable1 >>> combineUnstyled tokenText TextToken >>> Span)

tokenP :: Array Stop -> Parser String Token
tokenP stops = (try anchorP <#> AnchorToken) <|> ((textP $ [ tokenStop ] <> stops) <#> TextToken)

anchorP :: Parser String Anchor
anchorP = do
  _ <- char '['
  label <-
    many1Till_ (advance $ textP [ Stop $ lookAhead $ string "]" ]) (lookAhead $ string "]")
      <#> fst
      <#> NEA.fromFoldable1
  _ <- char ']'
  _ <- many $ char ' '
  _ <- char '('
  href <- untilTokenStopOr [ Stop $ string ")" ]
  pure $ Anchor (combineUnstyled Just identity label) href

textP :: Array Stop -> Parser String Text
textP stops =
  choice
    [ try $ notStartWithStop $ parseWith InlineCode $ string "`"
    , try $ notStartWithStop $ parseWith BoldItalic (choice $ [ "***", "**_", "_**" ] <#> string)
    , try $ notStartWithStop $ parseWith Italic (choice $ [ singleAsterisk, string "_" ])
    , try $ notStartWithStop $ parseWith Bold $ string "**"
    , notStartWithStop
        $ anyChar
        <#> NEA.singleton
        <#> fromNonEmptyCharArray
        <#> NES.toString
        <#> Unstyled
    ]
  where
  notStartWithStop p = (notFollowedBy $ choice $ stops <#> stop) >>= const p

  singleAsterisk =
    string "*"
      <#> (pure >>> const)
      >>= discard (notFollowedBy $ string "*")

  closingDelim "`" = string "`"

  closingDelim "_" = string "_"

  closingDelim "*" = string "*"

  closingDelim "**" = string "**"

  closingDelim "***" = string "***"

  closingDelim "**_" = string "_**"

  closingDelim "_**" = string "**_"

  closingDelim _ = fail "unreachable"

  parseWith ctor delim = do
    openDelim <- delim
    text <- untilTokenStopOr $ stops <> [ Stop $ closingDelim openDelim ]
    pure $ ctor text
