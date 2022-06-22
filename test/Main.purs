module Test.Main where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Array.NonEmpty.Internal (NonEmptyArray(..))
import Data.Either (Either(..), either)
import Data.Foldable (for_)
import Data.List (List(Nil), (:))
import Data.Maybe (Maybe(..))
import Data.String.NonEmpty.Internal (NonEmptyString(..))
import Effect (Effect)
import Effect.Aff (Aff, forkAff, joinFiber, launchAff_, makeAff)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Kwap.Markdown
  ( Anchor(..)
  , CodeFence(..)
  , CodeFenceFileType(..)
  , Document(..)
  , Element(..)
  , Heading(..)
  , Span(..)
  , Text(..)
  , Token(..)
  , anchorP
  , codeFenceP
  , documentP
  , headingP
  , spanP
  , textP
  , tokenP
  )
import Parsing (Parser, runParser)
import Parsing.Combinators (many)
import Test.Spec (describe, it)
import Test.Spec.Assertions (fail, shouldEqual, shouldSatisfy)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main =
  let
    isOne 1 = true

    isOne _ = false

    testParser
      :: ∀ m a
       . MonadThrow Error m
      => Show a
      => Eq a
      => Parser String a
      -> String
      -> a
      -> m Unit
    testParser p inp expect = runParser inp p #
      (either (show >>> fail) ((flip shouldEqual) expect))
  in
    launchAff_
      $ runSpec [ consoleReporter ] do
          describe "markdown" do
            describe "text" do
              it "should parse unstyled text" do
                testParser (spanP []) "foo"
                  (Span $ NonEmptyArray [ TextToken $ Unstyled "foo" ])
              it "should parse _italic_ text" do
                testParser (textP []) "_foo_" (Italic "foo")
              it "should parse _italic_ text 2" do
                testParser (spanP []) " _foo_"
                  ( Span $ NonEmptyArray $ [ Unstyled " ", Italic "foo" ] <#>
                      TextToken
                  )
              it "should parse some text" do
                testParser (spanP []) "`what` _foo_"
                  ( Span $ NonEmptyArray $
                      [ InlineCode "what", Unstyled " ", Italic "foo" ] <#>
                        TextToken
                  )
              it "should parse `inlineCode` text" do
                testParser (textP []) "`foo`" (InlineCode "foo")
              it "should parse *italic* text" do
                testParser (textP []) "*foo*" (Italic "foo")
              it "should parse **bold** text" do
                testParser (textP []) "**foo**" (Bold "foo")
              it "should parse ***bold italic*** text" do
                testParser (textP []) "***foo***" (BoldItalic "foo")
              it "should parse _**bold italic**_ text" do
                testParser (textP []) "_**foo**_" (BoldItalic "foo")
              it "should parse **_bold italic_** text" do
                testParser (textP []) "**_foo_**" (BoldItalic "foo")
              it "should parse a **_bold italic**_ edgecase" do
                testParser (textP []) "**_foo**_" (BoldItalic "foo**_")
              it "should parse a **_bold italic_** edgecase" do
                testParser (textP []) "_**foo_**" (BoldItalic "foo_**")
            describe "token" do
              it "should parse _italic_ text" do
                testParser (tokenP []) "_foo_" (TextToken (Italic "foo"))
              it "should parse _**bold italic**_ text" do
                testParser (tokenP []) "_**foo**_"
                  (TextToken (BoldItalic "foo"))
              it "should parse [_**bold italic**_](cheese.com) link" do
                testParser (tokenP []) "[_**bold italic**_](cheese.com)"
                  $ AnchorToken
                  $ Anchor (NonEmptyArray [ BoldItalic "bold italic" ])
                      "cheese.com"
              it "should parse [_**bold**_]   (cheese.com) link with spaces" do
                testParser (tokenP []) "[**bold**]   (cheese.com)"
                  $ AnchorToken
                  $ Anchor (NonEmptyArray [ Bold "bold" ]) "cheese.com"
              it
                "should parse [_**bold italic**_ multiple *tokens*](cheese.com) link"
                do
                  testParser (tokenP []) "[gold _bar_  **bart**](cheese.com)"
                    $ AnchorToken
                    $ Anchor
                        ( NonEmptyArray
                            [ Unstyled "gold "
                            , Italic "bar"
                            , Unstyled "  "
                            , Bold "bart"
                            ]
                        )
                        "cheese.com"
            describe "heading" do
              it "should parse # [h1](with link)" do
                testParser headingP "# [**foo** stink](bar)"
                  ( H1
                      ( Span
                          ( NonEmptyArray
                              [ AnchorToken
                                  ( Anchor
                                      ( NonEmptyArray
                                          [ Bold "foo", Unstyled " stink" ]
                                      )
                                      "bar"
                                  )
                              ]
                          )
                      )
                  )
              it "should parse # h1" do
                testParser headingP "# _foo_"
                  (H1 (Span (NonEmptyArray [ TextToken (Italic "foo") ])))
              it "should parse ## h2" do
                testParser headingP "## _foo_"
                  (H2 (Span (NonEmptyArray [ TextToken (Italic "foo") ])))
              it "should parse ### h3" do
                testParser headingP "### _foo_"
                  (H3 (Span (NonEmptyArray [ TextToken (Italic "foo") ])))
              it "should parse #### h4" do
                testParser headingP "#### _foo_"
                  (H4 (Span (NonEmptyArray [ TextToken (Italic "foo") ])))
              it "should parse ##### h5" do
                testParser headingP "##### _foo_"
                  (H5 (Span (NonEmptyArray [ TextToken (Italic "foo") ])))
              it "should parse ###### h6" do
                testParser headingP "###### _foo_"
                  (H6 (Span (NonEmptyArray [ TextToken (Italic "foo") ])))
            describe "code fence" do
              it "should parse code of type rust" do
                testParser codeFenceP
                  "```rust\n\
                  \pub fn main() {\n\
                  \  println!(\"it works!\");\n\
                  \}\n\
                  \```"
                  ( CodeFence (Just (CodeFenceFileType (NonEmptyString "rust")))
                      "pub fn main() {\n  println!(\"it works!\");\n}"
                  )
              it "should parse code" do
                testParser codeFenceP
                  "```\n\
                  \> echo 'foo'\n\
                  \```"
                  (CodeFence Nothing "> echo 'foo'")
            describe "document" do
              it "should parse document" do
                testParser
                  documentP
                  "# hello\n\
                  \this is my **markdown** document\n\n\
                  \`it has code` _and style_\n\n\
                  \```rust\n\
                  \pub fn main() {\n\
                  \  println!(\"it works!\");\n\
                  \}\n\
                  \```[check out my *website*](cheese.com)"
                  $ Document
                      [ ElementHeading
                          ( H1
                              ( Span
                                  ( NonEmptyArray
                                      [ TextToken (Unstyled "hello") ]
                                  )
                              )
                          )
                      , ElementSpan
                          ( Span
                              ( NonEmptyArray
                                  [ TextToken (Unstyled "this is my ")
                                  , TextToken (Bold "markdown")
                                  , TextToken (Unstyled " document")
                                  ]
                              )
                          )
                      , ElementSpan
                          ( Span
                              ( NonEmptyArray
                                  [ TextToken (InlineCode "it has code")
                                  , TextToken (Unstyled " ")
                                  , TextToken (Italic "and style")
                                  ]
                              )
                          )
                      , ElementCodeFence
                          ( CodeFence
                              (Just (CodeFenceFileType (NonEmptyString "rust")))
                              "pub fn main() {\n  println!(\"it works!\");\n}"
                          )
                      , ElementSpan
                          ( Span
                              ( NonEmptyArray
                                  [ AnchorToken
                                      ( Anchor
                                          ( NonEmptyArray
                                              [ Unstyled "check out my "
                                              , Italic "website"
                                              ]
                                          )
                                          "cheese.com"
                                      )
                                  ]
                              )
                          )
                      ]
