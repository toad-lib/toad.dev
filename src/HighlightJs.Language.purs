module HighlightJs.Language (requireLanguage, Language(..), LanguageModule, LanguageAlias(..), defaultAlias) where

import Prelude

newtype LanguageAlias = LanguageAlias String

defaultAlias :: Language -> LanguageAlias
defaultAlias = str >>> LanguageAlias
  where
  str = case _ of
    Lang1c -> "1c"
    Abnf -> "abnf"
    Accesslog -> "accesslog"
    Actionscript -> "actionscript"
    Ada -> "ada"
    Angelscript -> "angelscript"
    Apache -> "apache"
    Applescript -> "applescript"
    Arcade -> "arcade"
    Arduino -> "arduino"
    Armasm -> "armasm"
    Asciidoc -> "asciidoc"
    Aspectj -> "aspectj"
    Autohotkey -> "autohotkey"
    Autoit -> "autoit"
    Avrasm -> "avrasm"
    Awk -> "awk"
    Axapta -> "axapta"
    Bash -> "bash"
    Basic -> "basic"
    Bnf -> "bnf"
    Brainfuck -> "brainfuck"
    Cal -> "cal"
    Capnproto -> "capnproto"
    Ceylon -> "ceylon"
    C -> "c"
    Clean -> "clean"
    Clojure -> "clojure"
    ClojureRepl -> "clojure-repl"
    Cmake -> "cmake"
    Coffeescript -> "coffeescript"
    Coq -> "coq"
    Cos -> "cos"
    Cpp -> "cpp"
    Crmsh -> "crmsh"
    Crystal -> "crystal"
    Csharp -> "csharp"
    Csp -> "csp"
    Css -> "css"
    Dart -> "dart"
    Delphi -> "delphi"
    Diff -> "diff"
    Django -> "django"
    D -> "d"
    Dns -> "dns"
    Dockerfile -> "dockerfile"
    Dos -> "dos"
    Dsconfig -> "dsconfig"
    Dts -> "dts"
    Dust -> "dust"
    Ebnf -> "ebnf"
    Elixir -> "elixir"
    Elm -> "elm"
    Erb -> "erb"
    Erlang -> "erlang"
    ErlangRepl -> "erlang-repl"
    Excel -> "excel"
    Fix -> "fix"
    Flix -> "flix"
    Fortran -> "fortran"
    Fsharp -> "fsharp"
    Gams -> "gams"
    Gauss -> "gauss"
    Gcode -> "gcode"
    Gherkin -> "gherkin"
    Glsl -> "glsl"
    Gml -> "gml"
    Go -> "go"
    Golo -> "golo"
    Gradle -> "gradle"
    Graphql -> "graphql"
    Groovy -> "groovy"
    Haml -> "haml"
    Handlebars -> "handlebars"
    Haskell -> "haskell"
    Haxe -> "haxe"
    Hsp -> "hsp"
    Html -> "html"
    Http -> "http"
    Hy -> "hy"
    Inform7 -> "inform7"
    Ini -> "ini"
    Irpf90 -> "irpf90"
    Isbl -> "isbl"
    Java -> "java"
    Javascript -> "javascript"
    JbossCli -> "jboss-cli"
    Json -> "json"
    Julia -> "julia"
    JuliaRepl -> "julia-repl"
    Kotlin -> "kotlin"
    Lasso -> "lasso"
    Latex -> "latex"
    Ldif -> "ldif"
    Leaf -> "leaf"
    Less -> "less"
    Lisp -> "lisp"
    Livecodeserver -> "livecodeserver"
    Livescript -> "livescript"
    Llvm -> "llvm"
    Lsl -> "lsl"
    Lua -> "lua"
    Makefile -> "makefile"
    Markdown -> "markdown"
    Mathematica -> "mathematica"
    Matlab -> "matlab"
    Maxima -> "maxima"
    Mel -> "mel"
    Mercury -> "mercury"
    Mipsasm -> "mipsasm"
    Mizar -> "mizar"
    Mojolicious -> "mojolicious"
    Monkey -> "monkey"
    Moonscript -> "moonscript"
    N1ql -> "n1ql"
    Nestedtext -> "nestedtext"
    Nginx -> "nginx"
    Nim -> "nim"
    Nix -> "nix"
    NodeRepl -> "node-repl"
    Nsis -> "nsis"
    Objectivec -> "objectivec"
    Ocaml -> "ocaml"
    Openscad -> "openscad"
    Oxygene -> "oxygene"
    Parser3 -> "parser3"
    Perl -> "perl"
    Pf -> "pf"
    Pgsql -> "pgsql"
    Php -> "php"
    PhpTemplate -> "php-template"
    Plaintext -> "plaintext"
    Pony -> "pony"
    Powershell -> "powershell"
    Processing -> "processing"
    Profile -> "profile"
    Prolog -> "prolog"
    Properties -> "properties"
    Protobuf -> "protobuf"
    Puppet -> "puppet"
    Purebasic -> "purebasic"
    Python -> "python"
    PythonRepl -> "python-repl"
    Q -> "q"
    Qml -> "qml"
    Reasonml -> "reasonml"
    Rib -> "rib"
    R -> "r"
    Roboconf -> "roboconf"
    Routeros -> "routeros"
    Rsl -> "rsl"
    Ruby -> "ruby"
    Ruleslanguage -> "ruleslanguage"
    Rust -> "rust"
    Sas -> "sas"
    Scala -> "scala"
    Scheme -> "scheme"
    Scilab -> "scilab"
    Scss -> "scss"
    Shell -> "shell"
    Smali -> "smali"
    Smalltalk -> "smalltalk"
    Sml -> "sml"
    Sqf -> "sqf"
    Sql -> "sql"
    Stan -> "stan"
    Stata -> "stata"
    Step21 -> "step21"
    Stylus -> "stylus"
    Subunit -> "subunit"
    Swift -> "swift"
    Taggerscript -> "taggerscript"
    Tap -> "tap"
    Tcl -> "tcl"
    Thrift -> "thrift"
    Tp -> "tp"
    Twig -> "twig"
    Typescript -> "typescript"
    Vala -> "vala"
    Vbnet -> "vbnet"
    VbscriptHtml -> "vbscript-html"
    Vbscript -> "vbscript"
    Verilog -> "verilog"
    Vhdl -> "vhdl"
    Vim -> "vim"
    Wasm -> "wasm"
    Wren -> "wren"
    X86asm -> "x86asm"
    Xl -> "xl"
    Xml -> "xml"
    Xquery -> "xquery"
    Yaml -> "yaml"
    Zephir -> "zephir"

foreign import data LanguageModule :: Type

foreign import lang1c :: LanguageModule
foreign import abnf :: LanguageModule
foreign import accesslog :: LanguageModule
foreign import actionscript :: LanguageModule
foreign import ada :: LanguageModule
foreign import angelscript :: LanguageModule
foreign import apache :: LanguageModule
foreign import applescript :: LanguageModule
foreign import arcade :: LanguageModule
foreign import arduino :: LanguageModule
foreign import armasm :: LanguageModule
foreign import asciidoc :: LanguageModule
foreign import aspectj :: LanguageModule
foreign import autohotkey :: LanguageModule
foreign import autoit :: LanguageModule
foreign import avrasm :: LanguageModule
foreign import awk :: LanguageModule
foreign import axapta :: LanguageModule
foreign import bash :: LanguageModule
foreign import basic :: LanguageModule
foreign import bnf :: LanguageModule
foreign import brainfuck :: LanguageModule
foreign import cal :: LanguageModule
foreign import capnproto :: LanguageModule
foreign import ceylon :: LanguageModule
foreign import c :: LanguageModule
foreign import clean :: LanguageModule
foreign import clojure :: LanguageModule
foreign import clojureRepl :: LanguageModule
foreign import cmake :: LanguageModule
foreign import coffeescript :: LanguageModule
foreign import coq :: LanguageModule
foreign import cos :: LanguageModule
foreign import cpp :: LanguageModule
foreign import crmsh :: LanguageModule
foreign import crystal :: LanguageModule
foreign import csharp :: LanguageModule
foreign import csp :: LanguageModule
foreign import css :: LanguageModule
foreign import dart :: LanguageModule
foreign import delphi :: LanguageModule
foreign import diff :: LanguageModule
foreign import django :: LanguageModule
foreign import d :: LanguageModule
foreign import dns :: LanguageModule
foreign import dockerfile :: LanguageModule
foreign import dos :: LanguageModule
foreign import dsconfig :: LanguageModule
foreign import dts :: LanguageModule
foreign import dust :: LanguageModule
foreign import ebnf :: LanguageModule
foreign import elixir :: LanguageModule
foreign import elm :: LanguageModule
foreign import erb :: LanguageModule
foreign import erlang :: LanguageModule
foreign import erlangRepl :: LanguageModule
foreign import excel :: LanguageModule
foreign import fix :: LanguageModule
foreign import flix :: LanguageModule
foreign import fortran :: LanguageModule
foreign import fsharp :: LanguageModule
foreign import gams :: LanguageModule
foreign import gauss :: LanguageModule
foreign import gcode :: LanguageModule
foreign import gherkin :: LanguageModule
foreign import glsl :: LanguageModule
foreign import gml :: LanguageModule
foreign import go :: LanguageModule
foreign import golo :: LanguageModule
foreign import gradle :: LanguageModule
foreign import graphql :: LanguageModule
foreign import groovy :: LanguageModule
foreign import haml :: LanguageModule
foreign import handlebars :: LanguageModule
foreign import haskell :: LanguageModule
foreign import haxe :: LanguageModule
foreign import hsp :: LanguageModule
foreign import http :: LanguageModule
foreign import hy :: LanguageModule
foreign import inform7 :: LanguageModule
foreign import ini :: LanguageModule
foreign import irpf90 :: LanguageModule
foreign import isbl :: LanguageModule
foreign import java :: LanguageModule
foreign import javascript :: LanguageModule
foreign import jbossCli :: LanguageModule
foreign import json :: LanguageModule
foreign import julia :: LanguageModule
foreign import juliaRepl :: LanguageModule
foreign import kotlin :: LanguageModule
foreign import lasso :: LanguageModule
foreign import latex :: LanguageModule
foreign import ldif :: LanguageModule
foreign import leaf :: LanguageModule
foreign import less :: LanguageModule
foreign import lisp :: LanguageModule
foreign import livecodeserver :: LanguageModule
foreign import livescript :: LanguageModule
foreign import llvm :: LanguageModule
foreign import lsl :: LanguageModule
foreign import lua :: LanguageModule
foreign import makefile :: LanguageModule
foreign import markdown :: LanguageModule
foreign import mathematica :: LanguageModule
foreign import matlab :: LanguageModule
foreign import maxima :: LanguageModule
foreign import mel :: LanguageModule
foreign import mercury :: LanguageModule
foreign import mipsasm :: LanguageModule
foreign import mizar :: LanguageModule
foreign import mojolicious :: LanguageModule
foreign import monkey :: LanguageModule
foreign import moonscript :: LanguageModule
foreign import n1ql :: LanguageModule
foreign import nestedtext :: LanguageModule
foreign import nginx :: LanguageModule
foreign import nim :: LanguageModule
foreign import nix :: LanguageModule
foreign import nodeRepl :: LanguageModule
foreign import nsis :: LanguageModule
foreign import objectivec :: LanguageModule
foreign import ocaml :: LanguageModule
foreign import openscad :: LanguageModule
foreign import oxygene :: LanguageModule
foreign import parser3 :: LanguageModule
foreign import perl :: LanguageModule
foreign import pf :: LanguageModule
foreign import pgsql :: LanguageModule
foreign import php :: LanguageModule
foreign import phpTemplate :: LanguageModule
foreign import plaintext :: LanguageModule
foreign import pony :: LanguageModule
foreign import powershell :: LanguageModule
foreign import processing :: LanguageModule
foreign import profile :: LanguageModule
foreign import prolog :: LanguageModule
foreign import properties :: LanguageModule
foreign import protobuf :: LanguageModule
foreign import puppet :: LanguageModule
foreign import purebasic :: LanguageModule
foreign import python :: LanguageModule
foreign import pythonRepl :: LanguageModule
foreign import q :: LanguageModule
foreign import qml :: LanguageModule
foreign import reasonml :: LanguageModule
foreign import rib :: LanguageModule
foreign import r :: LanguageModule
foreign import roboconf :: LanguageModule
foreign import routeros :: LanguageModule
foreign import rsl :: LanguageModule
foreign import ruby :: LanguageModule
foreign import ruleslanguage :: LanguageModule
foreign import rust :: LanguageModule
foreign import sas :: LanguageModule
foreign import scala :: LanguageModule
foreign import scheme :: LanguageModule
foreign import scilab :: LanguageModule
foreign import scss :: LanguageModule
foreign import shell :: LanguageModule
foreign import smali :: LanguageModule
foreign import smalltalk :: LanguageModule
foreign import sml :: LanguageModule
foreign import sqf :: LanguageModule
foreign import sql :: LanguageModule
foreign import stan :: LanguageModule
foreign import stata :: LanguageModule
foreign import step21 :: LanguageModule
foreign import stylus :: LanguageModule
foreign import subunit :: LanguageModule
foreign import swift :: LanguageModule
foreign import taggerscript :: LanguageModule
foreign import tap :: LanguageModule
foreign import tcl :: LanguageModule
foreign import thrift :: LanguageModule
foreign import tp :: LanguageModule
foreign import twig :: LanguageModule
foreign import typescript :: LanguageModule
foreign import vala :: LanguageModule
foreign import vbnet :: LanguageModule
foreign import vbscriptHtml :: LanguageModule
foreign import vbscript :: LanguageModule
foreign import verilog :: LanguageModule
foreign import vhdl :: LanguageModule
foreign import vim :: LanguageModule
foreign import wasm :: LanguageModule
foreign import wren :: LanguageModule
foreign import x86asm :: LanguageModule
foreign import xl :: LanguageModule
foreign import xml :: LanguageModule
foreign import xquery :: LanguageModule
foreign import yaml :: LanguageModule
foreign import zephir :: LanguageModule

requireLanguage :: Language -> LanguageModule
requireLanguage = case _ of
    Lang1c -> lang1c
    Abnf -> abnf
    Accesslog -> accesslog
    Actionscript -> actionscript
    Ada -> ada
    Angelscript -> angelscript
    Apache -> apache
    Applescript -> applescript
    Arcade -> arcade
    Arduino -> arduino
    Armasm -> armasm
    Asciidoc -> asciidoc
    Aspectj -> aspectj
    Autohotkey -> autohotkey
    Autoit -> autoit
    Avrasm -> avrasm
    Awk -> awk
    Axapta -> axapta
    Bash -> bash
    Basic -> basic
    Bnf -> bnf
    Brainfuck -> brainfuck
    Cal -> cal
    Capnproto -> capnproto
    Ceylon -> ceylon
    C -> c
    Clean -> clean
    Clojure -> clojure
    ClojureRepl -> clojureRepl
    Cmake -> cmake
    Coffeescript -> coffeescript
    Coq -> coq
    Cos -> cos
    Cpp -> cpp
    Crmsh -> crmsh
    Crystal -> crystal
    Csharp -> csharp
    Csp -> csp
    Css -> css
    Dart -> dart
    Delphi -> delphi
    Diff -> diff
    Django -> django
    D -> d
    Dns -> dns
    Dockerfile -> dockerfile
    Dos -> dos
    Dsconfig -> dsconfig
    Dts -> dts
    Dust -> dust
    Ebnf -> ebnf
    Elixir -> elixir
    Elm -> elm
    Erb -> erb
    Erlang -> erlang
    ErlangRepl -> erlangRepl
    Excel -> excel
    Fix -> fix
    Flix -> flix
    Fortran -> fortran
    Fsharp -> fsharp
    Gams -> gams
    Gauss -> gauss
    Gcode -> gcode
    Gherkin -> gherkin
    Glsl -> glsl
    Gml -> gml
    Go -> go
    Golo -> golo
    Gradle -> gradle
    Graphql -> graphql
    Groovy -> groovy
    Haml -> haml
    Handlebars -> handlebars
    Haskell -> haskell
    Haxe -> haxe
    Hsp -> hsp
    Html -> xml
    Http -> http
    Hy -> hy
    Inform7 -> inform7
    Ini -> ini
    Irpf90 -> irpf90
    Isbl -> isbl
    Java -> java
    Javascript -> javascript
    JbossCli -> jbossCli
    Json -> json
    Julia -> julia
    JuliaRepl -> juliaRepl
    Kotlin -> kotlin
    Lasso -> lasso
    Latex -> latex
    Ldif -> ldif
    Leaf -> leaf
    Less -> less
    Lisp -> lisp
    Livecodeserver -> livecodeserver
    Livescript -> livescript
    Llvm -> llvm
    Lsl -> lsl
    Lua -> lua
    Makefile -> makefile
    Markdown -> markdown
    Mathematica -> mathematica
    Matlab -> matlab
    Maxima -> maxima
    Mel -> mel
    Mercury -> mercury
    Mipsasm -> mipsasm
    Mizar -> mizar
    Mojolicious -> mojolicious
    Monkey -> monkey
    Moonscript -> moonscript
    N1ql -> n1ql
    Nestedtext -> nestedtext
    Nginx -> nginx
    Nim -> nim
    Nix -> nix
    NodeRepl -> nodeRepl
    Nsis -> nsis
    Objectivec -> objectivec
    Ocaml -> ocaml
    Openscad -> openscad
    Oxygene -> oxygene
    Parser3 -> parser3
    Perl -> perl
    Pf -> pf
    Pgsql -> pgsql
    Php -> php
    PhpTemplate -> phpTemplate
    Plaintext -> plaintext
    Pony -> pony
    Powershell -> powershell
    Processing -> processing
    Profile -> profile
    Prolog -> prolog
    Properties -> properties
    Protobuf -> protobuf
    Puppet -> puppet
    Purebasic -> purebasic
    Python -> python
    PythonRepl -> pythonRepl
    Q -> q
    Qml -> qml
    Reasonml -> reasonml
    Rib -> rib
    R -> r
    Roboconf -> roboconf
    Routeros -> routeros
    Rsl -> rsl
    Ruby -> ruby
    Ruleslanguage -> ruleslanguage
    Rust -> rust
    Sas -> sas
    Scala -> scala
    Scheme -> scheme
    Scilab -> scilab
    Scss -> scss
    Shell -> shell
    Smali -> smali
    Smalltalk -> smalltalk
    Sml -> sml
    Sqf -> sqf
    Sql -> sql
    Stan -> stan
    Stata -> stata
    Step21 -> step21
    Stylus -> stylus
    Subunit -> subunit
    Swift -> swift
    Taggerscript -> taggerscript
    Tap -> tap
    Tcl -> tcl
    Thrift -> thrift
    Tp -> tp
    Twig -> twig
    Typescript -> typescript
    Vala -> vala
    Vbnet -> vbnet
    VbscriptHtml -> vbscriptHtml
    Vbscript -> vbscript
    Verilog -> verilog
    Vhdl -> vhdl
    Vim -> vim
    Wasm -> wasm
    Wren -> wren
    X86asm -> x86asm
    Xl -> xl
    Xml -> xml
    Xquery -> xquery
    Yaml -> yaml
    Zephir -> zephir

data Language =
    Lang1c
  | Abnf
  | Accesslog
  | Actionscript
  | Ada
  | Angelscript
  | Apache
  | Applescript
  | Arcade
  | Arduino
  | Armasm
  | Asciidoc
  | Aspectj
  | Autohotkey
  | Autoit
  | Avrasm
  | Awk
  | Axapta
  | Bash
  | Basic
  | Bnf
  | Brainfuck
  | Cal
  | Capnproto
  | Ceylon
  | C
  | Clean
  | Clojure
  | ClojureRepl
  | Cmake
  | Coffeescript
  | Coq
  | Cos
  | Cpp
  | Crmsh
  | Crystal
  | Csharp
  | Csp
  | Css
  | Dart
  | Delphi
  | Diff
  | Django
  | D
  | Dns
  | Dockerfile
  | Dos
  | Dsconfig
  | Dts
  | Dust
  | Ebnf
  | Elixir
  | Elm
  | Erb
  | Erlang
  | ErlangRepl
  | Excel
  | Fix
  | Flix
  | Fortran
  | Fsharp
  | Gams
  | Gauss
  | Gcode
  | Gherkin
  | Glsl
  | Gml
  | Go
  | Golo
  | Gradle
  | Graphql
  | Groovy
  | Haml
  | Handlebars
  | Haskell
  | Haxe
  | Hsp
  | Html
  | Http
  | Hy
  | Inform7
  | Ini
  | Irpf90
  | Isbl
  | Java
  | Javascript
  | JbossCli
  | Json
  | Julia
  | JuliaRepl
  | Kotlin
  | Lasso
  | Latex
  | Ldif
  | Leaf
  | Less
  | Lisp
  | Livecodeserver
  | Livescript
  | Llvm
  | Lsl
  | Lua
  | Makefile
  | Markdown
  | Mathematica
  | Matlab
  | Maxima
  | Mel
  | Mercury
  | Mipsasm
  | Mizar
  | Mojolicious
  | Monkey
  | Moonscript
  | N1ql
  | Nestedtext
  | Nginx
  | Nim
  | Nix
  | NodeRepl
  | Nsis
  | Objectivec
  | Ocaml
  | Openscad
  | Oxygene
  | Parser3
  | Perl
  | Pf
  | Pgsql
  | Php
  | PhpTemplate
  | Plaintext
  | Pony
  | Powershell
  | Processing
  | Profile
  | Prolog
  | Properties
  | Protobuf
  | Puppet
  | Purebasic
  | Python
  | PythonRepl
  | Q
  | Qml
  | Reasonml
  | Rib
  | R
  | Roboconf
  | Routeros
  | Rsl
  | Ruby
  | Ruleslanguage
  | Rust
  | Sas
  | Scala
  | Scheme
  | Scilab
  | Scss
  | Shell
  | Smali
  | Smalltalk
  | Sml
  | Sqf
  | Sql
  | Stan
  | Stata
  | Step21
  | Stylus
  | Subunit
  | Swift
  | Taggerscript
  | Tap
  | Tcl
  | Thrift
  | Tp
  | Twig
  | Typescript
  | Vala
  | Vbnet
  | VbscriptHtml
  | Vbscript
  | Verilog
  | Vhdl
  | Vim
  | Wasm
  | Wren
  | X86asm
  | Xl
  | Xml
  | Xquery
  | Yaml
  | Zephir
