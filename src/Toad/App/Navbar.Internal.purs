module Toad.App.Navbar.Internal where

import Toad.Prelude

data Active = Active | Inactive

derive instance eqActive :: Eq Active

data Expanded = Expanded | Collapsed

derive instance eqExpanded :: Eq Expanded

data Visible = Visible | Hidden

data ChildIs a = ChildIs a

derive instance eqChildIs :: Eq a => Eq (ChildIs a)

data SiblingIs a = SiblingIs a

derive instance eqSiblingIs :: Eq a => Eq (SiblingIs a)
