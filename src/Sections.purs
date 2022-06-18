module Sections where

import Prelude
import Color (Accent(..))
import Card (Card, education, work, qualities, contact, projects)
import Halogen.HTML.Core (ClassName(..))

data Section = Past
             | Present
             | Future

asInt :: Section -> Int
asInt Past    = 0
asInt Present = 1
asInt Future  = 2

instance eqSection :: Eq Section where
  eq a b = asInt a == asInt b

instance ordSection :: Ord Section where
  compare a b = asInt a `compare` asInt b

instance showSection :: Show Section where
  show Past = "Where I've Been"
  show Present = "Who I Am"
  show Future = "Where I'm Going"

allSections :: Array Section
allSections = [Past, Present]--, Future]

getColorClass :: Section -> ClassName
getColorClass Past    = ClassName "bg-blue"
getColorClass Present = ClassName "bg-yellow"
getColorClass Future  = ClassName "bg-green"

getColor :: Section -> Accent
getColor Past    = Blue
getColor Present = Green
getColor Future  = Yellow

getCards :: forall w i. Section -> Array (Card w i)
getCards Present = [qualities, contact, projects]
getCards Past    = [work, education, projects]
getCards Future  = []
