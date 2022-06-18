module Card where

import Prelude
import Utils (classes)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML (HTML)

data CardSize = Large
              | Small

type Card w i =
  { size :: CardSize
  , title :: String
  , items :: Array (CardItem w i)
  }

type CardItem w i =
  { title :: String
  , contents :: HTML w i
  }

qualities :: forall w i. Card w i
qualities =
  { size: Large
  , title: "Qualities"
  , items: [ { title: "Beginner's Mind"
             , contents: HH.div [ classes [ "flex", "vert", "mt-0" ] ]
                                $ [ p "I approach problems with an open mind and make sure I have a full understanding before moving forward with a solution."
                                  , p "I believe there's no such thing as fully knowing anything, especially in software, and try to refine my understanding at every opportunity."
                                  ]
             }
           , { title: "Swiss Army Knife"
             , contents: HH.div [ classes [ "flex", "vert", "mt-0" ] ]
                                $ [ p "My thirst for knowledge drives me to learn as much as I can, about as much as I can."
                                  , p "This includes technical skills, tooling, and areas in my career that I have yet to explore."
                                  , p "I'm passionate about functional programming, but those values can be found & explored within idiomatic Java, C#, or Javascript."
                                  , HH.h3_ [ HH.text "I care much less about How, than What and Why." ]
                                  ]
             }
           , { title: "Ready to teach & be taught"
             , contents: HH.div [ classes [ "flex", "vert", "mt-0" ] ]
                                $ [ "I love to mentor, and be mentored."
                                  , "In the course of helping someone work through an issue I know the solution to, I ask if they want the answer, or want me to lead them to the answer."
                                  , "I do my best to ask the questions that I ask myself in pursuit of a solution. This way, the other person leaves not only having solved the problem, but with the skill to solve similar problems in the future."
                                  ] <#> HH.text >>> pure >>> HH.p_
             }
           , { title: "Open-Source Lover"
             , contents: HH.div [ classes [ "flex", "vert", "mt-0" ] ]
                                $ [ "I am very passionate writing code for others. By this I mean that the process of writing code that is meant to be read often by eyes other than yours is paramount to a well-structured, discoverable solution."
                                  , "The saying \"You are what you measure\" extends beyond metrics; The values you embody when creating or maintaining a product will drive the quality & kind of product you make."
                                  ] <#> HH.text >>> pure >>> HH.p_
             }
           ]
  }
  where
    p :: String -> HTML w i
    p = HH.p_ <<< pure <<< HH.text

contact :: forall w i. Card w i
contact =
  { size: Small
  , title: "Contact"
  , items: [ { title: "Phone", contents: HH.a  [ HP.href "tel:1-989-443-0197" ]            [ HH.text "989 443 0197" ] }
           , { title: "Email", contents: HH.a  [ HP.href "mailto:orionkindel@gmail.com" ]  [ HH.text "orionkindel@gmail.com" ] }
           , { title: "Github", contents: HH.a [ HP.href "https://github.com/cakekindel" ] [ HH.text "cakekindel" ] }
           ]
  }

projects :: forall w i. Card w i
projects =
  { size: Small
  , title: "Projects"
  , items: [ { title: "GitHub"
             , contents: HH.div [ classes [ "flex", "vert" ] ]
                                [ codelink "cakekindel/slack-blocks-rs"
                                , codelink "cakekindel/matchbook-ts"
                                , codelink "denoland/deno"
                                , codelink "rust/rust-lang"
                                , codelink "awslabs/aws-lambda-rust-runtime"
                                ]
             }
           ]
  }
  where
    codelink text =
      HH.a [ HP.href $ "https://www.github.com/" <> text ]
           [ HH.code_ [ HH.text text ] ]

work :: forall w i. Card w i
work = { size: Large
       , title: "Work"
       , items: [ { title: "StrongMind"
                  , contents: HH.div_ [ HH.div_ [ HH.h3_ [ HH.text "Software Engineer I" ]
                                                , HH.p_  [ HH.i_ [ HH.text "May 2017 - Jan 2019 (1yr 9mos)" ] ]
                                                , HH.p_  [ HH.text "As a Junior Engineer at StrongMind, I was empowered by the company and my team to grow in every way. Part of my responsibilities were bringing patterns and trends back to the team for discussion & adoption, I encouraged us to switch from the legacy TFS build system to Git, which allowed us to deliver value more quickly. I also was able to step into a more UI-focused role, embracing the strengths I brought to the table despite my technical inexpertise." ]
                                                ]
                                      , HH.div_ [ HH.h3_ [ HH.text "Software Engineer II" ]
                                                , HH.p_  [ HH.i_ [ HH.text "Jan 2019 - Present (2yr)" ] ]
                                                , HH.p_  [ HH.text "As a more seasoned Product Engineer at Strongmind, I have continued to grow beyond the responsibilities within my role and successfully unified the previously fragmented UI/UX team and spearheaded the creation of a Strongmind design system. I also found my voice within the org as an advocate for the more recent developments in the industry, like functional programming, automated testing, and thorough CI/CD processes. On the hit-squad team for the mobile app, I pioneered with the first use of the Rust programming language in production, establishing a new standard for reliability, performance, and code approachability within the company. " ]
                                                ]
                                      ]
                  }
                , { title: "High-School Math Tutor"
                  , contents: HH.div_ [ HH.p_  [ HH.i_ [ HH.text "Freelance, May 2016 - April 2017" ] ]
                                      , HH.p_  [ HH.text "As a Math Tutor, I was able to start learning how to manage my relationship with clients, and figure out mentoring styles that worked for me and each student." ]
                                      , HH.p_  [ HH.text "This experience helped my eventual transition to a mentor role at StrongMind immensely." ]
                                      ]
                  }
                ]
       }

education :: forall w i. Card w i
education = { size: Small
       , title: "Education"
       , items: [ { title: "Associate's Of Science"
                  , contents: HH.div_ [ HH.h3_ [ HH.text "Scottsdale Community College" ]
                                      , HH.p_  [ HH.i_ [ HH.text "awarded May 2016" ] ]
                                      ]
                  }
                , { title: "Web Development"
                  , contents: HH.div_ [ HH.h3_ [ HH.text "Self-Taught" ]
                                      , HH.p_  [ HH.i_ [ HH.text "2015 - 2017" ] ]
                                      ]
                  }
                , { title: "High School"
                  , contents: HH.div_ [ HH.h3_ [ HH.text "Sequoia Choice" ]
                                      , HH.p_  [ HH.i_ [ HH.text "grad. May 2016" ] ]
                                      ]
                  }
                ]
       }
