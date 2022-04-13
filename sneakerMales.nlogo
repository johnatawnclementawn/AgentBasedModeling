;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Johnathan Clementi ;;;;

globals [
  adult-sex-ratio-list     ;; tracks adult-sex-ratio at each tick to compute running average and standard deviation
  age-list                 ;; list of ages of dead individuals
  adult-age                ;; tracks the age of adulthood
]

breed [females female]
breed [parentalMales parentalMale]
breed [sneakerMales sneakerMale]

patches-own [
 is-nest?              ;; is this patch part of a nest?
 parental           ;; capture the parental male that built the nest
]

females-own [
  age                         ;; to keep track of age
  longevity                   ;; gets assigned at birth - age up to which an individual lives
  sexual-maturity             ;; universal sexual maturity for females
                              ;; the actual age-at-sexual maturity for each group is an order of magnitude smaller
  nest                        ;; during mating, fish is assigned to a nest
  partner                     ;; a variable to temporarily store a mating-partner
  sneaker-child-chance        ;; this is a trait variable that influences the probability of giving birth to a sneaker child
  num-of-exes                 ;; tracks mating partners an individual had in its life
  num-of-children             ;; tracks how many children an individual had
  adult?                      ;; boolean to flag if this agent is an adult
]

parentalMales-own [
  age                         ;; tracks of age
  longevity                   ;; gets assigned at birth - age up to which an individual lives
  sexual-maturity             ;; universal sexual maturity for parental males
                              ;; the actual age-at-sexual maturity for each group is an order of magnitude smaller
  nest                        ;; during mating, fish is assigned to a nest
  nestAge                     ;; time incrementor for how long a parental has been nesting, used for destroying nests after 'breeding season'
  partner                     ;; a variable to temporary store a mating-partner
  sneaker-child-chance        ;; this is a trait variable that influences the probability of giving birth to a sneaker child
  temp-sneaker-child-chance   ;; stores sneaker-child-chance for a particular father-mother pair
  numOffspring                ;; number of offspring in a nesting - for our purposes, this refers to fish reaching adulthood, rather than eggs
  nestWait                    ;; time incrementor for how long a parental male needs to wait until nesting again - simulates temporal nature of breeding
  num-of-exes                 ;; tracks mating partners an individual had in its life
  num-of-children             ;; track how many children it has
  adult?                      ;; boolean to flag if this agent is an adult
]

sneakerMales-own [
  age                   ;; tracks of age
  longevity             ;; gets assigned at birth - age up to which an individual lives
  sexual-maturity       ;; universal sexual maturity for sneaker males
                        ;; the actual age-at-sexual maturity for each group is an order of magnitude smaller
  nest                  ;; during mating, fish is assigned to a nest
  partner               ;; a variable to temporary store a mating-partner
  sneaker-child-chance  ;; this is a trait variable that influences the probability of giving birth to a sneaker child
  num-of-exes           ;; tracks mating partners an individual had in its life
  num-of-children       ;; track how many children it has
  adult?                ;; boolean to flag if this agent is an adult
]


to setup
  __clear-all-and-reset-ticks
  set-default-shape females "fish"            ;; make agent symbols fish
  set-default-shape parentalMales "fish"      ;; make agent symbols fish
  set-default-shape sneakerMales "fish"       ;; make agent symbols fish

  ;; Initialize simulation with these features:
  ask patches [ set pcolor blue ]

  ;; Calculate female, parentalMale, and sneakerMale population sizes based on user input
  let femaleInitPop round((initial-adult-sex-ratio / 100) * initial-population-size)                     ;; female initial pop is (females : (parentalMales + sneakerMales))
  let sneakerInitPop round((initial-population-size - femaleInitPop) * (initial-sneaker-ratio / 100))    ;; sneaker init pop is (total pop - femaleInitPop) * percent of sneakers
  let parentalMaleInitPop (initial-population-size - femaleInitPop - sneakerInitPop)                     ;; parentalMale init pop is (total pop - (femaleInitPop + sneakerInitPop))

  set adult-sex-ratio-list []
  set age-list []
  set adult-age int (0.15 * mean-longevity)          ;; Agents with age more than 25% of mean-longevity are considered

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Create sneaker male agents and initialize them
  create-sneakerMales ( sneakerInitPop ) [
    setxy random-xcor random-ycor
    set size 1
    set color yellow

    ;; males are assigned initial sneaker-child-chance from a random-normal distribution
    ;; bull males are more likely to pass on genes that code for bull males, rather than sneaker males
    set sneaker-child-chance random-normal initial-average-sneaker-child-chance 0.05

    ;; curtail negative or greater-than-1 probabilities
    if sneaker-child-chance < 0 [ set sneaker-child-chance 0 ]
    if sneaker-child-chance > 1 [ set sneaker-child-chance 1 ]

    ;; initialize turtle variables
    set age int (random-normal (mean-longevity / 2) (mean-longevity / 10))
    set longevity int (random-normal mean-longevity (mean-longevity / 10))
    set sexual-maturity 10                                                    ;; sneakers become sexually mature at roughly 2 years old
    ifelse (age >= sexual-maturity) [set adult? True] [set adult? False]      ;; fish are adults if their age is greater than or equal to their sexual maturity age
    set partner nobody
    set num-of-exes 0
    set num-of-children 0
  ]
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Create bull male agents and initialize them
  create-parentalMales ( parentalMaleInitPop ) [
    setxy random-xcor random-ycor
    set size 2
    set color green

    ;; males are assigned initial sneaker-child-chance from a random-normal distribution
    ;; bull males are more likely to pass on genes that code for bull males, rather than sneaker males
    set sneaker-child-chance random-normal initial-average-sneaker-child-chance 0.05

    ;; curtail negative or greater-than-1 probabilities
    if sneaker-child-chance < 0 [ set sneaker-child-chance 0 ]
    if sneaker-child-chance > 1 [ set sneaker-child-chance 1 ]

    ;; initialize turtle variables
    set age int (random-normal (mean-longevity / 2) (mean-longevity / 10))
    set longevity int (random-normal mean-longevity (mean-longevity / 10))
    set sexual-maturity 40                                                    ;; parental males become sexually mature at roughly 7 years old
    ifelse (age >= sexual-maturity) [set adult? True] [set adult? False]      ;; fish are adults if their age is greater than or equal to their sexual maturity age
    set partner nobody
    set num-of-exes 0
    set num-of-children 0
  ]
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Create female agents and initialize them
  create-females ( femaleInitPop ) [
    setxy random-xcor random-ycor
    set size 1.5
    set color pink
    set partner nobody

    ;; females are assigned initial sneaker-child-chance from a random-normal distribution
    set sneaker-child-chance random-normal initial-average-sneaker-child-chance 0.05

    ;; curtail negative or greater-than-1 probabilities
    if sneaker-child-chance < 0 [ set sneaker-child-chance 0 ]
    if sneaker-child-chance > 1 [ set sneaker-child-chance 1 ]

    ;; initialize rest of turtle variables
    set age int (random-normal (mean-longevity / 2) (mean-longevity / 10))
    set longevity int (random-normal mean-longevity (mean-longevity / 10))
    set sexual-maturity 30                                                   ;; females become sexually mature at roughly 4 years old
    ifelse (age >= sexual-maturity) [set adult? True] [set adult? False]     ;; fish are adults if their age is greater than or equal to their sexual maturity age
    set num-of-exes 0
    set num-of-children 0

  ]
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

end


to go
  if not any? turtles [stop]              ;; if there are no more turtles, end the simulation

  ask turtles [ check-if-dead ]           ;; remove turtles that have become too old
  ask turtles [ check-if-adult ]          ;; check if turtles are sexually mature

  ;; set turtle shape - fish shapes can cover up interactions
  ifelse fish-shape [
    ask turtles [ set shape "fish" ]
  ][
    ask turtles [ set shape "dot" ]
  ]

  ask parentalMales [
    ifelse adult? = True [                ;; if parental male has reached sexual maturity, find space to make a nest
      makeNest
    ][
      swim                                ;; if the parental male isn't sexually mature yet, just swim around
    ]
  ]

  ask females [
    ifelse adult? = True [                ;; if female is sexually mature, it can breed
      findNest
    ][
      swim                                ;; if the female isn't sexually mature yet, just swim around
    ]
  ]

  ask sneakerMales [
   ifelse adult? = True [
      sneak
    ][
      swim
    ]
  ]

  tick
end

;; procedure to move randomly
to swim                                           ;; all fish procedure
  rt random 60
  lt random 60
  fd 0.3
end

;; if an individual is older than the assigned longevity it dies
to check-if-dead                                  ;; all fish procedure
  ifelse age > longevity [
    set age-list lput [age] of self age-list
    if nest != 0 [
      ask nest [set pcolor blue
                set is-nest? False
                set parental 0
      ]
    ]
    ifelse partner = nobody [
      die
    ][
      ifelse partner = 0 [
        die
      ][
        ask partner [set partner 0                 ;; reset partner's link to this fish
                     set nest 0                    ;; reset partner's nest
        die
        ]
      ]
    ]
    die
  ][
    set age (age + 1)
  ]
end

;; if an individual is older than the assigned sexual maturity it is an adult
to check-if-adult ; turtle procedure
  ifelse (age >= sexual-maturity) [
    set adult? True
  ][
    set adult? False
  ]
end

;; check nest age, remove nest if it has reached a pre-determined nesting time
to check-nest-age                           ;; parental male procedure
  if (nest != 0 and nestAge > 15) [
    ask nest [set pcolor blue               ;; return the nest to the ocean
              set is-nest? False            ;; patches are no longer a nest
              set parental 0                ;; there is no longer a parental male nesting on these patches
    ]
    set nest 0                              ;; remove patch set from parental male's attributes
    set nestWait 0                          ;; reset the parental male's timer for waiting to nes
  ]
end

;; parentalMales make nests if they are sexually mature
to makeNest
  check-nest-age                                                    ;; check if I have been nesting for a while, if I have remove the old nest

  let nestNearby patches in-radius 3 with [pcolor = brown]          ;; find any nearby nests - nests are spatially dispersed

  ifelse nest != 0 [                                                ;; if I have a nest already
    set nestAge nestAge + 1                                         ;; increment the age of the nest
    move-to one-of nest                                             ;; move around in my nest.
  ][                                                                ;; If I don't have a nest
    ifelse nestWait >= 5 [                                          ;; and if I've waited enough time after nesting, I can build a new nest
      set nestAge  0                                                ;; first reset the new nest's age to 0.
      ifelse nestNearby != nobody [                                 ;; Check if there is a nest nearby - if there is
        move-to one-of patches in-radius 5 with [pcolor = blue]     ;; move to an open area with no nests nearby and make a nest
        set nest patches in-radius 2                                ;; create patch-set for nest
        ask nest [set pcolor brown                                  ;; delineate nests from ocean
                  set is-nest? True                                 ;; patch will report that it is part of a nest
                  set parental myself                               ;; patch will report the male that created it
        ]
      ][                                                            ;; If there isn't a nest nearby, finally, I can make my own nest!
        set nest patches in-radius 2                                ;; create patch-set for nest
        ask nest [set pcolor brown                                  ;; delineate nests from ocean
                  set is-nest? True                                 ;; patch will report that it is part of a nest
                  set parental myself                               ;; patch will report the male that created it
        ]
      ]
    ][
      set nestWait nestWait + 1                                     ;; if I haven't waited long enough after nesting, keep track of how long I've waited
      swim                                                          ;; move around until its time to nest again
    ]
  ]
end

;; adult females will search for a nest to lay their eggs
to findNest
  let nearby-nest one-of neighbors with [pcolor = brown]      ;; capture a nearby-nest
  ifelse nearby-nest != nobody [                              ;; if there is a nest nearby
    move-to nearby-nest                                       ;; move to it
    findPartner
  ][
    swim                                                      ;; if there are no nests nearby, swim around
  ]
end

;; once an adult female has found a nest, identify the parental male that built the nest
to findPartner
  set partner ([parental] of patch-here)                     ;; set female's partner to the parental male
  if partner = 0 or partner = nobody [ stop ]                ;; if there is no longer a parental male (died), don't mate
  set nest ([nest] of partner)                               ;; assign female to a nest
  set num-of-exes num-of-exes + 1                            ;; increment number of mating partners
  ask partner [
    set partner myself                                       ;; set parental male's partner to the nesting female
    set num-of-exes num-of-exes + 1                          ;; increment number of mating partners
      ;; sex of a male child is determined by sneaker-child-chance (determined by father and mother)
    set temp-sneaker-child-chance ( sneaker-child-chance + [sneaker-child-chance] of myself ) / 2
    reproduce                                                ;; once the fish have found a partner, lay and fertilize eggs (abstracted)
  ]

end

;; once the adult female and male have linked up, lay and fertilize the eggs
to reproduce
  set numOffspring round(random-normal 5 2)

  let numFemales round(numOffspring / 2)                     ;; for our purposes, assume 50/50 chance between male:female offspring
                                                             ;; in reality, this has been shown to be temperature dependent and fluctuates based on environmental factors
  repeat numFemales [                                        ;; hatch females first
    hatch-females 1 [
      set size 1.5
      set color pink
      ifelse fish-shape[ set shape "fish" ][ set shape "dot" ]
      set heading random 360
      set age 0
      set longevity int (random-normal mean-longevity (mean-longevity / 10))
      set sexual-maturity 40
      set adult? False
      set partner nobody
      set sneaker-child-chance random-normal [sneaker-child-chance] of myself 0.05
      ;; curtail negative or greater-than-1 probabilities
      if sneaker-child-chance < 0 [ set sneaker-child-chance 0 ]
      if sneaker-child-chance > 1 [ set sneaker-child-chance 1 ]
      set num-of-exes 0
      set num-of-children 0
      set nest 0
    ]
  ]

  let numMales numOffspring - numFemales                                       ;; calculate the number of remaining offspring
  let chanceSneakers (numMales) * temp-sneaker-child-chance                    ;; calculate the chance that male is a sneaker
  repeat numMales [                                                            ;; hatch the males
    ifelse random-float 1 < chanceSneakers [                                   ;; chance of having a sneaker over a parental
      hatch-sneakerMales 1 [
        set size 1
        set color yellow
        ifelse fish-shape[ set shape "fish" ][ set shape "dot" ]
        set heading random 360
        set age 0
        set longevity int (random-normal mean-longevity (mean-longevity / 10))
        set sexual-maturity 10
        set adult? False
        set partner nobody
        set sneaker-child-chance random-normal [sneaker-child-chance] of myself 0.05
        ;; curtail negative or greater-than-1 probabilities
        if sneaker-child-chance < 0 [ set sneaker-child-chance 0 ]
        if sneaker-child-chance > 1 [ set sneaker-child-chance 1 ]
        set num-of-exes 0
        set num-of-children 0
        set nest 0
      ]
    ][
      hatch-parentalMales 1 [
        set size 2
        set color green
        ifelse fish-shape[ set shape "fish" ][ set shape "dot" ]
        set heading random 360
        set age 0
        set longevity int (random-normal mean-longevity (mean-longevity / 10))
        set sexual-maturity 40
        set adult? False
        set partner nobody
        set sneaker-child-chance random-normal [sneaker-child-chance] of myself 0.05
        ;; curtail negative or greater-than-1 probabilities
        if sneaker-child-chance < 0 [ set sneaker-child-chance 0 ]
        if sneaker-child-chance > 1 [ set sneaker-child-chance 1 ]
        set num-of-exes 0
        set num-of-children 0
        set nest 0
      ]
    ]
  ]
end

to sneak

end
@#$#@#$#@
GRAPHICS-WINDOW
11
10
525
525
-1
-1
12.3415
1
10
1
1
1
0
1
1
1
-20
20
-20
20
0
0
1
Ticks
30.0

BUTTON
542
15
622
63
Setup
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
543
66
622
114
Go!
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

SLIDER
631
19
882
52
initial-population-size
initial-population-size
50
300
80.0
10
1
NIL
HORIZONTAL

SLIDER
630
102
882
135
initial-sneaker-ratio
initial-sneaker-ratio
5
95
30.0
5
1
%
HORIZONTAL

SLIDER
630
142
881
175
initial-average-sneaker-child-chance
initial-average-sneaker-child-chance
0
0.5
0.11
0.01
1
NIL
HORIZONTAL

SLIDER
905
20
1155
53
mean-longevity
mean-longevity
0
120
110.0
5
1
NIL
HORIZONTAL

SLIDER
631
60
883
93
initial-adult-sex-ratio
initial-adult-sex-ratio
5
95
25.0
5
1
%
HORIZONTAL

SWITCH
537
191
635
224
fish-shape
fish-shape
1
1
-1000

PLOT
536
350
918
524
Distribution of sexes
Time
Count of individuals
0.0
1000.0
0.0
1000.0
true
true
"" ""
PENS
"Parental-males" 1.0 0 -10899396 true "" "plot count parentalMales"
"Sneaker-males" 1.0 0 -1184463 true "" "plot count sneakerMales"
"Females" 1.0 0 -2064490 true "" "plot count females"

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.2.2
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
