;extensions [array table]
;;;;; creating the users breed ;;;;;
breed[users user]
users-own[
  ;;;;;;attributes values -----------------------------------------------------
  age
  gender
  university
  country 
  current-city 
  my-interest 
  my-language 
  my-relationship-status 
  privacy-concern
  my-friendship-threshold 
  my-maximum-degree
  settings-configured?
  ;;;;;;privacy settings -----------------------------------------------------
  age-visibility 
  gender-visibility 
  university-visibility 
  country-visibility 
  current-city-visibility 
  interests-visibility 
  language-visibility 
  relationship-status-visibility 
  ]

;;;;; creating globals ;;;;;
globals [genders universities countries cities interests languages relationship-status privacy-settings attributes-weights is-random-data ]


;;;;; initializing globals ;;;;;
to intitialize-globals
  set genders ["male" "female"] ;; 2
  set universities ["UCT" "UP" "UWC" "Wits" "Rhodes"] ;; 5
  set countries ["south-africa" "sudan" "nigeria" "france" "canada" "Swaziland" "kenya" "cuba" "ghana"] ;; 9
  set cities ["khartoum" "cape-town" "Johannesburg" "Durban" "Pretoria" "Mhluzi" "Port-Elizabeth"] ;; 7
  set interests ["music" "sports" "reading" "dance" "programming" "traveling" "theater" "cinema"] ;; 8
  set languages ["arabic" "english" "french" "xhosa" "zulu" "afrikaans"] ;; 6
  set relationship-status ["single" "in-a-relationship" "married" "engaged"] ;; 4
  set privacy-settings ["public" "friends-of-friends" "friends" "only-me"]
end

;;;;; setting up the simulation ;;;;;
to setup
  ca ;; clearing all variables
  reset-ticks
  intitialize-globals ;; initializing globals  
  set is-random-data false
  ;; now creating the agents (i.e. users)
  create-users num-users [
    setxy random-xcor random-ycor
    set shape "circle"
    set color red
    set age ceiling abs random-normal 30 5 ;;the age is given by a gaussian with mean 30 and variance 5
    set gender item (random 2) genders ;; random gender
    set university item (random 5) universities ;; random university
    set country item (random 9) countries ;; random country
    set current-city item (random 7) cities ;; random city
    set my-interest item (random 8) interests ;; random interest
    set my-language item (random 6) languages ;; random language
    set my-relationship-status item (random 4) relationship-status ;; random relationship status
    set privacy-concern abs random-normal .5 .15 ;; setting the privacy concern to a gaussian with mean .5 and variance ,15
    set my-friendship-threshold abs random-normal .5 .15 ;; setting the friendship threshold to a gaussian with mean .5 and variance ,15
    set my-maximum-degree ceiling abs random-normal 0 10 ;; setting the maximum degree to a gaussian with mean 0 and variance 15
    set settings-configured? false
    
    ;;;; default privacy settings ----------------------------------------
    set age-visibility "public"
    set gender-visibility "public"
    set university-visibility "public"
    set country-visibility "public"
    set current-city-visibility "public"
    set interests-visibility "public"
    set language-visibility "public"
    set relationship-status-visibility "public"
    set is-random-data false
    ]
end


;;;;; counting friend-in-common ;;;;;
to-report friend-in-common [source dest]
  let result 0 ;; initially no friends in common
  let flag false
  ask [link-neighbors] of source
  [
    set flag true
    if (member? dest link-neighbors) [set result result + 1] ;; counting the number of friend-in-common     
  ]
  
  ifelse flag [report result / count [link-neighbors] of source][report 0]
end

;;;;; forming the graph ;;;;;
to form-graph
 tick
 set-current-plot "No. Users who Configured Their Settings"
 clear-plot
 
 layout-spring users links .5 10 5 ;; making the layout of the graph
    ask users [
     ;;------------------------------------------------
     if( (count link-neighbors) <= my-maximum-degree)[
       let me self
       let possible-friends other turtles with [not member? self [link-neighbors] of me]
       
       let new-friend one-of possible-friends
       let mutual-friends-percentage friend-in-common me new-friend
       let attribute-similarity similarity me new-friend
       
       let frienship-score ( (.7 * attribute-similarity) + (.3 * mutual-friends-percentage) )
       
       
         if ( frienship-score >= [my-friendship-threshold] of me) [
           create-link-with new-friend
         ]
      
     ]
        
  ]
    
    
    
    if(not any? users with [ (count link-neighbors) <= my-maximum-degree]) [stop]
end

;;;;; similarity function ;;;;;
to-report similarity [me otheruser]
  let sim-result 0 ;; initializing the similarity result
  
  if ([age] of me = [age] of otheruser ) [set sim-result  sim-result + 1]
  if ([gender] of me = [gender] of otheruser ) [set sim-result  sim-result + 1]
  if ([university] of me = [university] of otheruser ) [set sim-result  sim-result + 1]
  if ([country] of me = [country] of otheruser ) [set sim-result  sim-result + 1]
  if ([current-city] of me = [current-city] of otheruser ) [set sim-result  sim-result + 1]
  if ([my-interest] of me = [my-interest] of otheruser ) [set sim-result  sim-result + 1]
  if ([my-language] of me = [my-language] of otheruser ) [set sim-result  sim-result + 1]
  if ([my-relationship-status] of me = [my-relationship-status] of otheruser ) [set sim-result  sim-result + 1]
  if ( ([privacy-concern] of me - [privacy-concern] of otheruser) < .05 ) [set sim-result  sim-result + 1]
  
  set sim-result sim-result / 9 ;; similarity percentage
  
  report sim-result
end


;;;;; simulating privacy settings configration ;;;;;
to simulate-privacy-settings
  ifelse (count users with [ not settings-configured? ] >= 10) [
    tick
    
    ask n-of (random 10) users with [ not settings-configured? ]  [
    
    set color yellow
    
    ;; calculating user's motivation
    let friends-who-configuered-settings 0
    if any? link-neighbors [ set friends-who-configuered-settings (count link-neighbors with [settings-configured?] / count link-neighbors ) ] 
    let motivation ( alpha * privacy-concern +  beta * friends-who-configuered-settings ) ;------------------------------------------------------------------------some upgrades needed---------------------------------------------------------------
    
    ifelse motivation >= 0.30 [ configure-privacy-settings self][ set color red ]
        
    ]
    
    
    ][stop]
end


;;;;; configure privacy settings ;;;;;
to configure-privacy-settings [ selected-users ]
  ask selected-users [
    
    set age-visibility get-settings-value ( (alpha * privacy-concern) + (beta * age-weight) )
    set gender-visibility get-settings-value ( (alpha * privacy-concern) + (beta * gender-weight) )
    set university-visibility get-settings-value ( (alpha * privacy-concern) + (beta * university-weight) )
    set country-visibility get-settings-value ( (alpha * privacy-concern) + (beta * country-weight) )
    set current-city-visibility get-settings-value ( (alpha * privacy-concern) + (beta * current-city-weight) )
    set interests-visibility get-settings-value ( (alpha * privacy-concern) + (beta * interest-weight) )
    set language-visibility get-settings-value ( (alpha * privacy-concern) + (beta * language-weight) )
    set relationship-status-visibility get-settings-value ( (alpha * privacy-concern) + (beta * relationship-weight) )
    
    set color white
    set settings-configured? true   
    ]
end


;;;;; get settings value ;;;;;
to-report get-settings-value [ input ]
  if ( input >= 0    and input <= 0.25) [report item 0 privacy-settings]
  if ( input >  0.25 and input <= 0.5)  [report item 1 privacy-settings]
  if ( input >  0.50 and input <= 0.75) [report item 2 privacy-settings]
  if ( input >  0.75                 )  [report item 3 privacy-settings]
end


;;;;; temporary function ----------------------------------------------------------
to reset
ask users [
    set settings-configured? false
    set age-visibility "public"
    set gender-visibility "public"
    set university-visibility "public"
    set country-visibility "public"
    set current-city-visibility "public"
    set interests-visibility "public"
    set language-visibility "public"
    set relationship-status-visibility "public"
    set color red
  ] 
  set-current-plot "No. Users who Configured Their Settings"
 clear-plot
end



;;;;; statistics ;;;;;
to-report average-degree
  report round ( mean [(count link-neighbors)] of users )
end

to-report max-degree 
   report max [(count link-neighbors)] of users
end

to-report min-degree
   report min [(count link-neighbors)]  of users
end

to-report no-of-friendships
  report  sum [(count link-neighbors)] of users / 2  
end


;;;;; descritizing settings value ;;;;;
to-report convert-setting [setting-value]
  ifelse (setting-value = "public" or setting-value = "friends-of-friends") [report 0][report 1] 
end


;;;;; generating random privacy settings ;;;;;
to random-settings
  setup
  set is-random-data true
  ask users [    
    set age-visibility item (random 4) privacy-settings
    set gender-visibility item (random 4) privacy-settings
    set university-visibility item (random 4) privacy-settings
    set country-visibility item (random 4) privacy-settings
    set current-city-visibility item (random 4) privacy-settings
    set interests-visibility item (random 4) privacy-settings
    set language-visibility item (random 4) privacy-settings
    set relationship-status-visibility item (random 4) privacy-settings
    set settings-configured? true   
    set color white
    ]
end



;;;;; exporting simulation data ;;;;;
to export-gexf
  let time date-and-time
  set time (word substring date-and-time 0 8 "@" substring date-and-time 16 27)
  set time replace-item 2 time "-"
  set time replace-item 5 time "-"
  
    
  let extention ""
  let rand ""
  let graph-file-name (word "Exported Files\\graph-[" num-users "]-" time ".gexf")
  
  if (output-file-type = "weka-arff") [
    set extention ".arff"
    ]
  if (output-file-type = "csv" or output-file-type = "csv-IRT") [
    set extention ".csv"
    ]
  if (output-file-type = "general") [
    set extention ".txt"
    ]
  
  if(is-random-data) [set rand "random-"]
  let dataset-file-name (word "Exported Files\\" rand "dataset-[" num-users "]-" time); extention) 
  
   ;;;;---------------------------------------------gephi graph file ---------------------------------------------;;;
  file-open ( graph-file-name )
  file-print (word "<?xml version="  "\"""1.0""\""  " encoding="  "\"""UTF-8""\""  "?>")
  file-print (word "<gexf"  " xmlns=" "\"""https://people.cs.uct.ac.za/~aabuelgasim""\"""" " version= " "\""1.2"\""  " >")
  file-print (word "<meta lastmodifieddate="  "\""timer"\""  " >" )
  file-print (word "<creator>Ammar M. Abuelgasim</creator>")
  file-print (word "<description> Simulated Social Network Dataset </description>")
  file-print (word "</meta>")
  file-print (word "<graph mode=" "\"""static" "\""" defaultedgetype=" "\"""undirected""\"" ">")
  file-print  "<nodes>"  

  ask users [
    file-print (word "<node id=" "\""who"\"" " label=" "\""who"\""  " />")
    ]
  file-print "</nodes>"
  file-print "<edges>"
  
  let link-counter 0
  ask links [
    file-print (word "<edge id=" "\""link-counter"\"" " source="  "\""[who] of end1"\""  " target="  "\"" [who] of end2 "\"" " />")
    set link-counter link-counter + 1
    ] 

  file-print "</edges>"
  file-print "</graph>"
  file-print "</gexf>"
   
  file-close 
  ;;;;---------------------------------------------gephi graph file end-----------------------------------------;;;
  
  ;;;;---------------------------------------------the arff file------------------------------------------------;;;
  
  if (output-file-type = "weka-arff" or output-file-type = "all") [
    file-open( (word dataset-file-name ".arff" ))
    ;file-print (word "@relation " "Simulated-Dataset-[" time "] \r\n" )
    file-print (word "@relation " "Simulated-" (substring dataset-file-name 15 49) " \r\n" )
    file-print " \r\n"
    
    ;;feature attributes
    file-print (word "@attribute age real \r\n")
    file-print (word "@attribute gender {male, female} \r\n")
    file-print (word "@attribute university {UCT, UP, UWC, Wits, Rhodes} \r\n")
    file-print (word "@attribute country {south-africa, sudan, nigeria, france, canada, Swaziland, kenya, cuba, ghana} \r\n")
    file-print (word "@attribute current-city {khartoum, cape-town, Johannesburg, Durban, Pretoria, Mhluzi, Port-Elizabeth} \r\n") 
    file-print (word "@attribute my-interest {music, sports, reading, dance, programming, traveling, theater, cinema} \r\n") 
    file-print (word "@attribute my-language {arabic, english, french, xhosa, zulu, afrikaans} \r\n") 
    file-print (word "@attribute my-relationship-status {single, in-a-relationship, married, engaged} \r\n")
    ;;class attributes
    file-print (word "@attribute age-visibility {public, friends-of-friends, friends, only-me} \r\n") 
    file-print (word "@attribute gender-visibility {public, friends-of-friends, friends, only-me} \r\n") 
    file-print (word "@attribute university-visibility {public, friends-of-friends, friends, only-me} \r\n") 
    file-print (word "@attribute country-visibility {public, friends-of-friends, friends, only-me} \r\n") 
    file-print (word "@attribute current-city-visibility {public, friends-of-friends, friends, only-me} \r\n") 
    file-print (word "@attribute interests-visibility {public, friends-of-friends, friends, only-me} \r\n") 
    file-print (word "@attribute language-visibility {public, friends-of-friends, friends, only-me} \r\n") 
    file-print (word "@attribute relationship-status-visibility {public, friends-of-friends, friends, only-me} \r\n")
    file-print " \r\n"
    
    file-print "@data \r\n"
    ask users with [settings-configured?] [
      file-print (word  age "," gender "," university "," country  "," current-city ","  my-interest  "," my-language ","  my-relationship-status ","
        age-visibility ","  gender-visibility  "," university-visibility  "," country-visibility  "," current-city-visibility ","  interests-visibility ","  language-visibility  "," relationship-status-visibility "\r\n" ) 
      ]    
      file-close
  ]
  
   ;;;;---------------------------------------------the arff file------------------------------------------------;;;
   
   ;;;;---------------------------------------------the csv file-------------------------------------------------;;;
  
  if(output-file-type = "csv" or output-file-type = "all") [
    file-open( (word dataset-file-name ".csv" ))
    file-print (word "age,gender,university,country,current-city,my-interest"
      ",my-language,my-relationship-status,age-visibility,gender-visibility,university-visibility,"
      "country-visibility,current-city-visibility,interests-visibility,language-visibility,relationship-status-visibility" "\r\n" )
    ask users with [settings-configured?] [
      file-print (word age "," gender "," university "," country  "," current-city ","  my-interest  "," my-language ","  my-relationship-status ","
        age-visibility ","  gender-visibility  "," university-visibility  "," country-visibility  "," current-city-visibility ","  interests-visibility ","  language-visibility  "," relationship-status-visibility "\r\n" )    
    ]
     file-close 
  ]
   ;;;;---------------------------------------------the csv file end---------------------------------------------;;;
  
  
   ;;;;---------------------------------------------the csv-IRT--------------------------------------------------;;;
  
  
   if(output-file-type = "csv-IRT" or output-file-type = "all") [
    file-open( (word dataset-file-name "[IRT].csv" ))
    file-print (word "userID,age,gender,university,"
      "country,city,interests,language,relationship" "\r\n" )
       ask users with [settings-configured?] [
      file-print (word who "," convert-setting age-visibility "," convert-setting gender-visibility  "," convert-setting university-visibility  "," convert-setting country-visibility  ","
        convert-setting current-city-visibility "," convert-setting interests-visibility "," convert-setting language-visibility  "," convert-setting relationship-status-visibility "\r\n" )    
    ]    
      file-close    
  ]
 
   ;;;;---------------------------------------------the csv-IRT---------------------------------------------------;;;  
   
   
   
   ;;;;---------------------------------------------taking a screenshot-------------------------------------------;;;  
   Export-Interface (word dataset-file-name ".png" )
   ;;;;---------------------------------------------end-----------------------------------------------------------;;;  
  user-message (word "File Exported Successfully!")
end
@#$#@#$#@
GRAPHICS-WINDOW
352
43
1115
827
79
79
4.74
1
10
1
1
1
0
1
1
1
-79
79
-79
79
0
0
1
Iterations
30.0

BUTTON
25
83
88
116
NIL
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

SLIDER
24
45
339
78
num-users
num-users
100
2000
100
1
1
user(s)
HORIZONTAL

BUTTON
97
83
213
116
create graph
form-graph
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
26
310
146
343
configure settings
simulate-privacy-settings
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
25
454
339
574
Gender Distripution
Gender
Perc. (%)
0.0
0.0
0.0
100.0
true
true
"" ""
PENS
"male" 1.0 1 -10899396 true "" "plot-pen-reset\nif count users != 0 [plotxy 0 ( (count users with [gender = \"male\"]) / (count users) ) * 100]"
"female" 1.0 1 -13345367 true "" "plot-pen-reset\nif count users != 0 [plotxy 1 ( (count users with [gender = \"female\"]) / (count users) ) * 100]"

MONITOR
1360
262
1443
307
avg. degree
average-degree
17
1
11

MONITOR
1360
318
1443
363
max. degree
max-degree
17
1
11

MONITOR
1359
374
1443
419
min. degree
min-degree
17
1
11

PLOT
1126
262
1355
419
Number of Friendships
Time Intervals
No. Friendships
0.0
0.0
0.0
0.0
true
false
"" ""
PENS
"default" 1.0 0 -2674135 true "" "plot no-of-friendships"

PLOT
25
579
339
699
Universities Distribution
Universities
Perc. (%)
0.0
4.0
0.0
100.0
true
true
"" ""
PENS
"UCT" 1.0 1 -16777216 true "" "plot-pen-reset\nif count users != 0 [plotxy 0 ( (count users with [university = \"UCT\"]) / (count users) ) * 100]"
"UP" 1.0 1 -7500403 true "" "plot-pen-reset\nif count users != 0 [plotxy 1 ( (count users with [university = \"UP\"]) / (count users) ) * 100]"
"UWC" 1.0 1 -2674135 true "" "plot-pen-reset\nif count users != 0 [plotxy 2 ( (count users with [university = \"UWC\"]) / (count users) ) * 100]"
"Wits" 1.0 1 -955883 true "" "plot-pen-reset\nif count users != 0 [plotxy 3 ( (count users with [university = \"Wits\"]) / (count users) ) * 100]"
"Rhodes" 1.0 1 -6459832 true "" "plot-pen-reset\nif count users != 0 [plotxy 4 ( (count users with [university = \"Rhodes\"]) / (count users) ) * 100]"

PLOT
1124
42
1437
220
Countries Distribution
Countries
Percentage (%)
0.0
8.0
0.0
100.0
true
true
"" ""
PENS
"south africa" 1.0 1 -16777216 true "" "plot-pen-reset\nif count users != 0 [plotxy 0 ( (count users with [country = \"south africa\"]) / (count users) ) * 100]"
"sudan" 1.0 1 -7500403 true "" "plot-pen-reset\nif count users != 0 [plotxy 1 ( (count users with [country = \"sudan\"]) / (count users) ) * 100]"
"nigeria" 1.0 1 -2674135 true "" "plot-pen-reset\nif count users != 0 [plotxy 2 ( (count users with [country = \"nigeria\"]) / (count users) ) * 100]"
"france" 1.0 1 -955883 true "" "plot-pen-reset\nif count users != 0 [plotxy 3 ( (count users with [country = \"france\"]) / (count users) ) * 100]"
"canada" 1.0 1 -6459832 true "" "plot-pen-reset\nif count users != 0 [plotxy 4 ( (count users with [country = \"canada\"]) / (count users) ) * 100]"
"Swaziland" 1.0 1 -1184463 true "" "plot-pen-reset\nif count users != 0 [plotxy 5 ( (count users with [country = \"Swaziland\"]) / (count users) ) * 100]"
"kenya" 1.0 1 -10899396 true "" "plot-pen-reset\nif count users != 0 [plotxy 6 ( (count users with [country = \"kenya\"]) / (count users) ) * 100]"
"cuba" 1.0 1 -13840069 true "" "plot-pen-reset\nif count users != 0 [plotxy 7 ( (count users with [country = \"cuba\"]) / (count users) ) * 100]"
"ghana" 1.0 1 -14835848 true "" "plot-pen-reset\nif count users != 0 [plotxy 8 ( (count users with [country = \"ghana\"]) / (count users) ) * 100]"

TEXTBOX
54
10
306
35
--- Simulation Options ---
20
0.0
1

TEXTBOX
68
426
266
451
--- Demographics ---
20
0.0
1

PLOT
25
706
340
826
Age Distribution
Age
No. Users
0.0
0.0
0.0
0.0
true
false
"set-plot-x-range 0 max-pxcor\nset-plot-y-range 0 num-users\n;set-histogram-num-bars 5" ""
PENS
"default" 3.0 1 -2674135 true "" "histogram [age] of users"

TEXTBOX
606
10
840
35
--- The Social Network ---
20
0.0
1

TEXTBOX
1172
230
1380
261
--- Graph Statistics ---
20
0.0
1

BUTTON
223
83
339
116
graph one step
form-graph
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
154
310
338
343
configure settings one step
simulate-privacy-settings
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
148
349
230
394
export data
ifelse (any? links or is-random-data) [\nexport-gexf\n]\n[\nuser-message (\"Sorry, No Data to be Exported!\")\n]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
1127
425
1444
563
Degree Distribution
Degrees
No. Users
0.0
0.0
0.0
10.0
true
false
"set-plot-x-range 0 100\nset-plot-y-range 0 num-users" "if any? links [\nset-plot-x-range 0 max [count link-neighbors] of users\nset-plot-y-range 0 (count users with [count link-neighbors = min [count link-neighbors] of users]) + 1\n]"
PENS
"default" 1.0 2 -2674135 false "" "histogram [(count link-neighbors)] of users"
"pen-1" 1.0 0 -3026479 true "" "histogram [(count link-neighbors)] of users"

SLIDER
26
119
183
152
alpha
alpha
0
1
0.5
.1
1
NIL
HORIZONTAL

SLIDER
189
119
339
152
beta
beta
0
1 - alpha
0.5
.1
1
NIL
HORIZONTAL

SLIDER
27
156
60
306
age-weight
age-weight
0
1
0.3
.01
1
NIL
VERTICAL

SLIDER
66
156
99
306
gender-weight
gender-weight
0
1
0.15
.01
1
NIL
VERTICAL

SLIDER
105
156
138
306
university-weight
university-weight
0
1
0.75
.01
1
NIL
VERTICAL

SLIDER
144
155
177
307
country-weight
country-weight
0
1
0.27
.01
1
NIL
VERTICAL

SLIDER
184
156
217
306
current-city-weight
current-city-weight
0
1
0.89
.01
1
NIL
VERTICAL

SLIDER
223
156
256
306
interest-weight
interest-weight
0
1
0.51
.01
1
NIL
VERTICAL

SLIDER
263
156
296
306
language-weight
language-weight
0
1
0.22
.01
1
NIL
VERTICAL

SLIDER
303
157
336
306
relationship-weight
relationship-weight
0
1
0.83
.01
1
NIL
VERTICAL

PLOT
1128
571
1442
719
No. Users who Configured Their Settings
Time Intervals
No. Users
0.0
10.0
0.0
10.0
true
false
"set-plot-y-range 0 num-users" ""
PENS
"# Configured" 1.0 0 -2674135 true "" "plot count users with [ settings-configured? ]"

CHOOSER
27
349
142
394
output-file-type
output-file-type
"all" "weka-arff" "csv" "csv-IRT" "general"
0

BUTTON
235
349
338
394
random-settings
random-settings
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

@#$#@#$#@
## WHAT IS IT?

A model that simulates the friendship formation and the propagation of privacy settings configuration in online social networks

## HOW IT WORKS

For generating the social graph of the social network, agents simply rely on homophily (i.e. tendency of individuals to associate and bond with similar others) and the number of mutual friends, to create their friendships, and to simulate the propagation of privacy settings configuration, agents rely on their internal privacy concern level and the privacy weight of each attribute to configure their privacy settings in a ways that suits each agent personally.

## HOW TO USE IT

Despite how it looks the model is simple, the simulation is done in two steps: creating the graph, and configuring privacy settings (in the same order).

**[a]** To simulate the social graph creation:
1.  Choose the number of user (optional)
2.  Press the setup button to create the users
3.  Press create graph to start the simulation
**[b]** To  simulate privacy settings configuration:
1.  Select suitable values for the parameters 
2.  Press configure settings

## THINGS TO TRY

Manipulate the different model parameters to get different results

## CREDITS AND REFERENCES

[Ammar M. Abuelgasim](https://people.cs.uct.ac.za/~aabuelgasim/)
Masters student
University of Cape Town
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
NetLogo 5.1.0
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
