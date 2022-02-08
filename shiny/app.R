#lets do some thoughts, then figure out how to git commit with a button, 
#then get programming.

#well, the first thing is that I like the multi nava bar on the top, so it can
#do many things. I thought about a section on the left...but then I'm scrolling
#I think just radio buttons for attack, defense, icon (not done)----

#so, row 1, general info. Level dropdown (not done) High Weridness (not done), warp (not done), daily/battle count (not done)----
#row 2, etc - things I have to do
#make sure the high weirdness has an update so we can update on a crit

#attack (not done)----
#really easy, just the 3 attack spells + sorcerer spot
#option to gather power (and d6 dropdown)

#defense----
#high weirdness
#warp effect!
#defense spells + sorcerer spot

#icon----
#d12 drop down to dynamically show spells + sorcerer spot
#option to gather power (and d6 drop down)

#did it work?

library(tidyverse)
library(shiny)
library(conflicted)

conflict_scout()
conflict_prefer("filter", "dplyr")
hwt <- "1–2 	You accidentally summon 1d3 wibbles that either attack you or drift off to wreak a small amount of havoc elsewhere in the battle.
3–4 	You’re hit by a pulsing wrinkle in time. You move and speak ever–so–slightly slower than you should until you catch up. There’s no effect this turn, but at the end of your turn, decrease your initiative 2d6 points, to a minimum of 1.
5–6 	Each creature in the battle with temporary hit points loses half of them.
7–8 	You can only speak by asking questions. If you or your character violates this requirement, your character takes 1 damage the first time, 2 damage the second time, and so on. (Have another player keep track.)
9–10 	Your magic items’ quirks take over. If you aren’t doing a good enough job of roleplaying this personality fiasco, the GM and the rest of the players are authorized to suggest (in)appropriate behavior.
11 	You leech personality traits from surrounding spirits, whatever those happen to be. These are only traits, not personality overrides.
12 	You must speak in what you think could be the voice of the last creature your chaos mage attacked. If it doesn’t seem to have a voice, invent one.
13 	Small squeaking rodents erupt from any plausible cover that you go near. There’s no real effect except they’re somewhat noisy and rodents suddenly pop up in unexpected places.
14 	Your (the PC) favorite song begins playing around you magically, getting louder and louder (tell the table what type of song it is, or maybe hum it). It might or might not interfere with bardic songs or monsters that need to be heard properly to get their dirty work done.
15 	Your gender changes. At your discretion, the shift could be permanent when the weirdness ends. Or as permanent as things get for you.
16 	You grow horns or other spikes all over. If you already have horns, then you lose them. Some of the horns, or lack thereof, persist after the weirdness ends.
17 	One of your arms becomes a functional tentacle. It has no mechanical effects, but unless you’re special or lucky it’s probably not a very pretty tentacle. Your option on whether or not it remains after the weirdness ends.
18 	A great gust of wind circles around the battlefield. It probably has no serious effect unless there’s something happening that a great gust of wind could seriously affect.
19 	All creatures leave colored trails behind them as they move, turning the battle scene into a strange glowing artwork. Images fade every ten seconds or so.
20 	Some minor detail of your appearance changes hair color, gaps between teeth, handedness, and so on. The change is permanent–ish.
21–22 	Grit, explosive dust, or other debris explodes into the air around you, dealing 1d4 damage per tier to each nearby creature.
23–24 	There’s tension in the air, or the rumble of distant thunder, or a sense of impending disaster, and the next creature that misses with an attack this battle takes damage equal your Charisma modifier (double your Charisma modifier at 5th level; triple it at 8th level).
25–26 	Quickly passing auras blur and shake across the battlefield, or cold winds whip through and grow warmer as they pass, or the lights flicker . . . and the creature that has taken the most damage in the battle gains temporary hit points equal to 10% of its maximum hit points.
27–28 	One random creature in the battle other than you teleports next to and is engaged by one of its random enemies other than you.
29–30 	(Global effect) Space seriously twists, affecting the spells and ranged attacks of each creature in the battle creatures that are nearby count as if they were far away, and creatures that are far away count as if they are nearby.
31–32 	The first spell you cast this battle has effects (not damage) like a spell two levels higher than it, if possible.
33–34 	(Global effect) All normal saves made by creatures in the battle are actually easy saves (6+).
35–36 	(Global effect) There’s a blurring at the edge of all things. No creature can intercept another. Disengage attempts automatically succeed.
37–38 	(Global effect) The champions shall inherit the dirt! Until the end of your next turn, saves that fail count as if they succeed, and saves that succeed count as if they fail!
39–40 	Roll the escalation die and use the new result.
41–42 	(Global effect) Each creature in the battle taking ongoing damage immediately takes that damage. Then all ongoing damage effects end.
43–44 	(Global effect) Each creature that makes an attack targeting PD targets MD instead. Attacks against MD target PD instead.
45–46 	Your shadow detaches and flits around you. Until the weirdness ends, you gain a +2 attack bonus but take a –2 penalty to saves. Your personality may or may not be affected. It’s up to you.
47–48 	Choose yourself or one ally with temporary hit points and double those temporary hit points.
49–50 	There’s a large magical special effect of your choice (non-mechanical), and each creature in the battle ignores all resistances.
51–55 	You gain an additional quick action during each of your turns while this weirdness is in effect.
56–60 	When one of your allies casts an arcane spell this battle, the spell gains a small bonus effect chosen by the GM (something that suits the spell and the story).
61–65 	You and your allies gain small halos, or celestial light pours in, or a subtle glow illuminates each countenance. When one of your allies casts a divine spell this battle, it gains a small bonus effect chosen by the GM, something that suits the spell and the story.
66–70 	Your features shift and settle into a temporary new pattern. You gain a random racial ability until the end of your next turn. Ignore results that duplicate a racial ability you already have. Roll a d8. 1: dwarf’s that’s your best shot; 2: dark elf’s cruel; 3: high elf’s highblood teleport; 4: gnome’s confounding; 5: half–elf’s surprising; 6: halfling’s evasive; 7: holy one’s halo; 8: tieflings’s curse of chaos.
71–75 	If one of your allies is at 0 hit points or below, that ally can roll a free death save that won’t count against their missed death save total.
76–80 	Choose one creature (including you) that has already rallied this battle. It can rally again this battle (using the same action it normally would) as if it hadn’t already rallied (no roll if the first use).
81–85 	Your presence blurs through space, spirit, and time, and you can fight in spirit on your turn (see Combat Rules, Special Action) in addition to taking your normal turn.
86–90 	You shift, you waver, or you go transparent. You don’t take any miss damage while this weirdness is affecting you.
91–95 	The magic items in the area all start talking at once. You or one ally of your choice can roll to recharge one magic item (affected creature’s choice).
96–97 	Something related to your one unique thing goes very right for you. This is on you and the GM to work out together. The GM has the final say, though.
98 	If you and your allies flee RIGHT NOW, you don’t take a campaign loss for your discretion. This may take some explaining. It’s all about the chaos magic.
99 	Roll twice more on this table. If you wish you can ignore one of the rolled results but must stick with the other. If you roll the same result twice, you get that weirdness just once.
100 	You gain an extra standard action during the next turn after this weirdness goes into effect."
hwt <- gsub("\n", "\t", hwt)
k <- str_split(hwt, "\t") %>% unlist()

hwdt <- tibble(value = rep(NA, length(k)/2),
               text = NA)

for(i in 1:nrow(hwdt)){
  hwdt$value[i] <- k[2*i-1]
  hwdt$text[i]  <- k[2*i]
}

hwdt$value <- gsub("\u2013", ":", hwdt$value)
hwdt$value <- gsub(" ", "", hwdt$value)

hwls <- list()
hwls[[nrow(hwdt)]] <- eval(parse(text = hwdt$value[nrow(hwdt)]))

for(i in 1:(nrow(hwdt) - 1)){
  hwls[[i]] <- eval(parse(text = hwdt$value[i]))
}

hw.lookup.func <- function(j, vec, lookup){
  #ah, we need a zero option
  if(length(j) == 1){
    out <- vec[which(sapply(lookup, `%in%`, x = j))]
  }
  else if(length(j) == 2){
    out <- vec[which(sapply(lookup, `%in%`, x = j), arr.ind = TRUE)[,2]]
  }
  else {
    out <- "No Effect (yet)"
  }
  return(out)
}

def.icon <- tibble(roll = 1:6,
                   text = c(
                     "Air: Once before the end of your next turn, you can heal using a recovery as a quick action.",
                     "Earth: You gain temporary hit points equal to your Wisdom modifier (double your Wisdom modifier at 5th level; triple it at 8th level).",
                     "Fire: Until the end of your next turn, when an enemy moves to engage you, it takes fire damage equal to your Wisdom modifier (double your Wisdom modifier at 5th level; triple it at 8th level).",
                     "Water: Until the end of your next turn, when you heal using a recovery, add hit points equal to your Wisdom modifier to that healing (double your Wisdom modifier at 5th level; triple it at 8th level).",
                     "Metal: Until the end of your next turn, you gain a +2 bonus to AC.",
                     "Void: Until the end of your next turn, the first time an attack hits you, as a free action you can choose to lose hit points equal to your level to force the attacker to reroll the attack."
                   )
)



ui <- tagList(
  #shinythemes::themeSelector(),
  navbarPage(
    theme = shinythemes::shinytheme("readable"),
    "Hello!",
    tabPanel("Chaos Mage",
             selectInput(inputId = "Level",
                         label = "Level",
                         choices = c(1:10), 
                         selected = 1, selectize=FALSE),
             
             selectInput(inputId = "High Weirdness",
                         label = "High Weirdness Roll",
                         choices = c(1:100),
                         multiple=TRUE, selectize=TRUE),
             verbatimTextOutput("High Weirdness"),
             
             selectInput(inputId = "Defense Warp",
                         label = "Defense Warp Roll",
                         choices = c("", 1:6),
                         multiple=FALSE, selectize=FALSE),
             textOutput("Defense Warp"),
             
             radioButtons("Chaos Magic", "chaos.magic",
                          choices = c("Attack", "Defense", "Iconic"),
                          inline = TRUE)
             #three options!
             
             #end of chaos mage tab
    ),
    
    
    tabPanel("Initiative",
             titlePanel("Can I put in navlists in a navbar?"),
             navlistPanel("Header",
                          tabPanel("First",
                                   h3("Forthcoming")
                          ),
                          tabPanel("Second",
                                   h3("some more words")
                          )
             )
    )
  )
)


server <- function(input, output){
  
  output$"High Weirdness" <- renderPrint(hw.lookup.func(input$"High Weirdness",
                                                        vec = hwdt$text,
                                                        lookup = hwls
  ))
  #output$"High Weirdness input" <- renderPrint(input$"High Weirdness")
  output$"Defense Warp"   <- renderPrint(
    if(length(input$"Defense Warp" == 1)) {def.icon %>%
        filter(roll == input$"Defense Warp") %>%
        pull(text)
    }
    else "No Defense Warp Effect"
  )
  
  
  
  
}

shinyApp(ui, server)