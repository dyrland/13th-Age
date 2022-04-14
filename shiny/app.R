#new notes!
#I need to get persistent storage
#i need to get a minimum width - DONE!
#and I need baddies to be random on rest

library(tidyverse)
library(shiny)
library(gt)
library(conflicted)
library(DT)

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
    out <- paste(vec[which(sapply(lookup, `%in%`, x = j), arr.ind = TRUE)[,2]],
                 collapse = "<br/>")
  }
  else {
    out <- "No Effect (yet)"
  }
  return(out) 
  #the cat() removes the [number]
}

hw.lookup.func(c(2,7), hwdt$text, hwls) -> test

icon.warp <- tibble(roll = 1:6,
                   text = c(
                     "Air: Randomly determine two icon associations for the spell you’ll cast instead of one. Choose one of those associations to use for that spell.",
                     "Earth: Until the end of your next turn, you gain a bonus to PD and MD equal to your Intelligence modifier.",
                     "Fire: Until the end of your next turn, you gain the once-per-battle racial power of a random nearby ally; ignore this benefit if it duplicates your own racial power or if it doesn’t make sense during the battle (human, for example).",
                     "Water: Until the end of your next turn, you gain a bonus to saves equal to your Intelligence modifier.",
                     "Metal: Until the end of your next turn, critical hits scored against you only count as normal hits.",
                     "Void: Until the end of your next turn, when you cast a daily iconic spell, roll a hard save (16+). If you succeed, you don’t expend that use of a daily spell slot, but you still can’t cast that specific daily iconic spell again until you take a full heal-up."
                   )
)

gathering.power.table <-
  tibble(max.level = rep(c(4,7,10), each = 6), 
         roll = rep(1:6, 3),
         text = rep(c("You gain a +1 bonus to AC until the start of your next turn.",
                  "Deal damage equal to your level to all nearby staggered enemies.",
                  "Deal damage equal to your level to one nearby enemy.",
                  "You gain a +1 bonus to AC and Physical Defense until the start of your next turn.",
                  "Deal damage equal to your level + your Charisma modifier to all nearby staggered enemies.",
                  "Deal damage equal to your level + your Charisma modifier to one nearby enemy.",
                  "You gain a +1 bonus to all defenses until the start of your next turn.",
                  "Deal damage equal to your level + twice your Charisma modifier to all nearby staggered enemies.",
                  "Deal damage equal to your level + twice your Charisma modifier to one nearby enemy."
                  ), each = 2)
  )
# dwarven queen - dwarven king -4
# high priest - high priestess - 10
# general - ?
# Magus - Archmage - 1
# Prince of the Deeps - Elf Queen - 9
# Rebel - Crusader? Diabolist? 
# Brood Mother - Orc Lord - 9
# Dweller - Diabolist - 3
#
#
#
#
#the below is crazy. 
#I think I just have to type one person each day



#Title - icon spells
#subtitle - none
#col one: Icon
#col Two: At Will/ Per Battle /Daily
#Col Three - other text?
#col 3 



icon.spells <- tibble(
  roll = rep(1:12, each = 2),
  level = c(1, 5,
            1, 3,
            1, 1,
            1, 1,
            1, 3,
            1, 5,
            1, 3,
            1, 7,
            1, 3, 
            1, 1,
            1, 3,
            1, 3),
  Icon = rep(c("Grand Magus", "Crusader (og)", "Diabolist (og)",
               "Dwarf Queen", "Elf Prince", "Great Gold Wyrm (og)",
               "High Druid (og)", "Lich King (og)", "Orc Lord (og)",
               "High Priest", "Prince of Shadows (og)", "The Three (og)"),
             each = 2),
  Name = c("Silver Arrows", "Cascading Power",
           "Castigation", "Terribly Spiky Armor",
           "Tortured Scream", "Trace of Corruption",
           "Yours!", "Ours!",
           "Shards of Magic", "Coronation",
           "Fiery Claw", "Final Wrath",
           "Bolt and Thunder", "The Final Surge",
           "Evil Touch", "Unsummoning",
           "War Drums", "Savage Endings",
           "Holy Spark", "Temple Bells",
           "Shadow Dance", "Step into Shadow",
           "Twisted Beam", "Ancient Scales"),
  Frequency = c(rep(c("At-Will", "Daily"), times = 10),
                "At-Will", "Once-Per-Battle", "At-Will", "Daily"),
  Targets = c("1d3 Nearby Enemies", "A number of random Nearby creatures equal to the escalation die",
              "One enemy you are engaged with if possible; if not, then one nearby enemy", "Self",
              "One nearby enemy", "You or one nearby ally; the target must have a positive or conflicted relationship with a villainous icon",
              rep("spiderman", times = 18)),
  Text = c("The target takes 4 force damage.", "The targets are embroiled in silver fire! Each targeted ally can roll an immediate easy save (6+); if that ally succeeds, they regain one daily or recharge power of their choice. Then each targeted enemy takes damage equal to 1d10 x the escalation die.\nAfter the damage, roll the escalation die and use the new result.",
           "Attack: Charisma + Level vs. MD\nHit: 1d8 + Charisma psychic damage\nHit vs. a Staggered Target: As a hit, except there is no damage roll; the target takes maximum damage.\nMiss: Damage equal to your level.", "Until the end of the battle, you gain a +3 bonus to AC and when an enemy engaged with you misses you with an attack, it takes 3d6 + Charisma damage.",
           "Special: When you cast the spell, you or a willing nearby ally of your choice loses 1d6 hit points.\\nAttack: Charisma + Level vs. MD\\nHit: 3d6 + Charisma psychic damage.\\nMiss: Damage equal to your level.\\n3rd level spell: 6d6 damage, you or ally loses 2d6 hit points.\\n5th level spell: 6d10 damage, you or ally loses 4d6 hit points.\\n7th level spell: 10d10 damage, you or ally loses 6d6 hit points.\\n9th level spell: 2d8 x 10 damage, you or ally loses 8d6 hit points.", "Effect: The target rolls a save against each save ends effect affecting it. Then the target can heal using a recovery from a nearby ally (target’s choice, even if that ally isn’t willing.",
              
           rep("batman", times = 18))
)



villagers <-
  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/villagers.csv'
  ) %>% 
  select(-row_n) %>% 
  unique()
pc.ava <- c("Sterling", "Nibbles", "Lionel", "Roscoe")
npc.ava <- sample(villagers$name[!villagers$name %in% pc.ava], 1)

init.table <- tibble(round = 0,
                     Character = c("Baddies", "Glarna", "Jade",
                                    "Concha", "Dedna"),
                     name = c(npc.ava, pc.ava),
                              Initiative = c(20, 15, 25, 10, 10)
                            
)
                     
villagers <- villagers %>%
  #filter(name %in% c("Nibbles", "Sterling", "Big Top", "Lionel", "Roscoe")) %>%
  #me, joey, baddies, #britt
  mutate(
    Avatar = paste0(
      "<img src=\"",
      url,
      #"\" height=\"30\"",
      "\"></img>"
    )
  ) %>%
  select(Avatar, name, personality, phrase)

#we could probably randomly sample...

init.table <- left_join(init.table, villagers) %>%
  select(round, Avatar, Character, Initiative)

headerCallbackRemoveHeaderFooter <- c(
  "function(thead, data, start, end, display){",
  "  $('th', thead).css('display', 'none');",
  "}"
)

# js <- c(
#   "function(settings){",
#   "  $('#s').ionRangeSlider({",
#   "    type: 'double',",
#   "    grid: true,",
#   "    grid_num: 10,",
#   "    min: 0,",
#   "    max: 20,",
#   "    from: 5,",
#   "    to: 15",
#   "  });",
#   "}"
# )


#init.table <- villagers
# ui ----
ui <- tagList(
  #shinythemes::themeSelector(),
  fluidPage(
    theme = shinythemes::shinytheme("spacelab"),
    title = "Initiative",
    align = "center",
    #titlePanel("Initiative Table"),
    #mainPanel( 
             DTOutput("InitiativeTable"),
             
             actionButton("AdvanceButton", "Advance!"),
             actionButton("AddNPCButton", "Add N/PC!"),
             actionButton("Reset", "Reset"),
             actionButton("Snake", "Snake!")
        
             #DTOutput("test")
             
             #uiOutput('my_audio')
    #)
  #   tabPanel("Chaos Mage",
  #            selectInput(inputId = "Level",
  #                        label = "Level",
  #                        choices = 1:10, 
  #                        selected = 2, selectize=FALSE),
  #            
  #            selectInput(inputId = "High Weirdness",
  #                        label = "High Weirdness Roll",
  #                        choices = c(1:100),
  #                        multiple=TRUE, selectize=TRUE),
  #            htmlOutput("High Weirdness"),
  #            #textOutput("High Weirdness"),
  # 
  #            selectInput(inputId = "Icon Warp",
  #                        label = "Icon Warp Roll",
  #                        choices = c("", 1:6),
  #                        multiple=FALSE, selectize=FALSE),
  #            textOutput("Icon Warp"),
  #            
  #            radioButtons("chaos.magic", "Chaos Magic",
  #                         choices = c("Attack", "Defense", "Iconic"),
  #                         inline = TRUE),
  #            #three options!
  #            
  #            
  #            #end of chaos mage tab
  #   
  #   #maybe some conditionalPanel() work to be done here?
  #          uiOutput("Magic Type"),
  #          uiOutput("Gather Power Choice"),
  #          uiOutput("Gather Power Table")
  #   
  #   ),
  #   
  #   tabPanel("Initiative thoughts",
  #            titlePanel("Can I put in navlists in a navbar?"),
  #            navlistPanel("Header",
  #                         tabPanel("First",
  #                                  h3("Forthcoming")
  #                         ),
  #                         tabPanel("Second",
  #                                  h3("some more words, maybe https://yihui.shinyapps.io/DT-edit/")
  #                         )
  #                         #i think my thoughts are to make a table and then 
  #                         #hit a button and it auto sorts and highlights a row
  #            )
  #   )
  )
)


server <- function(input, output){
  
  # output$test <- renderDT({
  #   data <- data.frame(ROW = 1:5,
  #                      TEXT = '<input id="text" type="text" class="form-control" value=""/>',
  #                      SINGLE_SELECT = '<select id="single_select" style="width: 100%;">
  #                      <option value="" selected></option>
  #                      <option value="A">A</option>
  #                      <option value="B">B</option>
  #                      <option value="C">C</option>
  #                      </select>',
  #                      SLIDER = '<input type="text" id="s" name="slider" value="" />',
  #                      MULTIPLE_SELECT = '<select id="multiple_select" class="form-control" multiple="multiple">
  #                      <option value=""></option>
  #                      <option value="A">A</option>
  #                      <option value="B">B</option>
  #                      <option value="C">C</option>
  #                      </select>',
  #                      stringsAsFactors = FALSE)
  #   
  #   datatable(data = data,
  #             selection = "none",
  #             escape = FALSE,
  #             rownames = FALSE, 
  #             options = 
  #               list(
  #                 initComplete = JS(js)
  #               ))
  # })
  
  init.reactive <- reactiveValues()
  init.reactive$Data <- init.table
  
  observeEvent(input$AdvanceButton, {
    # temp <- reactive({
    #   temp <- init.reactive$Data
    #   temp$round[1] <- temp$round[1] + 1
    #   temp$Initiative[1] <- temp$Initiative[1] + 1
    #   temp <- temp %>%
    #     arrange(desc(Initiative), round)
    #   return(temp)
    # })
    # init.reactive$Data <- dt()
    init.reactive$Data <- init.reactive$Data %>%
      arrange(round, desc(Initiative))
    
    init.reactive$Data$round[1] <- init.reactive$Data$round[1] + 1
    #init.reactive$Data$Initiative[1] <- init.reactive$Data$Initiative[1] +1
    init.reactive$Data <- init.reactive$Data %>%
     arrange(round, desc(Initiative))
  }
  )
  
  output$"InitiativeTable" <- renderDT(
    datatable(
      init.reactive$Data, 
      selection = "none",
      editable = list(target = "cell"),
      rownames = FALSE, escape = FALSE, 
      callback = JS("$.fn.dataTable.ext.errMode = 'none';"),
      #colnames = c("","",""),
      options = list(autoWidth = TRUE,
                     ordering = FALSE,
                     paging = FALSE,
                     searching = FALSE,
                     dom = "t",
                     scrollX = TRUE,
                     columnDefs = list(list(visible=FALSE, targets=0))
                     #headerCallback = JS(headerCallbackRemoveHeaderFooter)
    )) %>% formatStyle(columns = c(4), `text-align` = "center"),
    server = TRUE
  )
  
  #something about observeevent, they just make a new data frame
  #and then row bind
  #event is just actionevent
  
  observeEvent(input$InitiativeTable_cell_edit, {
    init.reactive$Data <- editData(init.reactive$Data,
                                   input$InitiativeTable_cell_edit,
                                   "InitiativeTable",
                                   rownames = FALSE)
  })
  observeEvent(input$AddNPCButton, {
    temp <- init.reactive$Data %>% slice(1)
    temp$picture <- sample(villagers$picture[
                     !villagers$picture %in% init.reactive$Data$picture], 1)
    temp$Character <- "New Character"
    temp$Initiative <- 1
    init.reactive$Data <- bind_rows(init.reactive$Data, temp)
  }
  )
  
  observeEvent(input$Reset, {
    showModal(
      modalDialog(
        title = "R U 4 Realz?",
        "Something Witty",
        footer = tagList(
         modalButton("Nope"),
         actionButton("Yes", "No Doubt")
        ), easyClose = TRUE
      )
    )
  })
  
  observeEvent(input$Yes, {
    init.reactive$Data <- init.table
    removeModal()
  }
  )
  
  observeEvent(input$Snake, {
    num <- sample(1:100, 1)
    num <- case_when(between(num, 01, 19) ~ 1,
                     between(num, 20, 38) ~ 2,
                     between(num, 39, 57) ~ 3,
                     between(num, 58, 76) ~ 4,
                     between(num, 77, 95) ~ 5,
                     between(num, 96, 99) ~ 9,
                     between(num, 100, 100) ~ 10
    )
    file <- paste0("snake-", num, ".mp3")
    
    insertUI(selector = "#Snake",
             where = "afterEnd",
             ui = tags$audio(src = file, type = "audio/mp3", autoplay = NA, controls = NA, style="display:none;")  
      )
    })
  
  
  # output$my_audio <- renderUI({
  #   tags$audio(src = "snake-1.mp3", type = "audio/mp3", autoplay = NA, controls = NA)
  # })
  
  # output$"High Weirdness" <- renderUI({
  #   HTML(
  #     paste(
  #       hw.lookup.func(input$"High Weirdness",
  #                      vec = hwdt$text,
  #                      lookup = hwls
  #       ), sep = "<br/>"
  #     )
  #   )
  # })
  # 
  # output$"Icon Warp"   <- renderText(
  #   if(length(input$"Icon Warp" == 1)) {icon.warp %>%
  #       filter(roll == input$"Icon Warp") %>%
  #       pull(text)
  #   }
  #   else "No Icon Warp Effect"
  # )
  # output$"Magic Type" <- renderUI({
  #   switch(input$chaos.magic,
  #          "Attack" = fluidPage(radioButtons("gather.power", "Gather Power?",
  #                                  choices = c("No", "Yes"), 
  #                                  inline = TRUE),
  #          p("Choose an attack Spell!")
  #          ), #issue that this isn't appearing
  #          "Defense" = p("Choose a Defense spell"),
  #          "Iconic"  = fluidPage(p("Roll Weirdness AND Roll an Iconic warp, Roll a d12 (or 2), examine spells, gather power"),
  #            radioButtons("gather.power", "Gather Power?",
  #                                  choices = c("No", "Yes"), 
  #                                  inline = TRUE),
  #            #uncertain about this
  #            selectInput(inputId = "Icon Roll",
  #                        label = "Icon Spell Roll",
  #                        choices = c(1:12),
  #                        multiple=TRUE, selectize=TRUE),
  #            gt_output("icon.table")
  #          )
  #   )
  # })
  #   output$"Gather Power Choice" <- renderUI({
  #     switch(input$gather.power,
  #            "No" = "",
  #            "Yes" = selectInput(inputId = "Gather Power Table",
  #                                label = "Your Gather Power 1d6 Roll?",
  #                                choices = c("", 1:6),
  #                                multiple=FALSE, selectize=FALSE)
  #     )
  #   })
  #   output$"Gather Power Table" <- renderText({
  #     if(length(input$"Gather Power Table" == 1) & input$gather.power == "Yes"){
  #         gathering.power.table %>%
  #         filter(roll == input$"Gather Power Table") %>%
  #         filter(max.level >= as.numeric(input$Level)) %>%
  #         filter(max.level == min(max.level)) %>%
  #         pull(text)
  #         
  #       #this doesn't work for levels 8-10? but does for others?
  #       #something about maxlevel 10 dropping?
  #     }
  #     else ""
  #   })
  #   output$icon.table <- render_gt(
  #     expr = icon.spells %>% 
  #            filter(roll %in% input$"Icon Roll") %>%
  #            filter(level <= input$Level) %>%
  #            select(-roll, - level) %>%
  #            gt() %>%
  #            tab_header(title = "Icon Spell Choices"),
  #     align = "left"
  #   )
    

  
  
}

shinyApp(ui, server)