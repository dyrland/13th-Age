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

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
