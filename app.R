#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(robservable)
library(glue)

source("R/get_chart_data.R")
all_charts <- get_charts_list()

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("State of London Dashboard"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            select_drop_ui("seldrop"),
            hr(),
            breakdowns_panel_ui("breaks")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            main_chart_ui("mainc")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    seldrop <- select_drop_server("seldrop")
    brkdwns <- breakdowns_panel_server("breaks", seldrop)
    mainc <- main_chart_server("mainc", seldrop, brkdwns)
}

# Run the application
shinyApp(ui = ui, server = server)
