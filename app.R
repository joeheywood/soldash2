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
source("R/modules.R")

all_charts <- get_charts_list()

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("State of London Dashboard"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("chapt", 
                        "Chapter", 
                        choices = get_chapters()),
            selectInput("section", 
                        "..",
                        choices = list()
                        ),
            selectInput("chart",
                        "..",
                        choices = list()),
            selectizeInput("srch", choices = all_charts, multiple = TRUE, 
                           label = "Search:", selected = NULL, 
                           options = list(maxItems = 1, minItems = 1)),
            select_drop_ui("my_module_1")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            h3(textOutput("chart_title", )),
            h4(textOutput("chart_subtitle", )),
            robservableOutput("main_chart", width = 600),
            p(uiOutput("src")),
            div(uiOutput("analysis"))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    module_values_1 <- select_drop_server("my_module_1")
    
    output$chart_title <- renderText({"..."})
    output$chart_subtitle <- renderText({"..."})
    
    # output$analysis <- renderText({
    #     "
    #     ## Markdown title
    #     
    #     some para text with **bold** and *italics*
    #     
    #     "
    # })
    
    output$analysis <- renderUI({
        html <- get_html_for_chart(input$chart)
        tags$div(HTML(html))
        
    })
    
    output$main_chart <- renderRobservable({
        chrt <- input$chart
        print(chrt)
        if(nchar(chrt) > 0){
            x <- get_obs_chart(chrt)
            output$chart_title <- renderText({x$m$title})
            output$chart_subtitle <- renderText({x$m$sub})
            output$src <- renderUI({ a(x$m$source, href = x$m$link) })
            return(x$chart)
        } else {
            return(NULL)
        }
        
    })
    
    
    observe({
        chpt <- input$chapt
        sects <- get_subsections(chpt)
        updateSelectInput(session, "section", 
                          label = glue("Sections for {chpt}"),
                          choices = sects)
    })
    
    observe({
        chpt <- input$chapt
        sub <- input$section
        charts <- get_charts(chpt, sub)
        if(nchar(sub) > 0) {
            updateSelectInput(session, "chart",
                              label = glue("Charts for {sub}"),
                              choices = charts)
        }
        
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
