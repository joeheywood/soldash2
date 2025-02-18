# ui/my_module_ui.R (or similar file)
select_drop_ui <- function(id) {
    ns <- NS(id)  # Create a namespace function
    
    tagList(
        h4("Dropdown"),
        selectInput(ns("chapt"), 
                    "Chapter", 
                    choices = get_chapters()),
        selectInput(ns("section"), 
                    "..",
                    choices = list()
        ),
        selectInput("chart",
                    "..",
                    choices = list()),
        verbatimTextOutput(ns("module_output"))
    )
}