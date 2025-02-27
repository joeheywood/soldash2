# ui/my_module_ui.R (or similar file)
select_drop_ui <- function(id) {

    all_charts <- get_charts_list()

    ns <- NS(id)  # Create a namespace function

    tagList(
        h4("Dropdown"),
        p(textOutput(ns("module_output"))),
        selectInput(ns("chapt"),
                    "Chapter",
                    choices = get_chapters()),
        selectInput(ns("section"),
                    "Section",
                    choices = list()
        ),
        selectInput(ns("chart"),
                    "Chart",
                    choices = list()),
        hr(),
        selectizeInput(ns("srch"),
                       choices = all_charts, multiple = TRUE,
                       label = "Search:", selected = NULL,
                       options = list(placeholder = "SEARCH",  maxItems = 1,
                                      minItems = 1))
    )
}
