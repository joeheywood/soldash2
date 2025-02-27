# ui/my_module_ui.R (or similar file)
main_chart_ui <- function(id) {


    ns <- NS(id)  # Create a namespace function

    tagList(
        h3(textOutput(ns("chart_title"))),
        h4(textOutput(ns("chart_subtitle"))),
        robservableOutput(ns("main_chart"), width = 600),
        p(uiOutput(ns("src"))),
        div(uiOutput(ns("analysis")))
    )
}
