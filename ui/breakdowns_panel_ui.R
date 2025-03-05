# ui/my_module_ui.R (or similar file)
breakdowns_panel_ui <- function(id) {

    all_charts <- get_charts_list()

    ns <- NS(id)  # Create a namespace function

    tagList(
        h4("Breakdowns"),
        p(textOutput(ns("module_output")))
    )
}
