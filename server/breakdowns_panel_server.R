# server/my_module_server.R (or similar file)
breakdowns_panel_server <- function(id, seldrop) {
    moduleServer(id, function(input, output, session) {

        sel_brk <- reactiveVal()

        observe({
            brk <- get_breakdowns(seldrop()$chart)
            msg <- ""
            if(length(brk) == 1 & nchar(brk[1]) == 0) {
               msg <- "No breakdowns"
            } else {
               sel_brk(brk$chart[1])
               print(brk$chart[1])
               print(sel_brk())
                msg <- glue("{length(brk)} | sel: {sel_brk()}")
            }

            output$module_output <- renderText({glue("Out: {seldrop()$chart} | {msg}")})

        })

        # Return a reactive list of values (optional, but good practice)
        reactive({
            list(
                chapter = seldrop()$chart,
                breakdown = sel_brk()
            )
        })


    })
}
