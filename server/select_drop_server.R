# server/my_module_server.R (or similar file)
select_drop_server <- function(id) {
    moduleServer(id, function(input, output, session) {

        using_search <- FALSE

        sel_chapt <- reactive({ input$chapt })
        sel_section <- reactive({input$section})
        sel_chart <- reactive({input$chart})

        srch_sel <- reactive({input$srch})

        # output$module_output <- renderText({
        #     glue("Chapter: {sel_chapt()} | Section: {sel_section()} | Chart: {sel_chart()} | Search: {srch_sel()}")
        # })

        observeEvent(sel_chapt(),  {
            output$module_output <- renderText({
                if(using_search == FALSE) {
                    chpt <- sel_chapt()
                    sects <- get_subsections(chpt)
                    updateSelectInput(session, "section",
                                      label = glue("Sections for {chpt}"),
                                      choices = sects)
                    glue("Changing chapter to {chpt}")
                } else {
                    glue("Nah leave it.")
                }
            })
        })

        observeEvent(sel_section(), {
            chpt <- sel_chapt()
            sub <- sel_section()
            if(nchar(sub) > 0) {
                charts <- get_charts(chpt, sub)
                updateSelectInput(session, "chart",
                                  label = glue("Charts for {sub}"),
                                  choices = charts)
            }
        })

        observeEvent(srch_sel(), {
            sel <- srch_sel()
            using_search <- TRUE
            updateSelectInput(session, "chapt", selected = "Housing")
        })

        # Return a reactive list of values (optional, but good practice)
        reactive({
            list(
                chapter = "chapter",
                section = "section",
                chart = "chart"
            )
        })

    })
}
