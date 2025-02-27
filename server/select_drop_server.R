# server/my_module_server.R (or similar file)
select_drop_server <- function(id) {
    moduleServer(id, function(input, output, session) {

        using_search <- reactiveVal()
        using_search(FALSE)

        stop_search <- reactiveVal()
        stop_search(Sys.time() - 10)

        sel_chapt <- reactive({ input$chapt })
        sel_section <- reactive({input$section})
        sel_chart <- reactive({input$chart})

        srch_sel <- reactive({input$srch})

        # output$module_output <- renderText({
        #     glue("Chapter: {sel_chapt()} | Section: {sel_section()} | Chart: {sel_chart()} | Search: {srch_sel()}")
        # })

        observeEvent(sel_chapt(),  {
            output$module_output <- renderText({
                x <- Sys.time() - stop_search()
                if(x > 2) {
                    chpt <- sel_chapt()
                    sects <- get_subsections(chpt)
                    updateSelectInput(session, "section",
                                      label = glue("Sections for {chpt}"),
                                      choices = sects)


                    updateSelectizeInput(session, "srch", selected = character(0))
                    glue("Changing chapter to {chpt} {round(x, 1)}")
                } else {
                    glue("Nah leave it. {round(x, 1)}")
                }
            })
        })

        observeEvent(sel_section(), {
            chpt <- sel_chapt()
            sub <- sel_section()
            x <- Sys.time() - stop_search()
            if(nchar(sub) > 0 & x > 2) {
                charts <- get_charts(chpt, sub)
                updateSelectizeInput(session, "srch", selected = character(0))
                updateSelectInput(session, "chart",
                                  label = glue("Charts for {sub}"),
                                  choices = charts)
            }
        })

        observeEvent(srch_sel(), {
            using_search(TRUE)
            stop_search(Sys.time())

            sel <- srch_sel()
            mm <- get_meta_for_chart(sel)
            updateSelectInput(session, "chapt", selected = mm$sol_chapter)

            sects <- get_subsections(mm$sol_chapter)
            updateSelectInput(session, "section",
                              label = glue("Sections for {mm$sol_chapter}"),
                              choices = sects,
                              selected = mm$sol_subsection)

            charts <- get_charts(mm$sol_chapter, mm$sol_subsection)
            updateSelectInput(session, "chart",
                              label = glue("Charts for {mm$sol_subsection}"),
                              choices = charts,
                              selected = sel)
        })

        # Return a reactive list of values (optional, but good practice)
        reactive({
            list(
                chapter = sel_chapt(),
                section = sel_section(),
                chart = sel_chart()
            )
        })

    })
}
