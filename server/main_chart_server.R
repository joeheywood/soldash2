main_chart_server <- function(id, seldrop, brk) {
    moduleServer(id, function(input, output, session) {
        observe({
            if(nchar(seldrop()$chart) == 0) {return(FALSE)}
            # x <- get_obs_chart(seldrop()$chart, brk()$breakdown)
            print(brk()$breakdown)
            x <- get_obs_chart(seldrop()$chart)
            #print(seldrop()$chart)
            output$chart_title <- renderText({x$m$title})
            output$chart_subtitle <- renderText({x$m$sub})
            output$src <- renderUI({ a(x$m$source, href = x$m$link) })
            output$main_chart <- renderRobservable({x$chart})
            output$analysis <- renderUI({
                html <- get_html_for_chart(seldrop()$chart)
                tags$div(HTML(html))
            })

        })


    })
}
