# server/my_module_server.R (or similar file)
select_drop_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        
        # # Reactive expression to store the text input value
        # module_text <- reactive(input$module_text)
        # 
        # # Observe the button click
        # observeEvent(input$module_button, {
        #     print(paste("Button clicked in module:", module_text()))
        # })
        # 
        # # Output the text input value
        # output$module_output <- renderPrint({
        #     paste("Text entered in module:", module_text())
        # })
        
        observe({
            chpt <- input$chapt
            sects <- get_subsections(chpt)
            updateSelectInput(session, "section", 
                              label = glue("Sections for {chpt}"),
                              choices = sects)
            
        })
        
        
        # Return a reactive list of values (optional, but good practice)
        reactive({
            list(
                text = "test"
            )
        })
        
    })
}