find_stics_names <- function() {

  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Search a Stics parameter or variable"),
    miniUI::miniContentPanel(
      shiny::textInput(inputId = "name", label = shiny::strong("Enter a name or a part of") ,width = "100%" ),
      shiny::selectInput(inputId = "type", label = shiny::strong("Type of name"),
                         choices = c("variable", "parameter"),
                         selected = "parameter"),
      shiny::checkboxInput(inputId = "format", label = "Format name for RMarkdown", value = TRUE),
      scrollable = FALSE
    ),
    miniUI::miniContentPanel(
      shiny::tableOutput(outputId = "table")
    ),
    miniUI::miniButtonBlock(
      shiny::actionButton("insert", "Insert name"),
      shiny::actionButton("clean", "Clean output"),
      border = "top")

  )


  server <- function(input, output, session) {

    # Getting table an filtering it
    names_table <- shiny::reactive({
      l <- get_names_list(type = names_type())
      id <- grepl(pattern = input$name, x = l[[1]])
      l[id,]
    })

    # Getting type: var or par
    names_type <- shiny::reactive({substr(input$type, start = 1, stop = 3)})

    # getting format activation
    names_format <- shiny::reactive({input$format})

    # Changing name
    shiny::observeEvent(input$name, {
      output$table <- shiny::renderTable({
        names_table()
      })
      #}
    })

    # Handle the type selection in dropdown list
    shiny::observeEvent(input$type, {
      output$table <- shiny::renderTable({
        names_table()
      })
    })

    # Handle the Insert button being pressed.
    shiny::observeEvent(input$insert, {

      if (names_type() == "par") {
        shiny::stopApp(insert_stics_name(name = names_table()$name[1],
                                  kind = names_table()$kind[1],
                                  format = names_format(),
                                  type = "par"
        ))
      }

      if (names_type() == "var") {
        shiny::stopApp(insert_stics_name(name = names_table()$variable[1],
                                  format = names_format(),
                                  type = "var"
        ))
      }

    })

    # Quitting the app
    shiny::observeEvent(input$done, {
      shiny::stopApp("Bye")
    })


    shiny::observeEvent(input$clean, {
      output$table <- shiny::renderTable({
      })
    })



  }

  shiny::runGadget(ui, server, viewer = shiny::dialogViewer("find_stics_names"))

}

