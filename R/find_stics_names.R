find_stics_names <- function() {

  css <- "
  #name {
    color: red;
  }
  #table {
    background: #f2f2f2;
  }
"
  stics_versions <- SticsRFiles::get_stics_versions_compat()$versions_list
  last_version <- SticsRFiles::get_stics_versions_compat()$last_version

  ui <- miniUI::miniPage(title = "Stics names",
                         tags$style(css),
                         miniUI::gadgetTitleBar("Search a Stics parameter or variable"),
                         miniUI::miniContentPanel(

                           fillRow(height = "60%",
                                   fillCol(
                                     shiny::selectInput(inputId = "type", label = shiny::strong("Type of name"),
                                                        choices = c("variable", "parameter"),selected = "parameter")
                                   ),
                                   fillCol(
                                     shiny::selectInput(inputId = "version", label = shiny::strong("Stics version"),
                                                        choices = stics_versions,
                                                        selected = last_version))
                           ),

                           fillRow(height = "40%",
                                   fillCol(shiny::textInput(inputId = "name", label = shiny::strong("Searched name") ,
                                                            width = "80%" , placeholder = "Enter a name or a part of")
                                   ),
                                   fillCol(
                                     shiny::checkboxInput(inputId = "format", label = "Format name for RMarkdown", value = TRUE)
                                   )

                           ),
                         ),

                         miniUI::miniButtonBlock(
                           shiny::actionButton("insert", "Insert name"),
                           # shiny::actionButton("clean", "Clean output"),
                           border = "top"
                         ),

                         miniUI::miniContentPanel(
                           # miniUI::gadgetTitleBar("Results"),
                           shiny::tableOutput(outputId = "table")
                         )#,

                         # miniUI::miniButtonBlock(
                         #   shiny::actionButton("insert", "Insert name"),
                         #   # shiny::actionButton("clean", "Clean output"),
                         #   border = "top"
                         # )

  )


  server <- function(input, output, session) {

    # Getting type: var or par
    names_type <- shiny::reactive({substr(input$type, start = 1, stop = 3)})

    # Getting table an filtering it
    # Displaying only the 2 1st columns
    names_table <- shiny::reactive({
      print(input$version)
      if (length(in_name())) {
        l <- get_names_list(type = names_type(), stics_version = names_version())
        id <- grepl(pattern = make_pattern(in_name()), x = l[[1]])
        l[id, ]
      }
    })

    # getting format activation
    names_format <- shiny::reactive({input$format})

    names_version <- shiny::reactive({input$version})

    in_name <- shiny::reactive({input$name})


    # Changing name
    shiny::observeEvent(input$name, {
      output$table <- shiny::renderTable({
        names_table()
      }, caption = "Results",
      caption.placement = getOption("xtable.caption.placement", "top"),
      caption.width = getOption("xtable.caption.width", NULL)
      )
    })

    # Handle the type selection in dropdown list
    shiny::observeEvent(input$type, {
      output$table <- shiny::renderTable({
        names_table()
      }, caption = "Results",
      caption.placement = getOption("xtable.caption.placement", "top"),
      caption.width = getOption("xtable.caption.width", NULL)
      )
    })

    shiny::observeEvent(input$version, {
      output$table <- shiny::renderTable({
        names_table()
      }, caption = "Results",
      caption.placement = getOption("xtable.caption.placement", "top"),
      caption.width = getOption("xtable.caption.width", NULL)
      )
    })

    # Handle the Insert button being pressed.
    shiny::observeEvent(input$insert, {

      # print(names_type())

      loc_table <- names_table()

      if (dim(loc_table)[1] > 1) return()

      if (names_type() == "par") {
        # print(names_table()$name[1])
        # print(names_table()$kind[1])
        shiny::stopApp(insert_stics_name(name = loc_table$name[1],
                                         kind = loc_table$kind[1],
                                         format = names_format(),
                                         type = "par"
        ))
      }

      if (names_type() == "var") {
        # print(names_table()$variable[1])
        shiny::stopApp(insert_stics_name(name = loc_table$variable[1],
                                         format = names_format(),
                                         type = "var"
        ))
      }

    })

    # Quitting the app
    shiny::observeEvent(input$done, {
      shiny::stopApp("Bye")
    })


    # shiny::observeEvent(input$clean, {
    #   output$table <- shiny::renderTable({
    #   })
    # })



  }

  shiny::runGadget(ui, server, viewer = shiny::dialogViewer("find_stics_names"))

}

