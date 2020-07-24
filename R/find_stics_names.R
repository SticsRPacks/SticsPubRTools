#' Gadget for finding Stics names (parameters, variables)
#'
#'
#' @export
#'
# @examples

find_stics_names <- function() {
  require(SticsRFiles)

  # Masquing warnings display during app execution
  options(warn=-1)
  on.exit(options(warn=0))

  tags <- shiny::tags

  css <-
  "
  #name {
    color: blue;
  }
  #table {
    background: #f2f2f2;
  }
  #insert {
    background-color:#337ab7;
    color:#e8f0f7;
  }
  "
  stics_versions <- SticsRFiles::get_stics_versions_compat()$versions_list
  last_version <- SticsRFiles::get_stics_versions_compat()$last_version

  ui <- miniUI::miniPage(title = "Stics names",
                         tags$style(css),
                         miniUI::miniTitleBar(title = "Search a Stics parameter or variable"
                         ),
                         miniUI::miniContentPanel(

                           shiny::fillRow(height = "40%",
                                          shiny::fillCol(width = "70%",
                                            shiny::selectInput(inputId = "type", label = shiny::strong("Type of name"),
                                                               choices = c("variable", "parameter"),selected = "parameter")
                                          ),
                                          shiny::fillCol(width = "70%",
                                            shiny::selectInput(inputId = "version", label = shiny::strong("Stics version"),
                                                               choices = stics_versions,
                                                               selected = last_version))
                           ),

                           shiny::fillRow(height = "40%",
                                          shiny::fillCol(
                                            shiny::textInput(inputId = "name", label = shiny::strong("Searched name") ,
                                                             width = "80%" , placeholder = "Enter a name or a part of")
                                          ),
                                          shiny::fillCol(
                                            shiny::checkboxInput(inputId = "format", label = "Format name for RMarkdown", value = TRUE)
                                          )

                           ),
                           shiny::textOutput(outputId = "number")
                         ),

                         miniUI::miniButtonBlock(
                           shiny::actionButton("cancel", "Cancel"),
                           shiny::actionButton("insert", "Insert name"),
                           border = "top"
                         ),

                         miniUI::miniContentPanel(
                           # miniUI::gadgetTitleBar("Results"),
                           shiny::tableOutput(outputId = "table")
                         )

  )


  server <- function(input, output, session) {

    # Getting type: var or par
    names_type <- shiny::reactive({substr(input$type, start = 1, stop = 3)})

    # Getting table an filtering it
    #
    names_table <- shiny::reactive({
      #print(input$version)
      if (length(in_name())) {
        l <- get_names_list(type = names_type(), stics_version = names_version())
        id <- grepl(pattern = make_pattern(in_name()), x = l[[1]])
        l[id, ]
      }
    })

    # getting format activation
    names_format <- shiny::reactive({input$format})

    # getting selected version
    names_version <- shiny::reactive({input$version})

    # getting the name typed
    in_name <- shiny::reactive({input$name})

    # getting the found number of par or var
    #names_number <- shiny::reactive({nrow(output$table)})


    # Handle the name typed on table selection
    shiny::observeEvent(input$name, {
      output$table <- shiny::renderTable({
        names_table()
      }, caption = "Results",
      caption.placement = getOption("xtable.caption.placement", "top"),
      caption.width = getOption("xtable.caption.width", NULL)
      )
    })

    # Handle the type selection in dropdown list on loaded table
    shiny::observeEvent(input$type, {
      output$table <- shiny::renderTable({
        names_table()
      }, caption = "Results",
      caption.placement = getOption("xtable.caption.placement", "top"),
      caption.width = getOption("xtable.caption.width", NULL)
      )
    })

    # Handle the version selection on table loaded for a given type
    shiny::observeEvent(input$version, {
      output$table <- shiny::renderTable({
        names_table()
      }, caption = "Results",
      caption.placement = getOption("xtable.caption.placement", "top"),
      caption.width = getOption("xtable.caption.width", NULL)
      )
      #output$number <- renderText(nrow(output$table))
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

    # Handle the done button click for quitting the app
    shiny::observeEvent(input$done, {
      shiny::stopApp("Bye")
    })

    # Handle the cancel button click for quitting the app
    shiny::observeEvent(input$cancel, {
      shiny::stopApp()
    })

  }

  shiny::runGadget(ui, server, viewer = shiny::dialogViewer("find_stics_names"))

}

