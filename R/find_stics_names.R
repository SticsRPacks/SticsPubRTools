#' Shiny gadget for finding Stics names
#' @description Mini user interface for searching Stics
#' parameters or variable names and formatting them for
#' inserting in Rmarkdown documents.
#'
#' @param viewer viewer name to choose between "pane" (default) or "dialog"
#'
#' @details Possible selection of names type (parameter or variable),
#' Stics versions, formatting type.
#' And the names search is based on partial matching using case sensitive or not,
#' name starting matching or not.
#'
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' find_stics_names()
#'
#' find_stics_names( viewer = "dialog" )
#'
#' }

find_stics_names <- function(viewer = "pane") {
  library(SticsRFiles)

  viewers <- c("pane", "dialog")

  # Masquing warnings display during app execution
  # and restoring all warnings on app exiting
  options(warn=-1)
  on.exit(options(warn=0))

  css <-
    "
  #name {
    color: blue;
    background: #f2f2f2;
  }
  #table {
    background: #f2f2f2;
  }
  #number {
    color: blue;
  }
  #insert {
    background-color:#337ab7;
    color:#e8f0f7;
  }
  #indication {
    color:red;
  }
  #cancel {
    background-color:#A9A9A9;
  }
  "
  stics_versions <- SticsRFiles::get_stics_versions_compat()$versions_list
  last_version <- SticsRFiles::get_stics_versions_compat()$last_version

  # Taking into account an existing word selection in the IDE
  prev_text_sel <- get_doc_word_selection()
  in_text <- ""
  if (!base::is.null(prev_text_sel)) in_text <- prev_text_sel$text

  ui <- miniUI::miniPage(title = "Stics names",
                         shiny::tags$style(css),
                         miniUI::miniTitleBar(title = "Search a Stics parameter or variable"
                         ),
                         miniUI::miniContentPanel(

                           shiny::fillRow(height = "30%",
                                          shiny::fillCol(width = "70%",
                                                         shiny::selectInput(inputId = "what", label = shiny::strong("What to insert"),
                                                                            choices = c("name", "unit", "both (unit, name)"),selected = "name")
                                          ),
                                          shiny::fillCol(width = "70%",
                                                         shiny::selectInput(inputId = "type", label = shiny::strong("Type"),
                                                                            choices = c("variable", "parameter", "internal variable"),selected = "parameter")
                                          ),
                                          shiny::fillCol(width = "70%",
                                                         shiny::selectInput(inputId = "version", label = shiny::strong("Stics version"),
                                                                            choices = stics_versions,
                                                                            selected = last_version))
                           ),

                           shiny::fillRow(height = "30%",
                                          shiny::fillCol(
                                            shiny::textInput(inputId = "name", label = shiny::strong("Searched name") ,
                                                             width = "80%" , placeholder = "Enter a name or a part of",
                                                             value = in_text)
                                          )
                           ),

                           shiny::fillRow(height = "20%",
                                          shiny::fillCol(
                                            shiny::checkboxInput(inputId = "starting_with", label = "Name starting with ?", value = FALSE)
                                          ),
                                          shiny::fillCol(
                                            shiny::checkboxInput(inputId = "case_sensitive", label = "Case sentitive", value = FALSE)
                                          ),
                                          shiny::fillCol(
                                            shiny::checkboxInput(inputId = "link", label = "Rmarkdown name formating for inline use (uncheck for equation)", value = TRUE)
                                          )
                           ),
                           shiny::fillRow(height = "10%",
                                          shiny::fillCol(
                                            shiny::textOutput(outputId = "number")
                                          )
                           ),
                           shiny::fillRow(height = "10%",
                                          shiny::fillCol(
                                            shiny::textOutput(outputId = "indication")
                                          )
                           )

                         ),

                         miniUI::miniButtonBlock(
                           shiny::actionButton("cancel", "Cancel"),
                           shiny::actionButton("insert", "Insert name", shiny::icon("cog", lib = "glyphicon"))
                         ),

                         miniUI::miniContentPanel(
                           DT::dataTableOutput('table')
                         )

  )


  server <- function(input, output, session) {

    # Getting what to be inserted: name, unit, both
    what_type <- shiny::reactive({substr(input$what, start = 1, stop = 4)})

    # Getting type: var or par
    names_type <- shiny::reactive({substr(input$type, start = 1, stop = 3)})

    # Getting search kind: name starting with string or not
    is_starting_with <- shiny::reactive({

      if (input$starting_with) {
        "start"
      } else {
        NULL
      }

    })


    # Getting case sensitive choice
    case_sensitive <- shiny::reactive({input$case_sensitive})

    # Getting table and filtering it
    names_table <- shiny::reactive({

      l <- get_names_list(type = names_type(), stics_version = names_version())

      # An empty data.frame
      if (!nrow(l)) return(l)

      if (length(in_name())) {

        if (case_sensitive()) {
          pattern <- make_pattern(in_name(), where = is_starting_with())
          names_list <- l[[1]]
        } else {
          pattern <- make_pattern(tolower(in_name()), where = is_starting_with())
          names_list <- tolower(l[[1]])
        }

        id <- grepl(pattern = pattern, x = names_list)
        return(l[id,])
      }

      # For displaying only the first three columns
      if (nrow(l)) l <- l[,1:3]

      return(l)
    })


    # Getting name link format activation
    names_link <- shiny::reactive({input$link})

    # Getting selected version
    names_version <- shiny::reactive({input$version})

    # Getting what is to be inserted
    in_what <- shiny::reactive({input$what})

    # Getting the input string
    in_name <- shiny::reactive({input$name})

    # Getting the table rows number
    rows_num <- shiny::reactive({nrow(names_table())})

    # Handle the name typed on table selection
    shiny::observeEvent(input$name, {

      output$table <- DT::renderDataTable(DT::datatable(get_usefull_cols(names_table(), 1:3),
                                                        options = list(searching = FALSE),
                                                        caption = "Click on a line to select or unselect it"),
                                          server = TRUE)

      output$number <- shiny::renderText(paste0(as.character(rows_num()),
                                                " ", input$type,
                                                '(s) found'))
    })

    # Handle the type selection in dropdown list on loaded table
    shiny::observeEvent(input$type, {

      output$table <- DT::renderDataTable(DT::datatable(get_usefull_cols(names_table(), 1:3),
                                                        options = list(searching = FALSE),
                                                        caption = "Click on a line to select or unselect it"),
                                          server = TRUE)

      output$number <- shiny::renderText(paste0(as.character(rows_num()),
                                                " ", input$type,
                                                '(s) found'))

    })

    # Handle the version selection on table loaded for a given type
    shiny::observeEvent(input$version, {

      output$table <- DT::renderDataTable(DT::datatable(get_usefull_cols(names_table(), 1:3),
                                                        options = list(searching = FALSE),
                                                        caption = "Click on a line to select or unselect it"),
                                          server = TRUE)

      output$number <- shiny::renderText(paste0(as.character(rows_num()),
                                                " ", input$type,
                                                '(s) found'))

    })

    # Handle filtering names starting with the
    # string in the input
    shiny::observeEvent(input$starting_with, {

      output$table <- DT::renderDataTable(DT::datatable(get_usefull_cols(names_table(), 1:3),
                                                        options = list(searching = FALSE),
                                                        caption = "Click on a line to select or unselect it"),
                                          server = TRUE)

      output$number <- shiny::renderText(paste0(as.character(rows_num()),
                                                " ", input$type,
                                                '(s) found'))
    })


    # Handle filtering names using case sensitive search or not
    # with string in name
    shiny::observeEvent(input$case_sensitive, {

      output$table <- DT::renderDataTable(DT::datatable(get_usefull_cols(names_table(), 1:3),
                                                        options = list(searching = FALSE),
                                                        caption = "Click on a line to select or unselect it"),
                                          server = TRUE)

      output$number <- shiny::renderText(paste0(as.character(rows_num()),
                                                " ", input$type,
                                                '(s) found'))

    })

    # Handle the Insert button being pressed.
    shiny::observeEvent(input$insert, {

      loc_table <- names_table()

      lines_sel <- input$table_rows_selected

      # No possible insertion cases
      # Checking if one table line has been selected
      if(base::is.null(lines_sel)) {
        output$indication <- shiny::renderText("Click in one line to select it before inserting !")
        return()
      }
      # Checking if more than one table line has been selected
      if(length(lines_sel) > 1) {
        output$indication <- shiny::renderText("Only one line must be selected before inserting !")
        return()
      }

      # Clearing message if any, in indication area
      output$indication <- shiny::renderText("")

      # Inserting parameter name
      if (names_type() == "par" && what_type() %in% c("name","both") ) {

        insert_stics_name(name = loc_table$name[lines_sel],
                          kind = loc_table$kind[lines_sel],
                          # format = names_format(),
                          format =TRUE,
                          link = names_link(),
                          type = "par"
        )
      }

      # Inserting output variable name
      if (names_type() == "var" && what_type() %in% c("name","both")) {

        insert_stics_name(name = loc_table$name[lines_sel],
                          # format = names_format(),
                          format =TRUE,
                          link = names_link(),
                          type = "var"
        )
      }

      # Inserting internal variable name
      if (names_type() == "int" && what_type() %in% c("name","both")) {

        insert_stics_name(name = loc_table$name[lines_sel],
                          # format = names_format(),
                          format =TRUE,
                          link = names_link(),
                          type = "int"
        )
      }

      # Inserting unit cases
      if (what_type() == "unit" && names_link() ) {
        insert_stics_unit(loc_table$unit[lines_sel], braces = FALSE)
      }
      if (what_type() == "both" && names_link()) {
        insert_stics_unit(loc_table$unit[lines_sel])
      }
    })

    # Handle the cancel button click for quitting the app
    shiny::observeEvent(input$cancel, {
      shiny::stopApp()
    })

  }

  if ( viewer == "dialog") {
    shiny::runGadget(ui, server, viewer = shiny::dialogViewer(dialogName = "Find and insert Stics names"))#,width = 700, height = 2000))
  }

  if ( viewer == "pane" ) {
    # For keeping gadget and so multiple insertions capabilities
    shiny::runGadget(ui, server, viewer = shiny::paneViewer(minHeight = "maximize"))
  }

}

