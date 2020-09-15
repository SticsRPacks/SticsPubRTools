#' Shiny gadget for finding Stics names
#' @description Mini user interface for searching Stics
#' parameters or variable names and formatting them for
#' inserting in Rmarkdown documents.
#'
#' @details Possible selection of different Stics versions, formatting type,
#' and search based on partial names.
#'
#'
#' @export
#'
# @examples

find_stics_names <- function() {
  library(SticsRFiles)

  # Masquing warnings display during app execution
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
                                                         shiny::selectInput(inputId = "type", label = shiny::strong("Type of name"),
                                                                            choices = c("variable", "parameter"),selected = "parameter")
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
                                          # shiny::fillCol(
                                          #   shiny::checkboxInput(inputId = "format", label = "Format name for RMarkdown", value = TRUE)
                                          # ),
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
                         # miniUI::miniContentPanel(
                         #   # miniUI::gadgetTitleBar("Results"),
                         #   shiny::tableOutput(outputId = "show")
                         # ),

                         miniUI::miniContentPanel(
                           # miniUI::gadgetTitleBar("Results"),
                           #shiny::tableOutput(outputId = "table")
                           DT::dataTableOutput('table')
                         )

  )


  server <- function(input, output, session) {

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


    # Case sensitive
    case_sensitive <- shiny::reactive({input$case_sensitive})

    # Getting table an filtering it
    #
    names_table <- shiny::reactive({

      # commented bcause reverting get_names_list not to use stics environment
      # for managing stics version data.frames
      #l <- get_names_list(type = names_type(), stics_version = names_version())[[names_version()]]
      l <- get_names_list(type = names_type(), stics_version = names_version())

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
      return(l)
    })


    # getting name link format activation
    names_link <- shiny::reactive({input$link})

    # getting selected version
    names_version <- shiny::reactive({input$version})

    # getting the name typed
    in_name <- shiny::reactive({input$name})

    # getting the name typed
    rows_num <- shiny::reactive({nrow(names_table())})

    # getting the found number of par or var
    #names_number <- shiny::reactive({nrow(output$table)})


    # Handle the name typed on table selection
    shiny::observeEvent(input$name, {
      # output$table <- shiny::renderTable({
      #   names_table()
      # }, caption = "Results",
      # caption.placement = getOption("xtable.caption.placement", "top"),
      # caption.width = getOption("xtable.caption.width", NULL)
      # )

      dt_names <- names_table()[,1:3]

      output$table <- DT::renderDataTable(DT::datatable(dt_names,
                                                        options = list(searching = FALSE),
                                                        caption = "Click on a line to select or unselect it"),
                                          server = TRUE)

      output$number <- shiny::renderText(paste0(as.character(rows_num()),
                                                " ", input$type,
                                                '(s) found'))
    })

    # Handle the type selection in dropdown list on loaded table
    shiny::observeEvent(input$type, {
      # output$table <- shiny::renderTable({
      #   names_table()
      # }, caption = "Results",
      # caption.placement = getOption("xtable.caption.placement", "top"),
      # caption.width = getOption("xtable.caption.width", NULL)
      # )

      dt_names <- names_table()[,1:3]

      output$table <- DT::renderDataTable(DT::datatable(dt_names,
                                                        options = list(searching = FALSE),
                                                        caption = "Click on a line to select or unselect it"),
                                          server = TRUE)

      output$number <- shiny::renderText(paste0(as.character(rows_num()),
                                                " ", input$type,
                                                '(s) found'))

    })

    # Handle the version selection on table loaded for a given type
    shiny::observeEvent(input$version, {

      # output$table <- shiny::renderTable({
      #   names_table()
      # }, caption = "Results",
      # caption.placement = getOption("xtable.caption.placement", "top"),
      # caption.width = getOption("xtable.caption.width", NULL)
      # )

      dt_names <- names_table()[,1:3]

      output$table <- DT::renderDataTable(DT::datatable(dt_names,
                                                        options = list(searching = FALSE),
                                                        caption = "Click on a line to select or unselect it"),
                                          server = TRUE)

      output$number <- shiny::renderText(paste0(as.character(rows_num()),
                                                " ", input$type,
                                                '(s) found'))

    })

    # Handle filtering names starting with typed
    # string in name
    shiny::observeEvent(input$starting_with, {
      # output$table <- shiny::renderTable({
      #   names_table()
      # }, caption = "Results",
      # caption.placement = getOption("xtable.caption.placement", "top"),
      # caption.width = getOption("xtable.caption.width", NULL)
      # )

      dt_names <- names_table()[,1:3]

      output$table <- DT::renderDataTable(DT::datatable(dt_names,
                                                        options = list(searching = FALSE),
                                                        caption = "Click on a line to select or unselect it"),
                                          server = TRUE)

      output$number <- shiny::renderText(paste0(as.character(rows_num()),
                                                " ", input$type,
                                                '(s) found'))
    })


    # # Handle filtering names using case sensitive search or not
    # # with string in name
    shiny::observeEvent(input$case_sensitive, {
        # output$table <- shiny::renderTable({
        #   names_table()
        # }, caption = "Results",
        # caption.placement = getOption("xtable.caption.placement", "top"),
        # caption.width = getOption("xtable.caption.width", NULL)
        # )

        dt_names <- names_table()[,1:3]

        output$table <- DT::renderDataTable(DT::datatable(dt_names,
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

      #if (rows_num() > 1 || !rows_num()) return()

      lines_sel <- input$table_rows_selected

      if(base::is.null(lines_sel)) {
        output$indication <- shiny::renderText("Click in one line to select it before inserting !")
        return()
      }

      #print(lines_sel)

      if(length(lines_sel) > 1) {
        output$indication <- shiny::renderText("Only one line must be selected before inserting !")
        return()
      }

      #if (length(lines_sel) > 1 || base::is.null(lines_sel)) return()

      # Clearing message if any !
      output$indication <- shiny::renderText("")

      if (names_type() == "par") {
        #print(loc_table$name[lines_sel])
        #print(loc_table$kind[lines_sel])
        # shiny::stopApp(insert_stics_name(name = loc_table$name[lines_sel],
        #                                  kind = loc_table$kind[lines_sel],
        #                                  # format = names_format(),
        #                                  format =TRUE,
        #                                  link = names_link(),
        #                                  type = "par"
        # ))
        insert_stics_name(name = loc_table$name[lines_sel],
                          kind = loc_table$kind[lines_sel],
                          # format = names_format(),
                          format =TRUE,
                          link = names_link(),
                          type = "par"
        )
      }
      if (names_type() == "var") {
        # print(names_table()$variable[1])
        # shiny::stopApp(insert_stics_name(name = loc_table$name[lines_sel],
        #                                  # format = names_format(),
        #                                  format =TRUE,
        #                                  link = names_link(),
        #                                  type = "var"
        # ))
        insert_stics_name(name = loc_table$name[lines_sel],
                          # format = names_format(),
                          format =TRUE,
                          link = names_link(),
                          type = "var"
        )
      }

    })

    # Handle the cancel button click for quitting the app
    shiny::observeEvent(input$cancel, {
      shiny::stopApp()
    })

  }


  shiny::runGadget(ui, server, viewer = shiny::dialogViewer("Find and insert Stics names"))#,width = 700, height = 2000))

  # For keeping gadget and multiple insertions
  #shiny::runGadget(ui, server, viewer = paneViewer())

}

