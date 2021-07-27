
#' Setting book_style to a ggplot object
#'
#' @param gg_object
#'
#' @return a ggplot object
#' @export
#'
# @examples
set_style <- function(gg_object, type = "book", ...) {
  gg_object + book_style(book, ...)
}

#' Getting Stics book ggplot theme
#'
#' @param type theme adapted to docupent type (default : "book")
#' @param ...For overloading ggplot theme elements as for theme
#'
#' @return a ggplot theme
#' @export
#'
# @examples
book_style <- function(type="book",...) {

  font ="Times New Roman"
  font_size <- "11"
  font_color <- "#222222"
  theme_colors <- c("#FFDB6D", "#C4961A", "#F4EDCA", "#D16103", "#C3D7A4",
                      "#52854C", "#4E84C4", "#293352")


  # Default theme options
  theme_list <- ggplot2::theme(

    # Plot background
    # This sets the panel background as blank, removing the standard grey
    # ggplot background colour from the plot
    panel.background = ggplot2::element_blank(),

    #Strip background
    # This sets the panel background for facet-wrapped plots to white,
    # removing the standard grey ggplot background colour and sets the title
    # size of the facet-wrap title to font size 22)
    strip.background = ggplot2::element_rect(fill="white"),
    strip.text = ggplot2::element_text(size  = 22,  hjust = 0),

    # Grid lines
    #This removes all minor gridlines and adds major y gridlines.
    # In many cases you will want to change this to remove y gridlines
    # and add x gridlines. The cookbook shows you examples for doing so
    panel.grid.minor = ggplot2::element_blank(),
    #panel.grid.major.y = ggplot2::element_line(color="#cbcbcb"),
    #panel.grid.major.x = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_blank(),



    # Text format
    #This sets the font, size, type and colour of text
    plot.title = ggplot2::element_text(family=font,
                                       size=28,
                                       face="bold",
                                       color=font_color),

    # Legend format
    # This sets the position and alignment of the legend, removes a title and
    # backround for it and sets the requirements for any text within the legend.
    # The legend may often need some more manual tweaking when it comes to its
    # exact position based on the plot coordinates.
    legend.position = "right",
    legend.text.align = 0,
    legend.background = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(family=font,
                                        size=font_size,
                                        color=font_color),

    # Axis format
    # This sets the text font, size and colour for the axis test, as well as
    # setting the margins and removes lines and ticks. In some cases,
    # axis lines and axis ticks are things we would want to have in the chart
    # - the cookbook shows examples of how to do so.
    axis.title = ggplot2::element_blank(),
    axis.text = ggplot2::element_text(family=font,
                                      size=font_size,
                                      color=font_color),
    axis.text.x = ggplot2::element_text(margin=ggplot2::margin(5, b = 10)),
    axis.ticks = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),




  )

  # default colors
  theme_list <- list(theme_list,
                     scale_color_manual(values = theme_colors),
                     scale_fill_manual(values = theme_colors))

  # Manage modifiers given as ... arguments
  args <- list(...)
  if (length(args)==0) return(theme_list)

  if (detect_arg(args, "custom_cols")) {
    theme_colors <- args$custom_cols
  }
  theme_list <- list(theme_list,
                     scale_color_manual(values = theme_colors ),
                     scale_fill_manual(values = theme_colors))


  # remove legend
  if (detect_arg(args, "legend") && !args$legend) {
    theme_list <- list(theme_list, theme(legend.position = "none"))
  }


  # TODO
  # Treat all remaining args as theme args (see for title already in theme args)


  return(theme_list)
}


detect_arg <- function(args, name){
  if (!is.list(args) || is.null(names(args))) return(FALSE)
  if(name %in% names(args)) return(TRUE)
  FALSE
}
