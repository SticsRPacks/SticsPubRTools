#' Setting theme_stics to a ggplot object
#'
#' @param gg_object A ggplot object
#' @param type Type of document (for selecting the appropriate style)
#' @param ... For overloading ggplot theme elements as for theme
#'
#' @return a ggplot object
#' @export
#'
# @examples
set_style <- function(gg_object, type = "book", ...) {
  gg_object + theme_stics(type = type, ...)
}

#' Getting Stics book ggplot theme
#'
#' @param type theme adapted to docupent type (default : "book")
#' @param ... For overloading ggplot theme elements as for theme
#'
#' @return a ggplot theme
#' @export
#'
# @examples
theme_stics <- function(type = "book",...) {

  font ="Times New Roman"
  face = "plain"
  font_size <- "11"
  font_color <- "#222222"
  theme_colors <- c("#FFDB6D", "#C4961A", "#F4EDCA", "#D16103", "#C3D7A4",
                    "#52854C", "#4E84C4", "#293352")
  theme_linetypes <- c("twodash", "solid", "longdash", "dotted", "dotdash", "dashed", "blank")

  # Default formatting for text
  default_text <- set_text(family=font,
                           face = face,
                           size=font_size,
                           colour=font_color)
  # title text
  title_text <- set_text(default_text,
                         size=12,
                         face="bold",
                         colour=font_color)

  # Axis
  axis_line <- ggplot2::element_line(
    color = font_color,
    size = 0.3,
    linetype = "solid")

  # Line
  plot_geom_line <- ggplot2::geom_line(
    #color = font_color,
    size = 0.2)#,
  #linetype = "solid")

  # Default theme options
  base_theme <- ggplot2::theme(

    # Plot background
    # This sets the panel background as blank, removing the standard grey
    # ggplot background colour from the plot
    panel.background = ggplot2::element_blank(),

    # Strip background
    # This sets the panel background for facet-wrapped plots
    #strip.background = ggplot2::element_rect(fill="grey90"),
    strip.background = ggplot2::element_rect(fill="lightgrey"),
    # using text modifier function
    strip.text = default_text,

    # Grid lines
    #This removes all minor gridlines and adds major y gridlines.
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_blank(),
    #panel.grid.major.y = ggplot2::element_line(color="#cbcbcb"),
    #panel.grid.major.x = ggplot2::element_blank(),

    # panel
    panel.border = ggplot2::element_blank(),

    # Text format
    plot.title = title_text,
    # Legend format
    legend.position = "right",
    legend.text.align = 0,
    legend.background = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),
    legend.text = default_text,

    # Axis format
    # using text modifier function
    axis.title = default_text,
    axis.title.x = default_text,
    axis.title.y = default_text,
    axis.title.y.left = default_text,
    axis.title.y.right = default_text,
    axis.text = default_text,
    axis.text.x = set_text(default_text, margin=ggplot2::margin(5, b = 10)),
    axis.text.y.left = default_text,
    axis.text.y.right = default_text,
    axis.line = axis_line,
    axis.ticks = axis_line,
    axis.line.x.bottom = axis_line,
    axis.ticks.x.bottom = axis_line,
    axis.line.x.top = ggplot2::element_blank(),
    axis.ticks.x.top = ggplot2::element_blank(),
    axis.line.y.left = axis_line,
    axis.ticks.y.left = axis_line,
    axis.line.y.right = ggplot2::element_blank(),
    axis.ticks.y.right = ggplot2::element_blank()

  )



  # Manage modifiers given as ... arguments
  args_list <- list(...)
  # if (length(args_list)==0) return(base_theme)

  # set legend display: TRUE/FALSE
  # set position using keywords : "top", "bottom", "left", "right"
  # set position using keywords combination (inside plot area): "top-left"
  # "top-right", ...
  # set position usin relatices coordinates c(x,y), between 0 and 1
  if (detect_arg(args_list, "legend")) { # && !args_list$legend) {
    base_theme <- base_theme + legend(what = "position", value = args_list$legend)
  }
  # for removing legend title: FALSE
  if (detect_arg(args_list, "legend.title")) {
    # Add treatment for title !!!!
    base_theme <- base_theme + legend(what = "title", value = args_list$legend.title)
  }

  if (detect_arg(args_list, "legend.face")) {
    # Add treatment for title !!!!
    base_theme <- base_theme + legend(what = "face", value = args_list$legend.face)
  }

  if (detect_arg(args_list, "legend.colour")) {
    # Add treatment for title !!!!
    base_theme <- base_theme + legend(what = "colour", value = args_list$legend.colour)
  }

  if (detect_arg(args_list, "legend.size")) {
    # Add treatment for title !!!!
    base_theme <- base_theme + legend(what = "size", value = args_list$legend.size)
  }

  if (detect_arg(args_list, "legend.angle")) {
    # Add treatment for title !!!!
    base_theme <- base_theme + legend(what = "angle", value = args_list$legend.angle)
  }


  if (detect_arg(args_list, "x.axis")) {
    if (args_list$x.axis == "top")
      base_theme <- base_theme +
        ggplot2::theme(axis.line.x.top = axis_line, axis.ticks.x.top = axis_line)
  }

  if (detect_arg(args_list, "y.axis")) {
    if (args_list$y.axis == "both")
      base_theme <- base_theme +
        ggplot2::theme(axis.line.y.right = axis_line, axis.ticks.y.right = axis_line,
                       axis.line.y.left = axis_line, axis.ticks.y.left = axis_line)
    if (args_list$y.axis == "left")
      base_theme <- base_theme +
        ggplot2::theme(axis.line.y.right = element_blank(), axis.ticks.y.right = element_blank(),
                       axis.line.y.left = axis_line, axis.ticks.y.left = axis_line)
  }


  # Definition of lines (see at the beginning of the theme function)
  # base_theme <- base_theme + plot_geom_line # ??? ne fonctionne pas
  # base_theme <- list(base_theme, plot_geom_line)

  # TODO: voir comme pour les couleurs changer le type et epaisseur traits !
  # mais apparemment ne marche pas en faisant un + à la suite d'un objet ggplot ?
  # scale_size_manual(values=c(1, 1.5))
  # scale_linetype_manual(values=c("twodash", "dotted"))
  # Ne marche pas pour les fonctions water-balance:
  # TODO : solution
  # mettre aes dans le ggplot ... A TESTER !!!!


  # Choosing colors, default theme_colors or custom palette given in custom_cols arg
  if (detect_arg(args_list, "custom_cols")) {
    # theme_colors <- args_list$custom_cols
    base_theme <- list(base_theme,
                       ggplot2::scale_color_manual(values = args_list$custom_cols),
                       ggplot2::scale_fill_manual(values = args_list$custom_cols))
  } else {
    # default colors
    base_theme <- list(base_theme,
                       ggplot2::scale_color_manual(values = theme_colors),
                       ggplot2::scale_fill_manual(values = theme_colors))
  }

  # Choosing line types, default theme_linetypes or custom line types given in custom_lines arg
  if (detect_arg(args_list, "custom_lines")) {
    # theme_colors <- args_list$custom_lines
    base_theme <- list(base_theme,
                       ggplot2::scale_linetype_manual(values = args_list$custom_lines))
  } else {
    # default line types
    base_theme <- list(base_theme,
                       ggplot2::scale_linetype_manual(values = theme_linetypes))
  }

  #base_theme <- list(base_theme, plot_geom_line)


  # TODO
  # Treat all remaining args as theme args (see for title already in theme args)


  return(base_theme)
}




# Theme modifiers

# Legend modifiers
legend <- function(what, value) {
  #
  what_list <- c("position", "title", "face","colour", "size", "angle")

  if (!what %in% what_list) return(what_list)

  # Get the effective value/values list for the settings
  value <- legend_value(what, value)
  #
  if (what == "title") return(ggplot2::theme("legend.title" = value))

  if (what %in% c("face","colour", "size", "angle")) return(ggplot2::theme("legend.text" = value))

  if (what == "position") {

    t <- ggplot2::theme()

    if ("position" %in% names(value)) t <- t +
        ggplot2::theme("legend.position" = value$position)

    if ("justification" %in% names(value)) t <- t +
        ggplot2::theme("legend.justification" = value$justification)

    return(t)

  }


}

legend_value <- function(what, value) {

  # legend position
  if (is.numeric(value) & length(value) == 2 & all(value <= 1 & value >=0)) {
    return(list(position=value))
  }

  # inactivation
  if (is.logical(value)) {
    if (what == "position" && !value) return(list(position="none"))
    if (what == "title" && !value) return(ggplot2::element_blank())
    if (what == "title" && value) return(ggplot2::element_text())
  }
  if (is.character(value) && what == "title" && nchar(value)==0) return(ggplot2::element_blank())

  # Changing text in legend
  if(what=="face") return(ggplot2::element_text(face=value))
  if(what=="colour") return(ggplot2::element_text(colour=value))
  if(what=="size") return(ggplot2::element_text(size=value))
  if(what=="angle" & is.numeric(value)) return(ggplot2::element_text(angle=value))

  # for legend position using base: "right", "left", "top", "bottom"...
  # Combinations:
  # top-left, top-right, bottom-left, bottom-right
  if (is.character(value) && what == "position") {
    return(get_legend_position(value))
  }

  warning("Nothing has been done on legend position/title!")
}


# Getting legend position parameters
get_legend_position <- function(value) {
  base_position <- c("right", "left", "top", "bottom", "none")
  comb_position <- c("top", "bottom")
  comb_justif <- list(left = c(0,1), right=c(1,0))

  if (value %in% base_position) return(list(position=value))

  if(!grepl("-", value)) stop("Error : ",value," is not a correct value for specifying legend position !")

  w <- unlist(strsplit(value, split="-"))
  if (length(w) > 2 ) stop("Error : ",value," is not a correct value for specifying legend position !")

  if (!w[1] %in% comb_position) stop("Error : ",w[1]," is not a correct value for specifying vertical legend position !")

  if (!w[2] %in% names(comb_justif)) stop("Error : ",w[2]," is not a correct value for specifying horizontal legend position !")

  ret <- list(position=w[1], justification=comb_justif[[w[2]]])


}




# Colors modifiers


# axes ?


# annotations in plot


# text element, generator , modifier
set_text <- function(elt_t = NULL,...) {

  # Font
  # face values: "plain", "italic", "bold", "bold.italic"

  if (is.null(elt_t)) {
    elt_t <- ggplot2::element_text()
  }

  if (! "element_text" %in% class(elt_t)) {
    warning("The given elt_t is not of class element_text!")
    return(ggplot2::element_text())
  }

  fields <- c(names(elt_t), "color")
  args <- list(...)
  if (!length(args)) return(fields[1:8])

  # TODO: validate args[[f]] value : how to do this
  idx <- fields %in% names(args)
  if (!length(idx)) return()

  for (f in fields[idx]) {
    elt_t[[f]] <- args[[f]]
  }

  elt_t
}

# panel blank
blank.panel <- function() {
  ggplot2::element_blank()
}



detect_arg <- function(args, name){
  if (!is.list(args) || is.null(names(args))) return(FALSE)
  if(name %in% names(args)) return(TRUE)
  FALSE
}

