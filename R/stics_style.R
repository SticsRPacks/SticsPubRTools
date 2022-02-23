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
#' @param type theme adapted to document type (default : "book")
#' @param ... For overloading ggplot theme elements as for theme
#'
#' @return a ggplot theme
#' @export
#'
# @examples
theme_stics <- function(type = "book", grey_scale = FALSE,...) {

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

  axis_title_x <- ggplot2::element_text(
    color = font_color,
    face = face,
    family = font,
    size = font_size,
    hjust = 1,
    vjust = 0,
    margin = margin(b = 0))

  axis_title_y <- ggplot2::element_text(
    color = font_color,
    face = face,
    family = font,
    size = font_size,
    angle = 0)

  axis_title_y_left <- add_elt_text(axis_title_y,
                                    ggplot2::element_text(margin = margin(l = 0),
                                                          hjust = 0,
                                                          vjust = 1)
  )

  axis_title_y_right <- add_elt_text(axis_title_y,
                                     ggplot2::element_text(margin = margin(r = 0),
                                                           vjust = 1,
                                                           hjust = 1)
  )

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

    # margin around the plot
    plot.margin = margin(1.5,1.5,1.5,1.5, unit = 'cm'),

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
    axis.title.x = axis_title_x,
    axis.title.y = axis_title_y,
    axis.title.y.left = axis_title_y_left,
    axis.title.y.right = axis_title_y_right,
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
  if (length(args_list)==0) return(base_theme)


  # Title management
  if (detect_arg(args_list, "title")) {
    if (args_list$title == "none")
    base_theme <- base_theme + theme(plot.title = element_blank())

    # TODO : see for position ???

  }

  # legend management
  #-----------------------------------------------------------------------------
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
    base_theme <- base_theme + legend(what = "title", value = args_list$legend.title)
  }

  if (detect_arg(args_list, "legend.face")) {
    base_theme <- base_theme + legend(what = "face", value = args_list$legend.face)
  }

  if (detect_arg(args_list, "legend.colour")) {
    base_theme <- base_theme + legend(what = "colour", value = args_list$legend.colour)
  }

  if (detect_arg(args_list, "legend.size")) {
    base_theme <- base_theme + legend(what = "size", value = args_list$legend.size)
  }

  if (detect_arg(args_list, "legend.angle")) {
    base_theme <- base_theme + legend(what = "angle", value = args_list$legend.angle)
  }

  if (detect_arg(args_list, "legend.labels")) {
    base_theme <- base_theme + legend(what = "labels", value = args_list$legend.labels)
  }

  # axis management
  #-----------------------------------------------------------------------------
  # axis display --------------------------------------------------------------
  # x
  if (detect_arg(args_list, "x.axis")) {
    if (args_list$x.axis == "top")
      base_theme <- base_theme +
        ggplot2::theme(axis.line.x.top = axis_line, axis.ticks.x.top = axis_line)
  }

  # y (left and right axis )
  center_x_title <- FALSE
  if (detect_arg(args_list, "y.axis")) {
    if (args_list$y.axis == "both") {
      base_theme <- base_theme +
        ggplot2::theme(axis.line.y.right = axis_line, axis.ticks.y.right = axis_line,
                       axis.line.y.left = axis_line, axis.ticks.y.left = axis_line)
      # centering the x axis title
      center_x_title <- TRUE
      base_theme <- base_theme +
        ggplot2::theme(axis.title.x.top = element_text(hjust = 0.5, vjust = 0),
                       axis.title.x.bottom = element_text(hjust = 0.5, vjust = 0))
    }

    if (args_list$y.axis == "left")
      base_theme <- base_theme +
        ggplot2::theme(axis.line.y.right = element_blank(), axis.ticks.y.right = element_blank(),
                       axis.line.y.left = axis_line, axis.ticks.y.left = axis_line)
    if (args_list$y.axis == "right")
      base_theme <- base_theme +
        ggplot2::theme(axis.line.y.left = element_blank(), axis.ticks.y.left = element_blank(),
                       axis.line.y.right = axis_line, axis.ticks.y.right = axis_line)
    if (args_list$y.axis == "none")
      base_theme <- base_theme +
        ggplot2::theme(axis.line.y.right = element_blank(), axis.ticks.y.right = element_blank(),
                       axis.line.y.left = element_blank(), axis.ticks.y.left = element_blank())
  }

  # axis title display & position ----------------------------------------------
  if (detect_arg(args_list, "axis.title")) {
    if (args_list$axis.title == "none")
      base_theme <- base_theme + ggplot2::theme(axis.title.x = element_blank(),
                                                axis.title.y.left = element_blank(),
                                                axis.title.y.right = element_blank())
  }
  # axis title position -------------------------------------------------------
  if (detect_arg(args_list, "axis.title.y.left")) {

    if (is.numeric(args_list$axis.title.y.left))
      base_theme <- base_theme +
        ggplot2::theme(axis.title.y.left =
                         element_text(hjust = args_list$axis.title.y.left[1],
                                      vjust = args_list$axis.title.y.left[2]))
    if (is.character(args_list$axis.title.y.left) && args_list$axis.title.y.left=="top")
      base_theme <- base_theme +
        ggplot2::theme(axis.title.y.left = element_text(hjust = 0, vjust = 1.2))

    if (is.character(args_list$axis.title.y.left) && args_list$axis.title.y.left=="bottom")
      base_theme <- base_theme +
        ggplot2::theme(axis.title.y.left = element_text(hjust = 0, vjust = 0))

    if (is.character(args_list$axis.title.y.left) && args_list$axis.title.y.left=="none")
      base_theme <- base_theme +
        ggplot2::theme(axis.title.y.left = element_blank())
  }

  if (detect_arg(args_list, "axis.title.x.top") && !center_x_title) {
    # "left", "right", "none", numeric
    if (is.numeric(args_list$axis.title.x.top))
      base_theme <- base_theme +
        ggplot2::theme(axis.title.x.top =
                         element_text(hjust = args_list$axis.title.x.top[1],
                                      vjust = args_list$axis.title.x.top[2]))
    if (is.character(args_list$axis.title.x.top) && args_list$axis.title.x.top=="left")
      base_theme <- base_theme +
        ggplot2::theme(axis.title.x.top = element_text(hjust = 0, vjust = 1))

    if (is.character(args_list$axis.title.x.top) && args_list$axis.title.x.top=="right")
      base_theme <- base_theme +
        ggplot2::theme(axis.title.x.top = element_text(hjust = 1, vjust = 0))

    if (is.character(args_list$axis.title.x.top) && args_list$axis.title.x.top=="none")
      base_theme <- base_theme +
        ggplot2::theme(axis.title.x.top = element_blank())
  }

  if (detect_arg(args_list, "axis.title.x.bottom") && !center_x_title) {

    # TODO: complete
    if (is.character(args_list$axis.title.x.bottom) && args_list$axis.title.x.bottom=="none")
      base_theme <- base_theme +
        ggplot2::theme(axis.title.x.bottom = element_blank())
  }


  # axis scale management ---------------------------------------------
  # args:
  # scale.x.limits, scale.y.limits
  # scale.x.breaks, scale.y.breaks
  # scale.x.expand, scal.y.expand:
  #     scale_x_continuous("test x", limits =c(0,10),
  #                        expand=expansion(mult=c(0.,0.02)))
  # ou utilisation de add=c(val_to_start, val_to_end) ou
  # encore c(val_to_start, val_to_end) sans nom arg correspondant a arg "add"
  # scale.x.labels, scale.y.labels: list of labels
  #
  # ATTENTION: voir argument sec.axis des fonctions scale_x|y_continuous|discrete
  # pour controler les parametres des axes opposés!
  #
  #
  # axis title position ------------------------------------------------------
  # voir fonction legend la généraliser: function axis -> plot_elt, ggelt: legend, axis ...
  # ggelt <- function(elt, what, value) {
  # elt: ggplot elt: axis, legend,...
  # what: what to modify ...
  # value to set to attribute of the elt ...
  #}
  # ---------------------------------------------------------------------------


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


  base_theme <- list(base_theme)

  # Choosing colors, default theme_colors or custom palette given in custom_cols arg
  if (detect_arg(args_list, "custom_cols") ) {
    print("custom_cols detected !")
    base_theme <- c(base_theme,
                    ggplot2::scale_color_manual(values = args_list$custom_cols),
                    ggplot2::scale_fill_manual(values = args_list$custom_cols))
  } else {
    # default colors
    base_theme <- c(base_theme,
                    ggplot2::scale_color_manual(values = theme_colors),
                    ggplot2::scale_fill_manual(values = theme_colors))
  }

  if (grey_scale) {
    if (detect_arg(args_list, "custom_cols") ) {
      # print("custom_cols detected to grey cols !")
      # custom_scale <- unique(args_list$custom_cols)
      # custom_grey_scale <- gray.colors(length(custom_scale))
      # col_ids <- unlist(lapply(args_list$custom_cols, function(x) which(x == custom_scale)))
      # custom_grey_cols <- custom_grey_scale[col_ids]
      # # hack to pass through col number
      # custom_grey_cols <- c(custom_grey_cols, "black", "black", "black")
      #
      # base_theme <- c(base_theme,
      #                 ggplot2::scale_color_grey(custom_grey_cols),
      #                 ggplot2::scale_fill_grey(custom_grey_cols))

      custom_grey_cols <-  grey_cols(custom_cols = args_list$custom_cols)
    } else {
      # # default colors
      # base_theme <- c(base_theme,
      #                 ggplot2::scale_color_manual(gray.colors(length(theme_colors))),
      #                 ggplot2::scale_fill_manual(gray.colors(length(theme_colors))))
      custom_grey_cols <-  grey_cols(n = length(theme_colors))
    }


    base_theme <- c(base_theme,
                    ggplot2::scale_color_manual(custom_grey_cols),
                    ggplot2::scale_fill_manual(custom_grey_cols))
  }

  # Choosing line types, default theme_linetypes or custom line types given in custom_lines arg
  if (detect_arg(args_list, "custom_lines")) {
    print("custom_lines detected !")
    base_theme <- c(base_theme,
                    ggplot2::scale_linetype_manual(values = args_list$custom_lines))
  } else {
    # default line types
    base_theme <- c(base_theme,
                    ggplot2::scale_linetype_manual(values = theme_linetypes))
  }

  # if (detect_arg(args_list, "custom_labs")) {
  #   print("custom_labs detected !")
  #   base_theme <- list(base_theme,
  #                      ggplot2::scale_color_manual(labels = args_list$custom_labs))#,
  #                      #ggplot2::scale_fill_manual(labels = args_list$custom_labs))
  # }





  # voir scale_size_manual


  #base_theme <- list(base_theme, plot_geom_line)


  # TODO
  # Treat all remaining args as theme args (see for title already in theme args)


  return(base_theme)
}




# Theme modifiers
grey_cols  <- function(custom_cols = NULL, n = NULL) {

  # check inputs

  args <- c(custom_cols,n)
  if (is.null(args) | length(args) > 1) stop("only cols or cols number must be given!")

  if (!is.null(custom_cols)) {
    custom_scale <- unique(custom_cols)
    custom_grey_scale <- gray.colors(length(custom_scale))
    col_ids <- unlist(lapply(args_list$custom_cols, function(x) which(x == custom_scale)))
    custom_grey_cols <- custom_grey_scale[col_ids]
    # hack to pass through col number
    custom_grey_cols <- c(custom_grey_cols, "black", "black", "black")
  }

  # n is provided
  custom_grey_cols <- gray.colors(n)

  return(custom_grey_cols)
}


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


    #if (what == "labels") return(ggplot2::theme("legend.title" = value))

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

#' Switching a ggplot object to grey theme
#' @param ggplot_obj a ggplot object
#' @param col_levels number of grey levels
#' @param switch_flag logical indicating to switch (TRUE) not (FALSE) to a grey scale
#'
#' @return a ggplot object
#' @export
#'
# @examples
switch_to_grey <- function(ggplot_obj, col_levels = NULL, switch_flag = FALSE) {

  # Switch flag may be given by switch_flag = knitr::is_latex_output()
  # or conjunction of this one plus black and white other flag

  if (! switch_flag) return(ggplot_obj)

  # try to bet levels number from ggplot_obj data from the first col
  # if col_levels == NULL
  # TODO: test if class == factors
  if (is.null(col_levels)) {
    if (is.factor(ggplot_obj$data[[1]])) {
      col_levels <- length(levels(ggplot_obj$data[[1]]))
    } else {
      col_levels <- length(unique(ggplot_obj$data[[1]]))
    }
  }

  ggplot_obj + scale_colour_grey(gray.colors(col_levels)) # + theme_bw()

}


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


add_elt_text <- function(elt_t_a, elt_t_b) {

  for (i in names(elt_t_a)) {
    if (!is.null(elt_t_b[[i]])) {
      elt_t_a[[i]] <- elt_t_b[[i]]
    }
  }
  elt_t_a
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

