
#' @importFrom rlang `%||%`
#' @importFrom ggplot2 aes layer
NULL



#' @export
#' @rdname geom_bezier
stat_bezier <- function(mapping = NULL, data = NULL,
                        geom = "bezier", position = "identity",
                        ...,
                        show_handles = FALSE,
                        show_curve = TRUE,
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatBezier,
    geom = GeomBezier,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = rlang::list2(
      show_handles = show_handles,
      show_curve = show_curve,
      na.rm = na.rm,
      ...
    )
  )
}

#' Bezier curves
#'
#' A curve parameterized by control points and the tangent at those points.
#'
#' @eval ggplot2:::rd_aesthetics("geom", "bezier")
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_path
#' @param show_handles Flag indicating if the handles at the control
#'   points are drawn. Default: `FALSE`.
#' @param show_curve Flag indicating if the curve is drawn. Default: `TRUE`.
#'
#' @examples
#'  library(ggplot2)
#'  wave_data <- data.frame(
#'    x = seq(1, 12, by = 2),
#'    y = rep(c(0.5, -0.5), times = 3)
#'  )
#'
#'  ggplot(wave_data, aes(x = x, y = y)) +
#'    geom_bezier()
#'
#'  ggplot(wave_data, aes(x = x, y = y)) +
#'    geom_bezier(angle = 20, color = "red", show_handles = TRUE)
#'
#' @export
geom_bezier <- function(mapping = NULL, data = NULL,
                        stat = "bezier", position = "identity",
                        ...,
                        lineend = "butt",
                        linejoin = "round",
                        linemitre = 10,
                        arrow = NULL,
                        arrow.fill = NULL,
                        show_handles = FALSE,
                        show_curve = TRUE,
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomBezier,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = rlang::list2(
      show_handles = show_handles,
      show_curve = show_curve,
      lineend = lineend,
      linejoin = linejoin,
      linemitre = linemitre,
      arrow = arrow,
      arrow.fill = arrow.fill,
      na.rm = na.rm,
      ...
    )
  )
}

#' @export
#' @rdname geom_bezier
StatBezier <- ggplot2::ggproto(
  "StatBezier",
  ggplot2::Stat,
  required_aes = c("x", "y"),
  optional_aes = c("x_handle", "y_handle", "handle_length", "angle"),
  non_missing_aes = c("x_handle1", "x_handle2", "y_handle1", "y_handle2"),
  setup_params = function(self, data, params){
    params
  },
  compute_group = function(data, scales, handle_length = rel(0.3), angle = NULL){
    # Either no handle specification in which case I do something based on handle length
    # Or [xy]_handle are specified in which case I flip them to both sides
    # Or [xy]_handle[12] are specified
    handle_length <- data$handle_length %||% handle_length
    abs_handle_length <- if(inherits(handle_length, "rel")){
      handle_length * with(data, sqrt((lead(x) - lag(x))^2 + (lead(y) - lag(y))^2))
    }else{
      handle_length
    }
    data$angle <- data$angle %||% angle
    if(! any(c("x_handle", "x_handle1", "x_handle2", "y_handle", "y_handle1", "y_handle2", "angle") %in% colnames(data))){
      data <- transform(data, angle = atan2(lead(y) - lag(y), lead(x) - lag(x)))
    }

    if("x_handle" %in% colnames(data) && "angle" %in% colnames(data)){
      stop("You can only specify 'x_handle' or 'angle'")
    }else if("x_handle" %in% colnames(data) && any(c("x_handle1", "x_handle2") %in% colnames(data))){
      stop("You can only specify 'x_handle' or 'x_handle[12]'")
    }else if("x_handle" %in% colnames(data)){
      data <- transform(data,
                        x_handle1 = x - (x_handle - x),
                        x_handle2 = x + (x_handle - x))
    }else if("angle" %in% colnames(data)){
      data <- transform(data,
                        x_handle1 = x - cos(angle/180*pi) * abs_handle_length,
                        x_handle2 = x + cos(angle/180*pi) * abs_handle_length)
    }else if(all(c("x_handle1", "x_handle2") %in% colnames(data))){
      # Do nothing
    }else  if(sum(c("x_handle1", "x_handle2") %in% colnames(data)) == 1){
      stop("Please specify both 'x_handle1' and 'x_handle2'. Alternatively, use 'x_handle' or 'angle'")
    }

    # if(! any(c() %in% colnames(data))){
    #   data <- transform(data,
    #                     y_handle1 = y - ( * abs_handle_length) %|% 0,
    #                     y_handle2 = y + ((lead(y) - lag(y)) / 2 * abs_handle_length) %|% 0)
    # }else
    if("y_handle" %in% colnames(data) && "angle" %in% colnames(data)){
      stop("You can only specify 'xyhandle' or 'angle'")
    }else if("y_handle" %in% colnames(data) && any(c("y_handle1", "y_handle2") %in% colnames(data))){
      stop("You can only specify y_handle or 'y_handle[12]'")
    }else if("y_handle" %in% colnames(data)){
      data <- transform(data,
                        y_handle1 = y - (y_handle - y),
                        y_handle2 = y + (y_handle - y))
    }else if("angle" %in% colnames(data)){
      data <- transform(data,
                        y_handle1 = y - sin(angle/180*pi) * abs_handle_length,
                        y_handle2 = y + sin(angle/180*pi) * abs_handle_length)
    }else if(all(c("y_handle1", "y_handle2") %in% colnames(data))){
      # Do nothing
    }else  if(sum(c("y_handle1", "y_handle2") %in% colnames(data)) == 1){
      stop("Please specify both 'y_handle1' and 'y_handle2'. Alternatively, use 'y_handle'")
    }
    data
  }
)


#' @export
#' @rdname geom_bezier
GeomBezier <- ggplot2::ggproto(
  "GeomBezier",
  ggplot2::Geom,
  default_aes = aes(colour = "black", fill = NA, size = 0.5, shape = 19, linewidth = 0.5, linetype = 1,
                    alpha = NA, stroke = 1, n = 100),

  required_aes = c("x", "y"),
  non_missing_aes = c("x_handle1", "x_handle2", "y_handle1", "y_handle2", "n"),
  optional_aes = c("x_handle", "y_handle", "handle_length", "angle"),

  extra_params = c("na.rm", "show_handles"),

  setup_params = function(data, params){
    params$draw_handle <- params$show_curve %||% TRUE
    params$draw_handle <- params$draw_handle %||% TRUE
    params
  },

  setup_data = function(data, params) {
    data
  },

  setup_handles_data = function(data, params){
    handle_df <- data.frame(x = c(data$x, data$x),
                            y = c(data$y, data$y),
                            xend = c(data$x_handle1, data$x_handle2),
                            yend = c(data$y_handle1, data$y_handle2)
    )
    data_subset <- data[,-which(names(data) %in% c("x", "y", "x_handle1", "x_handle2", "y_handle1", "y_handle2"))]
    ggplot2::GeomSegment$setup_data(cbind(handle_df, rbind(data_subset,data_subset)))
  },

  setup_node_data = function(data, params){
    ggplot2::GeomPoint$setup_data(data)
  },

  setup_curve_data = function(data, params){
    drop_cols <- which(names(data) %in% c("x", "y"))
    df <- do.call(vctrs::vec_rbind, lapply(seq_len(nrow(data)-1), \(idx){
      t <- seq(0, 1, length.out = data$n[idx])
      bezier_coefs <- rbind((1-t)^3, 3 * t * (1 - t)^2, 3 * t^2 * (1-t), t^3)
      points <- with(data, rbind(x = c(x[idx], x_handle2[idx], x_handle1[idx+1], x[idx+1]),
                                 y = c(y[idx], y_handle2[idx], y_handle1[idx+1], y[idx+1])))
      subset <- data[idx,-drop_cols,drop=FALSE]
      vctrs::vec_cbind(as.data.frame(t(points %*% bezier_coefs)), subset)
    }))
    ggplot2::GeomPath$setup_data(df)
  },

  draw_group = function(self, data, panel_params, coord, show_handles = FALSE, show_curve = TRUE,
                        arrow = NULL, arrow.fill = NULL, lineend = "butt", linejoin = "round", linemitre = 10,
                        na.rm = FALSE) {
    curves <- if(show_curve){
      curve_df <- self$setup_curve_data(data, params)
      if(packageVersion("ggplot2") <= '3.5.1'){
        curve_grob <- ggplot2::GeomPath$draw_panel(curve_df, panel_params, coord,
                                                   arrow = arrow, lineend = lineend,
                                                   linejoin = linejoin, linemitre = linemitre, na.rm = na.rm)
      }else{
        curve_grob <- ggplot2::GeomPath$draw_panel(curve_df, panel_params, coord,
                                                   arrow = arrow, arrow.fill = arrow.fill, lineend = lineend,
                                                   linejoin = linejoin, linemitre = linemitre, na.rm = na.rm)
      }

    }

    handles <- if(show_handles){
      handle_df <- self$setup_handles_data(data, params)
      handle_grob <- ggplot2::GeomSegment$draw_panel(handle_df, panel_params, coord)

      point_df <- self$setup_node_data(data, params)
      node_grob <- ggplot2::GeomPoint$draw_panel(point_df, panel_params, coord, na.rm = na.rm)

      ggplot2:::ggname("geom_bezier_handle",
       grid::gTree(children = grid::gList(
         handle_grob,
         node_grob
       ))
      )
    }

    ggplot2:::ggname("geom_bezier",
      grid::gTree(children = grid::gList(
        handles, curves
      )))
  }
)




