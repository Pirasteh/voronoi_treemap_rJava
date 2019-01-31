# The following is mostly copy-pasted from ggplot's `geom_text` (GPL licence)

library(ggplot2)
library(dplyr)
Rcpp::sourceCpp("scale_rect_to_poly.cpp")

#' @export
#' @rdname geom_text
#' @param label.padding Amount of padding around label. Defaults to 0.25 lines.
#' @param label.r Radius of rounded corners. Defaults to 0.15 lines.
#' @param label.size Size of label border, in mm.
geom_text_in_poly <- function(mapping = NULL, data = NULL,
                              stat = "identity", position = "identity",
                              ...,
                              parse = FALSE,
                              nudge_x = 0,
                              nudge_y = 0,
                              label.padding = unit(0.25, "lines"),
                              label.r = unit(0.15, "lines"),
                              label.size = 0.25,
                              na.rm = FALSE,
                              show.legend = NA,
                              inherit.aes = TRUE) {
  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      stop("Specify either `position` or `nudge_x`/`nudge_y`", call. = FALSE)
    }
    
    position <- position_nudge(nudge_x, nudge_y)
  }
  
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTextInPoly,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      label.padding = label.padding,
      label.r = label.r,
      label.size = label.size,
      na.rm = na.rm,
      ...
    )
  )
}

#' ggname
#' Helper function taken from ggplot2 -- not exported, need to re-create
#' @param prefix prefix
#' @param grob grob
#'
#' @return ggname
#'
ggname <- function(prefix, grob) {
  grob$name <- grid::grobName(grob, prefix)
  grob
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomTextInPoly <- ggproto("GeomTextInPoly", Geom,
                          required_aes = c("x", "y", "xs", "ys", "label"),
                          
                          default_aes = aes(
                            colour = "black", fill = "white", max.size = 20, min.size = 0, angle = 0,
                            hjust = 0.5, vjust = 0.5, alpha = NA, family = "", fontface = 1,
                            lineheight = 1.2, group = "pathString"
                          ),
                          
                          draw_panel = function(self, data, panel_params, coord, parse = FALSE,
                                                na.rm = FALSE,
                                                label.padding = unit(0.25, "lines"),
                                                label.r = unit(0.15, "lines"),
                                                label.size = 0.25) {
                            lab <- data$label
                            if (parse) {
                              lab <- parse(text = as.character(lab))
                            }
                            # this transforms x and y, but we need to transform more variables
                            data <- coord$transform(data, panel_params)
                            tmp <- data.frame(x = data$xs, y = data$ys)
                            tmp <- coord$transform(tmp, panel_params)
                            data$xs <- tmp$x
                            data$ys <- tmp$y
                            
                            if (is.character(data$vjust)) {
                              data$vjust <- compute_just(data$vjust, data$y)
                            }
                            if (is.character(data$hjust)) {
                              data$hjust <- compute_just(data$hjust, data$x)
                            }
                            
                            grobs <- data %>%
                              group_by(group) %>%
                              summarise(x = first(x),
                                        y = first(y),
                                        max.size = first(max.size),
                                        min.size = first(min.size),
                                        colour = first(colour),
                                        family = first(family),
                                        alpha = first(alpha),
                                        fill = first(fill),
                                        fontface = first(fontface),
                                        lineheight = first(lineheight),
                                        vjust = first(vjust),
                                        hjust = first(hjust),
                                        xs = list(xs),
                                        ys = list(ys), 
                                        label = first(label)) %>% 
                              rowwise() %>%
                              do(grob = {
                                labelGrob(.,
                                          just = c(.$hjust, .$vjust),
                                          padding = label.padding,
                                          r = label.r,
                                          text.gp = gpar(
                                            col = .$colour,
                                            fontsize = .$max.size * .pt,
                                            fontfamily = .$family,
                                            fontface = .$fontface,
                                            lineheight = .$lineheight
                                          )
                                )
                              }) %>% .$grob
                            
                            class(grobs) <- "gList"
                            grob <- grobTree(children = grobs)
                            grob$name <- grid::grobName(grob, "geom_text_in_poly")
                            grob
                          },
                          
                          draw_key = draw_key_label
)

labelGrob <- function(data,
                      just = "center", padding = unit(0.25, "lines"), r = unit(0.1, "snpc"),
                      default.units = "npc", name = NULL,
                      text.gp = gpar(), vp = NULL) {
  
  stopifnot(length(data$label) == 1)
  
  if (!is.unit(data$x))
    data$x <- unit(data$x, default.units)
  if (!is.unit(data$y))
    data$y <- unit(data$y, default.units)
  if (!is.unit(data$xs))
    data$xs <- unit(data$xs, default.units)
  if (!is.unit(data$ys))
    data$ys <- unit(data$ys, default.units)
  
  gTree(data = data, just = just, padding = padding, r = r,
        name = name, text.gp = text.gp, vp = vp, cl = "textinpolygrob")
}



fit_grob_in_poly <- function(grob, xpos, ypos, xs, ys, min.size) {
  xs <- grid::convertX(xs, "native", TRUE)
  ys <- grid::convertY(ys, "native", TRUE)
  x <- grid::convertX(xpos, "native", TRUE)
  y <- grid::convertY(ypos, "native", TRUE)
  w <- grid::convertWidth(grid::grobWidth(grob), "native", TRUE)
  h <- grid::convertHeight(grid::grobHeight(grob), "native", TRUE)
  
  scale <- scale_rect_to_poly(x, y, w, h, xs, ys) 
  
  if (scale == 0.0) return(zeroGrob())
  # the following is taken from the ggfittext package
  fs1 <- grob$gp$fontsize
  grob$gp$fontsize <- grob$gp$fontsize * 2
  slopew <- fs1 / (grid::convertWidth(grid::grobWidth(grob), "native", TRUE) - w)
  slopeh <- fs1 / (grid::convertHeight(grid::grobHeight(grob), "native", TRUE) - h)
  
  # Calculate the target font size required to fit text to box along each
  # dimension
  targetfsw <- abs(w * scale) * slopew
  targetfsh <- abs(h * scale) * slopeh
  
  # Set to smaller of target font sizes
  grob$gp$fontsize <- ifelse(targetfsw < targetfsh, targetfsw, targetfsh)
  if (!is.null(min.size) && grob$gp$fontsize < min.size)
    zeroGrob()
  else
    grob
}


#' @export
makeContent.textinpolygrob <- function(x) {
  hj <- resolveHJust(x$just, NULL)
  vj <- resolveVJust(x$just, NULL)
  
  
  data <- x$data
  xpos <- data$x + 2 * (0.5 - hj) * x$padding
  ypos <- data$y + 2 * (0.5 - vj) * x$padding
  t <- textGrob(
    data$label,
    xpos,
    ypos,
    just = c(hj, vj),
    gp = x$text.gp,
    name = "text"
  )

  t <- fit_grob_in_poly(t, xpos, ypos, data$xs, data$ys, data$min.size) 
  
  setChildren(x, gList(t))
}


