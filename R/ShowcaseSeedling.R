#' Given a dataframe with the minimum mapping for `geom_segment`
#' (x, y, xend, yend) and a degree of rotation, rotate all points
#' by the given degree around the origin.
#'
#' @title Rotate
#'
#' @param SegmentDF A daframe with columns x, y, xend, and yend
#' @param d The angle to rotate points in degrees
#'
#' @author Daniel R. Kick (\email{drk8b9@@mail.missouri.edu})
#'
#' @export
Rotate <- function(SegmentDF,
                   d = 180){
  temp <- data.frame(
    x = (SegmentDF$x*cospi(d/180)) - (SegmentDF$y*sinpi(d/180)),
    y = (SegmentDF$x*sinpi(d/180)) + (SegmentDF$y*cospi(d/180)),
    xend = (SegmentDF$xend*cospi(d/180)) - (SegmentDF$yend*sinpi(d/180)),
    yend = (SegmentDF$xend*sinpi(d/180)) + (SegmentDF$yend*cospi(d/180)))
  temp <- cbind(
    temp,
    SegmentDF[, !(names(SegmentDF) %in% c("x", "y", "xend", "yend"))])
  return(temp)
}


#' Display a snowflake from the fully grown plant produced by `Grow`.
#'
#' @title Display
#'
#' @param tree slope of the line
#' @param reflections the number of times the tree is to be reflected around the origin
#' @param linecolor the desired color of the tree
#'
#' @author Daniel R. Kick (\email{drk8b9@@mail.missouri.edu})
#'
#' @export
Display <- function(
  tree,
  reflections = 5,
  linecolor = "#6BAED6"
){
  plt <-
    ggplot()+
    theme_void()+
    theme(legend.position = "")

  for (i in seq(0, 360, by = 360/reflections)[-1]){
    plt <- plt + geom_segment(data = Rotate(tree, i),
                              aes(x = x, y = y, xend = xend, yend = yend), color = linecolor)

  }
  return(plt)
}
