#' Given starting coordinates, a direction in degrees,
#' and a magnitude, return the coordinates for the vector's end.
#'
#' @title Step
#'
#' @param x starting x
#' @param y starting y
#' @param d direction in degrees
#' @param m magnitude of step
#'
#' @author Daniel R. Kick (\email{drk8b9@@mail.missouri.edu})
#'
#' @export
Step <- function(x, y, d, m){
  return(list(
    x = (cospi(d/180)*m)+x,
    y = (sinpi(d/180)*m)+y)
  )}

#' This function opperates withing `Grow`. It takes an active stem, finds
#' where the new branches will split off (with `r`), then returns two new
#' stems `d` degrees to either side of the parent stem  with length `m`.
#'
#' @title Water
#'
#' @param Stem list object produced within `Grow` from Guidelines. Contains the previous segment which is being branched.
#' @param d direction in degrees
#' @param m magnitude of step
#' @param r regression from end
#'
#' @author Daniel R. Kick (\email{drk8b9@@mail.missouri.edu})
#'
#' @export
Water <- function(Stem = Stem,
                  d = temp[i, "d"],
                  m = temp[i, "m"],
                  r = temp[i, "r"]){
  # branch along stem
  Meristem <- Step(
    Stem$xend,
    Stem$yend,
    Stem$d - 180,
    Stem$m*r
  )
  ds <- Stem$d + c(d, -1*d)
  # Find the new leaves
  Leaves <- Step(
    Meristem$x,
    Meristem$y,
    ds,
    m
  )
  return(
    list(
      Stem1 = list(x = Meristem$x,
                   y = Meristem$y,
                   xend = Leaves$x[1],
                   yend = Leaves$y[1],
                   d = ds[1],
                   m = m),
      Stem2 = list(x = Meristem$x,
                   y = Meristem$y,
                   xend = Leaves$x[2],
                   yend = Leaves$y[2],
                   d = ds[2],
                   m = m)
    )
  )
}

#' Given a starting value grow out the branches. Iterations are controlled by the length of the inputs.
#' The input parameters are put into a dataframe so all parameters should be of equal length or length of 1.
#'
#' @title Grow
#'
#' @param startx X to begin branching from. Defaults to 0.
#' @param starty Y to begin branching from. Defaults to 0.
#' @param d Degree change for each branch. Should be one for each iteration.
#' @param m Magnitude for each branch. Should be one for each iteration.
#' @param r Regression from tip of a stem to the branch point. Should be between 0 and 1. for each branch. Should be one for each iteration.
#'
#' @author Daniel R. Kick (\email{drk8b9@@mail.missouri.edu})
#'
#' @export
Grow <- function(
    startx = 0, # optional, will default to 0
    starty = 10, # optional, will default to 0
    d = seq(0, 90, length.out = 9), # Direction Changes
    m = seq(10, 1, length.out = 9), # Magnitudes, length of branch
    r = seq(0, 1, length.out = 9) # Regressions in branch point
){
  Guidelines <- data.frame(
    startx,
    starty,
    d,
    m,
    r)

  #  # for debugging
  # Guidelines = data.frame(
  #   startx = 0, # optional, will default to 0
  #   starty = 10, # optional, will default to 0
  #   d = seq(0, 90, length.out = iter), # Direction Changes
  #   m = seq(10, 1, length.out = iter), # Magnitudes, length of branch
  #   r = seq(0, 1, length.out = iter) # Regressions in branch point
  # )


  ## Set up Seedling ====
  if("startx" %in% names(Guidelines)){
    startx <- Guidelines$startx[1]
  } else {
    startx <- 0
  }
  if("starty" %in% names(Guidelines)){
    starty <- Guidelines$starty[1]
  } else {
    starty <- 0
  }

  Seedling <- list(
    list(
      list(x = 0, y = 0,
           xend = 0,
           yend = 0,
           d = (atan2(y = starty, x = startx)/pi)*180, # direction
           m = sqrt(startx^2 + starty^2))
    )
  )

  ## Grow seedling ====

  for(i in 1:nrow(Guidelines)){
    NewGrowth <- list()
    for(j in seq_along(Seedling[[i]])){
      print(c(i, j))
      NewGrowth <- append(NewGrowth,
                          Water(Stem = Seedling[[i]][[j]],
                                d = Guidelines[i, "d"],
                                m = Guidelines[i, "m"],
                                r = Guidelines[i, "r"])
      )
    }
    Seedling[[(1+length(Seedling))]] <- NewGrowth
  }

  walk(seq_along(Seedling), function(i){
    walk(seq_along(Seedling[[i]]), function(j){
      Seedling[[i]][[j]] <<- unlist(Seedling[[i]][[j]])
    })
  })

  Plant <- do.call(
    rbind,
    map(seq_along(Seedling), function(i){
      do.call(rbind, Seedling[[i]])
    })) %>%
    as.data.frame(row.names = F)

  return(Plant)
}
