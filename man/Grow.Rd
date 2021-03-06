% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GrowSeedling.R
\name{Grow}
\alias{Grow}
\title{Grow}
\usage{
Grow(
  startx = 0,
  starty = 10,
  d = seq(0, 90, length.out = 9),
  m = seq(10, 1, length.out = 9),
  r = seq(0, 1, length.out = 9)
)
}
\arguments{
\item{startx}{X to begin branching from. Defaults to 0.}

\item{starty}{Y to begin branching from. Defaults to 0.}

\item{d}{Degree change for each branch. Should be one for each iteration.}

\item{m}{Magnitude for each branch. Should be one for each iteration.}

\item{r}{Regression from tip of a stem to the branch point. Should be between 0 and 1. for each branch. Should be one for each iteration.}
}
\description{
Given a starting value grow out the branches. Iterations are controlled by the length of the inputs.
The input parameters are put into a dataframe so all parameters should be of equal length or length of 1.
}
\author{
Daniel R. Kick (\email{drk8b9@mail.missouri.edu})
}
