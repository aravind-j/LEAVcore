### This file is part of 'LEAVcore' package for R.

### Copyright (C) 2024-2026, ICAR-NBPGR.
#
# LEAVcore is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# LEAVcore is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  https://www.r-project.org/Licenses/

#' Round Numeric Values While Preserving a Target Sum
#'
#' Applies the Hamilton (largest remainder or Hare-Niemeyer or Vinton) rounding
#' method \insertCite{balinski_fair_2001}{LEAVcore} to a numeric vector so
#' that the rounded values sum to a specified target.
#'
#' Values are first rounded down using \code{floor()}, and the remaining deficit
#' is allocated to elements with the largest fractional parts.
#'
#' @param x A numeric vector to round.
#' @param target A numeric scalar giving the desired sum of the rounded values.
#'   Defaults to \code{round(sum(x))}.
#'
#' @return An numeric vector of the same length as \code{x}, where the elements
#'   sum to \code{target}.
#'
#' @export
#'
#' @references
#'
#' \insertAllCited{}
#'
#' @examples
#' round_preserve_sum(c(1.2, 2.7, 3.5))
#'
#' round_preserve_sum(c(10.4, 10.4, 10.2), target = 32)
#'
round_preserve_sum <- function(x, target = round(sum(x))) {
  y <- floor(x)
  deficit <- target - sum(y)

  if(deficit > 0) {
    idx <- order(x - y, decreasing = TRUE)[1:deficit]
    y[idx] <- y[idx] + 1
  }

  y
}



# # iterative adjustment rounding algorithm
# round_to_target <- function(x, target = round(sum(x))) {
#
#   while(sum(round(x)) - target > 0) {
#     i <- which.min(ifelse(x %% 1 < 0.5, 1, x %% 1))
#     x[i] <- x[i] - 1
#   }
#   while(sum(round(x)) - target < 0) {
#     i <- which.max(ifelse(x %% 1 > 0.5, 0, x %% 1))
#     x[i] <- x[i] + 1
#   }
#   round(x)
# }
