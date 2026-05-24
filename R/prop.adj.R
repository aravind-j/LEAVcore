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

#' Relative Frequency Adjustments
#'
#' Compute and transform relative frequencies for a qualitative trait in a
#' germplasm collection by the following methods
#' \insertCite{balakrishnan_Strategies_2001}{LEAVcore}:
#'  \itemize{ \item Square root-proportion \item
#' Log-frequency} \loadmathjax
#'
#' If \mjseqn{p_{i}} is the relative frequency of the \mjseqn{i}th descriptive
#' state for a qualitative trait in a collection, then the square
#' root-proportion transformed relative \mjseqn{q_{i}} is computed as
#'
#' \mjsdeqn{q_{i} = \frac{\sqrt{p_{i}}}{\sum_{i=1}^{s}\sqrt{p_{i}}}}
#'
#' Where \mjseqn{s} is the number of possible descriptor states for the
#' qualitative trait in the collection.
#'
#' Similarly, the log-frequency transformed relative \mjseqn{q_{i}} is computed
#' as
#'
#' \mjsdeqn{q_{i} = \frac{\log(F_{i} + k)}{\sum_{i=1}^{s}\log(F_{i} + k)}}
#'
#' Where \mjseqn{F_{i}} is the absolute frequency of the \mjseqn{i}th
#' descriptive state for a qualitative trait in a collection. It is incremented
#' by a constant \mjseqn{k = 0.000001} prior to log transformation. This ensures
#' that singleton descriptor states (where \mjseqn{F_{i} = 1}) yield a small but
#' non-zero proportion rather than being assigned a zero proportion due to
#' \mjseqn{\log(1) = 0}, which would otherwise exclude all accessions of that
#' descriptor state from core selection irrespective of \code{size.count}.
#'
#' When \code{size.count} is supplied, the transformed proportions
#' \mjseqn{q_{i}} are subject to iterative clamping to ensure that the implied
#' frequency \mjseqn{q_{i} \times n} for any descriptor state \mjseqn{i} does
#' not exceed its actual count in the collection, where \mjseqn{n} is
#' \code{size.count}. Excess proportion from clamped states is redistributed
#' proportionally among unclamped states and the process repeats until no state
#' exceeds its maximum allowable proportion \mjseqn{F_{i} / n}.
#'
#' @param x Data of a qualitative trait for accessions in a collection as a
#'   vector of type factor.
#' @param method The method for transformation. Either \code{"none"} for no
#'   transformation or \code{"log"} for log-frequency transformation or
#'   \code{"sqrt"} for square root-proportion transformation.
#' @param size.count A positive integer specifying the target size of the core
#'   collection. The sum of frequencies allocated across levels of each
#'   qualitative trait will not exceed this value, and serves as the upper bound
#'   for iterative proportion clamping when \code{size.count} is supplied. If
#'   \code{NULL}, no clamping is performed and the adjusted proportions are
#'   returned as-is.
#'
#' @returns The relative frequencies as a named numeric vector.
#' @export
#'
#' @references
#'
#' \insertAllCited{}
#'
#' @examples
#' suppressPackageStartupMessages(library(EvaluateCore))
#'
#' library(EvaluateCore)
#'
#' # Get data from EvaluateCore
#'
#' data("cassava_EC", package = "EvaluateCore")
#'
#' # Data of 'Colour of unexpanded apical leaves' qualitative trait
#' CUAL <- as.factor(cassava_EC$CUAL)
#'
#' # Raw relative frequencies
#' prop.adj(CUAL, method = "none")
#'
#' # Square root-proportion transformed relative frequencies
#' prop.adj(CUAL, method = "sqrt")
#'
#' # Square log-frequency transformed relative frequencies
#' prop.adj(CUAL, method = "log")
#'
prop.adj <- function(x, method = c("none", "log", "sqrt"),
                     size.count = NULL) {

  # Checks -----

  if (is.vector(x) & !is.factor(x)) {
    stop('"x" should be a vector of type factor.')
  }

  method <- match.arg(method)

  # Counts ----
  counts <- summary(x)

  # Relative frequency/proportion ----
  prop <- counts/sum(counts)

  if (method == "none") {
    prop <- prop
  }

  # Log adjustment
  if (method == "log") {
    # prop <- log(counts)/sum(log(counts))
    # Laplace-style smoothing
    prop <- log(counts + 0.000001) / sum(log(counts + 0.000001))
  }

  # Square root adjustment
  if (method == "sqrt") {
    prop <- sqrt(prop)/sum(sqrt(prop))
  }

  # Iterative clamping ----
  if (!is.null(size.count)) {

    max_prop <- counts / size.count   # max allowable proportion per level

    repeat {
      exceeds <- prop > max_prop
      if (!any(exceeds)) break

      prop[exceeds] <- max_prop[exceeds] # clamp exceeded levels
      residual <- 1 - sum(prop[exceeds]) # remaining proportion
      prop[!exceeds] <- prop[!exceeds] /
        sum(prop[!exceeds]) * residual  # redistribute
    }
  }

  return(prop)
}

