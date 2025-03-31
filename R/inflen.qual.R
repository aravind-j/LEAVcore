#' Compute Information Length for Qualitative Traits
#'
#' The function \code{inflen.qual} computes the length of information code that
#' can indicate the possession of a descriptor state of a qualitative trait
#' \insertCite{wallace_information_1968,balakrishnan_Strategies_2001,balakrishnan_Strategies_2001a,balakrishnan_Strategies_2003}{LEAVcore}
#' \loadmathjax.
#'
#' For each qualitative trait/descriptor \mjseqn{d} the probability of
#' occurrence of a descriptor state \mjseqn{m} in the in a subset \mjseqn{t} is
#' estimated as
#'
#' \mjsdeqn{p_{m,d,t} = \frac{n_{m,d,t} + 1}{n_{d,t} + M_{d}}}
#'
#' Where, \mjseqn{n_{m,d,t}} is the number of accessions with \mjseqn{m} state
#' of trait \mjseqn{d} in subset \mjseqn{t}, \mjseqn{n_{d,t}} is the number of
#' accessions with any known state of trait \mjseqn{d} in subset \mjseqn{t},i.e.
#' the number of accessions in subset \mjseqn{t} and \mjseqn{M_{d}} is the
#' number of descriptor states of trait \mjseqn{d}.
#'
#' This is a slightly biased estimate to include zero frequency descriptor
#' states in the computation. The actual estimate is
#'
#' \mjsdeqn{p_{m,d,t} = \frac{n_{m,d,t}}{n_{d,t}}}
#'
#' Now the length of the information code that can optimally indicate the
#' possession of descriptor state \mjseqn{m} of trait \mjseqn{d} in the subset
#' \mjseqn{t} is computed as
#'
#' \mjsdeqn{c_{m,d,t} = -\ln p_{m,d,t}}
#'
#' @inheritParams prop.adj
#' @param freq The absolute frequencies of the descriptor states of the
#'   qualitative trait \code{x} in the subset of all accessions.
#' @param adj logical. If \code{TRUE}, the proportion estimates are slightly
#'   biased to include zero frequency descriptor states in the computation (See
#'   \strong{Details}). Default is \code{TRUE}.
#'
#' @returns A data frame with 2 columns: \describe{ \item{x}{The qualitative
#'   trait data} \item{inflen}{Information length computed}}
#'
#' @import mathjaxr
#' @export
#'
#' @references
#'
#' \insertAllCited{}
#'
#' @examples
#'
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
#' # Get frequencies based on sample size
#' prop <-  prop.adj(CUAL, method = "sqrt")
#' size.prop <- 0.2
#' size.count <- ceiling(size.prop * length(CUAL))
#' CUALfreq <- round(prop * size.count)
#'
#' # Compute information length
#' CUALinflen <- inflen.qual(x = CUAL, freq = CUALfreq, adj = TRUE)
#'
#' head(CUALinflen)
#'
inflen.qual <- function(x, freq, adj = TRUE) {

  # Checks -----

  if (is.vector(x) & !is.factor(x)) {
    stop('"x" should be a vector of type factor.')
  }

  if (nlevels(droplevels(x)) < 2) {
    stop('"x" should have at leaset 2 levels.')
  }

  if (!(is.logical(adj) && length(adj) == 1)) {
    stop('"adj" should be a vector of type factor with unit length.')
  }

  if (!is.numeric(freq)) {
    stop('"freq" should be a numeric vector.')
  }

  fqlvl_chk <- identical(sort(names(freq)), sort(levels(droplevels(x))))

  if (is.null(names(freq)) & !fqlvl_chk) {
    stop('"freq" should be a named numeric vector and',
         'all the names should be present as levels of "x".')
  }

  # Adjustment (Biased estimate) ----
  # This is different from prop.adj

  if (adj) {

    prop <- (freq + 1) / (sum(freq) + length(freq))

  } else {

    prop <- freq / sum(freq)

  }

  # Information length ----
  inflen_class <- -log(prop)
  inflen_class <- data.frame(class = names(inflen_class),
                             inflen = unname(inflen_class))

  inflen <- dplyr::left_join(data.frame(x = x), inflen_class,
                             by = dplyr::join_by(x == class))

  return(inflen)

}
