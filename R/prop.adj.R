
#' Relative Frequency Adjustments
#'
#' Compute and transform relative frequencies or accessions for a qualitative
#' trait by the following methods
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
#' as \mjsdeqn{q_{i} = \frac{\log{F_{i}}}{\sum_{i=1}^{s}\log{F_{i}}}}
#'
#' Where \mjseqn{F_{i}} is the absolute frequency of the \mjseqn{i}th
#' descriptive state for a qualitative trait in a collection.
#'
#' @param x Data of a qualitative trait for accessions in a collection as a
#'   vector of type factor.
#' @param method The method for transformation. Either \code{"none"} for no
#'   transformation or \code{"log"} for log-frequency transformation or
#'   \code{"sqrt"} for square root-proportion transformation.
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
prop.adj <- function(x, method = c("none", "log", "sqrt")) {

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
    prop <- log(counts)/sum(log(counts))
  }

  # Square root adjustment
  if (method == "sqrt") {
    prop <- sqrt(prop)/sum(sqrt(prop))
  }

  return(prop)
}

