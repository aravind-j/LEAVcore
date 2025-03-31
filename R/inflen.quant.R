
#' Compute Information Length for Quantitative Traits
#'
#' The function \code{inflen.quant} computes the length of information code that
#' can indicate the possession of a specific value by a quantitative trait
#' \insertCite{wallace_information_1968,balakrishnan_Strategies_2001,balakrishnan_Strategies_2001a,balakrishnan_Strategies_2003}{LEAVcore}
#' \loadmathjax.
#'
#' For each quantitative trait \mjseqn{d}, it is assumed that it is normally
#' distributed within subset \mjseqn{t} with mean \mjseqn{\mu_{d,t}} and the
#' standard deviation \mjseqn{\sigma_{d,t}} estimated as below.
#'
#' \mjsdeqn{\mu_{d,t} = \frac{\sum x_{d,s}}{n_{d,t}}}
#'
#' \mjsdeqn{\sigma_{d,t} = \sqrt{\frac{\sum(x_{d,s} - \mu_{d,t})^2}{n_{d,t} -
#' 1}}}
#'
#' From this, a distribution normalizing constant \mjseqn{g_{d,t}} can be
#' estimated as
#'
#' \mjsdeqn{g_{d,t} = \ln \left ( \frac{\sigma_{d,t}}{K \cdot \varepsilon_{d}}
#' \right )}
#'
#' Where \mjseqn{K = \frac{1}{\sqrt{2\Pi}}}, \mjseqn{\varepsilon_{d}} is the
#' least count of measurement of the descriptor \mjseqn{d}. i.e. \mjseqn{x} is
#' measured to an accuracy of \mjseqn{\pm\varepsilon_{d}}.
#'
#' The probability of getting a measurement \mjseqn{x} from a distribution of
#' mean \mjseqn{\mu} and variance \mjseqn{\sigma} is approximately as follows.
#'
#' \mjsdeqn{K \cdot \frac{\varepsilon_{d}}{\sigma_{d,t}} \cdot
#' e^{\frac{-(x_{d,s} - \mu_{d,t})^2}{2 \sigma_{d,t}^{2}}}}
#'
#' Now the length of the information code that can optimally indicate the
#' possession of a value \mjseqn{x} by the trait \mjseqn{d} is computed as
#' follows:
#'
#' \mjsdeqn{c_{x,d,t} = g_{d,t} + \frac{(x_{d,s} - \mu_{d,t})^2}{2
#' \sigma_{d,t}^{2}}}
#'
#' @param x Data of a quantitative trait for accessions in a collection as a
#'   numeric vector.
#' @param mean The target mean.
#' @param sd The target standard deviation
#' @param e Th least count of measurement for the quantitative trait.
#'
#' @returns A data frame with 2 columns: \describe{ \item{x}{The quantitative
#'   trait data} \item{inflen}{Information length computed}}
#'
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
#' # Data of 'Average plant weight' quantitative trait
#' AVPW <- cassava_EC$AVPW
#'
#' # Compute information length
#' AVPWinflen <- inflen.quant(x = AVPW, mean = 4, sd = 3.25, e = 1)
#'
#' head(AVPWinflen)
#'
inflen.quant <- function(x, mean, sd, e = 1) {

  # Checks -----

  if (is.vector(x) & !is.numeric(x)) {
    stop('"x" should be a numeric vector.')
  }

  if (!(is.numeric(mean) && length(mean) == 1)) {
    stop('"mean" should be a numeric vector of unit length.')
  }

  if (!(is.numeric(sd) && length(sd) == 1)) {
    stop('"sd" should be a numeric vector of unit length.')
  }

  if (!(is.numeric(e) && length(e) == 1)) {
    stop('"e" should be a numeric vector of unit length.')
  }

  # Distribution normalizing constant ----
  kconst <- 1 / sqrt(2 * pi)
  normconst <- log(sd / (kconst * e))

  inflen <- normconst + (((x - mean) ^ 2) / (2 * (sd ^ 2)))

  inflen <- data.frame(x = x, inflen = inflen)

  return(inflen)

}
