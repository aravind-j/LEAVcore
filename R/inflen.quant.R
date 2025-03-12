

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
