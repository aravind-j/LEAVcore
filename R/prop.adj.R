

prop.adj <- function(x, method = c("none", "log", "sqrt")) {

  # Checks -----

  if (is.vector(x) & !is.factor(x)) {
    stop('"x" should be a vector of type factor.')
  }

  method <- match.arg(method)

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

  # Square root ajdustment
  if (method == "sqrt") {
    prop <- sqrt(prop)/sum(sqrt(prop))
  }

  return(prop)
}

