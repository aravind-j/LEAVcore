

prop.adj <- function(x, method = c("none", "log", "sqrt")) {

  method <- match.arg(method)

  counts <- summary(x)

  # Relative frequency/proportion
  prop <- counts/sum(counts)


  if (method == "log") {
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

