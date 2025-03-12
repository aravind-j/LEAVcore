inflen.qual <- function(freq, adj = TRUE) {

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

  # Adjustment ----

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
