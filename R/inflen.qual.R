inflen.qual <- function(freq, adj = TRUE) {


  if (adj) {

    prop <- (freq + 1) / (sum(freq) + length(freq))

  } else {

    prop <- freq / sum(freq)

  }

  # Information length
  inflen_class <- -log(prop)
  inflen_class <- data.frame(class = names(inflen), inflen = unname(inflen_class))

  inflen <- dplyr::left_join(data.frame(class = x), inflen_class, by = "class")

  return(inflen)

}
