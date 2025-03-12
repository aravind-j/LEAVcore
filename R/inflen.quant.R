

inflen.quant <- function(x, k, w, breaks, e = 1) {

  if(sum(c(!is.null(k),
           !is.null(w),
           !is.null(breaks))) != 1) {
    stop('Specify only one of the arguments "k", "w" or "breaks.')
  }

  # Find class intervals
  if (!is.null(k)) {
    classes <- cut_number(x, n = k)

  }

  if (!is.null(w)) {
    classes <- cut_width(x, width = w)

  }

  if (!is.null(breaks)) {
    classes <- cut(x, breaks = breaks)
  }

  # Normality test
  # Kolmogorov-Smirnov (K-S) test
  # Shapiro-Wilk’s test

  class_levels <- levels(classes)

  swlist <- lapply(seq_along(class_levels), function(i) {
    xc <- x[classes == class_levels[i]]
    xshapiro <- shapiro.test(xc)

    list(`Shapiro-Wilk Statistic` = xshapiro[["statistic"]],
         `p value` = xshapiro[["p.value"]])
  })
  names(swlist) <- class_levels

  swdf <- dplyr::bind_rows(lapply(swlist, data.frame),
                           .id = "Class interval")
  rownames(swdf) <- NULL

  nrmfail <- NULL
  if (any(!(swdf$p.value) > 0.05)) {
    nrmfail <- swdf[!(swdf$p.value > 0.05), ]$`Class interval`
  }

  # Compute basic stats
  class_stats <- lapply(seq_along(class_levels), function(i) {

    xc <- x[classes == class_levels[i]]
    list(mean = mean(xc, na.rm = TRUE), sd = sd(xc, na.rm = TRUE))

  })
  names(class_stats) <- class_levels

  class_stats <- dplyr::bind_rows(lapply(class_stats, data.frame),
                                  .id = "class")

  # distribution normalizing constant
  kconst <- 1 / sqrt(2 * pi)
  normconst <- log(class_stats$sd / (kconst * e))
  class_stats$normconst <- normconst


  inflen <- dplyr::left_join(data.frame(x, class = classes), class_stats, by = "class")

  # Information length
  inflen$inflen <- inflen$normconst + (((inflen$x - inflen$mean) ^ 2) /
                                         (2 * (inflen$sd ^ 2)))

  return(inflen)

}
