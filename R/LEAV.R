

LEAV <- function(data, names,
                 quantitative = NULL, qualitative = NULL,
                 freq, mean, sd, e) {

  # Checks ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # check if 'data' is a data frame object
  if (!is.data.frame(data)) {
    stop('"data" should be a data frame object.')
  }

  if (any(c("tbl_dataf", "tbl") %in% class(data))) {
    warning('"data" is of type tibble\nCoercing to data frame.')
    data <- as.data.frame(data)
  }


  # check if 'quantitative' argument is a character vector
  if (!is.null(quantitative)) {
    if (!is.character(quantitative)) {
      stop('"quantitative" should be a character vector.')
    }
  }

  # check if 'qualitative' argument is a character vector
  if (!is.null(qualitative)) {
    if (!is.character(qualitative)) {
      stop('"qualitative" should be a character vector.')
    }
  }

  # check if 'names' column is present in 'data'
  if (!(names %in% colnames(data))) {
    stop(paste('Column ', names,
               ' specified as the "names" column is not present in "data".',
               sep = ""))
  }

  # check if 'quantitative' columns are present in 'data'
  if (!is.null(quantitative)) {
    if (FALSE %in% (quantitative %in% colnames(data)))  {
      stop(paste('The following column(s) specified in "quantitative" are not present in "data":\n',
                 paste(quantitative[!(quantitative %in% colnames(data))],
                       collapse = ", "),
                 sep = ""))
    }
  }

  # check if 'qualitative' columns are present in 'data'
  if (!is.null(qualitative)) {
    if (FALSE %in% (qualitative %in% colnames(data)))  {
      stop(paste('The following column(s) specified in "qualitative" are not present in "data":\n',
                 paste(qualitative[!(qualitative %in% colnames(data))],
                       collapse = ", "),
                 sep = ""))
    }
  }

  # check if overlap exists between 'quantitative' and 'qualitative'
  if ((!is.null(quantitative)) & (!is.null(qualitative))) {
    if (length(intersect(quantitative, qualitative)) != 0) {
      stop(paste('The following column(s) is/are specified in both "quantitative" and "qualitative":\n',
                 paste(intersect(quantitative, qualitative),
                       collapse = ", "),
                 sep = ""))
    }
  }

  # check if 'names' column is of type character
  if (!is.character(data[, names])) {
    stop('"names" column in "data" should be of type character.')
  }

  # check if 'quantitative' columns are of type numeric/integer
  if (!is.null(quantitative)) {
    intquantcols <- unlist(lapply(data[, quantitative],
                                  function(x) FALSE %in% (is.vector(x, mode = "integer") | is.vector(x, mode = "numeric"))))
    if (TRUE %in% intquantcols) {
      stop(paste('The following "quantitative" column(s) in "data" are not of type numeric:\n',
                 paste(names(intquantcols[intquantcols]), collapse = ", ")))
    }
  }

  # check if 'qualitative' columns are of type factor
  if (!is.null(qualitative)) {
    intqualcols <- unlist(lapply(data[, qualitative],
                                 function(x) is.factor(x)))
    if (FALSE %in% intqualcols) {
      stop(paste('The following "qualitative" column(s) in "data" are not of type factor:\n',
                 paste(names(intqualcols[!intqualcols]), collapse = ", ")))
    }
  }

  # # check for missing values
  # missvcols <- unlist(lapply(data[, quantitative],
  #                            function(x) TRUE %in% is.na(x)))
  # if (TRUE %in% missvcols) {
  #   stop(paste('The following column(s) in "data" have missing values:\n',
  #              paste(names(missvcols[missvcols]), collapse = ", ")))
  # }

  # check for duplication in names
  if (any(duplicated(data[, names]))) {
    warning('Duplicated entries exist in "names" column.')
  }


  # check if freq sum is within nrows(data)
  if (sum(freq) > nrow(data)) {
    stop('Sum of frequencies specified in "freq" exceeds size of "data".')
  }

  # check freq list

  # all freq sum should be same


  # Qualitative traits - Information length ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if (!is.null(qualitative)) {
    adj <- TRUE
    # if (sum(freq) == nrow(data))

    qual_inflen <- lapply(qualitative, function(x) {
      out <- inflen.qual(x = data[, x], freq = freq[[x]], adj = adj)
      out <- data.frame(out[, "inflen"])
      colnames(out) <- x
      out
    })

    qual_inflen <- dplyr::bind_cols(qual_inflen)

    qual_inflen <- cbind(names = data[, names],  qual_inflen)
    colnames(qual_inflen)[colnames(qual_inflen) == "names"] <- names
  }

  # Quantitative traits - Information length ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if (!is.null(quantitative)) {
  quant_inflen <- lapply(quantitative, function(x) {
    out <- inflen.quant(x = data[, x], mean = mean[x], sd = sd[x], e = e[x])
    out <- data.frame(out[, "inflen"])
    colnames(out) <- x
    out
  })

  quant_inflen <- dplyr::bind_cols(quant_inflen)

  quant_inflen <- cbind(names = data[, names],  quant_inflen)
  colnames(quant_inflen)[colnames(quant_inflen) == "names"] <- names

  }

  # Subset/group - Message length ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  total_count <- nrow(data)
  subset_count <- sum(freq[[1]])

  lt <- -log(total_count/subset_count)

  # LEAV ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if (!is.null(qualitative) & !is.null(quantitative)) {
    LEAVdf <- merge.data.frame(qual_inflen, quant_inflen, by = names)
  }

  if (!is.null(qualitative) & is.null(quantitative)) {
    LEAVdf <- qual_inflen
  }

  if (!is.null(quantitative) & is.null(qualitative)) {
    LEAVdf <- quant_inflen
  }

  LEAVdf$lt <- lt
  LEAVdf <- LEAVdf[, c(names, "lt", qualitative, quantitative)]

  LEAVdf$LEAV <- rowSums(LEAVdf[, c("lt", qualitative, quantitative)])


  return(LEAVdf)


}



