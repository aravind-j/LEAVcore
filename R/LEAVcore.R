LEAVcore <- function(data, names,
                     quantitative = NULL, qualitative = NULL,
                     sample.count, prop.adj = c("none", "log", "sqrt"),
                     method = c(), e) {



  # Method I : ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  ## Find freq for qualitative traits ----
  freq1 <- lapply(qualitative, function(x) {
    prop <-  prop.adj(data[, x], method = prop.adj)
    fqout <- prop * size.count
    fq_overall <- summary(data[, x])
    fqout <- ifelse(fqout > fq_overall, fq_overall, fqout)
    fqout <- round.to.target(fqout)
    return(fqout)
  })
  names(freq1) <- qualitative

  freq2 <- lapply(qualitative, function(x) {
    summary(data[, x]) - freq1[[x]]
  })
  names(freq2) <- qualitative


  ## Find mean and variance for quantitative traits ----
  # mean1 and sd1: From kernel density of size = size.count
  # mean2 and sd2: From overall
  stat1 <- lapply(quantitative, function(x) {
    dens.obs <- density(data[,x], kernel = "gaussian", n = size.count)
    dens.fun <- approxfun(dens.obs)

    smpl <- sample(data[,x], size = size.count, replace = FALSE,
                   prob = dens.fun(data[,x]))

    out <- data.frame(mean = mean(smpl), sd = sd(smpl))

    return(out)
  })
  names(stat1) <- quantitative

  stat2 <- lapply(quantitative, function(x) {
    out <- data.frame(mean = mean(data[,x]), sd = sd(data[,x]))
    return(out)
  })
  names(stat2) <- quantitative

  mean1 <- unlist(lapply(stat1, function(x) { x[, "mean"] }))
  sd1 <- unlist(lapply(stat1, function(x) { x[, "sd"] }))
  mean2 <- unlist(lapply(stat2, function(x) { x[, "mean"] }))
  sd2 <- unlist(lapply(stat2, function(x) { x[, "sd"] }))

  ## Estimate LEAVs ----

  LEAVdf1 <- LEAV(data = data, names = names,
                  quantitative = quantitative, qualitative = qualitative,
                  freq = freq1, mean = mean1, sd = sd1, e = e)

  LEAVdf2 <- LEAV(data = data, names = names,
                  quantitative = quantitative, qualitative = qualitative,
                  freq = freq2, mean = mean2, sd = sd2, e = e)

  core_ind <- LEAVdf1$LEAV > LEAVdf2$LEAV

  ## Correction ----
  ## When core size exceeds size.count

  if (sum(core_ind) > size.count) {
    coredf <- LEAVdf1[core_ind, ]
    coredf <- sort_by(coredf, coredf$LEAV, decreasing = TRUE)
    core <- coredf[1:size.count, names]
  } else {
    core <- LEAVdf1[core_ind, names]
  }

  # Method II : ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  ## Find freq for qualitative traits ----
  freq <- lapply(qualitative, function(x) {
    prop <-  prop.adj(data[, x], method = prop.adj)
    fqout <- prop * nrow(data)
    # fqout <- round.to.target(fqout)
    return(fqout)
  })
  names(freq) <- qualitative

  ## Find mean and variance for quantitative traits ----

  stat <- lapply(quantitative, function(x) {
    out <- data.frame(mean = mean(data[,x]), sd = sd(data[,x]))
    return(out)
  })
  names(stat) <- quantitative

  mean <- unlist(lapply(stat, function(x) { x[, "mean"] }))
  sd <- unlist(lapply(stat, function(x) { x[, "sd"] }))

  ## Estimate LEAV ----
  LEAVdf <- LEAV(data = data, names = names,
                 quantitative = quantitative, qualitative = qualitative,
                 freq = freq, mean = mean, sd = sd, e = e)

  LEAVdf <- sort_by(LEAVdf, LEAVdf$LEAV, decreasing = TRUE)

  nStrata <- nclass.Sturges(LEAVdf$LEAV)
  strat_out <-
    stratification::strata.cumrootf(x = LEAVdf$LEAV,
                                    n = size.count,
                                    Ls = nStrata,
                                    nclass = nStrata*15) # see Details

  LEAVdf$LEAVStrata <-  strat_out$stratumID
  ##



}

round.to.target <- function(x, target = round(sum(x))) {

  while(sum(round(x)) - target > 0) {
    i <- which.min(ifelse(x %% 1 < 0.5, 1, x %% 1))
    x[i] <- x[i] - 1
  }
  while(sum(round(x)) - target < 0) {
    i <- which.max(ifelse(x %% 1 > 0.5, 0, x %% 1))
    x[i] <- x[i] + 1
  }
  round(x)
}
