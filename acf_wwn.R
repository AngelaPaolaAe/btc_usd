library(ggplot2)
#where h are the lags
# computes expectation of squares
estim_gamma <- function(x, h){
  n <- length(x)
  x <- as.numeric(x)
  sum(x[1:(n - h)] * x[(h + 1):n]) / (n - h)
}
# ------------------------------------------

# computes the band width for weak white noise
condsd_band <- function(x, h) {
  gamma <- purrr::map_dbl(1:h, estim_gamma, x = x ^ 2)
  gamma <- sqrt(gamma / (estim_gamma(x,0) ^ 2))
  gamma / sqrt(length(x))
}
# ------------------------------------------

# compute acf for weak white noise with bands
acf_wwn <- function(x, h, level = 0.95, type = c("correlation",
                                                 "partial", "generalized")){
  type = match.arg(type)
  x <- as.numeric(x)
  kconf  <- qnorm((1 + level)/2)
  if (type == "generalized") {
    type2 <- "correlation"
    width <- condsd_band(x, h)
    gen_lim <- width * kconf
  } else {
    type2 = type
  }
  
  
  
  acf_vals <- stats::acf(x, lag.max = h, plot = FALSE, type = type2)$acf
  acf_vals <- switch(type2, "correlation" = acf_vals[-1], "partial" = acf_vals)
  trad_lim <- rep(kconf / sqrt(length(x)), h)
  band_lim <- switch(type, "correlation" = trad_lim,
                     "partial" = trad_lim,
                     "generalized" = gen_lim )
  acf_data <- data.frame(h = 1:h, acf = acf_vals, wwn_band = band_lim)
}

acf_wwn_plot <- function(x, h, level = 0.95, type = c("correlation", "partial", "generalized")) {
  type <- match.arg(type)
  kconf  <- qnorm((1 + level)/2)
  
  acf_data <- acf_wwn(x, h, level, type)
  band_trad <-  kconf / sqrt(length(x))
  
  acf_plot <- ggplot2::ggplot(acf_data, aes(x = h, y = acf)) +
    ggplot2::geom_segment(aes(x = h, xend = h, y = 0, yend = acf)) +
    ggplot2::geom_hline(yintercept = c(-band_trad, band_trad),
                        colour = "red", linetype = "dashed") +
    ggplot2::xlab("Lag") + ylab("ACF")
  
  if (type == "generalized") {
    acf_plot +
      ggplot2::geom_ribbon(aes(ymin = -wwn_band, ymax = wwn_band ),
                           colour = "blue",  alpha = 0)
  } else {
    acf_plot
  }
}

ccf_gg <- function(x, level = 0.95){
  x <- as.numeric(x)
  n <- length(x)
  kconf  <- qnorm((1 + level)/2)
  data <- data.frame(unclass(ccf(x ^ 2, x, plot = FALSE))[c(4,1)])
  names(data) <- c("Lag", "CCF")
  data <- dplyr::filter(data, Lag < 0)
  ggplot(data, aes(x = Lag, y = CCF)) +
    geom_segment(aes(x = Lag, xend = Lag, y = 0, yend = CCF)) +
    geom_hline(yintercept = c(-kconf/sqrt(n), kconf/sqrt(n)) ,
               col = "red", linetype = "dashed") +
    ggtitle("CCF Squared vs Series")
}





