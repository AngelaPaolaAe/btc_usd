source('acf_wwn.R')
library(ggplot2)
library("purrr")
library(gridExtra)

garch_display <- function(x, h = 30, level = 0.95, type = c("generalized", "correlation")) {
  n <- length(x)
  type = match.arg(type)
  series <- qplot(1:n, x, geom = "line") +
    #ggtitle("Serie (o cuadrado)")
    ggtitle("Log returns BTCUSD")
  acf_series <- acf_wwn_plot(x, h, level, type = type)
  pacf_series <- acf_wwn_plot(x, h, level, type = "partial")
  L <- matrix(c(1,2,1,3), 2)
  gridExtra::arrangeGrob(series, acf_series, pacf_series, layout_matrix = L)
}

sq_garch_display <- function(x, h = 30, level = 0.95){
  garch_display(x ^ 2, h, level, type = "correlation")
}

vol_plot <- function(x, threshold = 0.95) {
  x <- as.numeric(x) ^ 2
  q <- quantile(x, threshold)
  
  x[x < q] <-  0
  df <- data.frame(Time = 1:length(x), SqRet = x)
  vol.plot <- ggplot(df, aes(x = Time, y = SqRet)) +
    geom_hline(yintercept = q, col = "red", linetype = "dashed") +
    geom_segment(aes(x = Time, xend = Time, y = 0, yend= SqRet)) +
    ggtitle("Volatility Clustering")
}

plotter <- function(which = c("series", "squared", "ccf",
                              "volatility")) {
  which <- match.arg(which)
  plotFun <- switch(which, "series" = garch_display,
                    "squared" = sq_garch_display,
                    "ccf" = ccf_gg,
                    "volatility" = vol_plot)
}

garch_examine <- function(x, which = c("all","series", "squared", "ccf",
                                       "volatility")) {
  L <- as.matrix(1)
  if ("all" %in% which) {
    which <- c("series", "squared", "ccf", "volatility")
    L <- matrix(c(1,1,3,4,2,2),2)
  }
  plots <- list_along(which)
  for (i in seq_along(which)) {
    plotFun <- plotter(which[i])
    plots[[i]] <- plotFun(x)
  }
  
  grob <- gridExtra::arrangeGrob(grobs = plots, layout_matrix = L)
  plot(grob)
}


