#' Correlation Plot
#' 
#' Function for making a correlation plot starting from a  formula and a data.frame
#'
#' @param x  formula
#' @param ... weitere Param an Lattice oder pairs
#'
#' @return nix
#' @export
#'
#' @examples
#' 
#' # graphics.off()
#' # library(stpvers)
#' 
#' #windows(8,8)
#' corr_plot(~ m1 + m2 + m3 + m4, varana)
#' #windows(8,8)
#' corr_plot( m1~  m2 + m3 + m4, varana)
#' 
corr_plot <- function(x, ...){
  UseMethod("corr_plot")
}

 
#' @param data  a data matrix
#' @rdname corr_plot
#' @export
#' 
corr_plot.formula <- function(x, data, ...){
  dat <- prepare_data2(x, data)
  if( is.null(dat$group.vars) )  corr_plot.data.frame(dat$data, ... )
  else {
    # sicherstellen dass dur eine measure.vars am anfang steht
    corr_plot.lm(NULL, dat$data[,c(dat$measure.vars[1], dat$group.vars)], ...)
    }
  
}


 
#' @param data aus x$model
#' @param type an xyplot c("p", "r")
#' @param scales  an xyplot
#' @param ylab,xlab  an xyplot
#' @param layout  an xyplot
#' 
#' @rdname corr_plot  
#' @export
#' 
corr_plot.lm <- function(x, data = x$model,
                         type = c("p", "r"),
                         scales = list(x = list(relation = "free")),
                         ylab = names(data)[1],
                         xlab = "",
                         layout = c(ncol(data)-1,1),
                         ...) {
  data <- dapply2(data, function(x)
    as.numeric(x))
  dat <- Melt2(data, id.vars = 1)
  names(dat)[1] <- "y"
  lattice::xyplot(
    y ~ value | variable,
    dat,
    ylab = ylab,
    xlab = xlab,
    type = type,
    scales = scales,
    layout =layout,
    ...
  )
}


#' @param jitter Rauschen
#' @param smooth  Gezeichnete Lineie
#' @param lines  Regressinsgerade
#' @param pch Symbole  pch=20
#' @param digits Nachkommastellen in plot
#' @param cex.cor,resize   Fixe groese mit cex.cor, resize abhaengig von r-Wert
#' @param method c("pearson", "kendall", "spearman")
#' @param stars Sternchen
#' @param hist Histogram TRUE/FLASE
#' 
#' @rdname corr_plot 
#' @export
#' 
corr_plot.data.frame <- function(data,
                                 jitter = FALSE,
                                 smooth = TRUE,
                                 lines = TRUE,
                                 pch = 20,
                                 digits = 2,
                                 cex.cor = NULL,
                                 method = "pearson",
                                 stars = FALSE,
                                 resize = FALSE,
                                 hist=TRUE,
                                 ...) { 
  cat(method, "\n")
  par(pch = pch, bty = 'n')
  data <- dapply2(data, function(x) as.numeric(x))
    pairs(
    data,
    lower.panel = panel.lines2,
    upper.panel = panel.cor,
    diag.panel =   if(hist)  panel.hist else NULL,
    # cex.labels=2,
    method = method,
    stars = stars,
    resize = resize ,
    cex.cor = cex.cor,
    digits = digits ,
    smooth = smooth,
    lines = lines,
    ...)
}


panel.cor <-
  function(x,
           y,
           digits,
           prefix = "",
           cex.cor,
           method,
           stars,
           resize,
           ...,
           cex_resize = .75)
  {
    usr <- par("usr")
    on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    # box(   col ="white")
    test <- cor.test(x, y , na.action = na.omit, method = method)
    
   # print(test)
    r <- test$estimate
  
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste(prefix, txt, sep = "")
    
    txt.cex <- format(c(abs(r), 0.123456789), digits = digits)[1]
    txt.cex <- paste(prefix, txt.cex, sep = "")
    
    if (is.null(cex.cor))
      cex <- cex_resize / strwidth(txt.cex)
    else cex<-cex.cor
    # borrowed from printCoefmat
    Signif <- stats::symnum(
      test$p.value,
      corr = FALSE,
      na = FALSE,
      cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
      symbols = c("***", "**", "*", ".", " ")
    )
    
    if (resize)
      text(0.5, 0.5, txt, cex = round(cex * abs(r), 2))
    else
      text(0.5, 0.5, txt, cex = cex)
    if (stars)
      text(.8, .8, Signif, cex = cex / 2, col = 2)
}


panel.hist <- function(x, ...)
{

  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5))
  
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks
  nB <- length(breaks)
  
  
  y <- h$counts
  y <- y / max(y)
  
  if (nlevels(factor(x)) < 5) {
    print(breaks[-nB])
    print(y)
  }
  box(lty = 1, col = 'white')
  rect(breaks[-nB], 0, breaks[-1], y, col = "RoyalBlue", border = "lightblue")
}


# Anpassungslieneie mit Itter
panel.lines2 <-
  function (x, y,
            col = par("col"),
            bg = NA,
            pch = par("pch"),
            cex = 1,
            col.smooth = "blue",
            span = 2 / 3,
            iter = 3,
            lines,
            smooth,
            ...)
  {
    if (nlevels(factor(x)) < 5)
      x <- jitter(x)
    if (nlevels(factor(y)) < 5)
      y <- jitter(y)
    
    points(x,y,
           pch = pch,
           col = col,
           bg = bg,
           cex = cex
    )
    axis(2, labels = FALSE)
    axis(1, labels = FALSE)
    
    if (lines)
      abline(lm(y ~ x, 
                data = na.omit(data.frame(x, y))), 
             col = col.smooth)
    if (smooth) {
      ok <- is.finite(x) & is.finite(y)
      if (any(ok))
        lines(stats::lowess(x[ok], y[ok], 
                            f = span, iter = iter),
              col = col.smooth)
    }
  }
