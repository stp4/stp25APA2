#' @name etaSquared2
#' @rdname etaSquared2
#' @title Eta-Quadrat
#' @description Kopie von lsr::etaSquared
#' @param x  fit lm oder anova
#' @param type Anova Type default ist 2
#' @param anova  Ausgabe der ANOVA Tabelle
#' @param ... nicht benutzt
#' @return Vector
#' @examples
#' fit1<-lm(y1~x1, anscombe)
#' #etaSquared2(aov (y1~x1, anscombe), anova=TRUE)
#' #etaSquared2(fit1, anova=TRUE)
#' etaSquared2(fit1 )
#'
#' @export
etaSquared2 <- function (x, type = 2, anova = FALSE, ...)      ##   <environment: namespace:lsr
{
  if (!is(anova, "logical") | length(anova) != 1) {
    stop("\"anova\" must be a single logical value")
  }
  if (!is(x, "lm")) {
    stop("\"x\" must be a linear model object")
  }
  if (!is(type, "numeric") | length(type) != 1) {
    stop("type must be equal to 1,2 or 3")
  }
  if (type == 1) {
    ss <- anova(x)[, "Sum Sq", drop = FALSE]
    ss.res <- ss[dim(ss)[1], ]
    ss.tot <- sum(ss)
    ss <- ss[-dim(ss)[1], , drop = FALSE]
    ss <- as.matrix(ss)
  }
  else {
    if (type == 2) {
      ss.tot <- sum((x$model[, 1] - mean(x$model[, 1]))^2)
      ss.res <- sum((x$residuals)^2)
      terms <- attr(x$terms, "factors")[-1, , drop = FALSE]
      l <- attr(x$terms, "term.labels")
      ss <- matrix(NA, length(l), 1)
      rownames(ss) <- l
      for (i in seq_along(ss)) {
        vars.this.term <- which(terms[, i] != 0)
        dependent.terms <- which(apply(terms[vars.this.term,
                                             , drop = FALSE], 2, prod) > 0)
        m0 <- lm(x$terms[-dependent.terms], x$model)
        if (length(dependent.terms) > 1) {
          m1 <- lm(x$terms[-setdiff(dependent.terms,
                                    i)], x$model)
          ss[i] <- anova(m0, m1)$`Sum of Sq`[2]
        }
        else {
          ss[i] <- anova(m0, x)$`Sum of Sq`[2]
        }
      }
    }
    else {
      if (type == 3) {
        mod <- drop1(x, scope = x$terms)
        ss <- mod[-1, "Sum of Sq", drop = FALSE]
        ss.res <- mod[1, "RSS"]
        ss.tot <- sum((x$model[, 1] - mean(x$model[,
                                                   1]))^2)
        ss <- as.matrix(ss)
      }
      else {
        stop("type must be equal to 1, 2 or 3")
      }
    }
  }
  if (anova == FALSE) {
    ss <- rbind(ss, ss.res)
    eta2 <- ss/ss.tot
    eta2p <- ss/(ss + ss.res)
    k <- length(ss)
    E <- cbind(eta2, eta2p)
    E[k, 2] <- NA
    colnames(E) <- c("eta.sq", "eta.sq.part")
    rownames(E) <- rownames(ss)
    rownames(E)[k] <- "Residuals"

    # eta2 <- ss/ss.tot
    # eta2p <- ss/(ss + ss.res)
    # E <- cbind(eta2, eta2p)
    # rownames(E) <- rownames(ss)
    # colnames(E) <- c("eta.sq", "eta.sq.part")
  }
  else {
    ss <- rbind(ss, ss.res)
    eta2 <- ss/ss.tot
    eta2p <- ss/(ss + ss.res)
    k <- length(ss)
   # eta2p[k] <- NA
    df <- anova(x)[, "Df"]
    ms <- ss/df
    Fval <- ms/ms[k]
    p <- 1 - pf(Fval, df, rep.int(df[k], k))
    E <- cbind( ss, df, ms, Fval, eta2, eta2p, p)
    E[k, c(4, 6, 7)] <- NA
    colnames(E) <- c( "SS", "df",
                     "MS", "F","eta.sq", "eta.sq.part", "p")

    rownames(E) <- rownames(ss)
    rownames(E)[k] <- "Residuals"
  }
  return(E)
}






#' @rdname RMSE
#' @title RMSE
#' @name RMSE
#' @description ## The RMSE is the square root of the variance of the residuals.
#' Compute the root mean squared error
#' (see \code{\link{sigma}})
#' @param x fit-Objekt lm glm
#' @param ... weitere Objekte nicht benutzt
#' @return Data.frame mit Namen
#'
#' @examples
#' #library(haven)
#'
#'
#' @export
RMSE<- function(x, ...){
  UseMethod("RMSE")

}

#' @rdname RMSE
#' @export
RMSE.default <- function(x,...)
{
  data.frame(sigma=sigma(x),
    RMSE = sqrt(mean(x$residuals^2)))
}

#' @rdname RMSE
#' @export
RMSE.mlm <- function(x,...)
{
 broom::fix_data_frame(
   data.frame(sigma=sigma(fit1),
             RMSE= apply(fit1$residuals, 2 ,
                         FUN=function(rr) sqrt(mean(rr^2)))))
}




