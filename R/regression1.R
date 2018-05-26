#' @rdname APA
#' @description APA.lm F-Test aus lm und Anova
#' @param include.r APA.lm: R-Squar
#' @export
APA.lm <- function(x, include.r = TRUE) {
  if (any(class(x) == "aov"))
    x <- lm(x)
  fitSummary <- summary(x)
  fstats <- fitSummary$fstatistic
  pValue <-  stats::pf(fstats[['value']],
                       fstats[['numdf']],
                       fstats[['dendf']], lower.tail = FALSE)
  if (include.r)
    rndr_lm(fstats[['value']] ,
            fstats[['numdf']],
            fstats[['dendf']],
            pValue,
            fitSummary$r.squared,
            fitSummary$adj.r.squared)
  else
    rndr_F(fstats[['value']] ,
           fstats[['numdf']],
           fstats[['dendf']],
           pValue)
  
}


#' @rdname APA
#' @export
APA.glm <- function(x, ...) {
  lrtst <-  lmtest::lrtest(x)
  paste0("LogLik=",
         Format2(lrtst[2, 2], 2),
         ", ",
         rndr_X(lrtst[2, 4],
                lrtst[1, 1],
                lrtst[2, 1],
                lrtst[2, 5]))
}

 
#' @rdname APA_
#' @description  APA_Durbin_Watson(fit, max.lag=1, simulate=TRUE, reps=1000,
#' method=c("resample","normal"),
#' alternative=c("two.sided", "positive", "negative")): Durbin-Watson Test for Autocorrelated Errors. 
#'   Kopie der Funktion car::durbinWatsonTest
#'   
#' @export
#'
#' @examples
#' x<-lm(score ~ grade + treatment + stdTest, schools)
#' APA2(car::durbinWatsonTest(x))
#' DW_Test2(x)
#' 
#' lmtest::dwtest(x)
#' car::durbinWatsonTest(x)
#' 
APA_Durbin_Watson<- function(x,
                             caption = "Durbin-Watson Test for Autocorrelated Errors",
                             note =NULL, ...){
  dw<-car::durbinWatsonTest(x,...) 
  APA2.durbinWatsonTest(dw, caption=caption, note=note)
}


#' @rdname APA2
#' @description Methode für car::durbinWatsonTest Kopie von car:::print.durbinWatsonTest
#' @export

APA2.durbinWatsonTest <-
  function(x,
           caption = "Durbin-Watson Test for Autocorrelated Errors",
           note =NULL,
           ...) {
    max.lag <- length(x$dw)
    result <- if (is.null(x$p))
      cbind(
        lag = 1:max.lag,
        Autocorrelation = x$r,
        `D-W Statistic` = x$dw
      )
    else cbind(lag = 1:max.lag, Autocorrelation = x$r, `D-W Statistic` = x$dw, 
               `p-value` = x$p)
    rownames(result) <- rep("", max.lag)
    
    note <- paste(" Alternative hypothesis: rho", 
                  if (max.lag > 1) "[lag]"
                  else "", 
                  c(" != ", " > ", " < ")[which(x$alternative == c("two.sided", "positive", "negative"))], "0", sep = "")
    
    result <- prepare_output(fix_format(data.frame(result)),
                             caption = caption, note = note)
    Output(result, ...)
    invisible(result)
  }
