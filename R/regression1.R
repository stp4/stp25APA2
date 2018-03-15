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



#' Testing Linear Regression Models
#'
#' Durbin-Watson test for autocorrelation of disturbances.
#' Computes Levene's test for homogeneity of variance across groups.
#' Bartlett Test of Homogeneity of Variances
#' Breusch-Pagan test against heteroskedasticity.
#'
#' @param x model- fit
#' @param include.F  Fsratistik
#' @param include.heteroskedasticity Breusch-Pagan test
#' @param include.durbin  autocorrelation
#' @param include.levene  homogeneity of variance across groupssiehe T-Test
#' @param include.bartlett Homogeneity of Variances siehe T-Test
#' @param caption 
#' @export
test_regression <- function(x,
                            include.F=TRUE,
                            include.heteroskedasticity = TRUE,
                            include.durbin = TRUE,
                            include.levene = FALSE,
                            include.bartlett = FALSE,
                            caption = "Testing Linear Regression Models") {
  
  res <- data.frame(Test = as.character(NA), statistic = as.character(NA),
                    stringsAsFactors=FALSE)
  if (include.F) {
    res <-
      rbind(res,
            c(Test = "F-Statistic",
              statistic = APA(x)))
  }
  
  
  if (include.heteroskedasticity) {
    res <-
      rbind(res,
            c(Test = "Breusch-Pagan (heteroskedasticity)",
              statistic = APA(lmtest::bptest(x))))
  }
  if (include.durbin) {
    res <-
      rbind(res,
            c(Test = "Durbin-Watson (autocorrelation)",
              statistic = APA(lmtest::dwtest(x))))
  }
  if (include.levene) {
    res <- rbind( res,
                  c(Test = "Levene's",
                    statistic = APA(car::leveneTest(x))))
  }
  if (include.bartlett) {
    res <-
      rbind(res,
            c(Test = "Breusch",
              statistic =  APA(bartlett.test(x$call$formula, x$model))))
    
  }
  
  
  prepare_output(res[-1,], caption=caption) 
  
}