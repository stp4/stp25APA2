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
#' Durbin-Watson test for autocorrelation of disturbances. Ist nur bei Zeitreihendaten sinnvoll.
#' Computes Levene's test for homogeneity of variance across groups.
#' Bartlett Test of Homogeneity of Variances
#' Breusch-Pagan test against heteroskedasticity.
#' variance inflation factor. VIF values over 5 are troubling, should probably investigate anything over 2.5.
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
                           # include.vif=TRUE,
                            caption = "Testing Linear Regression Models") {
  
  res <- data.frame(Test = as.character(NA), statistic = as.character(NA),
                    stringsAsFactors=FALSE)
  if (include.F) {
    res <-
      rbind(res,
            c(Test = "F-Statistic",
              statistic = APA(x,include.r = FALSE)))
  }
  
  
  if (include.heteroskedasticity) {
    res <-
      rbind(res,
            c(Test = "Heteroskedasticity (Breusch-Pagan)",
              statistic = APA(lmtest::bptest(x))))
  }
  if (include.durbin) {
    res <-
      rbind(res,
            c(Test = "Autocorrelation (Durbin-Watson )",
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
            c(Test = "Bartlett",
              statistic =  APA(bartlett.test(x$call$formula, x$model))))
    
  }
 # if(include.vif) VIF2(x)
  
  prepare_output(res[-1,], caption=caption) 
  
}



#' Validation of Linear Models Assumptions
#' 
#' Test von Heteroskedasticity und Autocorrelation
#'
#' @param ... 
#' @param include.F 
#' @param include.heteroskedasticity 
#' @param include.durbin 
#' @param include.levene 
#' @param include.bartlett 
#' @param caption 
#' @param note 
#' @param names 
#'
#' @return Output als html und data.frame
#' @export
#'
APA_Validation<- function(...,
                          include.F=TRUE,
                          include.heteroskedasticity = TRUE,
                          include.durbin = TRUE,
                          include.levene = FALSE,
                          include.bartlett = FALSE,
                          # include.vif=TRUE,
                          caption = "Testing Linear Regression Models",
                          note="",
                          names = NULL
)
{
  custom_model_names <- function() {
    if (length(myfits) == 1) { 
      ""
    }else{
      if (is.null(names)) paste0("(", 1:length(myfits), ")")  else
        names     
    }
  }
  
  myfits <- list(...)
  
  if (is(myfits[[1]], "list")) {
    myfits <- myfits[[1]]  # hier kommt ein fit_with-Objekt
    if (is.null(names))
      names <- names(myfits)
  }
  n<-length(myfits) 
  
  custom.model.names <- custom_model_names()
  
  res<- test_regression(myfits[[1]])
  if(n>1){
    for(i in 2:n){
      
      res<- cbind(res,  test_regression(myfits[[i]])[2])
    }
    names(res) <- c("Test", custom.model.names)
    
  }
  Output(res)
  invisible(res) 
} 
