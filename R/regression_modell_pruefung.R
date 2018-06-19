


 
test_regression <- function(x,
                            include.ftest=TRUE, include.loglik=FALSE, include.minus.LL=include.loglik,
                            include.r=FALSE, #include.pseudo=FALSE,
                            include.heteroskedasticity = TRUE,
                            include.durbin = TRUE,
                            include.levene = FALSE,
                            include.bartlett = FALSE,
                            include.vif=FALSE,
                            include.sigma=FALSE,
                            include.rmse=FALSE,
                            include.aic=TRUE, include.bic = TRUE,
                            include.residual=TRUE,
                            include.normality=TRUE,
                            include.multicollin=include.vif,
                            
                            include.deviance=TRUE,
                            
                            #include.obs=TRUE,
                            ...
                            ) {
 # cat(("\n in test_regression\n"))
  mdlnf <- model_info(x)
  type_glm  <- inherits(x, "glm")
  type_lm   <- inherits(x, "lm") & (!type_glm)
  type_lmer <- inherits(x, "lmerModLmerTest")
  
  #print(c(type_lm, type_glm, type_lmer))
  #print(mdlnf)
 # print(mdlnf$N)
  xs <- if(type_lmer) summary(x) else NULL
  
  
  res <- data.frame(Test ="Obs.", 
                    statistic = Format2(mdlnf$N, 0),
                    stringsAsFactors=FALSE)
  
  #print(res)
  
  if (include.ftest) {
    res <-
      rbind(res,
            c(Test = "F-Statistic",
              statistic = ifelse(type_lm,
                                 APA(x,include.r = FALSE), NA)))
  }
  
  if (include.loglik) {
    if (inherits(x, "glm")) {
      res <- rbind(res,
                   c(Test = "Likelihood Ratio Test",
                     statistic =   APA(x)))
      
      if (include.minus.LL) {
        minus_ll <- pscl::pR2(x)[1:3]
        minus_ll[1:2] <- minus_ll[1:2] * (-2)
        minus_ll <- Format2(minus_ll, 2)
        res <-
          rbind(res,
                c(
                  Test = "-2LL",
                  statistic =  paste0("LL=", minus_ll[1],
                                      ", LL-Null=", minus_ll[2],
                                      ", G2=", minus_ll[3])
                ))
        
      }
    } else{
      res <- rbind(res,
                   c(Test = "Likelihood Ratio Test",
                     statistic =   NA))
      
      if (include.minus.LL) {
        res <-
          rbind(res,
                c(Test = "-2LL", statistic = NA))
      }
    }
    }
  
  
  if (include.deviance) {
    res <- rbind(res,
                 c(Test = "Deviance Residuals",
                   statistic =   Format2(deviance(x,REML=FALSE), 1)))
    
  }
  
  
  
  if (include.r) {
    res <-rbind(res,
            c(Test = "R-Squared",
              statistic = ifelse( type_lm, 
                                 rndr_r2(R2(x)), 
                          ifelse( type_glm | type_lmer,
                                   rndr_r2pseudo(R2(x)),      
                                   NA))))
  
  }
  
  # if (include.pseudo) {
  #   res <- rbind(res,
  #           c(Test = "Pseudo R-Squared",
  #             statistic =  ifelse( type_glm | type_lmer,
  #                                  rndr_r2pseudo(R2(x)),
  #                                  NA)))
  # }
  

  
  if (include.heteroskedasticity) {
    res <-
      rbind(res,
            c(Test = "Heteroskedasticity (Breusch-Pagan)",
              statistic = ifelse( type_lm |type_glm, APA(lmtest::bptest(x)), NA )
              ))
  }
  
  if (include.durbin) {
    res <-
      rbind(res,
            c(Test = "Autocorrelation (Durbin-Watson)",
              statistic = ifelse( type_lm |type_glm, APA(lmtest::dwtest(x)), NA )
              ))
  }
  
  if (include.levene) {
    levi <- car::leveneTest(x$model[, 1], x$model[, 2])
    levi <-  rndr_F(levi[1, 2],  levi[1, 1],  levi[2, 1],  levi[1, 3])
    
    res <- rbind(res,
                 c(Test = "Homogeneity of Variances (Levene's)",
                   statistic = levi))
  }
  
  if (include.bartlett) {
    res <-
      rbind(res,
            c(Test = "Homogeneity of Variances (Bartlett)",
              statistic =  APA(bartlett.test(x$model[,1], x$model[,2]))))
    
  }
  
  
  if (include.normality) {
    res <-
      rbind(res,
            c(Test = "Shapiro-Wilk normality test",
              statistic =  test_normality(x)))
  }
  
  
  if (include.multicollin) {
    res <-
      rbind(res,
            c(Test = "Autocorrelation (VIF)",
              statistic =  test_multicollin(x)))
  }
  
  
  if (include.aic) {
    res <-
      rbind(res,
            c(Test = "AIC",
              statistic = Format2(AIC(x),1)  ))
    
  }
  
  
  if (include.bic) {
    res <-
      rbind(res,
            c(Test = "BIC",
              statistic = Format2(BIC(x),1)  ))
    
  }
  

  
  if (include.rmse) {
    res <-
      rbind(res,
            c(Test =  "RMSE",
              statistic = Format2(RMSE(x)[1,2],2)
            ))
  }
  
    if (include.sigma) {
    res <-
      rbind(res,
            c(Test = "Sigma",
              statistic = ifelse( type_lm |type_glm, Format2(RMSE(x)[1,1],2),
                                  ifelse( type_lmer,  Format2(xs$sigma, 2)))
              
            ))
  }
  
  
  if (include.residual) {
    res <-
      rbind(res,
            c(Test = "Var: Residual",
              statistic = ifelse( type_lm |type_glm, Format2(RMSE(x)[1,1]^2,2),
                                  ifelse( type_lmer,  Format2(xs$sigma^2, 2)))
              
            ))
  }
  
  
  # if(include.vif) VIF2(x)
 #cat("\n end \n")
  rbind(res[-1,],  
        res[1,]) 
  
}




## Kopien von require(sjstats)
#  p.val < 0.05 => "Non-normality
test_normality <- function(x) {
  # bei  sjstats ist im orginal stats::rstandard ich verwende aber resid
  APA( stats::shapiro.test(stats::resid(x)) )
}



test_multicollin <- function(x) {
  
  res<-""

    # check for autocorrelation
    ts <- sqrt(car::vif(x)) > 2
    
    if (any(ts)) {
      mp <- paste(sprintf("%s", names(ts)), collapse = ", ")
      res<-  paste0("Multicollinearity detected for following predictors: ", mp)
    } else {
      res <- "No multicollinearity detected."
    }
    

  res
}

test_nonconstvar <- function(model) {
  sumry <- summary(model)
  
  residuals <- stats::residuals(model, type = "pearson")
  S.sq <- stats::df.residual(model) * (sumry$sigma) ^ 2 / sum(!is.na(residuals))
  
  .U <- (residuals ^ 2) / S.sq
  mod <- lm(.U ~ fitted.values(model))
  
  SS <- stats::anova(mod)$"Sum Sq"
  RegSS <- sum(SS) - SS[length(SS)]
  Chisq <- RegSS / 2
  
  stats::pchisq(Chisq, df = 1, lower.tail = FALSE)
}



#' @rdname APA_
#' @description 
#' \strong{APA_Validation}
#' Validation of Linear Models Assumptions
#' (Testing Linear Regression Models)
#' 
#' \itemize{
#' \item{loglik} Likelihood Ratio Test The log-likelihood from the intercept-only restricted model.
#' -2LL: The LL (log-likelihood from the fitted model)
#' llhNull	(The log-likelihood from the intercept-only restricted model),
#' G2	(Minus two times the difference in the log-likelihoods)
#'
#' \item{Autocorrelation} Durbin-Watson test for autocorrelation of disturbances. Ist nur bei Zeitreihendaten sinnvoll.
#' 
#' \item{Homogeneity of Variances} Levene Computes Levene's test for homogeneity of variance across groups.
#'  Bartlett Test of Homogeneity of Variances
#' 
#' \item{Heteroskedasticity} Breusch-Pagan test against heteroskedasticity.
#' 
#' \item{VIF} variance inflation factor. VIF values over 5 are troubling, should probably investigate anything over 2.5.
#' 
#' \item{Residual} RMSE values should be low (<0.5 and <0.3, respectively).
#'  SigmaResidual standard error  When the residual standard error is exactly 0 then the model fits the data perfectly (likely due to overfitting)
#' \item{ R-Quadrats } Cox und Snell R2: [ 0.2 = akzeptabel, 0.4 = gut ] 
#' Nagelkerke R2: [ 0.2 = akzeptabel, 0.4 = gut, 0.5 = sehr gut] 
#' McFaddens R2: [ 0.2 = akzeptabel, 0.4 = gut ] (see pR2)
#' }
#' @param x model- fit
#' @param include.ftest,include.loglik  F-sratistik
#' @param include.heteroskedasticity Breusch-Pagan test
#' @param include.r,include.pseudo R-Quadrat
#' @param include.durbin  autocorrelation
#' @param include.levene  homogeneity of variance across groupssiehe T-Test
#' @param include.bartlett Homogeneity of Variances siehe T-Test
#' @param include.vif noch nicht Implementiert
#' @param include.sigma,include.rmse  RMSE Extract Residual Standard Deviation Sigma
 
#' @export
#'
#' @examples 
#' 
#' hkarz$Lai <- factor(hkarz$lai, 0:1, Cs(.neg, .pos))
#' fit2 <- glm(gruppe ~ tzell + Lai, hkarz, family = binomial)
#' fit1<-lm(score ~ grade + treatment + stdTest, schools)
#' APA_Validation(x) 
#' APA_Validation(fit1,fit2, include.pseudo = TRUE, include.r = TRUE, include.loglik = TRUE,
#'                include.rmse = TRUE)
#'                
#'                

APA_Validation<- function(...,
                          include.ftest=TRUE,include.loglik=FALSE,include.minus.LL = include.loglik,
                          include.pseudo=TRUE, include.r=include.pseudo, #R_qsuared
                          include.heteroskedasticity = TRUE,
                          include.durbin = TRUE,
                          include.levene = FALSE,
                          include.bartlett = FALSE,
                          include.vif=FALSE,
                          include.sigma=FALSE,
                          include.rmse=FALSE,
                          include.aic=TRUE, include.bic=include.aic,include.residual=TRUE,
                          include.normality=TRUE,
                          include.multicollin=include.vif,
                          
                          include.deviance=TRUE,
                          
                          caption = "Testing Regression Models",
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
  n <- length(myfits)
  
  custom.model.names <- custom_model_names()
  
  res <- test_regression(myfits[[1]],
                         include.ftest,include.loglik,include.minus.LL,
                         include.r, 
                         include.heteroskedasticity,  
                         include.durbin,
                         include.levene,
                         include.bartlett,
                         include.vif,
                         include.sigma, include.rmse,include.aic,
                         include.bic,include.residual,
                         include.normality,
                         include.multicollin
                         ,include.deviance
                         )
  
  if (n > 1) {
    for (i in 2:n) {
      res <- cbind(
        res,
        test_regression(
          myfits[[i]],
          include.ftest,include.loglik,include.minus.LL,
          include.r,
          include.heteroskedasticity,  
          include.durbin,
          include.levene,
          include.bartlett,
          include.vif,include.sigma, include.rmse,include.aic,
          include.bic,include.residual,
          include.normality,
          include.multicollin
          ,include.deviance
        )[2]
      )
    }
    names(res) <- c("Test", custom.model.names)
    
  }
  
  cat("\n ende Schleife\n")
  print(res)
  res <- prepare_output(res, caption, note)
 
  Output(res)
  invisible(res) 
} 
