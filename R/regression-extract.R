# The texreg package was written by Philip Leifeld.
# Please use the issue tracker at http://github.com/leifeld/texreg
# for bug reports, help or feature requests.


# generic Extract2 function
# setGeneric("Extract2", function(model, ...) standardGeneric("Extract2"),
#            package = "texreg")





extract_param <- function(x, 
                          include.b=TRUE,
                          include.ci = FALSE,
                          include.odds = FALSE,
                          include.se = if (include.ci) FALSE else TRUE,
                          include.statistic= FALSE,
                          include.p = FALSE,
                          include.stars = if (include.p) FALSE else TRUE,
                          include.beta=FALSE
){
  coef.names=character(0)
  coef=numeric(0)
  se = NA
  pvalues = NA
  stars = NA
  statistic =  NA
  ci.low = NA
  ci.up = NA
  
  or=NA
  or.ci.low = NA
  or.ci.up = NA
  
  beta= NA
  
  
  data.frame(
    coef.names = coef.names,
    coef = coef,
    se = se,
    pvalues = pvalues,
    stars = stars,
    statistic = statistic,
    ci.low = ci.low,
    ci.up = ci.up,
    or=or,
    or.ci.low = or.ci.low,
    or.ci.up = or.ci.up,
    beta=beta,
    stringsAsFactors =FALSE
  ) 
  
}



extract_gof <- function(x,
                        include.r = TRUE, 
                        include.pseudo = TRUE,

                        include.rmse = TRUE, 
                        include.sigma=FALSE,
                       
                        include.variance = FALSE,
                        include.devianze = FALSE,
                        include.loglik = FALSE,
                        include.aic = TRUE,
                        include.bic = include.aic,
                        include.nobs = TRUE
                        
                        
){
  
  
  
  
  
  
}

#' @rdname Extract2
#' @description Extract2 function by Philip Leifeld http://github.com/leifeld/texreg
#' @export
Extract2<- function(model, ...){
  UseMethod("Extract2")
  
}



createTexreg <- function(coef.names=character(0),
                         coef=numeric(0),
                         se = NA,
                         pvalues = NA,
                         statistic =  NA,
                         ci.low = NA,
                         ci.up = NA,
                         
                         or=NA,
                         or.ci.low = NA,
                         or.ci.up = NA,
                         
                         beta= NA,
                         
                         
                         default.gof=c(
                           R2= "R2",        
                           adjR2 ="adj. R2",   
                           McFadden = "R2 McFadden",
                           NoxSnell ="Cox & Snell",
                           Nagelkerke ="Nagelkerke",
                           Marginal="Marginal", 
                           Conditional= "Conditional",
                           Sigma="sigma",
                           RMSE="RMSE", 
                           AIC= "AIC",
                           BIC="BIC",
                           LogLik="Log Likelihood",
                           Deviance="Deviance",
                           FStat="F statistic",
                           NumObs= "Num. Obs."
                           
                           # NumGroups= "Num Groups",
                           # VarResidual="Var: Residual",
                           # Var= "Var:",
                           # Cov="Cov:"
                           

                         ),
                         
                         
                         
                         gof.names = character(0),
                         gof = numeric(0),
                         gof.decimal = logical(0),
                         model.name = character(0)) {
  
  
  
  
  GOF <- data.frame(gof = default.gof)
  GOF$value <- NA
  which_to_fil <-  match(gof.names, rownames(GOF)) 
  which_to_fil_value <-  which(!is.na(which_to_fil )) 
  which_extra <-   which(is.na(which_to_fil )) 
  
  GOF_extra <-  data.frame( gof =gof.names[which_extra], value= gof[which_extra] )
  
  GOF[na.omit(which_to_fil), "value"] <- round(gof[which_to_fil_value] ,2)
  
  
  
  list(
    param = data.frame(
      coef.names = coef.names,
      coef = coef,
      se = se,
      pvalues = pvalues,
      tvalue = tvalue,
      
      ci.low = ci.low,
      ci.up = ci.up,
      
      or=or,
      or.ci.low = or.ci.low,
      or.ci.up = or.ci.up,
      
      beta=beta
      
      
      
    ),
    gof = GOF,
    gof_extra= GOF_extra
  )
}

#' @rdname Extract2
#' @export
#' @description  default Extract2 method prompts users to install the broom package
Extract2.default <- function(model,  
                             include.rsquared = TRUE, 
                             include.adjrs = TRUE,
                             include.nobs = TRUE, 
                             include.statistic = FALSE, 
                             include.rmse = TRUE, 
                             include.sigma=FALSE,
                             
                             include.beta = TRUE,
                             include.t =TRUE,
                             include.ci = FALSE,
                             include.p = TRUE,
                             include.stars =  TRUE,
                             include.variance = FALSE,
                             include.aic=TRUE,
                             include.bic=TRUE,
                             ci.level=.95,
                             ...) {
  # if (!'broom' %in% row.names(installed.packages())) {
  #   stop("texreg does not directly support models of class ",
  #        class(model),
  #        ", but it can sometimes use the ``broom`` package to Extract2 model information. Call texreg again after installing the ``broom`` package to see if this is possible.")
  # }
  coefficients <- try(broom_coefficients(model), silent = TRUE)
  gof <- try(broom_gof(model), silent = TRUE)
  if ((class(coefficients) == 'try-error') || (class(gof) == 'try-error')) {
    stop('Neither texreg nor broom supports models of class ', class(model), '.')
  }
  
  
  
  ci.up <- ci.low <- NA  
  if (include.ci)  {
    cis <- confint(model, level = ci.level)
    ci.low <- cis[,1]
    ci.up <- cis[,2]
  }
  
  
  if (include.rmse ) {
    
    gof$gof <- RMSE(model)$RMSE
    gof$gof.names <-   c(gof$gof.names, "RMSE")
    #gof.decimal <- c(gof.decimal, TRUE)
  }
  
   createTexreg(coef.names = coefficients$term,
                     coef = coefficients$estimate,
                     se = coefficients$std.error,
                     pvalues = coefficients$p.value,
                     statistic= coefficients$statistic,
                  
                ci.low = ci.low,
                ci.up = ci.up,
                  
                  
                     
                     gof.names = gof$gof.names,
                     gof = gof$gof 
  )
 
}

 

# Extract2 coefficients using the broom package
broom_coefficients <- function(x) {
  out <- broom::tidy(x)
  out <- out[, c('term', 'estimate', 'std.error', 'statistic', 'p.value')]
  return(out)
}

# Extract2 gof using the broom package
broom_gof <- function(x) {
  # Extract2
  out <- broom::glance(x)[1, ]
  gof.decimal <- sapply(out, function(k) class(k)[1]) # type inference
  gof.decimal <- ifelse(gof.decimal %in% c('integer', 'logical'), FALSE, TRUE)
  out <- data.frame('gof.names' = colnames(out),
                    'gof' = as.numeric(out),
                    'gof.decimal' = gof.decimal,
                    stringsAsFactors = FALSE)
  # rename
  gof_dict <- c(
    'adj.r.squared' = 'adjR2',
    'deviance' = 'Deviance',
    'df' = 'DF',
    'df.residual' = 'DF Resid.',
    'finTol' = 'Tolerance',
    'isConv' = 'Convergence',
    'logLik' = 'LogLik',
    'null.deviance' = 'Deviance (Null)',
    'p.value' = 'P Value',
    'r.squared' = 'R2',
    'sigma' = 'Sigma',
    'statistic' = 'Statistic'
  )
  gof_dict <- gof_dict[names(gof_dict) %in% out$gof.names]
  idx <- match(names(gof_dict), out$gof.names)
  out$gof.names[idx] <- gof_dict
  if (any(is.na(out$gof))) {
    warning('texreg used the broom package to Extract2 the following GOF measures, 
            but could not cast them to numeric type: ',
            out$gof.names[is.na(out$gof)])
  }
  out <- stats::na.omit(out)
  # output
  return(out)
}




#' @rdname Extract2
#' @export
#' @description   extension for lm objects
Extract2.lm <- function(model, 
                       include.rsquared = TRUE, 
                       include.adjrs = TRUE,
                       include.nobs = TRUE, 
                       include.statistic = FALSE, 
                       include.rmse = TRUE, 
                       include.sigma=FALSE,
                       
                       include.beta = TRUE,
                       include.t =TRUE,
                       include.ci = FALSE,
                       include.p = TRUE,
                       include.stars =  TRUE,
                       include.variance = FALSE,
                       include.aic=TRUE,
                       include.bic=TRUE,
                       ci.level=.95,
                       
                       
                       ...) {

  s <- summary(model, ...)
  
  names <- rownames(s$coefficients)
  co <- s$coefficients[, 1]
  se <- s$coefficients[, 2]
  pval <- s$coefficients[, 4]
  tval <- co/se
  ci.up <- ci.low <- NA  
  if (include.ci)  {
    cis <- confint(model, level = ci.level)
    ci.low <- cis[,1]
    ci.up <- cis[,2]
    }

  gof <- numeric()
  gof.names <- character()
  gof.decimal <- logical()

  rs <- s$r.squared  #Extract2 R-squared
  adj <- s$adj.r.squared  #Extract2 adjusted R-squared
  n <- nobs(model)  #Extract2 number of observations
  
  if (include.rsquared ) {
    gof <- c(gof, rs)
    gof.names <- c(gof.names, "R2")
   # gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.adjrs ) {
    gof <- c(gof, adj)
    gof.names <- c(gof.names, "adjR2")
   # gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.nobs ) {
    gof <- c(gof, n)
    gof.names <- c(gof.names, "NumObs")
  #  gof.decimal <- c(gof.decimal, FALSE)
  }
  if (include.statistic ) {
    fstat <- s$fstatistic[[1]]
    gof <- c(gof, fstat)
    gof.names <- c(gof.names, "FStat")
    #gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.rmse ) {
 
    gof <- c(gof, sqrt(mean(model$residuals^2)))
    gof.names <-   c(gof.names, "RMSE")
    #gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.sigma ) {
    
    gof <- c(gof, s$sigma[[1]])
    gof.names <- c(gof.names, "sigma")
    #gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.aic ) {
    
    gof <- c(gof,  stats::AIC(model))
    gof.names <- c(gof.names, "AIC")
 
  }
  if (include.bic ) {
    
    gof <- c(gof,  stats::BIC(model))
    gof.names <- c(gof.names, "BIC")
    
  }
 
  
  createTexreg(
      coef.names = names,
      coef = co,
      se = se,
      pvalues = pval,
      
      ci.low = ci.low,
      ci.up = ci.up,
      statistic=tval,
 
      gof.names = gof.names,
      gof = gof,
      gof.decimal = gof.decimal
   
  )
  
}


#' @rdname Extract2
#' @export
#' @description   extension for glm objects
Extract2.glm <- function(model, 
                         include.odds=TRUE,
                         include.ci = FALSE,
                         include.pseudo=TRUE,
                         include.aic = TRUE, 
                         include.bic = TRUE,
                         include.loglik = TRUE, 
                         include.deviance = TRUE, 
                         include.nobs = TRUE, ...) {
  s <- summary(model, ...)
  
  coefficient.names <- rownames(s$coef)
  coefficients <- s$coef[, 1]
  standard.errors <- s$coef[, 2]
  significance <- s$coef[, 4]
  
 
  if(include.odds) {
    or <- exp( coefficients )
     
  }

  
  
  n <- nobs(model)
  
  gof <- numeric()
  gof.names <- character()
  gof.decimal <- logical()
  if (include.aic == TRUE) {
    
     aic <- stats::AIC(model)
    gof <- c(gof, aic)
    gof.names <- c(gof.names, "AIC")
   # gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.bic == TRUE) {
    bic <- stats::BIC(model)
    
    gof <- c(gof, bic)
    gof.names <- c(gof.names, "BIC")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.loglik == TRUE) {
    
      lik <- stats::logLik(model)[1]
    gof <- c(gof, lik)
    gof.names <- c(gof.names, "LogLik")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.deviance == TRUE) {
    
    dev <- stats::deviance(model)
    gof <- c(gof, dev)
    gof.names <- c(gof.names, "Deviance")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.nobs == TRUE) {
    gof <- c(gof, n)
    gof.names <- c(gof.names, "NumObs")
    gof.decimal <- c(gof.decimal, FALSE)
  }
  
  tr <- createTexreg(
    coef.names = coefficient.names,
    coef = coefficients,
    se = standard.errors,
    pvalues = significance,
    gof.names = gof.names,
    gof = gof,
    gof.decimal = gof.decimal
  )
  return(tr)
}






Extract2.dynlm <- Extract2.lm


Extract2.ivreg <- Extract2.lm



#' @rdname Extract2
#' @export
#' @description   extension for lme objects
Extract2.lme <- function(model, include.aic = TRUE, include.bic = TRUE,
                        include.loglik = TRUE, include.nobs = TRUE, include.groups = TRUE,
                        include.variance = FALSE, ...) {
  
  s <- summary(model, ...)
  
  coefficient.names <- rownames(s$tTable)
  coefficients <- s$tTable[, 1]
  standard.errors <- s$tTable[, 2]
  significance <- s$tTable[, 5]
  
  gof <- numeric()
  gof.names <- character()
  gof.decimal <- logical()
  if (include.aic == TRUE) {
    aic <- s$AIC
    gof <- c(gof, aic)
    gof.names <- c(gof.names, "AIC")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.bic == TRUE) {
    bic <- s$BIC
    gof <- c(gof, bic)
    gof.names <- c(gof.names, "BIC")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.loglik == TRUE) {
    lik <- s$logLik
    gof <- c(gof, lik)
    gof.names <- c(gof.names, "LogLik")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.nobs == TRUE) {
    n <- nobs(model)
    gof <- c(gof, n)
    gof.names <- c(gof.names, "NumObs")
    gof.decimal <- c(gof.decimal, FALSE)
  }
  if (include.groups == TRUE) {
    grp <- model$dims$ngrps[1:model$dims$Q]
    gof <- c(gof, grp)
    gof.names <- c(gof.names, "NumGroups")
    gof.decimal <- c(gof.decimal, FALSE)
  }
  if (include.variance == TRUE ) {
    sig.all <- s$sigma
    if (!is.null(sig.all) && !is.na(sig.all)) {
      gof <- c(gof, sig.all)
      gof.names <- c(gof.names, "sigma")
      gof.decimal <- c(gof.decimal, TRUE)
    }
    
    vc <- nlme::VarCorr(model)
    if ("(Intercept)" %in% rownames(vc) && "StdDev" %in% colnames(vc)) {
      sig.RE <- as.numeric(vc["(Intercept)", "StdDev"])
      if (!is.null(sig.RE) && !is.na(sig.RE)) {
        gof <- c(gof, sig.RE)
        gof.names <- c(gof.names, "sigma.\ RE")
        gof.decimal <- c(gof.decimal, TRUE)
      }
    }
    
    cf <- coef(model$modelStruct, unconstrained = FALSE)["corStruct.Phi1"]
    rho <- unname(cf)
    if (!is.null(rho) && !is.na(rho)) {
      gof <- c(gof, rho)
      gof.names <- c(gof.names, "rho")
      gof.decimal <- c(gof.decimal, TRUE)
    }
  }
  
  tr <- createTexreg(
    coef.names = coefficient.names,
    coef = coefficients,
    se = standard.errors,
    pvalues = significance,
    gof.names = gof.names,
    gof = gof,
    gof.decimal = gof.decimal
  )
  return(tr)
}


Extract2.nlme <- Extract2.lme







Extract2.brglm <- Extract2.glm

Extract2.negbin <- Extract2.glm





#' @rdname Extract2
#' @export
#' @description   extension for lme4 (+ mer, lmerMod, glmerMod, nlmerMod) objects (lme4 package)
Extract2.lme4 <- function(model, 
                          method = c("naive", "profile", "boot", "Wald"),
                         level = 0.95, nsim = 1000, 
                         include.aic = TRUE, 
                         include.bic = TRUE,
                         include.dic = FALSE, 
                         include.deviance = FALSE, 
                         include.loglik = TRUE,
                         include.nobs = TRUE, 
                         include.groups = TRUE, 
                         include.variance = TRUE, ...) {
  
  if (packageVersion("lme4") < 1.0) {
    message("Please update to a newer 'lme4' version for full compatibility.")
  }
  
  gof <- numeric()
  gof.names <- character()
  gof.decimal <- logical()
  if (include.aic == TRUE) {
    aic <- AIC(model)
    gof <- c(gof, aic)
    gof.names <- c(gof.names, "AIC")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.bic == TRUE) {
    bic <- BIC(model)
    gof <- c(gof, bic)
    gof.names <- c(gof.names, "BIC")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.dic == TRUE) {  # code from the arm package, version 1.7-07
    is_REML <- lme4::isREML(model)
    llik <- logLik(model, REML = is_REML)
    dev <- deviance(lme4::refitML(model))
    n <-  lme4::getME(model, "devcomp")$dims["n"]
    Dhat <- -2 * (llik)
    pD <- dev - Dhat
    DIC <- dev + pD[[1]]
    gof <- c(gof, DIC)
    gof.names <- c(gof.names, "DIC")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.deviance == TRUE) {
    dev <- deviance(lme4::refitML(model))
    gof <- c(gof, dev)
    gof.names <- c(gof.names, "Deviance")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.loglik == TRUE) {
    lik <- logLik(model)[1]
    gof <- c(gof, lik)
    gof.names <- c(gof.names, "LogLik")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.nobs == TRUE) {
    n <- dim(model.frame(model))[1]
    gof <- c(gof, n)
    gof.names <- c(gof.names, "NumObs")
    gof.decimal <- c(gof.decimal, FALSE)
  }
  if (include.groups == TRUE) {
    grps <- sapply(model@flist, function(x) length(levels(x)))
    grp.names <- names(grps)
    grp.names <- paste("NumGroups", grp.names)
    gof <- c(gof, grps)
    gof.names <- c(gof.names, grp.names)
    gof.decimal <- c(gof.decimal, rep(FALSE, length(grps)))
  }
  if (include.variance == TRUE) {
    vc <- as.data.frame(lme4::VarCorr(model))
    for (i in 1:nrow(vc)) {
      if (is.na(vc[i, 2]) && is.na(vc[i, 3])) {
        gof.names <- c(gof.names, "Var: Residual")
      } else if (is.na(vc[i, 3])) {
        gof.names <- c(gof.names, paste("Var", vc[i, 1], vc[i, 2]))
      } else {
        gof.names <- c(gof.names, paste("Cov", vc[i, 1], vc[i, 2], vc[i, 3]))
      }
      gof <- c(gof, vc[i, 4])
      gof.decimal <- c(gof.decimal, TRUE)
    }
    #    vc <- lme4::VarCorr(model)
    #    varcomps <- c(unlist(lapply(vc, diag)),   # random intercept variances
    #        attr(vc, "sc")^2)                     # residual variance
    #    varnames <- names(varcomps)
    #    varnames[length(varnames)] <- "Residual"
    #    varnames <- paste("Variance:", varnames)
    #    if (is.na(attr(vc, "sc"))) {
    #      varnames <- varnames[-length(varnames)]
    #      varcomps <- varcomps[-length(varcomps)]
    #    }
    #    gof <- c(gof, varcomps)
    #    gof.names <- c(gof.names, varnames)
    #    gof.decimal <- c(gof.decimal, rep(TRUE, length(varcomps)))
  }
  
  betas <- lme4::fixef(model, ...)
  if ("confint.merMod" %in% methods("confint") && method[1] != "naive") {
    ci <- tryCatch({
      ci <- confint(model, method = method[1], level = level, nsim = nsim,
                    ...)
    },
    error = function(err) {
      method <- "naive"
      message(paste("Confidence intervals not available for",
                    "this model. Using naive p values instead."))
    }
    )
    if (is.null(ci)) {
      method <- "naive"
    } else {
      last <- nrow(ci)
      number <- length(betas)
      first <- last - number + 1
      ci <- ci[first:last, ]
      if (class(ci) == "matrix") {
        ci.l <- ci[, 1]
        ci.u <- ci[, 2]
      } else {
        ci.l <- ci[1]
        ci.u <- ci[2]
      }
    }
  } else if (method[1] != "naive") {
    method[1] <- "naive"
    message(paste("confint.merMod method not found. Using naive p values",
                  "instead."))
  }
  
  if (method[1] == "naive") {
    Vcov <- tryCatch({
      Vcov <- vcov(model, useScale = FALSE, ...)
    }, error = function(err) {  # Matrix package is sometimes used internally...
      stop(paste("Please load the Matrix package or update to the latest",
                 "development version of lme4 and run this command again."))
    })
    Vcov <- as.matrix(Vcov)
    se <- sqrt(diag(Vcov))
    zval <- betas / se
    pval <- 2 * pnorm(abs(zval), lower.tail = FALSE)
    
    tr <- createTexreg(
      coef.names = names(betas),
      coef = betas,
      se = se,
      pvalues = pval,
      gof.names = gof.names,
      gof = gof,
      gof.decimal = gof.decimal
    )
  } else {
    tr <- createTexreg(
      coef.names = names(betas),
      coef = betas,
      ci.low = ci.l,
      ci.up = ci.u,
      gof.names = gof.names,
      gof = gof,
      gof.decimal = gof.decimal
    )
  }
  
  return(tr)
}



Extract2.mer <- Extract2.lme4


Extract2.lmerMod <- Extract2.lme4

Extract2.glmerMod <- Extract2.lme4

Extract2.nlmerMod <- Extract2.lme4



#' @rdname Extract2
#' @export
#' @description   extension for coxph objects (survival package)
Extract2.coxph <- function(model, include.aic = TRUE, include.rsquared = TRUE,
                          include.maxrs = TRUE, include.events = TRUE, include.nobs = TRUE,
                          include.missings = TRUE, include.zph = TRUE, ...) {
  s <- summary(model, ...)
  
  coefficient.names <- rownames(s$coef)
  coefficients <- s$coef[, 1]
  if (is.null(model$naive.var)) {
    standard.errors <- s$coef[, 3]
    significance <- s$coef[, 5]
  } else {
    standard.errors <- s$coef[, 4]
    significance <- s$coef[, 6]
  }
  
  aic <- Extract2AIC(model)[2]
  event <- model$nevent
  n <- model$n
  mis <- length(model$na.action)
  rs <- s$rsq[1]
  maxrs <- s$rsq[2]
  
  gof <- numeric()
  gof.names <- character()
  gof.decimal <- logical()
  if (include.aic == TRUE) {
    gof <- c(gof, aic)
    gof.names <- c(gof.names, "AIC")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.rsquared == TRUE) {
    gof <- c(gof, rs)
    gof.names <- c(gof.names, "R2")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.maxrs == TRUE) {
    gof <- c(gof, maxrs)
    gof.names <- c(gof.names, "MaxR2")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.events == TRUE) {
    gof <- c(gof, event)
    gof.names <- c(gof.names, "Num. events")
    gof.decimal <- c(gof.decimal, FALSE)
  }
  if (include.nobs == TRUE) {
    gof <- c(gof, n)
    gof.names <- c(gof.names, "NumObs")
    gof.decimal <- c(gof.decimal, FALSE)
  }
  if (include.missings == TRUE) {
    gof <- c(gof, mis)
    gof.names <- c(gof.names, "Missings")
    gof.decimal <- c(gof.decimal, FALSE)
  }
  if (include.zph == TRUE) {
    zph <- survival::cox.zph(model)$table
    zph <- zph[length(zph[, 1]), length(zph[1, ])]
    gof <- c(gof, zph)
    gof.names <- c(gof.names, "PH test")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  
  tr <- createTexreg(
    coef.names = coefficient.names,
    coef = coefficients,
    se = standard.errors,
    pvalues = significance,
    gof.names = gof.names,
    gof = gof,
    gof.decimal = gof.decimal
  )
  return(tr)
}


#' @rdname Extract2
#' @export
#' @description   extension for coxph.penal objects (survival package)
Extract2.coxph.penal <- function(model, include.aic = TRUE,
                                include.rsquared = TRUE, include.maxrs = TRUE, include.events = TRUE,
                                include.nobs = TRUE, include.missings = TRUE, include.zph = TRUE, ...) {
  
  coefficients <- coef(model, ...)
  coefficient.names <- names(coefficients)
  standard.errors <- sqrt(diag(model$var))
  significance <- 1 - pchisq(model$coefficients^2 / diag(model$var), 1)
  
  aic <- Extract2AIC(model)[2]
  event <- model$nevent
  n <- model$n
  mis <- length(model$na.action)
  logtest <- -2 * (model$loglik[1] - model$loglik[2])
  rs <- 1 - exp( - logtest / model$n)
  maxrs <- 1 - exp((2 * model$loglik[1]) / model$n)
  
  gof <- numeric()
  gof.names <- character()
  gof.decimal <- logical()
  if (include.aic == TRUE) {
    gof <- c(gof, aic)
    gof.names <- c(gof.names, "AIC")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.rsquared == TRUE) {
    gof <- c(gof, rs)
    gof.names <- c(gof.names, "R2")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.maxrs == TRUE) {
    gof <- c(gof, maxrs)
    gof.names <- c(gof.names, "MaxR2")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.events == TRUE) {
    gof <- c(gof, event)
    gof.names <- c(gof.names, "Num. events")
    gof.decimal <- c(gof.decimal, FALSE)
  }
  if (include.nobs == TRUE) {
    gof <- c(gof, n)
    gof.names <- c(gof.names, "NumObs")
    gof.decimal <- c(gof.decimal, FALSE)
  }
  if (include.missings == TRUE) {
    gof <- c(gof, mis)
    gof.names <- c(gof.names, "Missings")
    gof.decimal <- c(gof.decimal, FALSE)
  }
  if (include.zph == TRUE) {
    zph <- survival::cox.zph(model)$table
    zph <- zph[length(zph[, 1]), length(zph[1, ])]
    gof <- c(gof, zph)
    gof.names <- c(gof.names, "PH test")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  
  tr <- createTexreg(
    coef.names = coefficient.names,
    coef = coefficients,
    se = standard.errors,
    pvalues = significance,
    gof.names = gof.names,
    gof = gof,
    gof.decimal = gof.decimal
  )
  return(tr)
}


#' @rdname Extract2
#' @export
#' @description   extension for clogit objects (survival package)
Extract2.clogit <- function(model, include.aic = TRUE, include.rsquared = TRUE,
                           include.maxrs = TRUE, include.events = TRUE, include.nobs = TRUE,
                           include.missings = TRUE, ...) {
  s <- summary(model, ...)
  
  coefficient.names <- rownames(s$coef)
  coefficients <- s$coef[, 1]
  if (is.null(model$naive.var)) {
    standard.errors <- s$coef[, 3]
    significance <- s$coef[, 5]
  } else {
    standard.errors <- s$coef[, 4]
    significance <- s$coef[, 6]
  }
  
  aic <- Extract2AIC(model)[2]
  event <- model$nevent
  n <- model$n
  mis <- length(model$na.action)
  rs <- s$rsq[1]
  maxrs <- s$rsq[2]
  
  gof <- numeric()
  gof.names <- character()
  gof.decimal <- logical()
  if (include.aic == TRUE) {
    gof <- c(gof, aic)
    gof.names <- c(gof.names, "AIC")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.rsquared == TRUE) {
    gof <- c(gof, rs)
    gof.names <- c(gof.names, "R2")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.maxrs == TRUE) {
    gof <- c(gof, maxrs)
    gof.names <- c(gof.names, "MaxR2")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.events == TRUE) {
    gof <- c(gof, event)
    gof.names <- c(gof.names, "Num. events")
    gof.decimal <- c(gof.decimal, FALSE)
  }
  if (include.nobs == TRUE) {
    gof <- c(gof, n)
    gof.names <- c(gof.names, "NumObs")
    gof.decimal <- c(gof.decimal, FALSE)
  }
  if (include.missings == TRUE) {
    gof <- c(gof, mis)
    gof.names <- c(gof.names, "Missings")
    gof.decimal <- c(gof.decimal, FALSE)
  }
  
  tr <- createTexreg(
    coef.names = coefficient.names,
    coef = coefficients,
    se = standard.errors,
    pvalues = significance,
    gof.names = gof.names,
    gof = gof,
    gof.decimal = gof.decimal
  )
  return(tr)
}


#' @rdname Extract2
#' @export
#' @description   extension for coeftest objects (lmtest package)
Extract2.coeftest <- function(model, ...) {
  
  names <- rownames(model)
  co <- model[, 1]
  se <- model[, 2]
  pval <- model[, 4]
  
  tr <- createTexreg(
    coef.names = names,
    coef = co,
    se = se,
    pvalues = pval
  )
  return(tr)
}






# extension for Arima objects (stats package)
Extract2.Arima <- function(model, include.pvalues = FALSE, include.aic = TRUE,
                          include.loglik = TRUE, ...) {
  
  mask <- model$mask
  nam <- names(model$coef)
  co <- model$coef
  sdev <- sqrt(diag(model$var.coef))
  
  if (include.pvalues == TRUE) {
    t.rat <- rep(NA, length(mask))
    t.rat[mask] <- co[mask] / sdev
    pt <- 2 * pnorm(-abs(t.rat))
    setmp <- rep(NA, length(mask))
    setmp[mask] <- sdev
  } else {
    pt <- numeric()
    setmp <- sdev
  }
  
  gof <- numeric()
  gof.names <- character()
  gof.decimal <- logical()
  if (include.aic == TRUE) {
    aic <- AIC(model)
    gof <- c(gof, aic)
    gof.names <- c(gof.names, "AIC")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.loglik == TRUE) {
    lik <- model$loglik
    gof <- c(gof, lik)
    gof.names <- c(gof.names, "LogLik")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  
  tr <- createTexreg(
    coef.names = nam,
    coef = co,
    se = setmp,
    pvalues = pt,
    gof.names = gof.names,
    gof = gof,
    gof.decimal = gof.decimal
  )
  return(tr)
}

 
# extension for ARIMA objects (forecast package)
Extract2.ARIMA <- function (model, include.pvalues = FALSE, include.aic = TRUE,
                           include.aicc = TRUE, include.bic = TRUE, include.loglik = TRUE, ...) {
  mask <- model$mask
  nam <- names(model$coef)
  co <- model$coef
  sdev <- sqrt(diag(model$var.coef))
  if (include.pvalues == TRUE) {
    t.rat <- rep(NA, length(mask))
    t.rat[mask] <- co[mask] / sdev
    pt <- 2 * pnorm(-abs(t.rat))
    setmp <- rep(NA, length(mask))
    setmp[mask] <- sdev
  } else {
    pt <- numeric()
    setmp <- sdev
  }
  gof <- numeric()
  gof.names <- character()
  gof.decimal <- logical()
  if (include.aic == TRUE) {
    aic <- AIC(model)
    gof <- c(gof, aic)
    gof.names <- c(gof.names, "AIC")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.aicc == TRUE) {
    gof <- c(gof, model$aicc)
    gof.names <- c(gof.names, "AICc")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.bic == TRUE) {
    gof <- c(gof, model$bic)
    gof.names <- c(gof.names, "BIC")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.loglik == TRUE) {
    lik <- model$loglik
    gof <- c(gof, lik)
    gof.names <- c(gof.names, "LogLik")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  tr <- createTexreg(
    coef.names = nam,
    coef = co,
    se = setmp,
    pvalues = pt,
    gof.names = gof.names,
    gof = gof,
    gof.decimal = gof.decimal
  )
  return(tr)
}

#  
# # extension for averaging objects (MuMIn package)
# Extract2.averaging <- function(model, use.ci = FALSE, adjusted.se = FALSE,
#                               include.nobs = TRUE, ...) {
#   
#   # MuMIn >= 1.15.0 : c('coefmat.subset', 'coefmat.full')
#   # MuMIn < 1.15.0 : c('coefmat', 'coefmat.full')
#   ct <- summary(model)$coefmat.full
#   coefs <- ct[, 1L]
#   se <- ct[, if(adjusted.se) 3L else 2L]
#   
#   if (include.nobs == TRUE) {
#     gof <- as.numeric(attr(model, "nobs"))
#     gof.names <- "NumObs"
#     gof.decimal <- FALSE
#   } else {
#     gof <- numeric(0L)
#     gof.names <- character(0L)
#     gof.decimal <- logical(0L)
#   }
#   
#   if (use.ci == TRUE) {
#     ci <- confint(model, full = TRUE)
#     tr <- createTexreg(
#       coef.names = names(coefs),
#       coef = coefs,
#       ci.low = ci[, 1L],
#       ci.up = ci[, 2L],
#       gof.names = gof.names,
#       gof = gof,
#       gof.decimal = gof.decimal
#     )
#   } else {
#     tr <- createTexreg(
#       coef.names = names(coefs),
#       coef = coefs,
#       se = se,
#       pvalues = ct[, 5L],
#       gof.names = gof.names,
#       gof = gof,
#       gof.decimal = gof.decimal
#     )
#   }
#   return(tr)
# }
# 
#  
# # extension for betamfx objects (mfx package)
# Extract2.betamfx <- function(model, include.pseudors = TRUE,
#                             include.loglik = TRUE, include.nobs = TRUE, ...) {
#   coefnames <- rownames(model$mfxest)
#   coefs <- model$mfxest[, 1]
#   se <- model$mfxest[, 2]
#   pval <- model$mfxest[, 4]
#   
#   gof <- numeric()
#   gof.names <- character()
#   gof.decimal <- logical()
#   if (include.nobs == TRUE) {
#     gof <- c(gof, model$fit$nobs)
#     gof.names <- c(gof.names, "NumObs")
#     gof.decimal <- c(gof.decimal, FALSE)
#   }
#   if (include.loglik == TRUE) {
#     gof <- c(gof, model$fit$loglik)
#     gof.names <- c(gof.names, "LogLik")
#     gof.decimal <- c(gof.decimal, TRUE)
#   }
#   if (include.pseudors == TRUE) {
#     gof <- c(gof, model$fit$pseudo.r.squared)
#     gof.names <- c(gof.names, "Pseudo R2")
#     gof.decimal <- c(gof.decimal, TRUE)
#   }
#   
#   tr <- createTexreg(
#     coef.names = coefnames,
#     coef = coefs,
#     se = se,
#     pvalues = pval,
#     gof.names = gof.names,
#     gof = gof,
#     gof.decimal = gof.decimal
#   )
#   return(tr)
# }
# 
#  
# 
# # extension for betaor objects (mfx package)
# Extract2.betaor <- function(model, include.pseudors = TRUE,
#                            include.loglik = TRUE, include.nobs = TRUE, ...) {
#   coefnames <- rownames(model$oddsratio)
#   coefs <- model$oddsratio[, 1]
#   se <- model$oddsratio[, 2]
#   pval <- model$oddsratio[, 4]
#   
#   gof <- numeric()
#   gof.names <- character()
#   gof.decimal <- logical()
#   if (include.nobs == TRUE) {
#     gof <- c(gof, model$fit$nobs)
#     gof.names <- c(gof.names, "NumObs")
#     gof.decimal <- c(gof.decimal, FALSE)
#   }
#   if (include.loglik == TRUE) {
#     gof <- c(gof, model$fit$loglik)
#     gof.names <- c(gof.names, "LogLik")
#     gof.decimal <- c(gof.decimal, TRUE)
#   }
#   if (include.pseudors == TRUE) {
#     gof <- c(gof, model$fit$pseudo.r.squared)
#     gof.names <- c(gof.names, "Pseudo R2")
#     gof.decimal <- c(gof.decimal, TRUE)
#   }
#   
#   tr <- createTexreg(
#     coef.names = coefnames,
#     coef = coefs,
#     se = se,
#     pvalues = pval,
#     gof.names = gof.names,
#     gof = gof,
#     gof.decimal = gof.decimal
#   )
#   return(tr)
# }
# 
#  
# extension for betareg objects (betareg package)
Extract2.betareg <- function(model, include.precision = TRUE,
                            include.pseudors = TRUE, include.loglik = TRUE, include.nobs = TRUE, ...) {
  
  s <- summary(model, ...)
  
  coef.block <- s$coefficients$mean
  if (include.precision == TRUE) {
    phi <- s$coefficients$precision
    rownames(phi) <- paste("Precision:", rownames(phi))
    coef.block <- rbind(coef.block, phi)
  }
  names <- rownames(coef.block)
  co <- coef.block[, 1]
  se <- coef.block[, 2]
  pval <- coef.block[, 4]
  
  gof <- numeric()
  gof.names <- character()
  gof.decimal <- logical()
  if (include.pseudors == TRUE) {
    pseudors <- model$pseudo.r.squared
    gof <- c(gof, pseudors)
    gof.names <- c(gof.names, "Pseudo R2")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.loglik == TRUE) {
    lik <- model$loglik
    gof <- c(gof, lik)
    gof.names <- c(gof.names, "LogLik")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.nobs == TRUE) {
    n <- nobs(model)
    gof <- c(gof, n)
    gof.names <- c(gof.names, "NumObs")
    gof.decimal <- c(gof.decimal, FALSE)
  }
  
  tr <- createTexreg(
    coef.names = names,
    coef = co,
    se = se,
    pvalues = pval,
    gof.names = gof.names,
    gof = gof,
    gof.decimal = gof.decimal
  )
  return(tr)
}

 

# extension for btergm objects
Extract2.btergm <- function(model, level = 0.95, include.nobs = TRUE, ...) {
  
  tab <- btergm::confint(model, level = level)
  
  gof <- numeric()
  gof.names <- character()
  gof.decimal <- logical()
  if (include.nobs == TRUE) {
    gof <- c(gof, model@nobs)
    gof.names <- c(gof.names, "NumObs")
    gof.decimal <- c(gof.decimal, FALSE)
  }
  
  tr <- createTexreg(
    coef.names = rownames(tab),
    coef = tab[, 1],
    ci.low = tab[, 2],
    ci.up = tab[, 3],
    gof.names = gof.names,
    gof = gof,
    gof.decimal = gof.decimal
  )
  
  return(tr)
}

 

# extension for censReg objects (censReg package)
Extract2.censReg <- function(model, include.aic = TRUE, include.bic = TRUE,
                            include.loglik = TRUE, include.nobs = TRUE, ...) {
  s <- summary(model, ...)
  
  coefs <- s$estimate[, 1]
  rn <- rownames(s$estimate)
  se <- s$estimate[, 2]
  pval <- s$estimate[, 4]
  
  gof <- numeric()
  gof.names <- character()
  gof.decimal <- logical()
  if (include.aic == TRUE) {
    gof <- c(gof, AIC(model)[1])
    gof.names <- c(gof.names, "AIC")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.bic == TRUE) {
    gof <- c(gof, BIC(model)[1])
    gof.names <- c(gof.names, "BIC")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.loglik == TRUE) {
    gof <- c(gof, logLik(model)[1])
    gof.names <- c(gof.names, "Log\ Likelihood")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.nobs == TRUE) {
    gof <- c(gof, s$nObs)
    gof.names <- c(gof.names, "NumObs", "Left-censored", "Uncensored",
                   "Right-censored")
    gof.decimal <- c(gof.decimal, FALSE, FALSE, FALSE, FALSE)
  }
  
  tr <- createTexreg(
    coef.names = rn,
    coef = coefs,
    se = se,
    pvalues = pval,
    gof.names = gof.names,
    gof = gof,
    gof.decimal = gof.decimal
  )
  return(tr)
}

 
# extension for clm objects (ordinal package)
Extract2.clm <- function(model, include.thresholds = TRUE, include.aic = TRUE,
                        include.bic = TRUE, include.loglik = TRUE, include.nobs = TRUE, ...) {
  s <- summary(model, ...)
  
  tab <- s$coefficients
  thresh <- tab[rownames(tab) %in% names(s$aliased$alpha), ]
  threshold.names <- rownames(thresh)
  threshold.coef <- thresh[, 1]
  threshold.se <- thresh[, 2]
  threshold.pval <- thresh[, 4]
  beta <- tab[rownames(tab) %in% names(s$aliased$beta), ]
  beta.names <- rownames(beta)
  beta.coef <- beta[, 1]
  beta.se <- beta[, 2]
  beta.pval <- beta[, 4]
  if (include.thresholds == TRUE) {
    names <- c(beta.names, threshold.names)
    coef <- c(beta.coef, threshold.coef)
    se <- c(beta.se, threshold.se)
    pval <- c(beta.pval, threshold.pval)
  } else {
    names <- beta.names
    coef <- beta.coef
    se <- beta.se
    pval <- beta.pval
  }
  
  n <- nobs(model)
  lik <- logLik(model)[1]
  aic <- AIC(model)
  bic <- BIC(model)
  gof <- numeric()
  gof.names <- character()
  gof.decimal <- logical()
  if (include.aic == TRUE) {
    gof <- c(gof, aic)
    gof.names <- c(gof.names, "AIC")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.bic == TRUE) {
    gof <- c(gof, bic)
    gof.names <- c(gof.names, "BIC")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.loglik == TRUE) {
    gof <- c(gof, lik)
    gof.names <- c(gof.names, "LogLik")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.nobs == TRUE) {
    gof <- c(gof, n)
    gof.names <- c(gof.names, "NumObs")
    gof.decimal <- c(gof.decimal, FALSE)
  }
  
  tr <- createTexreg(
    coef.names = names,
    coef = coef,
    se = se,
    pvalues = pval,
    gof.names = gof.names,
    gof = gof,
    gof.decimal = gof.decimal
  )
  return(tr)
}

 
Extract2.sclm <- Extract2.clm
 
# extension for clmm objects (ordinal package)
Extract2.clmm <- function(model, include.thresholds = TRUE,
                         include.loglik = TRUE, include.aic = TRUE,  include.bic = TRUE,
                         include.nobs = TRUE, include.groups = TRUE, include.variance = TRUE, ...) {
  s <- summary(model, ...)
  
  tab <- s$coefficients
  thresh <- tab[rownames(tab) %in% names(s$alpha), , drop = FALSE]
  threshold.names <- rownames(thresh)
  threshold.coef <- thresh[, 1]
  threshold.se <- thresh[, 2]
  threshold.pval <- thresh[, 4]
  beta <- tab[rownames(tab) %in% names(s$beta), , drop = FALSE]
  beta.names <- rownames(beta)
  beta.coef <- beta[, 1]
  beta.se <- beta[, 2]
  beta.pval <- beta[, 4]
  
  if (include.thresholds == TRUE) {
    cfnames <- c(beta.names, threshold.names)
    coef <- c(beta.coef, threshold.coef)
    se <- c(beta.se, threshold.se)
    pval <- c(beta.pval, threshold.pval)
  } else {
    cfnames <- beta.names
    coef <- beta.coef
    se <- beta.se
    pval <- beta.pval
  }
  
  gof <- numeric()
  gof.names <- character()
  gof.decimal <- logical()
  if (include.loglik == TRUE) {
    lik <- logLik(model)[1]
    gof <- c(gof, lik)
    gof.names <- c(gof.names, "LogLik")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.aic == TRUE) {
    aic <- AIC(model)
    gof <- c(gof, aic)
    gof.names <- c(gof.names, "AIC")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.bic == TRUE) {
    bic <- BIC(model)
    gof <- c(gof, bic)
    gof.names <- c(gof.names, "BIC")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.nobs == TRUE) {
    n <- nobs(model)
    gof <- c(gof, n)
    gof.names <- c(gof.names, "NumObs")
    gof.decimal <- c(gof.decimal, FALSE)
  }
  if (include.groups == TRUE) {
    grp <- s$dims$nlev.gf
    grp.names <- paste0("Groups (", names(grp), ")")
    gof <- c(gof, grp)
    gof.names <- c(gof.names, grp.names)
    gof.decimal <- c(gof.decimal, rep(FALSE, length(grp)))
  }
  if (include.variance == TRUE) {
    var.names <- character()
    var.values <- numeric()
    for (i in 1:length(s$ST)) {
      variances <- diag(s$ST[[i]] %*% t(s$ST[[i]]))
      var.names <- c(var.names, paste0("Variance: ", names(s$ST)[[i]], ": ",
                                       names(variances)))
      var.values <- c(var.values, variances)
    }
    gof <- c(gof, var.values)
    gof.names <- c(gof.names, var.names)
    gof.decimal <- c(gof.decimal, rep(TRUE, length(var.values)))
  }
  
  tr <- createTexreg(
    coef.names = cfnames,
    coef = coef,
    se = se,
    pvalues = pval,
    gof.names = gof.names,
    gof = gof,
    gof.decimal = gof.decimal
  )
  return(tr)
}

 

# extension for ergm objects
Extract2.ergm <- function(model, include.aic = TRUE, include.bic = TRUE,
                         include.loglik = TRUE, ...) {
  s <- summary(model, ...)
  
  coefficient.names <- rownames(s$coefs)
  coefficients <- s$coefs[, 1]
  standard.errors <- s$coefs[, 2]
  significance <- s$coefs[, 4]
  
  gof <- numeric()
  gof.names <- character()
  gof.decimal <- logical()
  if (include.aic == TRUE && !is.null(s$aic)) {
    aic <- s$aic
    gof <- c(gof, aic)
    gof.names <- c(gof.names, "AIC")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.bic == TRUE && !is.null(s$bic)) {
    bic <- s$bic
    gof <- c(gof, bic)
    gof.names <- c(gof.names, "BIC")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.loglik == TRUE && !is.null(model$mle.lik)) {
    lik <- model$mle.lik[1]
    gof <- c(gof, lik)
    gof.names <- c(gof.names, "LogLik")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  
  tr <- createTexreg(
    coef.names = coefficient.names,
    coef = coefficients,
    se = standard.errors,
    pvalues = significance,
    gof.names = gof.names,
    gof = gof,
    gof.decimal = gof.decimal
  )
  return(tr)
}

 

# extension for ets objects (forecast package)
Extract2.ets <- function (model, include.pvalues = FALSE, include.aic = TRUE,
                         include.aicc = TRUE, include.bic = TRUE, include.loglik = TRUE, ...) {
  mask <- model$mask
  nam <- names(model$par)
  co <- model$par
  sdev <- rep(-Inf,length(co))
  name <- model$method
  if (include.pvalues == TRUE) {
    t.rat <- rep(NA, length(mask))
    t.rat[mask] <- co[mask] / sdev
    pt <- 2 * pnorm(-abs(t.rat))
    setmp <- rep(NA, length(mask))
    setmp[mask] <- sdev
  } else {
    pt <- numeric()
    setmp <- sdev
  }
  gof <- numeric()
  gof.names <- character()
  gof.decimal <- logical()
  if (include.aic == TRUE) {
    aic <- AIC(model)
    gof <- c(gof, aic)
    gof.names <- c(gof.names, "AIC")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.aicc == TRUE) {
    gof <- c(gof, model$aicc)
    gof.names <- c(gof.names, "AICc")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.bic == TRUE) {
    gof <- c(gof, model$bic)
    gof.names <- c(gof.names, "BIC")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.loglik == TRUE) {
    lik <- model$loglik
    gof <- c(gof, lik)
    gof.names <- c(gof.names, "LogLik")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  tr <- createTexreg(
    coef.names = nam,
    coef = co,
    se = setmp,
    pvalues = pt,
    gof.names = gof.names,
    gof = gof,
    gof.decimal = gof.decimal,
    model.name = name
  )
  return(tr)
}

 

# extension for ergmm objects (latentnet package)
Extract2.ergmm <- function(model, include.bic = TRUE, ...) {
  s <- summary(model)
  
  coefficient.names <- rownames(s$pmean$coef.table)
  coefficients <- s$pmean$coef.table[, 1]
  ci.low <- s$pmean$coef.table[, 2]
  ci.up <- s$pmean$coef.table[, 3]
  
  gof <- numeric()
  gof.names <- character()
  gof.decimal <- logical()
  if (include.bic == TRUE) {
    gof <- c(gof, s$bic$overall, s$bic$Y, s$bic$Z)
    gof.names <- c(gof.names, "BIC (Overall)", "BIC (Likelihood)",
                   "BIC (Latent Positions)")
    gof.decimal <- c(gof.decimal, TRUE, TRUE, TRUE)
  }
  
  tr <- createTexreg(
    coef.names = coefficient.names,
    coef = coefficients,
    ci.low = ci.low,
    ci.up = ci.up,
    gof.names = gof.names,
    gof = gof,
    gof.decimal = gof.decimal
  )
  return(tr)
}

 
# 
# # extension for felm objects (lfe package)
# Extract2.felm <- function(model, include.nobs = TRUE, include.rsquared = TRUE,
#                          include.adjrs = TRUE, include.fstatistic = FALSE, ...) {
#   
#   s <- summary(model)
#   nam <- rownames(s$coefficients)
#   co <- s$coefficients[, 1]
#   se <- s$coefficients[, 2]
#   pval <- s$coefficients[, 4]
#   
#   gof <- numeric()
#   gof.names <- character()
#   gof.decimal <- logical()
#   if (include.nobs == TRUE) {
#     gof <- c(gof, s$N)
#     gof.names <- c(gof.names, "NumObs")
#     gof.decimal <- c(gof.decimal, FALSE)
#   }
#   if (include.rsquared == TRUE) {
#     gof <- c(gof, s$r2, s$P.r.squared)
#     gof.names <- c(gof.names, "R2 (full model)", "R2 (proj model)")
#     gof.decimal <- c(gof.decimal, TRUE, TRUE)
#   }
#   if (include.adjrs == TRUE) {
#     gof <- c(gof, s$r2adj, s$P.adj.r.squared)
#     gof.names <- c(gof.names, "adjR2 (full model)",
#                    "adjR2 (proj model)")
#     gof.decimal <- c(gof.decimal, TRUE, TRUE)
#   }
#   if (include.fstatistic == TRUE) {
#     gof <- c(gof, s$F.fstat[1], s$F.fstat[4],
#              s$P.fstat[length(s$P.fstat) - 1], s$P.fstat[1])
#     gof.names <- c(gof.names, "FStat (full model)",
#                    "F (full model): p-value", "FStat (proj model)",
#                    "F (proj model): p-value")
#     gof.decimal <- c(gof.decimal, TRUE, TRUE, TRUE, TRUE)
#   }
#   
#   tr <- createTexreg(
#     coef.names = nam,
#     coef = co,
#     se = se,
#     pvalues = pval,
#     gof.names = gof.names,
#     gof = gof,
#     gof.decimal = gof.decimal
#   )
#   return(tr)
# }
# 
#  

# extension for fGARCH objects (fGarch package)
Extract2.fGARCH <- function(model, include.nobs = TRUE, include.aic = TRUE,
                           include.loglik = TRUE, ...) {
  namesOfPars <- rownames(model@fit$matcoef)
  co <- model@fit$matcoef[, 1]
  se <- model@fit$matcoef[, 2]
  pval <- model@fit$matcoef[, 4]
  
  gof <- numeric()
  gof.names <- character()
  gof.decimal <- logical()
  if (include.nobs == TRUE) {
    n <- length(model@data)
    gof <- c(gof, n)
    gof.names <- c(gof.names, "NumObs")
    gof.decimal <- c(gof.decimal, FALSE)
  }
  if (include.aic == TRUE) {
    aic <- model@fit$ics[1]
    gof <- c(gof, aic)
    gof.names <- c(gof.names, "AIC")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.loglik == TRUE) {
    lik <- model@fit$value
    gof <- c(gof, lik)
    gof.names <- c(gof.names, "LogLik")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  
  tr <- createTexreg(
    coef.names = namesOfPars,
    coef = co,
    se = se,
    pvalues = pval,
    gof.names = gof.names,
    gof.decimal = gof.decimal,
    gof = gof
  )
  return(tr)
}

 

# extension for forecast objects (forecast package)
Extract2.forecast <- function (model, ...) {
  model <- model$model
  return(Extract2(model))
}

 

# extension for gam and bam objects (mgcv package)
Extract2.gam <- function(model, include.smooth = TRUE, include.aic = TRUE,
                        include.bic = TRUE, include.loglik = TRUE, include.deviance = TRUE,
                        include.dev.expl = TRUE, include.dispersion = TRUE, include.rsquared = TRUE,
                        include.gcv = TRUE, include.nobs = TRUE, include.nsmooth = TRUE, ...) {
  
  s <- summary(model, ...)
  
  coef.block <- s$p.table
  if (include.smooth == TRUE) {
    smooth <- s$s.table
    rownames(smooth) <- paste0("EDF:\ ", rownames(smooth))
    coef.block <- rbind(coef.block, smooth)
  }
  names <- rownames(coef.block)
  co <- coef.block[, 1]
  se <- coef.block[, 2]
  pval <- coef.block[, 4]
  
  gof <- numeric()
  gof.names <- character()
  gof.decimal <- logical()
  if (include.aic == TRUE) {
    aic <- AIC(model)
    gof <- c(gof, aic)
    gof.names <- c(gof.names, "AIC")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.bic == TRUE) {
    bic <- BIC(model)
    gof <- c(gof, bic)
    gof.names <- c(gof.names, "BIC")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.loglik == TRUE) {
    lik <- logLik(model)[1]
    gof <- c(gof, lik)
    gof.names <- c(gof.names, "Log\ Likelihood")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.deviance == TRUE) {
    dev <- deviance(model)
    gof <- c(gof, dev)
    gof.names <- c(gof.names, "Deviance")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.dev.expl == TRUE) {
    dev.expl <- s$dev.expl
    gof <- c(gof, dev.expl)
    gof.names <- c(gof.names, "Deviance\ explained")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.dispersion == TRUE) {
    dispersion <- s$dispersion
    gof <- c(gof, dispersion)
    gof.names <- c(gof.names, "Dispersion")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.rsquared == TRUE) {
    rsq <- s$r.sq
    gof <- c(gof, rsq)
    gof.names <- c(gof.names, "R2")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.gcv == TRUE) {
    gcv <- model$gcv.ubre
    gof <- c(gof, gcv)
    gof.names <- c(gof.names, "GCV\ score")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.nobs == TRUE) {
    n <- nobs(model)
    gof <- c(gof, n)
    gof.names <- c(gof.names, "NumObs")
    gof.decimal <- c(gof.decimal, FALSE)
  }
  if (include.nsmooth == TRUE) {
    m <- s$m
    gof <- c(gof, m)
    gof.names <- c(gof.names, "Num. smooth\ terms")
    gof.decimal <- c(gof.decimal, FALSE)
  }
  
  tr <- createTexreg(
    coef.names = names,
    coef = co,
    se = se,
    pvalues = pval,
    gof.names = gof.names,
    gof = gof,
    gof.decimal = gof.decimal
  )
  return(tr)
}

 

Extract2.bam <- Extract2.gam
 


# # extension for gamlss objects (gamlss package)
# Extract2.gamlss <- function(model, robust = FALSE, include.nobs = TRUE,
#                            include.nagelkerke = TRUE, include.gaic = TRUE, ...) {
#   
#   # VCOV Extract2ion; create coefficient block
#   covmat <- suppressWarnings(stats::vcov(model, type = "all", robust = robust,
#                                          ...))
#   cf <- covmat$coef  # coefficients
#   namesOfPars <- names(cf)  # names of coefficients
#   se <- covmat$se  # standard errors
#   tvalue <- cf / se
#   pvalue <-  2 * pt(-abs(tvalue), model$df.res)  # p values
#   
#   #add the parameter names to coefficients
#   possiblePars <- c("$\\mu$", "$\\sigma$", "$\\nu$", "$\\tau$")
#   parIndex <- 0
#   parVector <- character()
#   for (i in 1:length(namesOfPars)) {
#     if (namesOfPars[i] == "(Intercept)") {
#       parIndex <- parIndex + 1
#     }
#     parName <- possiblePars[parIndex]
#     parVector <- c(parVector, parName)
#   }
#   namesOfPars <- paste(parVector, namesOfPars)
#   
#   # GOF block
#   gof <- numeric()
#   gof.names <- character()
#   gof.decimal <- logical()
#   if (include.nobs == TRUE) {
#     n <- nobs(model)
#     gof <- c(gof, n)
#     gof.names <- c(gof.names, "NumObs")
#     gof.decimal <- c(gof.decimal, FALSE)
#   }
#   if (include.nagelkerke == TRUE) {
#     nk <- gamlss::Rsq(model)
#     gof <- c(gof, nk)
#     gof.names <- c(gof.names, "Nagelkerke R2")
#     gof.decimal <- c(gof.decimal, TRUE)
#   }
#   if (include.gaic == TRUE) {
#     gaic <- gamlss::GAIC(model)
#     gof <- c(gof, gaic)
#     gof.names <- c(gof.names, "Generalized AIC")
#     gof.decimal <- c(gof.decimal, TRUE)
#   }
#   
#   # create and return texreg object
#   tr <- createTexreg(
#     coef.names = namesOfPars,
#     coef = cf,
#     se = se,
#     pvalues = pvalue,
#     gof.names = gof.names,
#     gof.decimal = gof.decimal,
#     gof = gof
#   )
#   return(tr)
# }
# 
#  

# extension for gee objects (gee package)
Extract2.gee <- function(model, robust = TRUE, include.dispersion = TRUE,
                        include.nobs = TRUE, ...) {
  s <- summary(model, ...)
  
  names <- rownames(coef(s))
  co <- coef(s)[,1]
  if (robust == TRUE) {
    se <- coef(s)[, 4]
    zval <- coef(s)[, 5]
  } else {
    se <- coef(s)[, 2]
    zval <- coef(s)[, 3]
  }
  pval <- 2 * pnorm(abs(zval), lower.tail = FALSE)
  
  n <- nobs(model)
  disp <- s$scale
  
  gof <- numeric()
  gof.names <- character()
  gof.decimal <- logical()
  if (include.dispersion == TRUE) {
    gof <- c(gof, disp)
    gof.names <- c(gof.names, "Dispersion")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.nobs == TRUE) {
    gof <- c(gof, n)
    gof.names <- c(gof.names, "NumObs")
    gof.decimal <- c(gof.decimal, FALSE)
  }
  
  tr <- createTexreg(
    coef.names = names,
    coef = co,
    se = se,
    pvalues = pval,
    gof.names = gof.names,
    gof = gof,
    gof.decimal = gof.decimal
  )
  return(tr)
}

 
# extension for geeglm objects (geepack package)
Extract2.geeglm <- function(model, include.scale = TRUE,
                           include.correlation = TRUE, include.nobs = TRUE, ...) {
  s <- summary(model)
  names <- rownames(s$coef)
  co <- s$coef[, 1]
  se <- s$coef[, 2]
  pval <- s$coef[, 4]
  
  gof <- numeric()
  gof.names <- character()
  gof.decimal <- logical()
  
  if (include.scale == TRUE) {
    gof = c(gof, s$geese$scale$estimate, s$geese$scale$san.se)
    gof.names = c(gof.names, "Scale parameter: gamma", "Scale parameter: SE")
    gof.decimal = c(gof.decimal, TRUE, TRUE)
  }
  if (include.correlation == TRUE) {
    gof = c(gof, s$geese$correlation$estimate, s$geese$correlation$san.se)
    gof.names = c(gof.names, "Correlation parameter: alpha",
                  "Correlation parameter: SE")
    gof.decimal = c(gof.decimal, TRUE, TRUE)
  }
  if (include.nobs == TRUE) {
    n <- nrow(model.frame(model))
    nclust <- length(s$geese$clusz)
    gof = c(gof, n, nclust)
    gof.names = c(gof.names, "NumObs", "Num. clust.")
    gof.decimal = c(gof.decimal, FALSE, FALSE)
  }
  
  tr <- createTexreg(
    coef.names = names,
    coef = co,
    se = se,
    pvalues = pval,
    gof.names = gof.names,
    gof = gof,
    gof.decimal = gof.decimal
  )
  return(tr)
}

 

 

# # extension for glmmadmb objects (glmmADMB package)
# Extract2.glmmadmb <- function(model, include.variance = TRUE,
#                              include.dispersion = TRUE, include.zero = TRUE, include.aic = TRUE,
#                              include.bic = TRUE, include.loglik = TRUE, include.nobs = TRUE,
#                              include.groups = TRUE, ...) {
#   
#   cf <- model$b
#   nam <- names(cf)
#   se <- model$stdbeta
#   tval <- cf / se
#   pval <- 2 * pnorm(-abs(tval))
#   
#   gof <- numeric()
#   gof.names <- character()
#   gof.decimal <- logical()
#   if (include.variance == TRUE && !is.null(model$S)) {
#     vc <- nlme::VarCorr(model)
#     vari <- unlist(vc)
#     for (i in 1:length(vari)) {
#       gof <- c(gof, vari[i])
#       gof.names <- c(gof.names, paste("Variance:", names(vari)[i]))
#       gof.decimal <- c(gof.decimal, TRUE)
#     }
#   }
#   if (include.dispersion == TRUE && !is.null(model$alpha)) {
#     label <- switch(model$family,
#                     truncnbinom = "Dispersion",
#                     nbinom = "Dispersion",
#                     gamma = "Shape",
#                     beta = "Dispersion",
#                     betabinom = "Dispersion",
#                     gaussian = "Residual variance",
#                     logistic = "Scale",
#                     "Dispersion"
#     )
#     dsp.lab <- paste0(label, ": parameter")
#     sd.lab <- paste0(label, ": SD")
#     disp <- model$alpha
#     sd <- model$sd_alpha
#     gof <- c(gof, disp, sd)
#     gof.names <- c(gof.names, dsp.lab, sd.lab)
#     gof.decimal <- c(gof.decimal, TRUE, TRUE)
#   }
#   if (include.zero == TRUE && !is.null(model$pz)) {
#     zero <- model$pz
#     zero.sd <- model$sd_pz
#     gof <- c(gof, zero, zero.sd)
#     gof.names <- c(gof.names, "Zero inflation: parameter", "Zero inflation: SD")
#     gof.decimal <- c(gof.decimal, TRUE, TRUE)
#   }
#   if (include.aic == TRUE) {
#     aic <- AIC(model)
#     gof <- c(gof, aic)
#     gof.names <- c(gof.names, "AIC")
#     gof.decimal <- c(gof.decimal, TRUE)
#   }
#   if (include.bic == TRUE) {
#     bic <- BIC(model)
#     gof <- c(gof, bic)
#     gof.names <- c(gof.names, "BIC")
#     gof.decimal <- c(gof.decimal, TRUE)
#   }
#   if (include.loglik == TRUE) {
#     lik <- model$loglik
#     gof <- c(gof, lik)
#     gof.names <- c(gof.names, "LogLik")
#     gof.decimal <- c(gof.decimal, TRUE)
#   }
#   if (include.nobs == TRUE) {
#     n <- nobs(model)
#     gof <- c(gof, n)
#     gof.names <- c(gof.names, "NumObs")
#     gof.decimal <- c(gof.decimal, FALSE)
#   }
#   if (include.groups == TRUE && !is.null(model$q)) {
#     groups <- model$q
#     for (i in 1:length(groups)) {
#       gof <- c(gof, groups[i])
#       gof.names <- c(gof.names, paste("NumGroups", names(groups)[i]))
#       gof.decimal <- c(gof.decimal, FALSE)
#     }
#   }
#   
#   tr <- createTexreg(
#     coef.names = nam,
#     coef = cf,
#     se = se,
#     pvalues = pval,
#     gof.names = gof.names,
#     gof = gof,
#     gof.decimal = gof.decimal
#   )
#   return(tr)
# }
# 
#  

# extension for gls objects (nlme package)
Extract2.gls <- function(model, include.aic = TRUE, include.bic = TRUE,
                        include.loglik = TRUE, include.nobs = TRUE, ...) {
  s <- summary(model, ...)
  
  coefficient.names <- rownames(s$tTable)
  coefficients <- s$tTable[, 1]
  standard.errors <- s$tTable[, 2]
  significance <- s$tTable[, 4]
  
  lik <- s$logLik
  aic <- s$AIC
  bic <- s$BIC
  n <- nobs(model)
  
  gof <- numeric()
  gof.names <- character()
  gof.decimal <- logical()
  if (include.aic == TRUE) {
    gof <- c(gof, aic)
    gof.names <- c(gof.names, "AIC")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.bic == TRUE) {
    gof <- c(gof, bic)
    gof.names <- c(gof.names, "BIC")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.loglik == TRUE) {
    gof <- c(gof, lik)
    gof.names <- c(gof.names, "LogLik")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.nobs == TRUE) {
    gof <- c(gof, n)
    gof.names <- c(gof.names, "NumObs")
    gof.decimal <- c(gof.decimal, FALSE)
  }
  
  tr <- createTexreg(
    coef.names = coefficient.names,
    coef = coefficients,
    se = standard.errors,
    pvalues = significance,
    gof.names = gof.names,
    gof = gof,
    gof.decimal = gof.decimal
  )
  return(tr)
}

 
# extension for gel objects (gmm package)
Extract2.gel <- function (model, include.obj.fcn = TRUE,
                         include.overidentification = FALSE, include.nobs = TRUE,
                         overIdentTest = c("LR", "LM", "J "), ...) {
  
  overIdentTest <- match.arg(overIdentTest)
  s <- summary(model, ...)
  coefs <- s$coefficients
  names <- rownames(coefs)
  coef <- coefs[, 1]
  se <- coefs[, 2]
  pval <- coefs[, 4]
  
  gof <- numeric()
  gof.names <- character()
  gof.decimal <- logical()
  if (include.obj.fcn == TRUE) {
    obj.fcn <- model$objective * 10^5
    gof <- c(gof, obj.fcn)
    gof.names <- c(gof.names, "Criterion function")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.overidentification == TRUE) {
    w <- which(strtrim(rownames(s$stest$test), 2) == overIdentTest)
    jtest <- s$stest$test[w, ]
    gof <- c(gof, jtest)
    ntest <- rownames(s$stest$test)[w]
    gof.names <- c(gof.names, c(ntest, paste0(ntest, " p-value")))
    gof.decimal <- c(gof.decimal, TRUE, TRUE)
  }
  if (include.nobs == TRUE) {
    n <- NROW(model$gt)
    gof <- c(gof, n)
    gof.names <- c(gof.names, "NumObs")
    gof.decimal <- c(gof.decimal, FALSE)
  }
  
  tr <- createTexreg(
    coef.names = names,
    coef = coef,
    se = se,
    pvalues = pval,
    gof.names = gof.names,
    gof = gof,
    gof.decimal = gof.decimal
  )
  return(tr)
}

 

# extension for gmm objects (gmm package)
Extract2.gmm <- function(model, include.obj.fcn = TRUE,
                        include.overidentification = FALSE, include.nobs = TRUE, ...) {
  
  s <- summary(model, ...)
  
  coefs <- s$coefficients
  names <- rownames(coefs)
  coef <- coefs[, 1]
  se <- coefs[, 2]
  pval <- coefs[, 4]
  
  n <- model$n  # number of observations
  gof <- numeric()
  gof.names <- character()
  gof.decimal <- logical()
  if (include.obj.fcn == TRUE) {
    obj.fcn <- model$objective * 10^5  # the value of the objective function
    gof <- c(gof, obj.fcn)
    gof.names <- c(gof.names, "Criterion function")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.overidentification == TRUE) {
    jtest <- s$stest$test[1]
    gof <- c(gof, jtest)
    gof.names <- c(gof.names, "J-Test")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.nobs == TRUE) {
    gof <- c(gof, n)
    gof.names <- c(gof.names, "NumObs")
    gof.decimal <- c(gof.decimal, FALSE)
  }
  
  tr <- createTexreg(
    coef.names = names,
    coef = coef,
    se = se,
    pvalues = pval,
    gof.names = gof.names,
    gof = gof,
    gof.decimal = gof.decimal
  )
  return(tr)
}

 

# # extension for H2OBinomialModel objects (h2o package)
# Extract2.H2OBinomialModel <- function(model, standardized = FALSE,
#                                      include.mse = TRUE, include.rsquared = TRUE, include.logloss = TRUE,
#                                      include.meanerror = TRUE, include.auc = TRUE, include.gini = TRUE,
#                                      include.deviance = TRUE, include.aic = TRUE, ...) {
#   
#   # Extract2 coefficient table from model:
#   coefnames <- model@model$coefficients_table$names
#   if (standardized == TRUE) {
#     coefs <- model@model$coefficients_table$standardized_coefficients
#   } else {
#     coefs <- model@model$coefficients_table$coefficients
#   }
#   
#   # create empty GOF vectors and subsequently add GOFStats from model:
#   gof <- numeric()
#   gof.names <- character()
#   gof.decimal <- logical()
#   if (include.mse == TRUE) {
#     mse <- model@model$training_metrics@metrics$MSE
#     gof <- c(gof, mse)
#     gof.names <- c(gof.names, "MSE")
#     gof.decimal <- c(gof.decimal, TRUE)
#   }
#   if (include.rsquared == TRUE) {
#     r2 <- model@model$training_metrics@metrics$r2
#     gof <- c(gof, r2)
#     gof.names <- c(gof.names, "R^2")
#     gof.decimal <- c(gof.decimal, TRUE)
#   }
#   if (include.logloss == TRUE) {
#     logloss <- model@model$training_metrics@metrics$logloss
#     gof <- c(gof, logloss)
#     gof.names <- c(gof.names, "LogLoss")
#     gof.decimal <- c(gof.decimal, TRUE)
#   }
#   if (include.meanerror == TRUE) {
#     mpce <- model@model$training_metrics@metrics$mean_per_class_error
#     gof <- c(gof, mpce)
#     gof.names <- c(gof.names, "Mean Per-Class Error")
#     gof.decimal <- c(gof.decimal, TRUE)
#   }
#   if (include.auc == TRUE) {
#     auc <- model@model$training_metrics@metrics$AUC
#     gof <- c(gof, auc)
#     gof.names <- c(gof.names, "AUC")
#     gof.decimal <- c(gof.decimal, TRUE)
#   }
#   if (include.gini == TRUE) {
#     gini <- model@model$training_metrics@metrics$Gini
#     gof <- c(gof, gini)
#     gof.names <- c(gof.names, "Gini")
#     gof.decimal <- c(gof.decimal, TRUE)
#   }
#   if (include.deviance == TRUE) {
#     nulldev <- model@model$training_metrics@metrics$null_deviance
#     resdev <- model@model$training_metrics@metrics$residual_deviance
#     gof <- c(gof, nulldev, resdev)
#     gof.names <- c(gof.names, "Null Deviance", "Residual Deviance")
#     gof.decimal <- c(gof.decimal, TRUE, TRUE)
#   }
#   if (include.aic == TRUE) {
#     aic <- model@model$training_metrics@metrics$AIC
#     gof <- c(gof, aic)
#     gof.names <- c(gof.names, "AIC")
#     gof.decimal <- c(gof.decimal, TRUE)
#   }
#   
#   # create texreg object:
#   tr <- createTexreg(
#     coef.names = coefnames,
#     coef = coefs,
#     gof.names = gof.names,
#     gof = gof,
#     gof.decimal = gof.decimal
#   )
#   return(tr)
# }

 

 

Extract2.glmmPQL <- Extract2.lme
 


 

# 
# # extension for lmrob and glmrob objects (robustbase package)
# Extract2.lmrob <- function(model, include.nobs = TRUE, ...) {
#   s <- summary(model, ...)
#   
#   names <- rownames(s$coef)
#   co <- s$coef[, 1]
#   se <- s$coef[, 2]
#   pval <- s$coef[, 4]
#   
#   gof <- numeric()
#   gof.names <- character()
#   gof.decimal <- logical()
#   
#   if (include.nobs == TRUE) {
#     n <- length(model$residuals)
#     gof <- c(gof, n)
#     gof.names <- c(gof.names, "NumObs")
#     gof.decimal <- c(gof.decimal, FALSE)
#   }
#   
#   tr <- createTexreg(
#     coef.names = names,
#     coef = co,
#     se = se,
#     pvalues = pval,
#     gof.names = gof.names,
#     gof = gof,
#     gof.decimal = gof.decimal
#   )
#   return(tr)
# }
# 
#  
# 
# Extract2.glmrob <- Extract2.lmrob
#  
# 
# 
# # extension for lnam objects (sna package)
# Extract2.lnam <- function(model, include.rsquared = TRUE, include.adjrs = TRUE,
#                          include.aic = TRUE, include.bic = TRUE, include.loglik = TRUE, ...) {
#   coefs <- coef(model, ...)
#   coef.names <- names(coefs)
#   se <- c(model$beta.se, model$rho1.se, model$rho2.se)
#   p <- 2 * (1 - pnorm(abs(coefs), 0, se))
#   
#   rss <- sum(model$residuals^2)
#   mss <- sum((model$fitted - mean(model$fitted))^2)
#   rdfns <- model$df.residual + 1
#   rsquared <- mss / (mss + rss)
#   adj.rsquared <- 1 - (1 - mss / (mss + rss)) * model$df.total / rdfns
#   lik <- model$lnlik.model
#   aic <- -2 * model$lnlik.model + 2 * model$df.model
#   bic <- -2 * model$lnlik.model + log(model$df.total) * model$df.model
#   
#   gof <- numeric()
#   gof.names <- character()
#   gof.decimal <- logical()
#   if (include.rsquared == TRUE) {
#     gof <- c(gof, rsquared)
#     gof.names <- c(gof.names, "R2")
#     gof.decimal <- c(gof.decimal, TRUE)
#   }
#   if (include.adjrs == TRUE) {
#     gof <- c(gof, adj.rsquared)
#     gof.names <- c(gof.names, "adjR2")
#     gof.decimal <- c(gof.decimal, TRUE)
#   }
#   if (include.aic == TRUE) {
#     gof <- c(gof, aic)
#     gof.names <- c(gof.names, "AIC")
#     gof.decimal <- c(gof.decimal, TRUE)
#   }
#   if (include.bic == TRUE) {
#     gof <- c(gof, bic)
#     gof.names <- c(gof.names, "BIC")
#     gof.decimal <- c(gof.decimal, TRUE)
#   }
#   if (include.loglik == TRUE) {
#     gof <- c(gof, lik)
#     gof.names <- c(gof.names, "LogLik")
#     gof.decimal <- c(gof.decimal, TRUE)
#   }
#   
#   tr <- createTexreg(
#     coef.names = coef.names,
#     coef = coefs,
#     se = se,
#     pvalues = p,
#     gof.names = gof.names,
#     gof = gof,
#     gof.decimal = gof.decimal
#   )
#   return(tr)
# }
# 
#  
# 
# 
# # extension for logitmfx objects (mfx package)
# Extract2.logitmfx <- function(model, include.nobs = TRUE, include.loglik = TRUE,
#                              include.deviance = TRUE, include.aic = TRUE, include.bic = TRUE, ...) {
#   coefnames <- rownames(model$mfxest)
#   coefs <- model$mfxest[, 1]
#   se <- model$mfxest[, 2]
#   pval <- model$mfxest[, 4]
#   
#   n <- nrow(model$fit$model)
#   ll <- (model$fit$aic - (2 * length(model$fit$coefficients))) / -2
#   
#   gof <- numeric()
#   gof.names <- character()
#   gof.decimal <- logical()
#   if (include.nobs == TRUE) {
#     gof <- c(gof, n)
#     gof.names <- c(gof.names, "NumObs")
#     gof.decimal <- c(gof.decimal, FALSE)
#   }
#   if (include.loglik == TRUE) {
#     gof <- c(gof, ll)
#     gof.names <- c(gof.names, "LogLik")
#     gof.decimal <- c(gof.decimal, TRUE)
#   }
#   if (include.deviance == TRUE) {
#     gof <- c(gof, model$fit$deviance)
#     gof.names <- c(gof.names, "Deviance")
#     gof.decimal <- c(gof.decimal, TRUE)
#   }
#   if (include.aic == TRUE) {
#     gof <- c(gof, model$fit$aic)
#     gof.names <- c(gof.names, "AIC")
#     gof.decimal <- c(gof.decimal, TRUE)
#   }
#   if (include.bic == TRUE) {
#     bic <- (-2 * ll) + (length(model$fit$coefficients) * log(n))
#     gof <- c(gof, bic)
#     gof.names <- c(gof.names, "BIC")
#     gof.decimal <- c(gof.decimal, TRUE)
#   }
#   
#   tr <- createTexreg(
#     coef.names = coefnames,
#     coef = coefs,
#     se = se,
#     pvalues = pval,
#     gof.names = gof.names,
#     gof = gof,
#     gof.decimal = gof.decimal
#   )
#   return(tr)
# }
# 
#  
# 
# 
# # extension for probitmfx objects (mfx package)
# Extract2.probitmfx <- Extract2.logitmfx
#  
# 
# 
# # extension for logitor objects (mfx package)
# Extract2.logitor <- function(model, include.nobs = TRUE, include.loglik = TRUE,
#                             include.deviance = TRUE, include.aic = TRUE, include.bic = TRUE, ...) {
#   coefnames <- rownames(model$oddsratio)
#   coefs <- model$oddsratio[, 1]
#   se <- model$oddsratio[, 2]
#   pval <- model$oddsratio[, 4]
#   
#   n <- nrow(model$fit$model)
#   ll <- (model$fit$aic - (2 * length(model$fit$coefficients))) / -2
#   
#   gof <- numeric()
#   gof.names <- character()
#   gof.decimal <- logical()
#   if (include.nobs == TRUE) {
#     gof <- c(gof, n)
#     gof.names <- c(gof.names, "NumObs")
#     gof.decimal <- c(gof.decimal, FALSE)
#   }
#   if (include.loglik == TRUE) {
#     gof <- c(gof, ll)
#     gof.names <- c(gof.names, "LogLik")
#     gof.decimal <- c(gof.decimal, TRUE)
#   }
#   if (include.deviance == TRUE) {
#     gof <- c(gof, model$fit$deviance)
#     gof.names <- c(gof.names, "Deviance")
#     gof.decimal <- c(gof.decimal, TRUE)
#   }
#   if (include.aic == TRUE) {
#     gof <- c(gof, model$fit$aic)
#     gof.names <- c(gof.names, "AIC")
#     gof.decimal <- c(gof.decimal, TRUE)
#   }
#   if (include.bic == TRUE) {
#     bic <- (-2 * ll) + (length(model$fit$coefficients) * log(n))
#     gof <- c(gof, bic)
#     gof.names <- c(gof.names, "BIC")
#     gof.decimal <- c(gof.decimal, TRUE)
#   }
#   
#   tr <- createTexreg(
#     coef.names = coefnames,
#     coef = coefs,
#     se = se,
#     pvalues = pval,
#     gof.names = gof.names,
#     gof = gof,
#     gof.decimal = gof.decimal
#   )
#   return(tr)
# }
# 
#  


# extension for lqmm objects (lqmm package)
Extract2.lqmm <- function(model, include.aic = TRUE, include.bic = TRUE,
                         include.loglik = TRUE, include.nobs = TRUE, include.groups = TRUE,
                         include.tau = FALSE, use.ci = FALSE, beside = TRUE, ...) {
  
  s <- summary(model, ...)
  
  tau <- model$tau
  if (length(tau) == 1 && class(s$tTable) != "list") {
    tab <- list(s$tTable)  # if only one tau value, wrap in list
  } else {
    tab <- s$tTable  # multiple tau values: already wrapped in list
  }
  
  if (beside == TRUE) {
    trlist <- list()
    for (i in 1:length(tau)) {
      coefficient.names <- rownames(tab[[i]])
      coefficients <- tab[[i]][, 1]
      standard.errors <- tab[[i]][, 2]
      significance <- tab[[i]][, 5]
      ci.l <- tab[[i]][, 3]
      ci.u <- tab[[i]][, 4]
      
      gof <- numeric()
      gof.names <- character()
      gof.decimal <- logical()
      if (include.aic == TRUE) {
        gof <- c(gof, AIC(model)[i])
        gof.names <- c(gof.names, "AIC")
        gof.decimal <- c(gof.decimal, TRUE)
      }
      if (include.bic == TRUE) {
        gof <- c(gof, BIC(model)[i])
        gof.names <- c(gof.names, "BIC")
        gof.decimal <- c(gof.decimal, TRUE)
      }
      if (include.loglik == TRUE) {
        gof <- c(gof, logLik(model)[i])
        gof.names <- c(gof.names, "LogLik")
        gof.decimal <- c(gof.decimal, TRUE)
      }
      if (include.nobs == TRUE) {
        n <- nobs(model)
        gof <- c(gof, n)
        gof.names <- c(gof.names, "NumObs")
        gof.decimal <- c(gof.decimal, FALSE)
      }
      if (include.groups == TRUE) {
        gof <- c(gof, model$ngroups)
        gof.names <- c(gof.names, "NumGroups")
        gof.decimal <- c(gof.decimal, FALSE)
      }
      if (include.tau == TRUE) {
        gof <- c(gof, tau[i])
        gof.names <- c(gof.names, "tau")
        gof.decimal <- c(gof.decimal, TRUE)
      }
      
      if (use.ci == FALSE) {
        tr <- createTexreg(
          coef.names = coefficient.names,
          coef = coefficients,
          se = standard.errors,
          pvalues = significance,
          gof.names = gof.names,
          gof = gof,
          gof.decimal = gof.decimal,
          model.name = as.character(tau[i])
        )
      } else {
        tr <- createTexreg(
          coef.names = coefficient.names,
          coef = coefficients,
          pvalues = significance,
          ci.low = ci.l,
          ci.up = ci.u,
          gof.names = gof.names,
          gof = gof,
          gof.decimal = gof.decimal,
          model.name = as.character(tau[i])
        )
      }
      trlist[[i]] <- tr
    }
    return(trlist)
  } else {
    coefficient.names <- character()
    coefficients <- numeric()
    standard.errors <- numeric()
    significance <- numeric()
    ci.l <- numeric()
    ci.u <- numeric()
    
    for (i in 1:length(tau)) {
      coefficient.names <- c(coefficient.names, paste0(rownames(tab[[i]]),
                                                       " (", tau[i], ")"))
      coefficients <- c(coefficients, tab[[i]][, 1])
      standard.errors <- c(standard.errors, tab[[i]][, 2])
      significance <- c(significance, tab[[i]][, 5])
      ci.l <- c(ci.l, tab[[i]][, 3])
      ci.u <- c(ci.u, tab[[i]][, 4])
    }
    
    gof <- numeric()
    gof.names <- character()
    gof.decimal <- logical()
    if (include.aic == TRUE) {
      gof <- c(gof, AIC(model))
      gof.names <- c(gof.names, paste0("AIC (", tau, ")"))
      gof.decimal <- c(gof.decimal, rep(TRUE, length(tau)))
    }
    if (include.bic == TRUE) {
      gof <- c(gof, BIC(model))
      gof.names <- c(gof.names, paste0("BIC (", tau, ")"))
      gof.decimal <- c(gof.decimal, rep(TRUE, length(tau)))
    }
    if (include.loglik == TRUE) {
      gof <- c(gof, logLik(model))
      gof.names <- c(gof.names, paste0("LogLik (", tau, ")"))
      gof.decimal <- c(gof.decimal, rep(TRUE, length(tau)))
    }
    if (include.nobs == TRUE) {
      n <- nobs(model)
      gof <- c(gof, n)
      gof.names <- c(gof.names, "NumObs")
      gof.decimal <- c(gof.decimal, FALSE)
    }
    if (include.groups == TRUE) {
      gof <- c(gof, model$ngroups)
      gof.names <- c(gof.names, "NumGroups")
      gof.decimal <- c(gof.decimal, FALSE)
    }
    
    if (use.ci == FALSE) {
      tr <- createTexreg(
        coef.names = coefficient.names,
        coef = coefficients,
        se = standard.errors,
        pvalues = significance,
        gof.names = gof.names,
        gof = gof,
        gof.decimal = gof.decimal
      )
    } else {
      tr <- createTexreg(
        coef.names = coefficient.names,
        coef = coefficients,
        pvalues = significance,
        ci.low = ci.l,
        ci.up = ci.u,
        gof.names = gof.names,
        gof = gof,
        gof.decimal = gof.decimal
      )
    }
    return(tr)
  }
}

 


# extension for lrm objects (Design or rms package); submitted by Fabrice Le Lec
Extract2.lrm <- function(model, include.pseudors = TRUE, include.lr = TRUE,
                        include.nobs = TRUE, ...) {
  attributes(model$coef)$names <- lapply(attributes(model$coef)$names,
                                         function(x) gsub(">=", " $\\\\geq$ ", x))
  coef.names <- attributes(model$coef)$names
  coef <- model$coef
  se <- sqrt(diag(model$var))
  p <- pnorm(abs(model$coef / sqrt(diag(model$var))),
             lower.tail = FALSE) * 2
  
  gof <- numeric()
  gof.names <- character()
  gof.decimal <- logical()
  if (include.nobs == TRUE) {
    n <- model$stats[1]  # Extract2 number of observations
    gof <- c(gof, n)
    gof.names <- c(gof.names, "NumObs")
    gof.decimal <- c(gof.decimal, FALSE)
  }
  if (include.pseudors == TRUE) {
    pseudors <- model$stats[10]  # Extract2 pseudo R-squared
    gof <- c(gof, pseudors)
    gof.names <- c(gof.names, "Pseudo R2")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.lr == TRUE) {
    LR <- model$stats[3]  # Extract2 LR
    gof <- c(gof, LR)
    gof.names <- c(gof.names, "L.R.")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  
  tr <- createTexreg(
    coef.names = coef.names,
    coef = coef,
    se = se,
    pvalues = p,
    gof.names = gof.names,
    gof = gof,
    gof.decimal = gof.decimal
  )
  return(tr)
}

 

# # extension for maBina objects (erer package)
# Extract2.maBina <- function(model, ...) {
#   
#   coefficient.names <- rownames(model$out)
#   coefficients <- model$out[, 1]
#   standard.errors <- model$out[, 2]
#   significance <- model$out[, 4]
#   
#   w <- Extract2(model$w, ...)
#   gof <- w@gof
#   gof.names <- w@gof.names
#   gof.decimal <- w@gof.decimal
#   
#   tr <- createTexreg(
#     coef.names = coefficient.names,
#     coef = coefficients,
#     se = standard.errors,
#     pvalues = significance,
#     gof.names = gof.names,
#     gof = gof,
#     gof.decimal = gof.decimal
#   )
#   return(tr)
# }
# 
#  


# extension for mlogit objects (mlogit package)
Extract2.mlogit <- function(model, include.aic = TRUE, include.loglik = TRUE,
                           include.nobs = TRUE, ...) {
  s <- summary(model, ...)
  
  coefs <- s$CoefTable[, 1]
  rn <- rownames(s$CoefTable)
  se <- s$CoefTable[, 2]
  pval <- s$CoefTable[, 4]
  
  gof <- numeric()
  gof.names <- character()
  gof.decimal <- logical()
  if (include.aic == TRUE) {
    gof <- AIC(model)
    gof.names <- c(gof.names, "AIC")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.loglik == TRUE) {
    gof <- c(gof, logLik(model)[1])
    gof.names <- c(gof.names, "Log\ Likelihood")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.nobs == TRUE) {
    gof <- c(gof, nrow(s$residuals))
    gof.names <- c(gof.names, "NumObs")
    gof.decimal <- c(gof.decimal, FALSE)
  }
  
  tr <- createTexreg(
    coef.names = rn,
    coef = coefs,
    se = se,
    pvalues = pval,
    gof.names = gof.names,
    gof = gof,
    gof.decimal = gof.decimal
  )
  return(tr)
}

 


# extension for mnlogit objects (mnlogit package)
Extract2.mnlogit <- function(model, include.aic = TRUE, include.loglik = TRUE,
                            include.nobs = TRUE, include.groups = TRUE, include.intercept = TRUE,
                            include.iterations = FALSE, beside = FALSE, ...) {
  
  s <- summary(model, ...)
  coT <- s$CoefTable
  coefnames <- rownames(coT)
  
  gof <- numeric()
  gof.names <- character()
  gof.decimal <- logical()
  if (include.aic == TRUE) {
    aic <- s$AIC
    gof <- c(gof, aic)
    gof.names <- c(gof.names, "AIC")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.loglik == TRUE) {
    lik <- s$logLik
    gof <- c(gof, lik)
    gof.names <- c(gof.names, "Log\ Likelihood")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.nobs == TRUE) {
    N <- s$model.size$N
    gof <- c(gof, N)
    gof.names <- c(gof.names, "NumObs")
    gof.decimal <- c(gof.decimal, FALSE)
  }
  if (include.groups == TRUE) {
    K <- s$model.size$K
    gof <- c(gof, K)
    gof.names <- c(gof.names, "K")
    gof.decimal <- c(gof.decimal, FALSE)
  }
  if (include.intercept == TRUE) {
    b0 <- s$model.size$intercept
    gof <- c(gof, b0)
    gof.names <- c(gof.names, "Intercept")
    gof.decimal <- c(gof.decimal, FALSE)
  }
  if (include.iterations == TRUE) {
    iter <- s$est.stats$niters
    gradNorm <- s$est.stats$gradNorm
    diffLike <- s$est.stats$funcDiff
    gof <- c(gof, iter, gradNorm, diffLike)
    gof.names <- c(gof.names, "Iterations", "Gradient 2-norm",
                   "Diff.\ Likelihood")
    gof.decimal <- c(gof.decimal, c(FALSE, TRUE, TRUE))
  }
  
  if (beside == FALSE) {
    co <- coT[, 1]
    se <- coT[, 2]
    pval <- coT[, 4]
    
    tr <- createTexreg(
      coef.names = coefnames,
      coef = co,
      se = se,
      pvalues = pval,
      gof.names = gof.names,
      gof = gof,
      gof.decimal = gof.decimal
    )
    return(tr)
  } else {
    models <- attributes(model$freq)$names[-1]
    trlist <- list()
    for (i in 1:length(models)) {
      rows <- which(grepl(paste0(models[i], "$"), coefnames))
      coeftable <- coT[rows, ]
      cn <- coefnames[rows]
      cn <- gsub(paste0(":", models[i], "$"), "", cn)
      co <- coeftable[, 1]
      se <- coeftable[, 2]
      pval <- coeftable[, 4]
      
      tr <- createTexreg(
        coef.names = cn,
        coef = co,
        se = se,
        pvalues = pval,
        gof.names = gof.names,
        gof = gof,
        gof.decimal = gof.decimal,
        model.name = models[i]
      )
      trlist[[i]] <- tr
    }
    return(trlist)
  }
}

 


# # extension for model.selection objects (MuMIn package)
# Extract2.model.selection <- function(model, include.loglik = TRUE,
#                                     include.aicc = TRUE, include.delta = TRUE, include.weight = TRUE,
#                                     include.nobs = TRUE, ...) {
#   
#   includecols <- c(loglik = include.loglik, ic = include.aicc,
#                    delta = include.delta, weight = include.weight)
#   include <- c(includecols, nobs = include.nobs)
#   decimal <- c(TRUE, TRUE, TRUE, TRUE, FALSE)[include]
#   colidx <- ncol(model) - c(loglik = 3L, ic = 2L, delta = 1L, weight = 0L)
#   z <- as.matrix(`[.data.frame`(model, TRUE, colidx[includecols], drop = FALSE))
#   if (include.nobs) z <- cbind(z, nobs = attr(model, "nobs"))
#   mode(z) <- "numeric"
#   gofnames <- as.character(c(loglik = "LogLik",
#                              ic = colnames(model)[colidx["ic"]],
#                              delta = "Delta", weight = "Weight",
#                              nobs = "NumObs")[include])
#   
#   coeftables <- MuMIn::coefTable(model)
#   
#   ## use t-test if dfs available, otherwise z-test:
#   pval <- function(ct) {
#     zval <- abs(ct[, 1L] / ct[, 2L])
#     2 * if (!any(is.na(ct[, 3L]))) {
#       pt(zval, df = ct[, 3L], lower.tail = FALSE)
#     } else {
#       pnorm(zval, lower.tail = FALSE)
#     }
#   }
#   
#   n <- nrow(z)
#   rval <- vector(length = n, mode = "list")
#   for (i in 1L:n) {
#     ct <- coeftables[[i]]
#     rval[[i]] <- createTexreg(
#       coef.names = rownames(ct),
#       coef = ct[, 1L],
#       se = ct[, 2L],
#       pvalues = pval(ct),
#       gof.names = gofnames,
#       gof = z[i, ],
#       gof.decimal = decimal
#     )
#   }
#   rval
# }
# 
#  

# extension for mtergm objects (btergm package)
Extract2.mtergm <- function(model, include.nobs = TRUE, include.aic = TRUE,
                           include.bic = TRUE, include.loglik = TRUE, ...) {
  
  coefficient.names <- names(model@coef)
  coefficients <- model@coef
  standard.errors <- model@se
  significance <- model@pval
  
  gof <- numeric()
  gof.names <- character()
  gof.decimal <- logical()
  if (include.nobs == TRUE) {
    gof <- c(gof, model@nobs)
    gof.names <- c(gof.names, "NumObs")
    gof.decimal <- c(gof.decimal, FALSE)
  }
  if (include.aic == TRUE && !is.null(model@aic) && !is.nan(model@aic)) {
    gof <- c(gof, model@aic)
    gof.names <- c(gof.names, "AIC")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.bic == TRUE && !is.null(model@bic) && !is.nan(model@bic)) {
    gof <- c(gof, model@bic)
    gof.names <- c(gof.names, "BIC")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.loglik == TRUE && !is.null(model@loglik) &&
      !is.nan(model@loglik)) {
    gof <- c(gof, model@loglik)
    gof.names <- c(gof.names, "LogLik")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  
  tr <- createTexreg(
    coef.names = coefficient.names,
    coef = coefficients,
    se = standard.errors,
    pvalues = significance,
    gof.names = gof.names,
    gof = gof,
    gof.decimal = gof.decimal
  )
  return(tr)
}

 


# extension for multinom objects (nnet package)
Extract2.multinom <- function(model, include.pvalues = TRUE, include.aic = TRUE,
                             include.bic = TRUE, include.loglik = TRUE, include.deviance = TRUE,
                             include.nobs = TRUE, levels = model$lev, beside = TRUE, ...) {
  
  s <- summary(model, ...)
  
  coefnames <- model$coefnames
  co <- s$coefficients
  se <- s$standard.errors
  
  if (class(co) != "matrix") {
    co <- t(as.matrix(co))
    se <- t(as.matrix(se))
  }
  
  gof <- numeric()
  gof.names <- character()
  gof.decimal <- logical()
  if (include.aic == TRUE) {
    aic <- AIC(model)
    gof <- c(gof, aic)
    gof.names <- c(gof.names, "AIC")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.bic == TRUE) {
    bic <- BIC(model)
    gof <- c(gof, bic)
    gof.names <- c(gof.names, "BIC")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.loglik == TRUE) {
    lik <- logLik(model)[1]
    gof <- c(gof, lik)
    gof.names <- c(gof.names, "Log\\ Likelihood")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.deviance == TRUE) {
    dev <- deviance(model)
    gof <- c(gof, dev)
    gof.names <- c(gof.names, "Deviance")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.nobs == TRUE) {
    n <- nrow(s$fitted.values)
    gof <- c(gof, n)
    gof.names <- c(gof.names, "NumObs")
    gof.decimal <- c(gof.decimal, FALSE)
  }
  
  if (beside == TRUE) {
    trlist <- list()
    for (i in which(rownames(co) %in% levels)) {
      if (include.pvalues == TRUE) {
        zval <- co[i, ] / se[i, ]
        pval <- 2 * pnorm(abs(zval), lower.tail = FALSE)
      } else {
        pval <- numeric(0)
      }
      
      tr <- createTexreg(
        coef.names = coefnames,
        coef = co[i, ],
        se = se[i, ],
        pvalues = pval,
        gof.names = gof.names,
        gof = gof,
        gof.decimal = gof.decimal,
        model.name = rownames(co)[i]
      )
      
      trlist <- c(trlist, tr)
    }
    if (length(trlist) == 1) {
      return(trlist[[1]])
    } else {
      return(trlist)
    }
  } else {
    pval <- numeric()
    stderr <- numeric()
    coefs <- numeric()
    nm <- character()
    for (i in which(rownames(co) %in% levels)) {
      nm <- c(nm, paste0(rownames(co)[i], ": ", coefnames))
      coefs <- c(coefs, co[i, ])
      stderr <- c(stderr, se[i, ])
      if (include.pvalues == TRUE) {
        zval <- co[i, ] / se[i, ]
        pval <- c(pval, 2 * pnorm(abs(zval), lower.tail = FALSE))
      }
    }
    tr <- createTexreg(
      coef.names = nm,
      coef = coefs,
      se = stderr,
      pvalues = pval,
      gof.names = gof.names,
      gof = gof,
      gof.decimal = gof.decimal
    )
    return(tr)
  }
}

 

# # extension for negbinirr objects (mfx package)
# Extract2.negbinirr <- function(model, include.nobs = TRUE,
#                               include.loglik = TRUE, include.deviance = TRUE, include.aic = TRUE,
#                               include.bic = TRUE, ...) {
#   coefnames <- rownames(model$irr)
#   coefs <- model$irr[, 1]
#   se <- model$irr[, 2]
#   pval <- model$irr[, 4]
#   
#   n <- nrow(model$fit$model)
#   if ("negbinirr" %in% class(model)) {
#     ll <- model$fit$twologlik / 2
#   } else if ("poissonirr" %in% class(model)) {
#     ll <- (model$fit$aic - (2 * length(model$fit$coefficients))) / -2
#   }
#   
#   gof <- numeric()
#   gof.names <- character()
#   gof.decimal <- logical()
#   if (include.nobs == TRUE) {
#     gof <- c(gof, n)
#     gof.names <- c(gof.names, "NumObs")
#     gof.decimal <- c(gof.decimal, FALSE)
#   }
#   if (include.loglik == TRUE) {
#     gof <- c(gof, ll)
#     gof.names <- c(gof.names, "LogLik")
#     gof.decimal <- c(gof.decimal, TRUE)
#   }
#   if (include.deviance == TRUE) {
#     gof <- c(gof, model$fit$deviance)
#     gof.names <- c(gof.names, "Deviance")
#     gof.decimal <- c(gof.decimal, TRUE)
#   }
#   if (include.aic == TRUE) {
#     gof <- c(gof, model$fit$aic)
#     gof.names <- c(gof.names, "AIC")
#     gof.decimal <- c(gof.decimal, TRUE)
#   }
#   if (include.bic == TRUE) {
#     bic <- (-2 * ll) + ((length(model$fit$coefficients) + 1) * log(n))
#     gof <- c(gof, bic)
#     gof.names <- c(gof.names, "BIC")
#     gof.decimal <- c(gof.decimal, TRUE)
#   }
#   print(gof.names)
#   print(gof)
#   tr <- createTexreg(
#     coef.names = coefnames,
#     coef = coefs,
#     se = se,
#     pvalues = pval,
#     gof.names = gof.names,
#     gof = gof,
#     gof.decimal = gof.decimal
#   )
#   return(tr)
# }
# 
#  
# # extension for poissonirr objects (mfx package)
# Extract2.poissonirr <- Extract2.negbinirr
#  
# # extension for negbinmfx objects (mfx package)
# Extract2.negbinmfx <- function(model, include.nobs = TRUE,
#                               include.loglik = TRUE, include.deviance = TRUE, include.aic = TRUE,
#                               include.bic = TRUE, ...) {
#   coefnames <- rownames(model$mfxest)
#   coefs <- model$mfxest[, 1]
#   se <- model$mfxest[, 2]
#   pval <- model$mfxest[, 4]
#   
#   n <- nrow(model$fit$model)
#   if ("negbinmfx" %in% class(model)) {
#     ll <- model$fit$twologlik / 2
#   } else if ("poissonmfx" %in% class(model)) {
#     ll <- (model$fit$aic - (2 * length(model$fit$coefficients))) / -2
#   }
#   
#   gof <- numeric()
#   gof.names <- character()
#   gof.decimal <- logical()
#   if (include.nobs == TRUE) {
#     gof <- c(gof, n)
#     gof.names <- c(gof.names, "NumObs")
#     gof.decimal <- c(gof.decimal, FALSE)
#   }
#   if (include.loglik == TRUE) {
#     gof <- c(gof, ll)
#     gof.names <- c(gof.names, "LogLik")
#     gof.decimal <- c(gof.decimal, TRUE)
#   }
#   if (include.deviance == TRUE) {
#     gof <- c(gof, model$fit$deviance)
#     gof.names <- c(gof.names, "Deviance")
#     gof.decimal <- c(gof.decimal, TRUE)
#   }
#   if (include.aic == TRUE) {
#     gof <- c(gof, model$fit$aic)
#     gof.names <- c(gof.names, "AIC")
#     gof.decimal <- c(gof.decimal, TRUE)
#   }
#   if (include.bic == TRUE) {
#     bic <- (-2 * ll) + ((length(model$fit$coefficients) + 1) * log(n))
#     gof <- c(gof, bic)
#     gof.names <- c(gof.names, "BIC")
#     gof.decimal <- c(gof.decimal, TRUE)
#   }
#   
#   tr <- createTexreg(
#     coef.names = coefnames,
#     coef = coefs,
#     se = se,
#     pvalues = pval,
#     gof.names = gof.names,
#     gof = gof,
#     gof.decimal = gof.decimal
#   )
#   return(tr)
# }

 
# # extension for poissonmfx objects (mfx package)
# Extract2.poissonmfx <- Extract2.negbinmfx
#  
# 
# # extension for netlogit objects (sna package)
# Extract2.netlogit <- function(model, include.aic = TRUE, include.bic = TRUE,
#                              include.deviance = TRUE, include.nobs = TRUE, ...) {
#   
#   gof <- numeric()
#   gof.names <- character()
#   gof.decimal <- logical()
#   if (include.aic == TRUE) {
#     gof <- c(gof, model$aic)
#     gof.names <- c(gof.names, "AIC")
#     gof.decimal <- c(gof.decimal, TRUE)
#   }
#   if (include.bic == TRUE) {
#     gof <- c(gof, model$bic)
#     gof.names <- c(gof.names, "BIC")
#     gof.decimal <- c(gof.decimal, TRUE)
#   }
#   if (include.deviance == TRUE) {
#     gof <- c(gof, model$deviance, model$null.deviance)
#     gof.names <- c(gof.names, "Deviance", "Null deviance")
#     gof.decimal <- c(gof.decimal, TRUE, TRUE)
#   }
#   if (include.nobs == TRUE) {
#     gof <- c(gof, model$n)
#     gof.names <- c(gof.names, "NumObs")
#     gof.decimal <- c(gof.decimal, TRUE)
#   }
#   
#   cf <- model$coefficients
#   pv <- model$pgreqabs
#   nm <- c("(Intercept)", paste0("x", 1:(length(cf) - 1)))
#   if (is.null(model$dist)) {  # "classical" fit (= simple logit model)
#     cvm <- chol2inv(model$qr$qr)
#     se <- sqrt(diag(cvm))
#   } else {  # QAP, CUG etc.
#     se <- rep(NaN, length(cf))  # not perfect; results in empty brackets!
#   }
#   
#   tr <- createTexreg(
#     coef.names = nm,
#     coef = cf,
#     se = se,
#     pvalues = pv,
#     gof.names = gof.names,
#     gof = gof,
#     gof.decimal = gof.decimal
#   )
#   return(tr)
# }
# 
#  
# # extension for ols objects (rms package)
# Extract2.ols <- function (model, include.nobs = TRUE, include.rsquared = TRUE,
#                          include.adjrs = TRUE, include.fstatistic = FALSE, include.lr = TRUE, ...) {
#   
#   names <- attributes(model$coef)$names
#   co <- model$coef
#   se <- sqrt(diag(model$var))
#   pval <- pnorm(abs(model$coef/sqrt(diag(model$var))), lower.tail = FALSE) * 2
#   
#   gof <- numeric()
#   gof.names <- character()
#   gof.decimal <- logical()
#   if (include.nobs == TRUE) {
#     n <- nobs(model)
#     gof <- c(gof, n)
#     gof.names <- c(gof.names, "NumObs")
#     gof.decimal <- c(gof.decimal, FALSE)
#   }
#   if (include.rsquared == TRUE) {
#     rs <- model$stats["R2"]
#     gof <- c(gof, rs)
#     gof.names <- c(gof.names, "R2")
#     gof.decimal <- c(gof.decimal, TRUE)
#   }
#   if (include.adjrs == TRUE) {
#     adj <- 1 - (1 - model$stats["R2"]) * (n - 1) / (n - model$stats["d.f."] - 1)
#     gof <- c(gof, adj)
#     gof.names <- c(gof.names, "adjR2")
#     gof.decimal <- c(gof.decimal, TRUE)
#   }
#   if (include.fstatistic == TRUE) {
#     tryCatch({
#       fs <- summary.lm(model)$fstatistic[[1]]  # won't work if penalty matrix
#       gof.names <- c(gof.names, "FStat") # is given (whatever that is)
#       gof.decimal <- c(gof.decimal, TRUE)
#       gof <- c(gof, fs)
#     }, error = {
#       warning("FStat could not be Extract2ed.")
#     }, warning = {
#       warning("FStat could not be Extract2ed.")
#     })
#   }
#   if (include.lr == TRUE) {
#     LR <- model$stats["Model L.R."]
#     gof <- c(gof, LR)
#     gof.names <- c(gof.names, "L.R.")
#     gof.decimal <- c(gof.decimal, TRUE)
#   }
#   
#   tr <- createTexreg(coef.names = names, coef = co, se = se, pvalues = pval,
#                      gof.names = gof.names, gof = gof, gof.decimal = gof.decimal)
#   return(tr)
# }
# 
#  

# extension for pgmm objects (from the plm package)
Extract2.pgmm <- function(model, include.nobs = TRUE, include.sargan = TRUE,
                         include.wald = TRUE, ...) {
  
  s <- summary(model, ...)
  
  coefficient.names <- rownames(s$coefficients)
  coefficients <- s$coefficients[, 1]
  standard.errors <- s$coefficients[, 2]
  significance <- s$coefficients[, 4]
  
  gof <- numeric()
  gof.names <- character()
  gof.decimal <- logical()
  if (include.nobs == TRUE) {
    n <- attr(s, "pdim")$nT$n
    T <- attr(s, "pdim")$nT$T
    N <- attr(s, "pdim")$nT$N
    ntot <- sum(unlist(s$residuals) != 0)
    gof <- c(gof, n, T, N, ntot)
    gof.names <- c(gof.names, "n", "T", "NumObs", "NumObs\ used")
    gof.decimal <- c(gof.decimal, FALSE, FALSE, FALSE, FALSE)
  }
  if (include.sargan == TRUE) {
    sarg.stat <- s$sargan$statistic
    sarg.par <- s$sargan$parameter
    sarg.pval <- s$sargan$p.value
    gof <- c(gof, sarg.stat, sarg.par, sarg.pval)
    gof.names <- c(gof.names, "Sargan Test: chisq", "Sargan Test: df",
                   "Sargan Test: p-value")
    gof.decimal <- c(gof.decimal, TRUE, TRUE, TRUE)
  }
  if (include.wald == TRUE) {
    wald.coef <- s$wald.coef$statistic[1]
    wald.pval <- s$wald.coef$p.value[1]
    wald.par <- s$wald.coef$parameter
    gof <- c(gof, wald.coef, wald.par, wald.pval)
    gof.names <- c(
      gof.names,
      "Wald Test Coefficients: chisq",
      "Wald Test Coefficients: df",
      "Wald Test Coefficients: p-value"
    )
    gof.decimal <- c(gof.decimal, TRUE, FALSE, TRUE)
    if (!is.null(s$wald.td)) {
      td.coef <- s$wald.td$statistic[1]
      td.pval <- s$wald.td$p.value[1]
      td.par <- s$wald.td$parameter
      gof <- c(gof, td.coef, td.par, td.pval)
      gof.names <- c(
        gof.names,
        "Wald Test Time Dummies: chisq",
        "Wald Test Time Dummies: df",
        "Wald Test Time Dummies: p-value"
      )
      gof.decimal <- c(gof.decimal, TRUE, FALSE, TRUE)
    }
  }
  
  tr <- createTexreg(
    coef.names = coefficient.names,
    coef = coefficients,
    se = standard.errors,
    pvalues = significance,
    gof.names = gof.names,
    gof = gof,
    gof.decimal = gof.decimal
  )
  return(tr)
}

 


# extension for plm objects (from the plm package)
Extract2.plm <- function(model, include.rsquared = TRUE, include.adjrs = TRUE,
                        include.nobs = TRUE, include.variance = TRUE, ...) {
  s <- summary(model, ...)
  
  coefficient.names <- rownames(coef(s))
  coefficients <- coef(s)[, 1]
  standard.errors <- coef(s)[, 2]
  significance <- coef(s)[, 4]
  
  rs <- s$r.squared[1]
  adj <- s$r.squared[2]
  n <- length(model$residuals)
  
  gof <- numeric()
  gof.names <- character()
  gof.decimal <- logical()
  if (include.variance == TRUE) {
    if (model$args$model == "random") {
      se <- sqrt(unlist(plm::ercomp(model)$sigma2))
      gof <- c(gof, se)
      gof.names <- c(gof.names, paste0("s_", names(se)))
      gof.decimal <- c(gof.decimal, rep(TRUE, length(se)))
    }
  }
  if (include.rsquared == TRUE) {
    gof <- c(gof, rs)
    gof.names <- c(gof.names, "R2")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.adjrs == TRUE) {
    gof <- c(gof, adj)
    gof.names <- c(gof.names, "adjR2")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.nobs == TRUE) {
    gof <- c(gof, n)
    gof.names <- c(gof.names, "NumObs")
    gof.decimal <- c(gof.decimal, FALSE)
  }
  
  tr <- createTexreg(
    coef.names = coefficient.names,
    coef = coefficients,
    se = standard.errors,
    pvalues = significance,
    gof.names = gof.names,
    gof = gof,
    gof.decimal = gof.decimal
  )
  return(tr)
}

 


# extension for pmg objects (from the plm package)
Extract2.pmg <- function(model, include.nobs = TRUE, ...) {
  s <- summary(model, ...)
  
  co <- s$coef
  se <- (diag(s$vcov))^(1/2)  # standard errors
  t <- co / se  # t-statistics
  n <- length(s$resid)  # number of observations
  d <- n - length(co)  # degrees of freedom
  pval <- 2 * pt(-abs(t), df = d)
  tab <- cbind(co, se, pval)  # coefficient table
  names <- rownames(tab)
  
  gof <- numeric()
  gof.names <- character()
  gof.decimal <- logical()
  if (include.nobs == TRUE) {
    gof <- c(gof, n)
    gof.names <- c(gof.names, "NumObs")
    gof.decimal <- c(gof.decimal, FALSE)
  }
  
  tr <- createTexreg(
    coef.names = names,
    coef = co,
    se = se,
    pvalues = pval,
    gof.names = gof.names,
    gof = gof,
    gof.decimal = gof.decimal
  )
  return(tr)
}

 


# extension for polr objects (MASS package)
Extract2.polr <- function(model, include.thresholds = FALSE, include.aic = TRUE,
                         include.bic = TRUE, include.loglik = TRUE, include.deviance = TRUE,
                         include.nobs = TRUE, ...) {
  s <- summary(model, ...)
  
  tab <- s$coefficients
  zeta.names <- names(s$zeta)
  beta <- tab[!rownames(tab) %in% zeta.names, ]
  thresh <- tab[rownames(tab) %in% zeta.names, ]
  
  if (sum(!rownames(tab) %in% zeta.names) == 1) {
    beta <- t(beta)
    rownames(beta) <- rownames(tab)[!rownames(tab) %in% zeta.names]
  }
  if (sum(rownames(tab) %in% zeta.names) == 1) {
    thresh <- t(thresh)
    rownames(thresh) <- rownames(tab)[rownames(tab) %in% zeta.names]
  }
  
  threshold.names <- rownames(thresh)
  threshold.coef <- thresh[, 1]
  threshold.se <- thresh[, 2]
  threshold.zval <- thresh[, 1] / thresh[, 2]
  threshold.pval <- 2 * pnorm(abs(threshold.zval), lower.tail = FALSE)
  
  beta.names <- rownames(beta)
  beta.coef <- beta[, 1]
  beta.se <- beta[, 2]
  beta.zval <- beta[, 1] / beta[, 2]
  beta.pval <- 2 * pnorm(abs(beta.zval), lower.tail = FALSE)
  
  if (include.thresholds == TRUE) {
    names <- c(beta.names, threshold.names)
    coef <- c(beta.coef, threshold.coef)
    se <- c(beta.se, threshold.se)
    pval <- c(beta.pval, threshold.pval)
  } else {
    names <- beta.names
    coef <- beta.coef
    se <- beta.se
    pval <- beta.pval
  }
  
  n <- nobs(model)
  lik <- logLik(model)[1]
  aic <- AIC(model)
  bic <- BIC(model)
  dev <- deviance(model)
  
  gof <- numeric()
  gof.names <- character()
  gof.decimal <- logical()
  if (include.aic == TRUE) {
    gof <- c(gof, aic)
    gof.names <- c(gof.names, "AIC")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.bic == TRUE) {
    gof <- c(gof, bic)
    gof.names <- c(gof.names, "BIC")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.loglik == TRUE) {
    gof <- c(gof, lik)
    gof.names <- c(gof.names, "LogLik")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.deviance == TRUE) {
    gof <- c(gof, dev)
    gof.names <- c(gof.names, "Deviance")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.nobs == TRUE) {
    gof <- c(gof, n)
    gof.names <- c(gof.names, "NumObs")
    gof.decimal <- c(gof.decimal, FALSE)
  }
  
  tr <- createTexreg(
    coef.names = names,
    coef = coef,
    se = se,
    pvalues = pval,
    gof.names = gof.names,
    gof = gof,
    gof.decimal = gof.decimal
  )
  return(tr)
}

 


# extension for rem.dyad objects (relevent package)
Extract2.rem.dyad <- function(model, include.nvertices = TRUE,
                             include.events = TRUE, include.aic = TRUE, include.aicc = TRUE,
                             include.bic = TRUE, ...) {
  
  coef <- model$coef
  coefnames <- names(coef)
  se <- diag(model$cov)^0.5
  zval <- coef / se
  pval <- 2 * (1 - pnorm(abs(zval)))
  
  gof <- numeric()
  gof.names <- character()
  gof.decimal <- logical()
  if (include.nvertices == TRUE) {
    num.nodes <- model$n
    gof <- c(gof, num.nodes)
    gof.names <- c(gof.names, "Num. nodes")
    gof.decimal <- c(gof.decimal, FALSE)
  }
  if (include.events == TRUE) {
    num.events <- model$m
    gof <- c(gof, num.events)
    gof.names <- c(gof.names, "Num. events")
    gof.decimal <- c(gof.decimal, FALSE)
  }
  if (include.aic == TRUE) {
    aic <- model$AIC
    gof <- c(gof, aic)
    gof.names <- c(gof.names, "AIC")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.aicc == TRUE) {
    aicc <- model$AICC
    gof <- c(gof, aicc)
    gof.names <- c(gof.names, "AICC")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.bic == TRUE) {
    bic <- model$BIC
    gof <- c(gof, bic)
    gof.names <- c(gof.names, "BIC")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  
  tr <- createTexreg(
    coef.names = coefnames,
    coef = coef,
    se = se,
    pvalues = pval,
    gof.names = gof.names,
    gof = gof,
    gof.decimal = gof.decimal
  )
  return(tr)
}

 
# extension for rlm objects (MASS package)
Extract2.rlm <- function (model, include.nobs = TRUE, ...) {
  s <- summary(model, ...)
  
  names <- rownames(s$coef)
  co <- s$coef[, 1]
  se <- s$coef[, 2]
  
  gof <- numeric()
  gof.names <- character()
  gof.decimal <- logical()
  if (include.nobs == TRUE) {
    n <- nobs(model)
    gof <- c(gof, n)
    gof.names <- c(gof.names, "NumObs")
    gof.decimal <- c(gof.decimal, FALSE)
  }
  
  tr <- createTexreg(
    coef.names = names,
    coef = co,
    se = se,
    gof.names = gof.names,
    gof = gof,
    gof.decimal = gof.decimal
  )
  return(tr)
}

 


# extension for rq objects (quantreg package)
Extract2.rq <- function(model, include.nobs = TRUE, include.percentile = TRUE,
                       ...) {
  s <- summary(model, cov = TRUE, ...)
  
  co <- s$coef[, 1]
  names <- rownames(s$coef)
  se <- s$coef[, 2]
  pval <- s$coef[, 4]
  
  n <- length(s$resid)
  tau <- s$tau
  
  gof <- numeric()
  gof.names <- character()
  gof.decimal <- logical()
  if (include.nobs == TRUE) {
    gof <- c(gof, n)
    gof.names <- c(gof.names, "NumObs")
    gof.decimal <- c(gof.decimal, FALSE)
  }
  if (include.percentile == TRUE) {
    gof <- c(gof, tau)
    gof.names <- c(gof.names, "Percentile")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  
  tr <- createTexreg(
    coef.names = names,
    coef = co,
    se = se,
    pvalues = pval,
    gof.names = gof.names,
    gof = gof,
    gof.decimal = gof.decimal
  )
  return(tr)
}

#  
# 
# 
# # extension for sarlm objects (spdep package)
# Extract2.sarlm <- function(model, include.nobs = TRUE, include.loglik = TRUE,
#                           include.aic = TRUE, include.lr = TRUE, include.wald = TRUE, ...) {
#   s <- summary(model, ...)
#   
#   names <- rownames(s$Coef)
#   cf <- s$Coef[, 1]
#   se <- s$Coef[, 2]
#   p <- s$Coef[, ncol(s$Coef)]
#   
#   if (model$type != "error") {  # include coefficient for autocorrelation term
#     rho <- model$rho
#     cf <- c(cf, rho)
#     names <- c(names, "$\\rho$")
#     if (!is.null(model$rho.se)) {
#       if (!is.null(model$adj.se)) {
#         rho.se <- sqrt((model$rho.se^2) * model$adj.se)
#       } else {
#         rho.se <- model$rho.se
#       }
#       rho.pval <- 2 * (1 - pnorm(abs(rho / rho.se)))
#       se <- c(se, rho.se)
#       p <- c(p, rho.pval)
#     } else {
#       se <- c(se, NA)
#       p <- c(p, NA)
#     }
#   }
#   
#   if (!is.null(model$lambda)) {
#     cf <-c(cf, model$lambda)
#     names <- c(names, "$\\lambda$")
#     if (!is.null(model$lambda.se)) {
#       if (!is.null(model$adj.se)) {
#         lambda.se <- sqrt((model$lambda.se^2) * model$adj.se)
#       } else {
#         lambda.se <- model$lambda.se
#       }
#       lambda.pval <- 2 * (1 - pnorm(abs(model$lambda / lambda.se)))
#       se <- c(se, lambda.se)
#       p <- c(p, lambda.pval)
#     } else {
#       se <- c(se, NA)
#       p <- c(p, NA)
#     }
#   }
#   
#   gof <- numeric()
#   gof.names <- character()
#   gof.decimal <- logical()
#   
#   if (include.nobs == TRUE) {
#     n <- length(s$fitted.values)
#     param <- s$parameters
#     gof <- c(gof, n, param)
#     gof.names <- c(gof.names, "NumObs", "Parameters")
#     gof.decimal <- c(gof.decimal, FALSE, FALSE)
#   }
#   if (include.loglik == TRUE) {
#     ll <- s$LL
#     gof <- c(gof, ll)
#     gof.names <- c(gof.names, "LogLik")
#     gof.decimal <- c(gof.decimal, TRUE)
#   }
#   if (include.aic == TRUE) {
#     aic <- AIC(model)
#     aiclm <- s$AIC_lm.model
#     gof <- c(gof, aiclm, aic)
#     gof.names <- c(gof.names, "AIC (Linear model)", "AIC (Spatial model)")
#     gof.decimal <- c(gof.decimal, TRUE, TRUE)
#   }
#   if (include.lr == TRUE && !is.null(s$LR1)) {
#     gof <- c(gof, s$LR1$statistic[[1]], s$LR1$p.value[[1]])
#     gof.names <- c(gof.names, "LR test: statistic", "LR test: p-value")
#     gof.decimal <- c(gof.decimal, TRUE, TRUE)
#   }
#   if (include.wald == TRUE && !is.null(model$Wald1)) {
#     waldstat <- model$Wald1$statistic
#     waldp <- model$Wald1$p.value
#     gof <- c(gof, waldstat, waldp)
#     gof.names <- c(gof.names, "Wald test: statistic", "Wald test: p-value")
#     gof.decimal <- c(gof.decimal, TRUE, TRUE)
#   }
#   
#   tr <- createTexreg(
#     coef.names = names,
#     coef = cf,
#     se = se,
#     pvalues = p,
#     gof.names = gof.names,
#     gof = gof,
#     gof.decimal = gof.decimal
#   )
#   return(tr)
# }
# 
#  
# 
# # extension for selection objects (sampleSelection package)
# Extract2.selection <- function(model, prefix = TRUE, include.selection = TRUE,
#                               include.outcome = TRUE, include.errors = TRUE, include.aic = TRUE,
#                               include.bic = TRUE, include.loglik = TRUE, include.rsquared = TRUE,
#                               include.adjrs = TRUE, include.nobs = TRUE, ...) {
#   
#   # Extract2 coefficients etc.
#   s <- summary(model, ...)
#   coefs <- coef(model)
#   coef.tab <- coef(s)
#   rn <- names(coefs)
#   se <- coef.tab[, 2]
#   p <- coef.tab[, 4]
#   
#   # add prefixes to labels of selection and outcome components
#   indices.selection <- s$param$index$betaS
#   if (model$tobitType == 5) {
#     indices.outcome <- s$param$index$outcome
#   } else if(model$tobitType == 2) {
#     indices.outcome <- s$param$index$betaO
#   }
#   indices.errorterms <- s$param$index$errTerms
#   if (prefix == TRUE) {
#     rn[indices.selection] <- paste("S:", rn[indices.selection])
#     rn[indices.outcome] <- paste("O:", rn[indices.outcome])
#   }
#   
#   # retain only those components that are set up in the arguments
#   include <- numeric()
#   if (include.selection == TRUE) {
#     include <- c(include, indices.selection)
#   }
#   if (include.outcome == TRUE) {
#     include <- c(include, indices.outcome)
#   }
#   if (include.errors == TRUE) {
#     include <- c(include, indices.errorterms)
#   }
#   coefs <- coefs[include]
#   rn <- rn[include]
#   se <- se[include]
#   p <- p[include]
#   
#   # GOF block
#   gof <- numeric()
#   gof.names <- character()
#   gof.decimal <- logical()
#   if (include.aic == TRUE && "loglik" %in% names(s)) {
#     gof <- c(gof, AIC(model))
#     gof.names <- c(gof.names, "AIC")
#     gof.decimal <- c(gof.decimal, TRUE)
#   }
#   if (include.bic == TRUE && "loglik" %in% names(s)) {
#     gof <- c(gof, BIC(model))
#     gof.names <- c(gof.names, "BIC")
#     gof.decimal <- c(gof.decimal, TRUE)
#   }
#   if (include.loglik == TRUE && "loglik" %in% names(s)) {
#     gof <- c(gof, logLik(model)[1])
#     gof.names <- c(gof.names, "Log\ Likelihood")
#     gof.decimal <- c(gof.decimal, TRUE)
#   }
#   if (include.rsquared == TRUE && "rSquared" %in% names(s)) {
#     gof <- c(gof, s$rSquared$R2)
#     gof.names <- c(gof.names, "R2")
#     gof.decimal <- c(gof.decimal, TRUE)
#   }
#   if (include.adjrs == TRUE && "rSquared" %in% names(s)) {
#     gof <- c(gof, s$rSquared$R2adj)
#     gof.names <- c(gof.names, "adjR2")
#     gof.decimal <- c(gof.decimal, TRUE)
#   }
#   if (include.nobs == TRUE) {
#     if(model$tobitType == 5) {
#       gof <- c(gof, s$param$nObs, s$param$N1, s$param$N2)
#     } else if(model$tobitType == 2) {
#       gof <- c(gof, s$param$nObs, s$param$N0, s$param$N1)
#     }
#     gof.names <- c(gof.names, "NumObs", "Censored", "Observed")
#     gof.decimal <- c(gof.decimal, FALSE, FALSE, FALSE)
#   }
#   
#   # create and return texreg object
#   tr <- createTexreg(
#     coef.names = rn,
#     coef = coefs,
#     se = se,
#     pvalues = p,
#     gof.names = gof.names,
#     gof = gof,
#     gof.decimal = gof.decimal
#   )
# }
# 
#  

# 
# # extension for sienaFit objects (RSiena package)
# Extract2.sienaFit <- function(model, include.iterations = TRUE, ...) {
#   
#   s <- summary(model, ...)
#   
#   coefs <- c(model$rate, model$theta)
#   
#   theta.names <- s$effects$effectName
#   if (length(theta.names) == length(coefs)) {
#     coef.names <- theta.names
#   } else {
#     if (!is.null(model$rate)) {
#       rate <- model$rate
#     } else {
#       rate <- which(model$effects$type == "rate")
#     }
#     rate.names <- paste("Rate parameter period", 1:length(rate))
#     coef.names <- c(rate.names, theta.names)
#   }
#   
#   se <- c(model$vrate, sqrt(diag(model$covtheta)))
#   
#   pval <- 2 * pnorm(-abs(coefs / se))
#   
#   gof <- numeric()
#   gof.names <- character()
#   gof.decimal <- logical()
#   if (include.iterations == TRUE) {
#     n <- s$n
#     gof <- c(gof, n)
#     gof.names <- c(gof.names, "Iterations")
#     gof.decimal <- c(gof.decimal, FALSE)
#   }
#   
#   tr <- createTexreg(
#     coef.names = coef.names,
#     coef = coefs,
#     se = se,
#     pvalues = pval,
#     gof.names = gof.names,
#     gof = gof,
#     gof.decimal = gof.decimal
#   )
#   return(tr)
# }
# 
#  
# 
# 
# # extension for simex objects
# Extract2.simex <- function(model, jackknife = TRUE, include.nobs = TRUE, ...) {
#   s <- summary(model, ...)
#   
#   if (jackknife == TRUE) {
#     names <- rownames(s$coefficients$jackknife)
#     co <- s$coefficients$jackknife[, 1]
#     se <- s$coefficients$jackknife[, 2]
#     pval <- s$coefficients$jackknife[, 4]
#   } else {
#     names <- rownames(s$coefficients$asymptotic)
#     co <- s$coefficients$asymptotic[, 1]
#     se <- s$coefficients$asymptotic[, 2]
#     pval <- s$coefficients$asymptotic[, 4]
#   }
#   
#   gof <- numeric()
#   gof.names <- character()
#   gof.decimal <- logical()
#   if (include.nobs == TRUE) {
#     n <- length(model$model$residuals)
#     gof <- c(gof, n)
#     gof.names <- c(gof.names, "NumObs")
#     gof.decimal <- c(gof.decimal, FALSE)
#   }
#   
#   tr <- createTexreg(
#     coef.names = names,
#     coef = co,
#     se = se,
#     pvalues = pval,
#     gof.names = gof.names,
#     gof = gof,
#     gof.decimal = gof.decimal
#   )
#   return(tr)
# }
# 
#  
# 
# 
# # extension for stergm objects (tergm package)
# Extract2.stergm <- function(model, beside = FALSE, include.formation = TRUE,
#                            include.dissolution = TRUE, include.nvertices = TRUE, include.aic = FALSE,
#                            include.bic = FALSE, include.loglik = FALSE, ...) {
#   s <- summary(model, ...)
#   
#   if (beside == FALSE) {
#     co <- numeric()
#     se <- numeric()
#     names <- character()
#     pval <- numeric()
#     if (include.formation == TRUE) {
#       names <- paste("Formation:", rownames(s$formation$coefs))
#       co <- s$formation$coefs[, 1]
#       se <- s$formation$coefs[, 2]
#       pval <- s$formation$coefs[, 4]
#     }
#     if (include.dissolution == TRUE) {
#       names <- c(names, paste("Dissolution:", rownames(s$dissolution$coefs)))
#       co <- c(co, s$dissolution$coefs[, 1])
#       se <- c(se, s$dissolution$coefs[, 2])
#       pval <- c(pval, s$dissolution$coefs[, 4])
#     }
#     
#     gof <- numeric()
#     gof.names <- character()
#     gof.decimal <- logical()
#     if (include.nvertices == TRUE) {
#       nvertices <- model$formation.fit$network$gal$n
#       gof <- c(gof, nvertices)
#       gof.names <- c(gof.names, "Num. vertices")
#       gof.decimal <- c(gof.decimal, FALSE)
#     }
#     if (include.aic == TRUE) {
#       aic.dis <- s$dissolution$aic
#       aic.form <- s$formation$aic
#       gof <- c(gof, aic.form, aic.dis)
#       gof.names <- c(gof.names, "Formation: AIC", "Dissolution: AIC")
#       gof.decimal <- c(gof.decimal, TRUE, TRUE)
#     }
#     if (include.bic == TRUE) {
#       bic.dis <- s$dissolution$bic
#       bic.form <- s$formation$bic
#       gof <- c(gof, bic.form, bic.dis)
#       gof.names <- c(gof.names, "Formation: BIC", "Dissolution: BIC")
#       gof.decimal <- c(gof.decimal, TRUE, TRUE)
#     }
#     if (include.loglik == TRUE) {
#       lik <- logLik(model)[1]
#       gof <- c(gof, lik)
#       gof.names <- c(gof.names, "LogLik")
#       gof.decimal <- c(gof.decimal, TRUE)
#     }
#     
#     tr <- createTexreg(
#       coef.names = names,
#       coef = co,
#       se = se,
#       pvalues = pval,
#       gof.names = gof.names,
#       gof = gof,
#       gof.decimal = gof.decimal
#     )
#     
#     return(tr)
#   } else {
#     trList <- list()
#     
#     co <- numeric()
#     se <- numeric()
#     names <- character()
#     pval <- numeric()
#     if (include.formation == TRUE) {
#       f.names <- rownames(s$formation$coefs)
#       f.co <- s$formation$coefs[, 1]
#       f.se <- s$formation$coefs[, 2]
#       f.pval <- s$formation$coefs[, 4]
#     }
#     if (include.dissolution == TRUE) {
#       d.names <- rownames(s$dissolution$coefs)
#       d.co <- s$dissolution$coefs[, 1]
#       d.se <- s$dissolution$coefs[, 2]
#       d.pval <- s$dissolution$coefs[, 4]
#     }
#     
#     f.gof <- numeric()
#     f.gof.names <- character()
#     f.gof.decimal <- logical()
#     d.gof <- numeric()
#     d.gof.names <- character()
#     d.gof.decimal <- logical()
#     if (include.nvertices == TRUE) {
#       nvertices <- model$formation.fit$network$gal$n
#       f.gof <- c(f.gof, nvertices)
#       f.gof.names <- c(f.gof.names, "Num. vertices")
#       f.gof.decimal <- c(f.gof.decimal, FALSE)
#       d.gof <- c(d.gof, nvertices)
#       d.gof.names <- c(d.gof.names, "Num. vertices")
#       d.gof.decimal <- c(d.gof.decimal, FALSE)
#     }
#     if (include.aic == TRUE) {
#       f.aic <- s$formation$aic
#       f.gof <- c(f.gof, f.aic)
#       f.gof.names <- c(f.gof.names, "AIC")
#       f.gof.decimal <- c(f.gof.decimal, TRUE)
#       d.aic <- s$dissolution$aic
#       d.gof <- c(d.gof, d.aic)
#       d.gof.names <- c(d.gof.names, "AIC")
#       d.gof.decimal <- c(d.gof.decimal, TRUE)
#     }
#     if (include.bic == TRUE) {
#       f.bic <- s$formation$bic
#       f.gof <- c(f.gof, f.bic)
#       f.gof.names <- c(f.gof.names, "BIC")
#       f.gof.decimal <- c(f.gof.decimal, TRUE)
#       d.bic <- s$dissolution$bic
#       d.gof <- c(d.gof, d.bic)
#       d.gof.names <- c(d.gof.names, "BIC")
#       d.gof.decimal <- c(d.gof.decimal, TRUE)
#     }
#     if (include.loglik == TRUE) {
#       lik <- logLik(model)[1]
#       f.gof <- c(f.gof, lik)
#       f.gof.names <- c(f.gof.names, "LogLik")
#       f.gof.decimal <- c(f.gof.decimal, TRUE)
#       d.gof <- c(d.gof, lik)
#       d.gof.names <- c(d.gof.names, "LogLik")
#       d.gof.decimal <- c(d.gof.decimal, TRUE)
#     }
#     
#     if (include.formation == TRUE) {
#       tr <- createTexreg(
#         coef.names = f.names,
#         coef = f.co,
#         se = f.se,
#         pvalues = f.pval,
#         gof.names = f.gof.names,
#         gof = f.gof,
#         gof.decimal = f.gof.decimal,
#         model.name = "Formation"
#       )
#       trList[[length(trList) + 1]] <- tr
#     }
#     
#     if (include.dissolution == TRUE) {
#       tr <- createTexreg(
#         coef.names = d.names,
#         coef = d.co,
#         se = d.se,
#         pvalues = d.pval,
#         gof.names = d.gof.names,
#         gof = d.gof,
#         gof.decimal = d.gof.decimal,
#         model.name = "Dissolution"
#       )
#       trList[[length(trList) + 1]] <- tr
#     }
#     
#     return(trList)
#   }
# }
# 
#  

# extension for survreg objects (survival package)
Extract2.survreg <- function(model, include.aic = TRUE, include.bic = TRUE,
                            include.loglik = TRUE, include.deviance = TRUE, include.nobs = TRUE, ...) {
  
  s <- summary(model, ...)
  
  names <- rownames(s$table)
  co <- s$table[, 1]
  se <- s$table[, 2]
  pval <- s$table[, ncol(s$table)]
  
  gof <- numeric()
  gof.names <- character()
  gof.decimal <- logical()
  if (include.aic == TRUE) {
    aic <- AIC(model)
    gof <- c(gof, aic)
    gof.names <- c(gof.names, "AIC")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.bic == TRUE) {
    bic <- BIC(model)
    if (!is.null(bic) && !is.na(bic)) {
      gof <- c(gof, bic)
      gof.names <- c(gof.names, "BIC")
      gof.decimal <- c(gof.decimal, TRUE)
    }
  }
  if (include.loglik == TRUE) {
    lik <- logLik(model)[1]
    gof <- c(gof, lik)
    gof.names <- c(gof.names, "LogLik")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.deviance == TRUE) {
    dev <- deviance(model)
    if (!is.null(dev)) {
      gof <- c(gof, dev)
      gof.names <- c(gof.names, "Deviance")
      gof.decimal <- c(gof.decimal, TRUE)
    }
  }
  if (include.nobs == TRUE) {
    n <- length(model$linear.predictors)
    gof <- c(gof, n)
    gof.names <- c(gof.names, "NumObs")
    gof.decimal <- c(gof.decimal, FALSE)
  }
  
  tr <- createTexreg(
    coef.names = names,
    coef = co,
    se = se,
    pvalues = pval,
    gof.names = gof.names,
    gof = gof,
    gof.decimal = gof.decimal
  )
  return(tr)
}

 

Extract2.survreg.penal <- Extract2.survreg
 


# extension for svyglm objects (survey package)
Extract2.svyglm <- function(model, include.aic = FALSE, include.bic = FALSE,
                           include.loglik = FALSE, include.deviance = TRUE, include.dispersion = TRUE,
                           include.nobs = TRUE, ...) {
  s <- summary(model, ...)
  
  names <- rownames(coef(s))
  co <- coef(s)[, 1]
  se <- coef(s)[, 2]
  pval <- coef(s)[, 4]
  
  gof <- numeric()
  gof.names <- character()
  gof.decimal <- logical()
  if (include.aic == TRUE) {
    aic <- AIC(model)
    if (length(aic) > 0) {
      gof <- c(gof, aic)
      gof.names <- c(gof.names, "AIC")
      gof.decimal <- c(gof.decimal, TRUE)
    } else {
      warning("AIC was not available and will be skipped!")
    }
  }
  if (include.bic == TRUE) {
    bic <- BIC(model)
    if (length(bic) > 0) {
      gof <- c(gof, bic)
      gof.names <- c(gof.names, "BIC")
      gof.decimal <- c(gof.decimal, TRUE)
    } else {
      warning("BIC was not available and will be skipped!")
    }
  }
  if (include.loglik == TRUE) {
    lik <- logLik(model)[1]
    if (length(lik) > 0) {
      gof <- c(gof, lik)
      gof.names <- c(gof.names, "LogLik")
      gof.decimal <- c(gof.decimal, TRUE)
    } else {
      warning("The LogLik was not available and will be skipped!")
    }
  }
  if (include.deviance == TRUE) {
    dev <- deviance(model)
    gof <- c(gof, dev)
    gof.names <- c(gof.names, "Deviance")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.dispersion == TRUE) {
    disp <- s$dispersion[1]
    gof <- c(gof, disp)
    gof.names <- c(gof.names, "Dispersion")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.nobs == TRUE) {
    n <- nobs(model)
    gof <- c(gof, n)
    gof.names <- c(gof.names, "NumObs")
    gof.decimal <- c(gof.decimal, FALSE)
  }
  
  tr <- createTexreg(
    coef.names = names,
    coef = co,
    se = se,
    pvalues = pval,
    gof.names = gof.names,
    gof = gof,
    gof.decimal = gof.decimal
  )
  return(tr)
}

 


# extension for systemfit objects
Extract2.systemfit <- function(model, include.rsquared = TRUE,
                              include.adjrs = TRUE, include.nobs = TRUE, beside = FALSE,
                              include.suffix = FALSE, ...) {
  if (beside == TRUE) {
    equationList <- list()
    for(eq in model$eq) {  # go through estimated equations
      s <- summary(eq, ...)  # Extract2 model summary
      names <- rownames(coef(s))
      co <- coef(s)[, 1]
      se <- coef(s)[, 2]
      pval <- coef(s)[, 4]
      
      gof <- numeric()
      gof.names <- character()
      gof.decimal <- logical()
      if (include.rsquared == TRUE) {
        rs <- s$r.squared  # Extract2 r-squared
        gof <- c(gof, rs)
        gof.names <- c(gof.names, "R2")
        gof.decimal <- c(gof.decimal, TRUE)
      }
      if (include.adjrs == TRUE) {
        adj <- s$adj.r.squared  # Extract2 adjusted r-squared
        gof <- c(gof, adj)
        gof.names <- c(gof.names, "adjR2")
        gof.decimal <- c(gof.decimal, TRUE)
      }
      if (include.nobs == TRUE) {
        n <- length(s$residuals)  # Extract2 number of observations
        gof <- c(gof, n)
        gof.names <- c(gof.names, "NumObs")
        gof.decimal <- c(gof.decimal, FALSE)
      }
      
      tr <- createTexreg(
        coef.names = names,
        coef = co,
        se = se,
        pvalues = pval,
        gof.names = gof.names,
        gof = gof,
        gof.decimal = gof.decimal,
        model.name = s$eqnLabel
      )
      equationList[[eq$eqnNo]] <- tr
    }
    return(equationList)  #returns a list of table.content lists
  } else {
    nm <- character()
    co <- se <- pval <- numeric()
    for(eq in model$eq) {  # go through estimated equations
      s <- summary(eq, ...)  # Extract2 model summary
      nm <- c(nm, sapply(rownames(coef(s)), function(x) {
        if (include.suffix == FALSE) {
          paste0(eq$eqnLabel, ": ", x)
        } else {
          paste0(x, " (", eq$eqnLabel, ")")
        }
      }))
      co <- c(co, coef(s)[, 1])
      se <- c(se, coef(s)[, 2])
      pval <- c(pval, coef(s)[, 4])
    }
    
    gof <- numeric()
    gof.names <- character()
    gof.decimal <- logical()
    for(eq in model$eq) {
      s <- summary(eq, ...)
      if (include.rsquared == TRUE) {
        rs <- s$r.squared  # Extract2 r-squared
        gof <- c(gof, rs)
        gof.names <- c(gof.names, ifelse(include.suffix == TRUE,
                                         paste0("R2 (", eq$eqnLabel, ")"),
                                         paste0(eq$eqnLabel, ": R2")))
        gof.decimal <- c(gof.decimal, TRUE)
      }
    }
    
    for(eq in model$eq) {
      s <- summary(eq, ...)
      if (include.adjrs == TRUE) {
        adj <- s$adj.r.squared  # Extract2 adjusted r-squared
        gof <- c(gof, adj)
        gof.names <- c(gof.names, ifelse(include.suffix == TRUE,
                                         paste0("adjR2 (", eq$eqnLabel, ")"),
                                         paste0(eq$eqnLabel, ": adjR2")))
        gof.decimal <- c(gof.decimal, TRUE)
      }
    }
    
    if (include.nobs == TRUE) {  # number of observations
      gof <- c(gof, nobs(model))
      gof.names <- c(gof.names, "NumObs\ (total)")
      gof.decimal <- c(gof.decimal, FALSE)
    }
    
    tr <- createTexreg(
      coef.names = nm,
      coef = co,
      se = se,
      pvalues = pval,
      gof.names = gof.names,
      gof = gof,
      gof.decimal = gof.decimal
    )
    return(tr)
  }
}

 

# 
# # extension for texreg objects (texreg package)
# Extract2.texreg <- function(model, ...) {
#   tr <- createTexreg(
#     coef.names = model@coef.names,
#     coef = model@coef,
#     se = model@se,
#     pvalues = model@pvalues,
#     ci.low = model@ci.low,
#     ci.up = model@ci.up,
#     gof.names = model@gof.names,
#     gof = model@gof,
#     gof.decimal = model@gof.decimal
#   )
#   return(tr)
# }
# 
#  


# extension for tobit objects (AER package)
Extract2.tobit <- function(model, include.aic = TRUE, include.bic = TRUE,
                          include.loglik = TRUE, include.deviance = TRUE, include.nobs = FALSE,
                          include.censnobs = TRUE, include.wald = TRUE, ...) {
  s <- summary(model, ...)
  
  names <- rownames(s$coefficients)
  co <- s$coefficients[, 1]
  se <- s$coefficients[, 2]
  pval <- s$coefficients[, 4]
  
  gof <- numeric()
  gof.names <- character()
  gof.decimal <- logical()
  if (include.aic == TRUE) {
    aic <- AIC(model)
    gof <- c(gof, aic)
    gof.names <- c(gof.names, "AIC")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.bic == TRUE) {
    bic <- BIC(model)
    gof <- c(gof, bic)
    gof.names <- c(gof.names, "BIC")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.loglik == TRUE) {
    lik <- logLik(model)[1]
    gof <- c(gof, lik)
    gof.names <- c(gof.names, "LogLik")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.deviance == TRUE) {
    dev <- deviance(model)
    gof <- c(gof, dev)
    gof.names <- c(gof.names, "Deviance")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.nobs == TRUE) {
    n <- nobs(model)
    gof <- c(gof, n)
    gof.names <- c(gof.names, "NumObs")
    gof.decimal <- c(gof.decimal, FALSE)
  }
  if (include.censnobs == TRUE) {
    censnobs <- s$n
    censnobs.names <- names(censnobs)
    gof <- c(gof, censnobs)
    gof.names <- c(gof.names, censnobs.names)
    gof.decimal <- c(gof.decimal, rep(FALSE, length(censnobs)))
  }
  if (include.wald == TRUE) {
    wald <- s$wald
    gof <- c(gof, wald)
    gof.names <- c(gof.names, "Wald Test")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  
  tr <- createTexreg(
    coef.names = names,
    coef = co,
    se = se,
    pvalues = pval,
    gof.names = gof.names,
    gof = gof,
    gof.decimal = gof.decimal
  )
  return(tr)
}

 


# extension for vglm objects (VGAM package)
# please report errors to Christoph Riedl at Northeastern University;
# e-mail: c.riedl@neu.edu
Extract2.vglm <- function(model, include.loglik = TRUE, include.df = TRUE,
                         include.nobs = TRUE, ...) {
  
  s <- summary(model)
  names <- rownames(coef(s))
  co <- s@coef3[, 1]
  se <- s@coef3[, 2]
  pval <- s@coef3[, 4]
  
  gof <- numeric()
  gof.names <- character()
  gof.decimal <- logical()
  if (include.loglik == TRUE) {
    gof <- c(gof, VGAM::logLik.vlm(model))
    gof.names <- c(gof.names, "LogLik")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.df == TRUE) {
    gof <- c(gof, df <- s@df[2])
    gof.names <- c(gof.names, "DF")
    gof.decimal <- c(gof.decimal, FALSE)
  }
  if (include.nobs == TRUE) {
    gof <- c(gof, length(stats::residuals(s)))
    gof.names <- c(gof.names, "NumObs")
    gof.decimal <- c(gof.decimal, FALSE)
  }
  
  tr <- createTexreg(
    coef.names = names,
    coef = co,
    se = se,
    pvalues = pval,
    gof.names = gof.names,
    gof = gof,
    gof.decimal = gof.decimal
  )
  return(tr)
}

 
# 
# # extension for weibreg objects (eha package)
# Extract2.weibreg <- function(model, include.aic = TRUE, include.loglik = TRUE,
#                             include.lr = TRUE, include.nobs = TRUE, include.events = TRUE,
#                             include.trisk = TRUE, ...) {
#   
#   coefs <- model$coefficients
#   coef.names <- names(coefs)
#   se <- sqrt(diag(model$var))
#   pval <- 1 - pchisq((coefs / se)^2, 1)
#   
#   gof <- numeric()
#   gof.names <- character()
#   gof.decimal <- logical()
#   if (include.loglik == TRUE) {
#     lik <- model$loglik[2]
#     gof <- c(gof, lik)
#     gof.names <- c(gof.names, "LogLik")
#     gof.decimal <- c(gof.decimal, TRUE)
#   }
#   if (include.lr == TRUE) {
#     lr <- -2 * (model$loglik[1] - model$loglik[2])
#     gof <- c(gof, lr)
#     gof.names <- c(gof.names, "LR test")
#     gof.decimal <- c(gof.decimal, TRUE)
#   }
#   if (include.aic == TRUE) {
#     aic <- 2 * model$loglik[2] + 2 * length(coefs)
#     gof <- c(gof, aic)
#     gof.names <- c(gof.names, "AIC")
#     gof.decimal <- c(gof.decimal, FALSE)
#   }
#   if (include.nobs == TRUE) {
#     n <- nobs(model)
#     gof <- c(gof, n)
#     gof.names <- c(gof.names, "NumObs")
#     gof.decimal <- c(gof.decimal, FALSE)
#   }
#   if (include.events == TRUE) {
#     ev <- model$events
#     gof <- c(gof, ev)
#     gof.names <- c(gof.names, "Num. events")
#     gof.decimal <- c(gof.decimal, FALSE)
#   }
#   if (include.trisk == TRUE) {
#     trisk <- model$ttr
#     gof <- c(gof, trisk)
#     gof.names <- c(gof.names, "Total time at risk")
#     gof.decimal <- c(gof.decimal, FALSE)
#   }
#   
#   tr <- createTexreg(
#     coef.names = coef.names,
#     coef = coefs,
#     se = se,
#     pvalues = pval,
#     gof.names = gof.names,
#     gof = gof,
#     gof.decimal = gof.decimal
#   )
#   return(tr)
# }
# 
#  
# 
# Extract2.phreg <- Extract2.weibreg
#  
# Extract2.aftreg <- Extract2.weibreg
#  
# 
# Extract2.coxreg <- Extract2.weibreg
#  


# extension for wls objects (metaSEM package)
# please report errors to Christoph Riedl at Northeastern University;
# e-mail: c.riedl@neu.edu
Extract2.wls <- function(model, include.nobs = TRUE, ...) {
  
  coefnames <- rownames(summary(model)$coef)
  coefs <- summary(model)$coef[, 1]
  se <- as.numeric(summary(model)$coef[, 2])
  pval <- summary(model)$coef[, 6]
  
  # Compute average variance Extract2ed
  # Based on: http://openmx.psyc.virginia.edu/thread/3988
  # Could also check description of reliability() from {semTools}
  mat <- model$mx.fit$impliedS1$result
  if (is.null(mat)) {
    ave <- NULL
  } else {
    ave <- mean(mat[nrow(mat), -ncol(mat)])
  }
  
  chi      <- summary(model)$stat["Chi-square of independence model", 1]
  dfs       <- summary(model)$stat["DF of independence model", 1]
  # chi.pval <- summary(model)$stat["p value of target model", 1]
  # if(pval < .0001) pval <- "< .0001"
  rmsea    <- summary(model)$stat["RMSEA", 1]
  rmseall  <- summary(model)$stat["RMSEA lower 95% CI", 1]
  rmseaul  <- summary(model)$stat["RMSEA upper 95% CI", 1]
  cfi      <- summary(model)$stat["CFI", 1]
  
  gof <- c(chi, dfs, rmsea, rmseall, rmseaul, cfi)
  gof.names <- c("Chi-square of independence model",
                 "DF of independence model", "RMSEA", "RMSEA lower 95 percent CI",
                 "RMSEA upper 95 percent CI", "CFI")
  gof.decimal <- c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
  if (!is.null(ave)) {
    gof <- c(gof, ave)
    gof.names <- c(gof.names, "Average variance Extract2ed")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.nobs == TRUE) {
    gof <- c(gof, summary(model)$stat["Sample size", 1])
    gof.names <- c(gof.names, "NumObs")
    gof.decimal <- c(gof.decimal, FALSE)
  }
  
  tr <- createTexreg(
    coef.names = coefnames,
    coef = coefs,
    se = se,
    pvalues = pval,
    gof.names = gof.names,
    gof = gof,
    gof.decimal = gof.decimal
  )
  return(tr)
}

 


# extension for zelig objects (Zelig package < 5.0)
Extract2.zelig <- function(model, include.aic = TRUE, include.bic = TRUE,
                          include.loglik = TRUE, include.deviance = TRUE, include.nobs = TRUE,
                          include.rsquared = TRUE, include.adjrs = TRUE, include.fstatistic = TRUE,
                          ...) {
  
  s <- summary(model, ...)
  
  if ("relogit" %in% class(model) || "logit" %in% class(model) ||
      "ls" %in% class(model) || "probit" %in% class(model) ||
      "ologit" %in% class(model)) {
    coefficient.names <- rownames(s$coef)
    coefficients <- s$coef[, 1]
    standard.errors <- s$coef[, 2]
    if ("ologit" %in% class(model)) {
      tval <- s$coef[, 3]
      significance <- 2 * pt(-abs(tval), s$df.residual)
    } else {
      significance <- s$coef[, 4]
    }
    
    gof <- numeric()
    gof.names <- character()
    gof.decimal <- logical()
    if (include.aic == TRUE) {
      aic <- AIC(model)
      gof <- c(gof, aic)
      gof.names <- c(gof.names, "AIC")
      gof.decimal <- c(gof.decimal, TRUE)
    }
    if (include.bic == TRUE) {
      bic <- BIC(model)
      gof <- c(gof, bic)
      gof.names <- c(gof.names, "BIC")
      gof.decimal <- c(gof.decimal, TRUE)
    }
    if (include.loglik == TRUE) {
      lik <- logLik(model)[1]
      gof <- c(gof, lik)
      gof.names <- c(gof.names, "LogLik")
      gof.decimal <- c(gof.decimal, TRUE)
    }
    if (include.deviance == TRUE) {
      dev <- s$deviance
      if (!is.null(dev)) {
        gof <- c(gof, dev)
        gof.names <- c(gof.names, "Deviance")
        gof.decimal <- c(gof.decimal, TRUE)
      }
    }
    if (include.nobs == TRUE) {
      n <- nrow(model$data)
      gof <- c(gof, n)
      gof.names <- c(gof.names, "NumObs")
      gof.decimal <- c(gof.decimal, FALSE)
    }
    if (include.rsquared == TRUE) {
      rs <- s$r.squared  #Extract2 R-squared
      if (!is.null(rs)) {
        gof <- c(gof, rs)
        gof.names <- c(gof.names, "R2")
        gof.decimal <- c(gof.decimal, TRUE)
      }
    }
    if (include.adjrs == TRUE) {
      adj <- s$adj.r.squared  #Extract2 adjusted R-squared
      if (!is.null(adj)) {
        gof <- c(gof, adj)
        gof.names <- c(gof.names, "adjR2")
        gof.decimal <- c(gof.decimal, TRUE)
      }
    }
    if (include.fstatistic == TRUE) {
      fstat <- s$fstatistic[[1]]
      if (!is.null(fstat)) {
        gof <- c(gof, fstat)
        gof.names <- c(gof.names, "FStat")
        gof.decimal <- c(gof.decimal, TRUE)
      }
    }
    
    tr <- createTexreg(
      coef.names = coefficient.names,
      coef = coefficients,
      se = standard.errors,
      pvalues = significance,
      gof.names = gof.names,
      gof = gof,
      gof.decimal = gof.decimal
    )
    return(tr)
  } else if ("mlogit" %in% class(model)) {
    coefficient.names <- rownames(s@coef3)
    coefficients <- s@coef3[, 1]
    standard.errors <- s@coef3[, 2]
    zval <- s@coef3[, 3]
    significance <- 2 * pnorm(abs(zval), lower.tail = FALSE)
    
    gof <- numeric()
    gof.names <- character()
    gof.decimal <- logical()
    if (include.loglik == TRUE) {
      lik <- logLik(model)[1]
      gof <- c(gof, lik)
      gof.names <- c(gof.names, "LogLik")
      gof.decimal <- c(gof.decimal, TRUE)
    }
    if (include.deviance == TRUE) {
      dev <- deviance(s)
      if (!is.null(dev)) {
        gof <- c(gof, dev)
        gof.names <- c(gof.names, "Deviance")
        gof.decimal <- c(gof.decimal, TRUE)
      }
    }
    if (include.nobs == TRUE) {
      n <- nrow(model$data)
      gof <- c(gof, n)
      gof.names <- c(gof.names, "NumObs")
      gof.decimal <- c(gof.decimal, FALSE)
    }
    
    tr <- createTexreg(
      coef.names = coefficient.names,
      coef = coefficients,
      se = standard.errors,
      pvalues = significance,
      gof.names = gof.names,
      gof = gof,
      gof.decimal = gof.decimal
    )
    return(tr)
  } else if ("tobit" %in% class(model)) {
    coefficient.names <- rownames(s$table)
    coefficients <- s$table[, 1]
    standard.errors <- s$table[, 2]
    significance <- s$table[, 5]
    gof <- numeric()
    gof.names <- character()
    gof.decimal <- logical()
    if (include.aic == TRUE) {
      aic <- AIC(model)
      gof <- c(gof, aic)
      gof.names <- c(gof.names, "AIC")
      gof.decimal <- c(gof.decimal, TRUE)
    }
    if (include.bic == TRUE) {
      bic <- BIC(model)
      gof <- c(gof, bic)
      gof.names <- c(gof.names, "BIC")
      gof.decimal <- c(gof.decimal, TRUE)
    }
    if (include.loglik == TRUE) {
      lik <- logLik(model)[1]
      gof <- c(gof, lik)
      gof.names <- c(gof.names, "LogLik")
      gof.decimal <- c(gof.decimal, TRUE)
    }
    if (include.nobs == TRUE) {
      n <- nrow(model$data)
      gof <- c(gof, n)
      gof.names <- c(gof.names, "NumObs")
      gof.decimal <- c(gof.decimal, FALSE)
    }
    tr <- createTexreg(coef.names = coefficient.names, coef = coefficients,
                       se = standard.errors, pvalues = significance, gof.names = gof.names,
                       gof = gof, gof.decimal = gof.decimal)
    return(tr)
  } else {
    stop(paste("Only the following Zelig models are currently supported:",
               "logit, ls, mlogit, ologit, probit, relogit, tobit."))
  }
}

 


# extension for Zelig objects (Zelig package >= 5.0)
Extract2.Zelig <- function(model, include.nobs = TRUE, include.nimp = TRUE, ...) {
  if (model$mi) {
    if (!exists("combine_coef_se", where = "package:Zelig",
                mode = "function")) {
      stop("texreg relies on Zelig's combine_coef_se function to Extract2 model information. Install Zelig >= 5.0-17 to see if texreg can format your model.")
    }
    combined <- Zelig::combine_coef_se(model, messages = FALSE)
    gof <- gof.names <- gof.decimal <- NULL
    if (include.nobs) {
      gof <- c(gof, nrow(model$data))
      gof.names <- c(gof.names, 'NumObs')
      gof.decimal <- c(gof.decimal, FALSE)
    }
    if (include.nimp) {
      if (class(model$originaldata)[1] == 'amelia') {
        gof <- c(gof, model$originaldata$m)
        gof.names <- c(gof.names, 'Num. imp.')
        gof.decimal <- c(gof.decimal, FALSE)
      } else if (class(model$originaldata)[1] == 'mi') { # when imputed dataset was created using to_zelig_mi
        gof <- c(gof, length(model$originaldata))
        gof.names <- c(gof.names, 'Num. imp.')
        gof.decimal <- c(gof.decimal, FALSE)
      }
    }
    out <- createTexreg(coef.names = row.names(combined),
                        coef = combined[, 'Estimate'],
                        se = combined[, 'Std.Error'],
                        pvalues = combined[, 'Pr(>|z|)'],
                        gof.names = gof.names,
                        gof = gof,
                        gof.decimal = gof.decimal)
  } else {
    if ("Zelig-relogit" %in% class(model)) { # remove when users update to Zelig 5.0-16
      mod_original <- model$zelig.out$z.out[[1]]
      class(mod_original) <- "glm"
    }
    else if ("Zelig-tobit" %in% class(model)) { # remove when users update to Zelig 5.0-16
      mod_original <- model$zelig.out$z.out[[1]]
    } else {
      if (!exists("from_zelig_model", where = "package:Zelig",
                  mode = "function")) {
        stop("texreg relies on Zelig's from_zelig_model function to Extract2 model information. Install Zelig >= 5.0-16 to see if texreg can format your model.")
      }
      mod_original <- try(Zelig::from_zelig_model(model), silent = TRUE)
      if (class(mod_original)[1] == "try-error") {
        stop("texreg relies on Zelig's from_zelig_model function to Extract2 information from Zelig models. from_zelig_model does not appear to support models of class ",
             class(model)[1], ".")
      }
    }
    out <- Extract2(mod_original, include.nobs = include.nobs, ...)
  }
  return(out)
}

 


# extension for zeroinfl objects (pscl package)
Extract2.zeroinfl <- function(model, beside = FALSE, include.count = TRUE,
                             include.zero = TRUE, include.aic = TRUE, include.loglik = TRUE,
                             include.nobs = TRUE, ...) {
  
  s <- summary(model, ...)
  
  gof <- numeric()
  gof.names <- character()
  gof.decimal <- logical()
  if (include.aic == TRUE) {
    aic <- AIC(model)
    gof <- c(gof, aic)
    gof.names <- c(gof.names, "AIC")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.loglik == TRUE) {
    lik <- logLik(model)[1]
    gof <- c(gof, lik)
    gof.names <- c(gof.names, "Log\ Likelihood")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.nobs == TRUE) {
    n <- s$n
    gof <- c(gof, n)
    gof.names <- c(gof.names, "NumObs")
    gof.decimal <- c(gof.decimal, FALSE)
  }
  
  count <- s$coefficients$count
  zero <- s$coefficients$zero
  
  if (beside == FALSE) {
    if (include.count == TRUE && include.zero == TRUE) {
      rownames(count) <- paste("Count model:", rownames(count))
      rownames(zero) <- paste("Zero model:", rownames(zero))
      coef.block <- rbind(count, zero)
    } else if (include.count == TRUE) {
      coef.block <- count
    } else if (include.zero == TRUE) {
      coef.block <- zero
    } else {
      stop(paste("Either the include.count or the include.zero argument",
                 "must be TRUE."))
    }
    names <- rownames(coef.block)
    co <- coef.block[, 1]
    se <- coef.block[, 2]
    pval <- coef.block[, 4]
    
    tr <- createTexreg(
      coef.names = names,
      coef = co,
      se = se,
      pvalues = pval,
      gof.names = gof.names,
      gof = gof,
      gof.decimal = gof.decimal
    )
    return(tr)
  } else {
    trList <- list()
    
    c.names <- rownames(count)
    c.co <- count[, 1]
    c.se <- count[, 2]
    c.pval <- count[, 4]
    z.names <- rownames(zero)
    z.co <- zero[, 1]
    z.se <- zero[, 2]
    z.pval <- zero[, 4]
    
    if (include.count == TRUE) {
      tr <- createTexreg(
        coef.names = c.names,
        coef = c.co,
        se = c.se,
        pvalues = c.pval,
        gof.names = gof.names,
        gof = gof,
        gof.decimal = gof.decimal,
        model.name = "Count model"
      )
      trList[[length(trList) + 1]] <- tr
    }
    
    if (include.zero == TRUE) {
      tr <- createTexreg(
        coef.names = z.names,
        coef = z.co,
        se = z.se,
        pvalues = z.pval,
        gof.names = gof.names,
        gof = gof,
        gof.decimal = gof.decimal,
        model.name = "Zero model"
      )
      trList[[length(trList) + 1]] <- tr
    }
    if (length(trList) == 0) {
      stop(paste("Either the include.count or the include.zero argument",
                 "must be TRUE."))
    }
    return(trList)
  }
}

 

Extract2.hurdle <- Extract2.zeroinfl
 
