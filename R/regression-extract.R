#' extract_param
#'
#' Summary fuer Regressionsmodelle. Kopie von broom::tidy
#'
#' @param x Objekt
#' @param include.b,include.beta,include.ci,include.se,include.odds,include.odds.ci,include.statistic,include.p,include.stars,include.df,include.effects,conf.level,conf.method Parameter wie in APA_Tabelle beschrieben.
#' @param ... weitere params
#'
#' @return data.frame tibble  "term","estimate", "beta","std.error" ,"statistic","p.value","stars","conf.high","conf.low","odds","odds.conf.low","odds.conf.high","df","group"
#' @export
#'
#' @examples
#' fit <- lm(Sepal.Width ~ Petal.Length + Petal.Width, iris)
#' broom::tidy(fit)
#' broom::glance(fit)
#'
#' summary(fit)
#' Ordnen(fit)
#' extract_param(fit)
#' APA_Table(fit)
#' APA_Table(fit, type="long")
#'
extract_param <- function(x,
                          include.b = TRUE,
                          include.se = TRUE,
                          include.beta = FALSE,
                          include.ci = FALSE,
                          
                          include.odds = FALSE,
                          include.odds.ci = if(include.odds & include.ci) TRUE else  FALSE,
                          include.statistic = TRUE,
                          include.p = TRUE,
                          include.stars = FALSE,
                          include.df = FALSE,
                          
                          include.effects = c("ran_pars", "fixed"),
                          conf.int = TRUE,# if (include.ci | include.odds.ci) TRUE else  FALSE ,
                          conf.level = 0.95,
                          conf.method = "Wald",
                          fix_format = FALSE,
                          digits.param = 3,
                          digits.odds = 2,
                          digits.test = 2,
                          digits.beta = 2,
                          format="fg",
                          conf.style.1 = FALSE,
                          ...) {
  # "term","estimate", "beta","std.error" ,"statistic","p.value","stars","conf.high","conf.low","odds","odds.conf.low","odds.conf.high","df","group"
  
  
  
  
  
  param <-  "term"
  res <- NULL
  
  # Error in broom:tidy
  if (inherits(x, "lmerModLmerTest")) {
    res <- tidy_lmer(
      x,
      effects = include.effects,
      conf.int = conf.int,
      conf.level = conf.level,
      conf.method =  conf.method)
    res$beta <- NA
    coefs <- res
  }
  else{
     if (inherits(x, "lmerMod")) {
      res <- broom::tidy(
        x,
       # effects =  "fixed",
        conf.int = conf.int,
        conf.level = conf.level,
        conf.method =  conf.method)
      res$p.value <-NA
      res$p.value[which(res$group=="fixed")]<-
        as.vector(lmerTest:::summary.lmerModLmerTest(x)$coefficients[,5])
    }
    else{ 
      res <- broom::tidy(
      x,
      effects =  "fixed",
      conf.int = conf.int,
      conf.level = conf.level,
      conf.method =  conf.method)
      res$group <- "fixed"
      res$df <- NA
    }
    
    if (inherits(x, "lme")) {

      my_se <- sqrt(diag(vcov(x)))
      my_coef <- lme4::fixef(x)
      
      res$conf.low =  my_coef  - 1.96 * my_se
      res$conf.high =  my_coef + 1.96 * my_se
      warning("Eigene Methode: Assuming a normal approximation for the fixed effects (1.96*standard error).")
      }
 
    coefs <- res
  }    
    
  
  if (include.b) {
    param <- c(param,  "estimate")
    if (fix_format)
      coefs$estimate <-
        stp25rndr:::Format2.default(res$estimate,
                                    digits = digits.param, format = format)
  }
  
  
    if (include.ci) {
      if (fix_format) {
        if (conf.style.1) {
          param <- c(param,   c("conf"))
          coefs$conf <-
             rndr_CI2(
              res[, c("conf.high", "conf.low")] ,
              digits= digits.param,
              format=format)
        }
        else{
          param <- c(param, c("conf.high", "conf.low"))
          coefs$conf.high <-
            stp25rndr:::Format2.default(res$conf.high,
                                        digits.param, format = format)
          coefs$conf.low <-
            stp25rndr:::Format2.default(res$conf.low,
                                        digits.param, format = format)
        }
      } else {
        param <- c(param,   c("conf.high", "conf.low"))
      }
    }
    
    
    if (include.beta & inherits(x, "lm") & !inherits(x, "glm")) {
      param <- c(param, "beta")
      b <- res$estimate[-1]
      
      sx <- sapply(x$model[-1], function(z) {
        if (!is.numeric(z)) {
          cat("\nBeta macht bei ", class(z), "keinen Sinn!\n")
          z <- as.numeric(z)
        }
        sd(z, na.rm = TRUE)
      })
      sy <- sd(x$model[[1]], na.rm = TRUE)
      coefs$beta <- c(NA, b * sx / sy)
      
      if (fix_format)
        coefs$beta <-
        stp25rndr:::Format2.default(coefs$beta,
                                    digits.beta, 
                                    format = "f")
      
    }

  
  if (include.se) {
    param <- c(param, "std.error")
    if (fix_format)
      coefs$std.error <-
        stp25rndr:::Format2.default(res$std.error,
                                    digits.param, format = format)
    
  }
  
  if (include.statistic) {
    if (inherits(x, "glm") & conf.method == "Wald") 
      coefs$statistic <- (res$estimate / res$std.error) ^ 2
      
      param <- c(param, "statistic")
      if (fix_format)
        coefs$statistic <- stp25rndr:::Format2.default(res$statistic,
                                                     digits.test, format = "f")
    }
  
  if (include.odds & inherits(x, "glm")) {
    param <- c(param, "odds")
    if (fix_format)
      coefs$odds <- stp25rndr::rndr_ods(exp(res$estimate),  digits.odds)
    else
      coefs$odds <- exp(res$estimate)
    
    if (coefs[1, 1] == "(Intercept)")
      coefs$odds[1] <- NA
    
    }
  
  if (include.odds.ci & inherits(x, "glm")) {
    
    coefs$odds.conf.low<- exp(res$conf.low)
    coefs$odds.conf.high<- exp(res$conf.high)
    
    if (fix_format) {
      if (conf.style.1) {
        param <- c(param, c("odds.conf"))
        coefs$odds.conf <-
         rndr_CI2(res[, c("odds.conf.high", "odds.conf.low")] , 
                 digits= digits.odds,
                 format="f")
        if (res[1, 1] == "(Intercept)")
          coefs$odds.conf[1] <-  NA
      }
      else{
        param <- c(param, c("odds.conf.low", "odds.conf.high"))
        coefs$odds.conf.low <-
          stp25rndr::rndr_ods(coefs$odds.conf.low, digits.odds)
        
        coefs$odds.conf.high <-
          stp25rndr::rndr_ods(coefs$odds.conf.high , digits.odds)
        
        
        if (res[1, 1] == "(Intercept)") {
          coefs$odds.conf.low[1] <-  NA
          coefs$odds.conf.high[1]  <- NA
        }
      }
    }
    else{
      param <- c(param, c("odds.conf.low", "odds.conf.high"))
      if (coefs[1, 1] == "(Intercept)") {
        coefs$odds.conf.low[1] <-  NA
        coefs$odds.conf.high[1]  <- NA
      }
      
    }
    
      }
  
  if (include.stars) {
    param <- c(param, "stars")
    coefs$stars <- stp25rndr::rndr_Stars(res$p.value)
  }
  
  

  
  if (include.p) {
    param <- c(param, "p.value")
    if (fix_format)
      coefs$p.value <- stp25rndr::rndr_P(res$p.value, FALSE)
  }
  tibble::as_tibble(coefs[param])
}




#' Fix tidy_lmer
#' https://github.com/tidymodels/broom/issues/309
#' @param x Objekt
#' @param effects,scales,ran_prefix,conf.int,conf.level,conf.method Param antidy
#' @param ... extra params
#'
#' @return data.frame tibble

tidy_lmer <- function(x,
                      effects = c("ran_pars", "fixed"),
                      scales = NULL,
                      ## c("sdcor",NA),
                      ran_prefix = NULL,
                      conf.int = FALSE,
                      conf.level = 0.95,
                      conf.method = "Wald",
                      ...) {
  effect_names <- c("ran_pars", "fixed", "ran_modes")
  if (!is.null(scales)) {
    if (length(scales) != length(effects)) {
      stop("if scales are specified, values (or NA) must be provided ",
           "for each effect")
    }
  }
  if (length(miss <- setdiff(effects, effect_names)) > 0)
    stop("unknown effect type ", miss)
  base_nn <-
    c("estimate", "std.error", "df", "statistic", "p.value")
  ret_list <- list()
  if ("fixed" %in% effects) {
    # return tidied fixed effects rather than random
    ret <- stats::coef(summary(x))
    
    # p-values may or may not be included
    nn <- base_nn[1:ncol(ret)]
    
    if (conf.int) {
      cifix <- confint(x, parm = "beta_", method = conf.method, ...)
      ret <- data.frame(ret, cifix)
      nn <- c(nn, "conf.low", "conf.high")
    }
    if ("ran_pars" %in% effects || "ran_modes" %in% effects) {
      ret <- data.frame(ret, group = "fixed")
      nn <- c(nn, "group")
    }
    ret_list$fixed <-
      broom::fix_data_frame(ret, newnames = nn)
  }
  if ("ran_pars" %in% effects) {
    if (is.null(scales)) {
      rscale <- "sdcor"
    } else
      rscale <- scales[effects == "ran_pars"]
    if (!rscale %in% c("sdcor", "vcov"))
      stop(sprintf("unrecognized ran_pars scale %s", sQuote(rscale)))
    ret <- as.data.frame(lme4::VarCorr(x))
    ret[] <- lapply(ret, function(x)
      if (is.factor(x))
        as.character(x)
      else
        x)
    if (is.null(ran_prefix)) {
      ran_prefix <- switch(rscale,
                           vcov = c("var", "cov"),
                           sdcor = c("sd", "cor"))
    }
    pfun <- function(x) {
      v <- na.omit(unlist(x))
      if (length(v) == 0)
        v <- "Observation"
      p <- paste(v, collapse = ".")
      if (!identical(ran_prefix, NA)) {
        p <- paste(ran_prefix[length(v)], p, sep = "_")
      }
      return(p)
    }
    
    rownames(ret) <- paste(apply(ret[c("var1", "var2")], 1, pfun),
                           ret[, "grp"], sep = ".")
    
    ## FIXME: this is ugly, but maybe necessary?
    ## set 'term' column explicitly, disable fix_data_frame
    ##  rownames -> term conversion
    ## rownames(ret) <- seq(nrow(ret))
    
    if (conf.int) {
      ciran <- confint(x, parm = "theta_", method = conf.method, ...)
      ret <- data.frame(ret, ciran)
      nn <- c(nn, "conf.low", "conf.high")
    }
    
    
    ## replicate lme4:::tnames, more or less
    ret_list$ran_pars <-
      broom::fix_data_frame(ret[c("grp", rscale)],
                            newnames = c("group", "estimate"))
  }
  if ("ran_modes" %in% effects) {
    ## fix each group to be a tidy data frame
    
    nn <- c("estimate", "std.error")
    re <- ranef(x, condVar = TRUE)
    getSE <- function(x) {
      v <- attr(x, "postVar")
      setNames(as.data.frame(sqrt(t(
        apply(v, 3, diag)
      ))),
      colnames(x))
    }
    fix <- function(g, re, .id) {
      newg <-
        broom::fix_data_frame(g, newnames = colnames(g), newcol = "level")
      # fix_data_frame doesn't create a new column if rownames are numeric,
      # which doesn't suit our purposes
      newg$level <- rownames(g)
      newg$type <- "estimate"
      
      newg.se <- getSE(re)
      newg.se$level <- rownames(re)
      newg.se$type <- "std.error"
      
      data.frame(rbind(newg, newg.se),
                 .id = .id,
                 check.names = FALSE)
      ## prevent coercion of variable names
    }
    
    mm <- do.call(rbind, Map(fix, coef(x), re, names(re)))
    
    ## block false-positive warnings due to NSE
    type <- spread <- est <- NULL
    mm %>% tidyr::gather(term, estimate, -.id, -level, -type) %>%
      tidyr::spread(type, estimate) -> ret
    
    ## FIXME: doesn't include uncertainty of population-level estimate
    
    if (conf.int) {
      if (conf.method != "Wald")
        stop("only Wald CIs available for conditional modes")
      
      mult <- qnorm((1 + conf.level) / 2)
      ret <- transform(
        ret,
        conf.low = estimate - mult * std.error,
        conf.high = estimate + mult * std.error
      )
    }
    
    ret <- dplyr::rename(ret, group = .id)
    ret_list$ran_modes <- ret
  }
  return(plyr::rbind.fill(ret_list))
}




#' rndr_CI
#' 
#' Formatieren von Konfidenzintervallen.
#'
#' @param ci matrix
#' @param digits Nachkommastellen
#' @param sep,sep_1,sep_2 Seperator
#' @param format an formatC "fg"
#'
#' @return string
#' 
rndr_CI2 <-  function(ci,
                      digits = 3,
                      sep =   ",",
                      sep_1 =  "[",
                      sep_2 =  "]",
                      format = "g") {
cis <-  paste0(
    sep_1,
    stp25rndr:::Format2.default(ci[[1]],
                                digits, format = format),
    sep,
    " ",
    stp25rndr:::Format2.default(ci[[2]],
                                digits, format = format),
    sep_2
  )

cis[is.na(ci[[1]])] <- NA
cis
}


#' extract_gof
#'
#' @param x objekt
#' @param include.r,include.pseudo,include.rmse,include.sigma,include.variance,include.devianze,include.loglik,include.aic,include.bic,include.nobs Alles wie bei APA_TAble
#' @param digits nachkommastellen
#' @param ... extra Argumente
#' @return tibble
#' @export
#'
#' @examples
#'
#'
extract_gof <- function(x,
                        include.r = TRUE,include.pseudo = TRUE,
                        include.rmse = TRUE,include.sigma = FALSE,include.variance = FALSE,
                        include.devianze = FALSE,
                        include.loglik = FALSE,
                        include.test=FALSE,
                        include.aic = TRUE,include.bic = include.aic,
                        include.nobs = TRUE,
                        digits = 2,
                        fix_format=FALSE,
                        ...) {
  
  
  res <-  broom::glance(x)
  param <-  "term"
 
  
  
  if (include.r | include.pseudo) {
    if( any(names(res) %in% "r.squared")){
      param <- c(param, c("r.squared", "adj.r.squared"))
    }else{  ans_r <- R2(x)
    res <- cbind(res, ans_r)
    param <- c(param, names(ans_r))
    }
  
    
  }
  
  
  if (include.aic)
    param <- c(param, "AIC")
  if (include.bic)
    param <- c(param, "BIC")
  
  if (include.rmse) {
    param <- c(param, "RMSE")
    res <- cbind(res, RMSE(x)[2])
  }
  
  if (include.loglik)
    param <- c(param, "logLik")
  if (include.devianze)
    param <- c(param, "deviance")
  if (include.sigma)
    param <- c(param, "sigma")
  
  
 
 if(fix_format){
   res <- dplyr::tbl_df(
     plyr::llply(res,
                 function(z) formatC(z, digits=digits, format="f"))) 
 
 
   if (include.test){
     param <- c(param, "Test")
     res$Test <- "nicht implementiert"
   }
   
   if (include.nobs) {
     param <- c(param, "Obs")
     res$Obs <- formatC(nobs(x), digits=0, format="f")
   }
   
 }
  else{
     res <- round(res, digits = digits) 
     #include.test
     if (include.test){
       param <- c(param, "Test")
       res$Test <- "nicht implementiert"
     }
     
     if (include.nobs) {
       param <- c(param, "Obs")
       res$Obs <- nobs(x)
     }
  }

 
 
  param <- intersect(names(res), param)
  
  tibble::as_tibble(res[param])
}
