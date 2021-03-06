#' @rdname APA2
#' @description Ausgabe von Regressions Tabelle nach der APA-Style vorgabe. Die Funktion
#' ist eine Kopie von  texreg:::aggregate.matrix.
#' @param custom.model.names Namen ner Modelle
#' @param include.custom liste mit Statistiken für Gofs also zB F-Tests
#' @param include.se,include.ci,include.odds SE, 95-Ci, OR noch nicht fertig
#' @param include.ftest,include.loglik  noch nicht fertig
#' @param include.r,include.pseudo pseudo R
#' @param include.aic,include.bic geht nur zusammen
#' @param ci.level Ci default 95 Prozent
#' @param rgroup Zwischen Beschriftung

#'
#' @return invisible data.frame und Output mit html/knit oder Text.
#' @export
#'
#' @examples
#' library(lmerTest)
#'
#' fit1 <- lm(chol0 ~  ak + rrs0 + med + g, hyper)
#' fit2 <- glm(chol0 ~ med +   ak +   g + rrs0 , hyper, family = poisson())
#' fit3 <- lmerTest::lmer(chol0 ~ rrs0 + med +  ak  +  (1|g) , hyper )
#' fits <- list(fit1, fit2, fit3)
#'
#' APA2.list(fits,
#'           custom.model.names=c("lm", "glm", "lmer"),
#'           digits= list(c(1,2,3,4,5,6,7),
#'                        c(1,2,3,4,5,6,7),
#'                        c(1,2,3,4,5,6))
#'           include.custom=list(
#'                        Wald=c("F(1)=245", "F(2)=245","F(3)=245"),
#'                        Chi=c("X(4)=2.45", "X(5)=24.5","X(6)=24.5")))
#' 
#' 
APA2.list <-
  function (x,
            caption = "" ,  note = "", 
            output = stp25output::which_output(),
            digits = 2,  custom.model.names = NULL,
            include.custom = NULL,
            include.b=TRUE, include.ci = FALSE,
            include.odds = FALSE,
            include.se = if (include.ci) FALSE else TRUE,
            include.t= FALSE,  include.p = FALSE,
            include.stars = if (include.p) FALSE else TRUE,
            include.ftest = FALSE, include.loglik = FALSE,
            include.pseudo = TRUE, include.r = TRUE,
            include.aic = TRUE, include.bic = include.aic,
            include.sigma  = FALSE, include.rmse = TRUE, 
            include.gof=TRUE, include.param=TRUE,
            ci.level = .95, rgroup = c("Parameter", "Goodness of fit"),  
            test.my.fun=FALSE,
            ...)
  {
    param<- NULL
    if(test.my.fun) cat("\n  -> APA2.list()")
    
    if(include.b)  param <- "b"
    if (include.odds) param <- c(param, "OR")
    if (include.se)   param <- c(param, "SE")
    if (include.ci)   param <- c(param, "CI")
    if (include.p)    param <- c(param, "p")
    
    n_param <- length(param)
    n <- length(x)
    models <- NULL
    coefs <- list()
    coef.order <- character()
    
    # Goodnes of Fit GOF
    
  #  if(include.gof){
    gof.names <- character()
    if (is.null(custom.model.names) |
        length(custom.model.names)!=n)
      custom.model.names <- paste0("Model ", 1:n)
    
    #-- Extrahieren ----------------------------------
    for (i in seq_len(n)) {
      
      class_fit <- class(x[[i]])
      if(test.my.fun) cat("\n   texreg:::extract(", class_fit[1], ")")
      model <- texreg:::extract(x[[i]],
                                include.aic = FALSE,
                                include.bic = FALSE, 
                                #include.adjrs =TRUE,
                                #include.fstatistic
                                #include.loglik
                                # include.mse
                                #include.nagelkerke
                                # include.rmse
                                # include.rsquared
                                # include.wald
                                # standardized
                                ...)
      
      
      if (include.ci) {
        if(test.my.fun) cat("\n   include.ci: confint(", class_fit[1], ", level =", ci.level, ")")
        cis <- confint(x[[i]], level = ci.level)
        ci_inter <-
          which(rownames(cis) == "(Intercept)") # lmer gibt sigma aus
        ci_n <- nrow(cis)
        model@ci.low <- cis[ci_inter:ci_n, 1]
        model@ci.up <- cis[ci_inter:ci_n, 2]
      }
      
      if (include.pseudo) {
        if(test.my.fun) cat("\n   include.pseudo: R(", class_fit[1], ")")
        if (any(class_fit %in% "lm")) {
          if (any(class_fit %in% "glm")) {
            resr2 <- R2(x[[i]])
            model@gof.names <- c(names(resr2),  model@gof.names)
            model@gof  <- c(unlist(resr2),  model@gof)
            model@gof.decimal  <-
              c(rep(TRUE, length(resr2)),  model@gof.decimal)
          } else{
            
          }
        } else{
          # R2(x[[i]])    # Magrinal + Cond
          resr2 <- R2(x[[i]])
          model@gof.names <- c(names(resr2),  model@gof.names)
          model@gof  <- c(unlist(resr2),  model@gof)
          model@gof.decimal  <-
            c(rep(TRUE, length(resr2)),  model@gof.decimal)
        }
      }
      
      if (include.aic) {
        if(test.my.fun) cat("\n   include.ci: AIC(", class_fit[1],  ")")
        model@gof.names <- c("AIC", "BIC",  model@gof.names)
        model@gof  <- c(AIC(x[[i]]), BIC(x[[i]]),  model@gof)
        model@gof.decimal  <- c(rep(TRUE, 2),  model@gof.decimal)
      }
      
      if (class(model) == "list") {
        models <- append(models, model)
      } else {
        models <- append(models, list(model))
      }
    }
    
    #-- Gof Names -----------------------------------
    for (i in  seq_len(n)) {
      gn <- models[[i]]@gof.names
      if (!is.null(gn) && length(gn) > 0) {
        for (j in  seq_len(length(gn))) {
          if (!gn[j] %in% gof.names) {
            gof.names <- append(gof.names, gn[j])
          }
        }
      }
    }
    
    if(test.my.fun) cat("\n   gof.names:  length =", length(gof.names) )
    gofs <- matrix(nrow = length(gof.names), ncol = length(models))
    row.names(gofs) <- gof.names
   # }
   # else {gofs <- NULL}
    
    
    #-- Coef + Gofs --------------------------------
    for (i in seq_len(n)) {
      cf <- models[[i]]@coef
      
      if (length(digits) == 1)
        dig <- digits
      else if (is.list(digits))
        dig <-  digits[[i]]
      else
        dig <- 2
      # print(dig)
      
      se <- models[[i]]@se
      pv <- models[[i]]@pvalues
      
      cf <- as.vector(Format2.matrix(cf, dig))
      se <- as.vector(Format2.matrix(se, dig))
      beta <-  "NA" # cf  # nicht fertig
      
      if(include.odds) {
        or <- exp( models[[i]]@coef )
        or <- as.vector(rndr_ods(or, 2))
      }
      else or <-  NA
      
      
      p_stars  <- ffsigstars(pv)
      pv    <- as.vector(ffpvalue(pv))
      if (include.ci) {
        cil <-
          models[[i]]@ci.low  ## z <-  qnorm(1 - ((1 - ci.level)/2)) models[[i]]@coef + (z * models[[i]]@se)
        ciu <-
          models[[i]]@ci.up  # models[[i]]@coef - (z * models[[i]]@se)
        ci <- rndr_CI(cbind(cil, ciu), digits)
      } else ci<- NA
      if (include.stars ){
        if(include.b)  cf <- paste0(cf, p_stars)
        else if(include.odds) or <- paste0(or, p_stars)
      }
      
      coef <- data.frame(
        b = cf,
        OR=or,
        beta = beta,
        SE = se,
        CI = ci,
        p = pv
      )
      coef <- coef[param]
      
      
      rownames(coef) <- models[[i]]@coef.names
      colnames(coef) <-paste0(custom.model.names[i], "_", colnames(coef))
      
      coefs[[i]] <- coef
      
      #-- Gof  sortieren
      if (include.gof & length(models[[i]]@gof) > 0) {
        for (j in seq_len(length(models[[i]]@gof)) ) {
          rn <- models[[i]]@gof.names[j]
          val <- models[[i]]@gof[j]
          col <- i
          if (is.na(models[[i]]@gof.decimal[j])) {
            dec <- 2
          }
          else if (models[[i]]@gof.decimal[j] == FALSE) {
            dec <- 0
          }
          else {
            dec <- 2
          }
          row <- which(row.names(gofs) == rn)
          gofs[row, col] <-  mapply(Format2, val, dec)
        }
      }
    }
    
    #-- Sortieren ----------------------------------
    for (i in  seq_len(length(coefs))) {
      for (j in  seq_len(length(rownames(coefs[[i]]))) ) {
        
        if (!rownames(coefs[[i]])[j] %in% coef.order) {
          coef.order <- append(coef.order, rownames(coefs[[i]])[j])
        }
      }
    }
    
    if (length(coefs) == 1) {
      m <- coefs[[1]]
    } else if (length(coefs) > 1) {
      m <- coefs[[1]]
      for (i in 2:length(coefs)) {
        m <- merge(m, coefs[[i]], by = 0, all = TRUE)
        rownames(m) <- m[, 1]
        m <- m[, colnames(m) != "Row.names"]
      }
    }
    
    
    m.temp <- matrix(nrow = nrow(m), ncol = ncol(m))
    
    for (i in  seq_len(nrow(m)) ) {
      new.row <- which(coef.order == rownames(m)[i])
      for (j in  seq_len(length(m[i, ])) ) {
        m.temp[new.row, j] <- as.character(m[i, j])
      }
    }
    rownames(m.temp) <- coef.order
    colnames(m.temp) <- colnames(m)
    if(test.my.fun) cat("\n   param sortieren und ordnen: length =", length(m.temp) )
    #- hinzufügen von Sonder Zeilen ------------------
    
    if (include.gof) {
      if (!is.null(include.custom)) {
        gofs <-  rbind(gofs,
                       matrix(
                         unlist(include.custom),
                         nrow = length(include.custom),
                         byrow = TRUE,
                         dimnames = list(names(include.custom))
                       ))
      }
      
      ngofs <- nrow(gofs)
      emptygofs <- rep(NA, ngofs * (n_param - 1))
      newgofs <-   gsub("[^[:alnum:] :().]", "", rownames(gofs))
      
      if (length(param > 1))
        for (i in  seq_len(n)) {
          gofs <-
            append(gofs, emptygofs,
                   after = ngofs * (1 + n_param * (i - 1)))
        }
      
      gofs <- matrix(gofs , nrow = ngofs)
      rownames(gofs) <- newgofs
      
      
      #-- Ausgabe --------------------------------------
      result <-
        if (include.param)
          prepare_output(fix_to_data_frame(rbind(m.temp, gofs)), 
                         caption, note)
      else
        prepare_output(fix_to_data_frame(gofs), 
                       caption, note)
      }
    else if (include.param) {
      result <- prepare_output(fix_to_data_frame(m.temp), 
                               caption, note)
    }
    else{
      result <- NULL
      
    }
    
    
    if (!is.logical(output)) {Output(result, output=output)}
    if(test.my.fun) cat("\n  <- APA2.list()")
    invisible(result)
  }


#' APA2 
#'
#' @param x lm object.
#' @param include.ci Confidence interval 
#' @param include.effect Text zu Effect_Size
#' @export
#'
#' @examples
#' 
#' library(psycho)
#' 
#'  
#'  df <- psycho::affective  # Load a dataset from the psycho package
#'  #df <- standardize(df)  # Standardize all numeric variables
#'  
#'  fit <- lm(Age ~ Salary, data=df)  # Fit a Bayesian linear model
#'  results <- analyze(fit)  # Format the output
#'  APA2(results )
#'  
#'  
#'  
#'  library(lmerTest)
#'  fit <- lmerTest::lmer(Sepal.Length ~ Sepal.Width + (1|Species), data=iris)
#'  
#'  results <- analyze(fit)
#'  APA2(results)
APA2.psychobject <- function(x,
                             caption = "",
                             note = NULL,
                             # paste("contrasts: ", paste(options()$contrasts, collapse=", ")),
                             include.ci = FALSE,
                             include.effect=FALSE,
                             output = stp25output::which_output(),
                             ...) {
  class(x)
  
  res <-
    fix_format(summary(x),
               pattern_pval = "p",
               pattern_est = c("SE", "SE.std"))
  
  if (!include.ci) {
    ci <- which(names(res) %in% c("CI_lower", "CI_higher"))
    res <- res[-ci]
    
  }
  
  if(!include.effect){
    eff <- which(names(res) == "Effect_Size")
    res <- res[-eff]
    
  }
  if (is.null(note)) {
    
    r2s <- x$values$model
    note <- ""
    for (i in names(r2s)) {
      note <- paste(note, i, "=", rndr_r(r2s[[i]], FALSE))
      if(names(r2s)[1] == i) note<- paste0(note, ",")
    }
    note
  }
  res <-  prepare_output(res, caption, note)
  
  Output(res, output = output)
  invisible(res)
}





#' @rdname APA2
#' @export
#' 
APA2.lm <- function(x, 
                    caption=NULL,
                    note=NULL,   #paste("contrasts: ", paste(options()$contrasts, collapse=", ")),
                    output = stp25output::which_output(),
                    col_names = NULL,
                    include.b = TRUE,
                    include.se = TRUE,
                    include.beta = FALSE,
                    include.ci = FALSE,
                    include.r = TRUE,
                    include.test = FALSE,
                    ci.level = .95,
                    test.my.fun=FALSE,conf.style.1 =TRUE,
                    ...
){
  if (test.my.fun)
    cat("\n    -> APA2.lm()")
  info <- model_info(x)
  AV <- ifelse(is.na(info$labels[info$y]), info$y, info$labels[info$y])
  
  if (is.null(note)) {
    note <-  paste0("Model: ", info$family[1])
    if (include.test)
      note <- paste(note, APA(x, FALSE))
    if (include.r) {
      if (test.my.fun)
        cat("\n       include.r: R( )")
      r2 <- R2(x)
      note <- paste(note, "\nr-squared:", rndr_r2(r2))
    }
  }
  if (is.null(caption))
    caption <-  paste0("AV: ", AV, " Obs: ", info$N)
  
  res <-prepare_output( extract_param(x,
                       include.b,
                       include.se,
                       include.beta,
                       include.ci,
                       ci.level = ci.level,
                       fix_format=TRUE, conf.style.1 = conf.style.1)
                       ,  caption, note, info$N, info$labels)
  
  
  
  if (!is.logical(output))
    Output(res, output = output, col_names = col_names)
  
  invisible(res)
}
  
  


#' @rdname APA2
#' @export
#' 
APA2.glm <- function(x,
                     caption = NULL, note = NULL,
                     output = stp25output::which_output(),
                     col_names = NULL,
                     include.b = TRUE, include.se = TRUE,
                     include.ci = FALSE,
                     include.odds = TRUE, include.odds.ci =  include.ci,
                     include.statistic = TRUE,
                     include.p = TRUE, include.stars = FALSE,
                     include.r = TRUE, include.pseudo = include.r,
                     include.test = FALSE,
                     include.rr = FALSE, include.rr.ci = include.ci,
                     ci.level = .95, conf.method = "Wald",
                     test.my.fun = FALSE,conf.style.1 =TRUE,
                     digits = 2,
                     ...) {
  if (test.my.fun)
    cat("\n    -> APA2.glm()")
  info <- model_info(x)
  AV <-
    ifelse(is.na(info$labels[info$y]), info$y, info$labels[info$y])
  
  if (is.null(note)) {
    note <-  paste0("Model: ", info$family[1])
    if (include.test)
      note <- paste(note, APA(x, FALSE))
    if (include.r) {
      if (test.my.fun)
        cat("\n       include.r: R( )")
      r2 <- R2(x)
      note <-  paste(note, "\npseudo r-squared:", rndr_r2pseudo(r2))
    }
  }
  if (is.null(caption))
    caption <-  paste0("AV: ", AV, " Obs: ", info$N)
  
  
  coefs <- extract_param(
    x,
    include.b = include.b,
    include.se = include.se,
    include.beta = FALSE,
    include.ci = include.ci,
    include.odds = include.odds,
    include.odds.ci =  include.odds.ci,
    include.statistic = include.statistic,
    include.p = include.p,
    include.stars =include.stars,
    ci.level = ci.level,
    conf.method = conf.method,
    fix_format = TRUE, conf.style.1 = conf.style.1
  )
  
  if (conf.method == "Wald") {
    note <- paste(note, "Wald-Test")
  } else {
    note <- paste(note, "LR-Test")
  }
  
  # Relaltv-Risk   include.rr
  if (include.rr) {
    rr <-  sjstats::odds_to_rr(x)
    rr <- data.frame(RR = rndr_ods(rr[, 1]),
                     RR.CI = rndr_ods_CI(rr[, 2:3]))
    
    if (rownames(coefs)[1] == "(Intercept)")
      rr[1, 1:2] <- NA
    
    if (include.rr.ci)
      coefs <- cbind(coefs, rr)
    else
      coefs <- cbind(coefs, rr[, 1])
  }
  
  res <- prepare_output(coefs,
                        caption, note, info$N, info$labels)
  
  if (!is.logical(output))
    Output(res, output = output, col_names = col_names)
  
  invisible(res)
}



  #APA2.aov()
   

#' @rdname APA2
#' @param anova_type  bei lme:  "F"  F-werte (wie SPSS) oder Chi (car::Anova)
#' @export
#' 
#' 
APA2.lme <- function(...){
  APA2.lmerMod(...)
}


# APA2.lme <- function(x,
#                      caption = "" ,
#                      note = "",
#                      output = stp25output::which_output(),
#                      col_names = NULL,
#                      type = "III",
#                      anova = TRUE,
#                      anova_type = "F",
#                      ...) {
#   Anov <- NULL
#   goodnes <- NULL
#   fit_param <- NULL
#   fit_sum <- summary(x)
#   fit_param <- as.data.frame(fit_sum$tTable)  ##  xtTab
#   names(fit_param)[1] <- c("Estimate")
#   
#   if (anova_type == "F") {
#     #cat(Anov)
#     #  cat("Verwende die Funktion anova stadt car::Anova\n")
#     #Text("Fehler in den Factorstufen es duerfen keine leeren Factoren vorkommen")
#     #-- Unbekannter aufgrung von anzahl an Factorstufen
#     Anov <- anova(x)
#     Anov <- as.data.frame(Anov)
#     
#     goodnes <- cbind(
#       Obs = fit_sum$dims[["N"]],
#       round(R2(x), 2) ,
#       BIC = round(fit_sum$BIC, 2),
#       logLik = round(c(fit_sum$logLik), 2)
#     )
#     
#   } else{
#     Anov <- car::Anova(x, type = type)
#     
#     goodnes <- cbind(
#       Obs = fit_sum$dims[["N"]],
#       round(r.squared(fit)[, 4:6], 2) ,
#       BIC = round(fit_sum$BIC, 2),
#       logLik = round(c(fit_sum$logLik), 2)
#     )
#   }
#   
#   fit_param <- prepare_output(
#     fix_data_frame2(Source = rownames(fit_param), fit_param),
#     caption = paste("Regression Model:", caption),
#     note = note
#   )
#   
#   Anov <-
#     prepare_output(
#       fix_data_frame2(Source = rownames(Anov), Anov),
#       caption = paste("ANOVA:", caption),
#       note = note
#     )
#   
#   goodnes <-
#     prepare_output(goodnes,
#                    caption = paste("Goodness-of-fit", caption),
#                    note = "R-Quadrat entspricht Marginal und Conditional")
#   
#   Output(fit_param, output=output)
#   Output(goodnes, output=output)
#   
#   if (anova)
#     Output(fit_param, output=output)
#   
#   invisible(list(
#     param = fit_param,
#     anova = Anov,
#     gof = goodnes
#   ))
# }




#' @rdname APA2
#' @export
APA2.lmerMod <- function(x,
                         caption = NULL,
                         note = NULL,
                         output = stp25output::which_output(),
                         col_names = NULL,
                         include.b = TRUE,
                         include.se = TRUE,
                         include.ci = FALSE,
                         include.odds = FALSE,
                         include.odds.ci =  include.ci,
                         include.statistic = TRUE,
                         include.p = TRUE,
                         include.stars = FALSE,
                         include.r = TRUE,
                         include.pseudo = include.r,
                         include.test = FALSE,
                         ci.level = .95,
                         conf.method = "Wald",
                         test.my.fun = FALSE,conf.style.1 =TRUE,
                         digits = 2,
                         ...) {
  if (test.my.fun){
    cat("\n    -> APA2.lmerMod() \n Input: ")
    print(class(x))
    cat("\n")
    }
  info <- model_info(x)
  AV <-
    ifelse(is.na(info$labels[info$y]), info$y, info$labels[info$y])
  
  if (is.null(note)) {
    note <-  paste0("Model: ", info$family[1])
    if (include.test)
      note <- paste(note, APA(x, FALSE))
    if (include.r) {
      if (test.my.fun)
        cat("\n       include.r: R( )")
      r2 <- R2(x)
      note <-  paste(note, "\npseudo r-squared:", rndr_r2pseudo(r2))
    }
  }
  if (is.null(caption))
    caption <-  paste0("AV: ", AV, " Obs: ", info$N)
  
  
  coefs <- extract_param(
    x,
    include.b = include.b,
    include.se = include.se,
    include.beta = FALSE,
    include.ci = include.ci,
    include.odds = include.odds,
    include.odds.ci =  include.odds.ci,
    include.statistic = include.statistic,
    include.p = include.p,
    include.stars = include.stars,
    ci.level = ci.level,
    conf.method = conf.method,
    fix_format = TRUE, conf.style.1 = conf.style.1
  )
  
  res <- prepare_output(coefs,
                        caption, note, info$N, info$labels)
  
  if (!is.logical(output))
    Output(res, output = output, col_names = col_names)
  
  invisible(res)
  
}


# @rdname APA2
# @export
# APA2.glmerMod <- function(x,
#                          caption = NULL ,
#                          note = NULL,
#                          output = stp25output::which_output(),
#                          col_names = NULL,
#                          include.random.effects = TRUE,
#                          include.odds=TRUE,
#                          ...){
#   APA2.merModLmerTest(x, 
#                       caption=caption,
#                       note=note, output = output ,
#                       col_names = col_names,
#                       include.random.effects=include.random.effects,
#                       include.odds= include.odds,
#                       ...
#                       )  
# }


# @rdname APA2
# @export
# @examples 
# 
#' fm1 <- lmer(Reaction ~ Days + (Days|Subject), sleepstudy)
# APA2(fm1)

# APA2.merModLmerTest <- function(x,
#                                 caption = NULL ,
#                                 note = NULL,
#                                 output = stp25output::which_output(),
#                                 col_names = NULL,
#                                 include.random.effects = TRUE,
#                                 include.odds=FALSE,
#                                 
#                                 ...) {
#    res <- Ordnen.merModLmerTest(x, 
#                                 include.odds=include.odds, 
#                                 ...)  
# 
#    if (is.null(caption))
#      caption <- paste(attr(res, "caption"),
#                       "Obs: ", attr(res, "N"))
#   
#    Output(fix_format(res),
#      caption =  caption, note = note, output=output, col_names=col_names)
#    
#    if (include.random.effects){
#      coef_ran <- broom::tidy(x)
#      coef_ran<- coef_ran[(nrow(res)+1):nrow(coef_ran), -c(3:4) ]
#      
#      Output(fix_format(coef_ran), caption="random effects", output=output)
#      }
#    
#    invisible(res)
# }



GOF_LMER <- function(x){
  
  res <- lmerTest::summary(x)
  # goodnes <- cbind(
  #   Obs = res$devcomp$dims["N"],
  #   round(r.squared.merMod(x)[, 4:6], 2),
  #   # BIC = round(res$BIC,2),
  #   logLik = round(c(as.numeric(res$logLik)), 2),
  #   REML =  round(res$devcomp$cmp["REML"], 2)
  # )
  
  
  # goodnes <-  prepare_output(goodnes,
  #                            caption = paste("Goodness-of-fit", caption),
  #                            note = "R-Quadrat entspricht Marginal und Conditional")
  
  
  
  res
}