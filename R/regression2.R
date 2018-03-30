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
            caption = "" ,
            note = "",
            digits = 2,
            custom.model.names = NULL,
            include.custom = NULL,
            include.b=TRUE,
            include.ci = FALSE,
            include.odds = FALSE,
            include.se = if (include.ci) FALSE else TRUE,
            include.p = FALSE,
            include.stars = if (include.p) FALSE else TRUE,
            include.ftest = FALSE,
            include.loglik = FALSE,
            include.pseudo = TRUE,
            include.r = TRUE,
            include.aic = TRUE,
            include.bic = include.aic,
            ci.level = .95,
            rgroup = c("Parameter", "Goodness of fit"),
            output = stp25output:::which_output(),
            ...)
  {
    param<- NULL
    
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
    gof.names <- character()
    if (is.null(custom.model.names) |
        length(custom.model.names)!=n)
      custom.model.names <- paste0("Model ", 1:n)
    
    #-- Extrahieren ----------------------------------
    for (i in seq_len(n)) {
      model <- texreg:::extract(x[[i]],
                                include.aic = FALSE,
                                include.bic = FALSE, ...)
      if (include.ci) {
        cis <- confint(x[[i]], level = ci.level)
        ci_inter <-
          which(rownames(cis) == "(Intercept)") # lmer gibt sigma aus
        ci_n <- nrow(cis)
        model@ci.low <- cis[ci_inter:ci_n, 1]
        model@ci.up <- cis[ci_inter:ci_n, 2]
      }
      
      if (include.pseudo) {
        if (any(class(x[[i]]) %in% "lm")) {
          if (any(class(x[[i]]) %in% "glm")) {
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
    
    gofs <- matrix(nrow = length(gof.names), ncol = length(models))
    row.names(gofs) <- gof.names
    
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
      
      cf <- as.vector(stp25APA2:::Format2.matrix(cf, dig))
      se <- as.vector(stp25APA2:::Format2.matrix(se, dig))
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
      if (length(models[[i]]@gof) > 0) {
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
    
    #- hinzufügen von Sonder Zeilen ------------------
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
    
    if(length(param>1))
      for (i in  seq_len(n)) {
        gofs <-
          append(gofs, emptygofs,
                 after = ngofs * (1 + n_param * (i - 1)))
      }
    
    gofs <- matrix(gofs , nrow = ngofs)
    rownames(gofs) <- newgofs
    
    
    #-- Ausgabe --------------------------------------
    result <- prepare_output(fix_to_data_frame(rbind(m.temp, gofs)),
                             caption , note)
    
    if (output == "html") {
      Output(result,
             rgroup = rgroup,
             n.rgroup = nrow(m.temp))
    }
    else if (output == "markdown") {
      Output_kable(result)
    }
    else {
      print(result)
    }
    invisible(result)
  }










#' @rdname APA2
#' @export
#' 
APA2.lm <- function(x, 
                    caption=NULL,
                    note=paste("contrasts: ", paste(options()$contrasts, collapse=", ")),
                    output = which_output(),
                    col_names = NULL,
                    ...)
 {
  res <- Ordnen(x, ...)
 
  if (is.null(caption))
    caption <- paste(attr(res, "caption"),
                     "Obs: ", attr(res, "N"))
  
  Output(
    fix_format(res),
    caption =  caption,
    note = note, 
    output=output, col_names=col_names)
  
  invisible(res)
}
  
  
  #APA2.aov()
   

#' @rdname APA2
#' @param anova_type  bei lme:  "F"  F-werte (wie SPSS) oder Chi (car::Anova)
#' @export
APA2.lme <- function(x,
                     caption = "" ,
                     note = "",
                     output = which_output(),
                     col_names = NULL,
                     type = "III",
                     anova = TRUE,
                     anova_type = "F",
                     ...) {
  Anov <- NULL
  goodnes <- NULL
  fit_param <- NULL
  fit_sum <- summary(x)
  fit_param <- as.data.frame(fit_sum$tTable)  ##  xtTab
  names(fit_param)[1] <- c("Estimate")
  
  if (anova_type == "F") {
    #cat(Anov)
    #  cat("Verwende die Funktion anova stadt car::Anova\n")
    #Text("Fehler in den Factorstufen es duerfen keine leeren Factoren vorkommen")
    #-- Unbekannter aufgrung von anzahl an Factorstufen
    Anov <- anova(x)
    Anov <- as.data.frame(Anov)
    
    goodnes <- cbind(
      Obs = fit_sum$dims[["N"]],
      round(R2(x), 2) ,
      BIC = round(fit_sum$BIC, 2),
      logLik = round(c(fit_sum$logLik), 2)
    )
    
  } else{
    Anov <- car::Anova(x, type = type)
    
    goodnes <- cbind(
      Obs = fit_sum$dims[["N"]],
      round(r.squared(fit)[, 4:6], 2) ,
      BIC = round(fit_sum$BIC, 2),
      logLik = round(c(fit_sum$logLik), 2)
    )
  }
  
  fit_param <- prepare_output(
    fix_data_frame2(Source = rownames(fit_param), fit_param),
    caption = paste("Regression Model:", caption),
    note = note
  )
  
  Anov <-
    prepare_output(
      fix_data_frame2(Source = rownames(Anov), Anov),
      caption = paste("ANOVA:", caption),
      note = note
    )
  
  goodnes <-
    prepare_output(goodnes,
                   caption = paste("Goodness-of-fit", caption),
                   note = "R-Quadrat entspricht Marginal und Conditional")
  
  Output(fit_param, output=output)
  Output(goodnes, output=output)
  
  if (anova)
    Output(fit_param, output=output)
  
  invisible(list(
    param = fit_param,
    anova = Anov,
    gof = goodnes
  ))
}




#' @rdname APA2
#' @export
APA2.lmerMod <- function(...) {
  Text(
    "
    ----
    Achtung: Paket library(lmerTest) laden.
    Bzw die update() Funktion nicht verwenden.
    ----
    "
  )
  
}


#' @rdname APA2
#' @export
APA2.glmerMod <- function(x,
                         caption = NULL ,
                         note = NULL,
                         output = which_output(),
                         col_names = NULL,
                         include.random.effects = TRUE,
                         include.odds=TRUE,
                         ...){
  APA2.merModLmerTest(x, 
                      caption=caption,
                      note=note, output = output ,
                      col_names = col_names,
                      include.random.effects=include.random.effects,
                      include.odds= include.odds,
                      ...
                      )  
}


#' @rdname APA2
#' @export
#' @examples 
#' 
#' fm1 <- lmer(Reaction ~ Days + (Days|Subject), sleepstudy)
#' APA2(fm1)
#' 
#' 
APA2.merModLmerTest <- function(x,
                                caption = NULL ,
                                note = NULL,
                                output = which_output(),
                                col_names = NULL,
                                include.random.effects = TRUE,
                                include.odds=FALSE,
                                
                                ...) {
   res <- Ordnen.merModLmerTest(x, 
                                include.odds=include.odds, 
                                ...)  

   if (is.null(caption))
     caption <- paste(attr(res, "caption"),
                      "Obs: ", attr(res, "N"))
  
   Output(fix_format(res),
     caption =  caption, note = note, output=output, col_names=col_names)
   
   if (include.random.effects){
     coef_ran <- broom::tidy(x)
     coef_ran<- coef_ran[(nrow(res)+1):nrow(coef_ran), -c(3:4) ]
     
     Output(fix_format(coef_ran), caption="random effects", output=output)
     }
   
   invisible(res)
}



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