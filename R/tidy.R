 
#' Ordnen (Tidy)
#' 
#' Diese Funktion meine Version von tidy.
#' @rdname Ordnen
#' @name Ordnen
#' 
#' @param x Objekt
#' @param ... weitere Objekte nicht benutzt
#' @return ein data.frame-Ojekt oder eine Liste von data.frames. Im Attribut N sind die Stichprobengroesse
#' und notes
#' @export
#' 
Ordnen <- function(x, ...) {
  UseMethod("Ordnen")
}


 
# Ordnen.prototyp <- function(x, 
#                        include.etwas = TRUE,
#                        include.column=FALSE,
#                        ...){
#   info <- model_info(x)
#   AV   <- ifelse(is.na(info$labels[info$y]), info$y, info$labels[info$y])
#   res <-  broom::tidy(x)
#   k <- ncol(res)
#   if (include.etwas) {
#     res <-
#         cbind(res[, -k], etwas=NA, res[k])
#     }
#   if (!include.column){
#     res <-  res[, names(res) != "name der Spalte"]
#   }
#  
#   
#   prepare_output(res,
#                  paste0("AV: ", AV),
#                  paste0("Model: ", info$family[1]),
#                  info$N,
#                  info$labels)
# }





#' @rdname Ordnen
#' @export
Ordnen.default <- function(x, ...) {
  info <- model_info(x)
  AV <-
    ifelse(is.na(info$labels[info$y]), info$y, info$labels[info$y])

  df <- broom::tidy(x)
  if(any(class(x) %in% "merModLmerTest")) {
    df<- cbind(df[1:(ncol(df)-1)], p.value=NA, df[ncol(df)])
    p_value<- lmerTest::summary(x)$coefficients[,5]
    df$p.value[ 1:length(p_value)]<- p_value
  }
  # else if(class(x)=="lmerMod"){
  #   Text(
  #     "
  #     ----
  #     Achtung: Paket library(lmerTest) laden.
  #     Bzw die update() Funktion nicht verwenden.
  #     ----
  #     "
  #   )
  # }
  else {}
  #attr(df,"class2") = info$class
  attr(df, "caption") =  paste0("AV: ", AV)
  attr(df, "note") = paste0("Model: ", info$family[1])
  attr(df, "N") = info$N
  attr(df, "labels") = info$labels

  df
  }





                                     

# include.b = TRUE,
# include.se = TRUE,
# include.beta = FALSE,
# include.eta = TRUE,
# include.odds = FALSE,
# include.ci = FALSE,
# 
# 
# #Fehler abfangeb
# include.p = FALSE,
# include.stars = if (include.p) FALSE  else TRUE,
# 
# include.variance = TRUE,
# include.r = TRUE,
# include.pseudo = FALSE,
# # noch nicht fertig
# include.ftest = FALSE,
# include.loglik = FALSE,
# # noch nicht fertig
# 
# include.custom = NULL,
# 
# include.aic = TRUE,
# include.bic = include.aic,
# 
# ci.level = .95,


#' @rdname Ordnen
#' @export
Ordnen.anova <- function(x, ...) Ordnen.aov(x, ...)



#' @description ANOVA - Methode ueber broom::tidy
#' @param include.eta  Eta Quadrat
#' @param include.sumsq,include.meansq  Quadrat- Summen
#'
#' @rdname Ordnen
#' @export
Ordnen.aov <- function(x, 
                       include.eta = TRUE,
                       include.sumsq = TRUE,
                       include.meansq = FALSE, 
                       ...){
  info <- model_info(x)
  AV <-
    ifelse(is.na(info$labels[info$y]), info$y, info$labels[info$y])
  res <- broom::tidy(x)
  
  if (include.eta) {
    if( is(x, "lm") | is(x, "anova") ){
    k <- ncol(res)
    res <-
      cbind(res[, -k], etaSquared2(x, 2, FALSE), res[k])
    }else cat("\nWarnung: include.eta geht nur bei aov\n")
  }
  if (!include.sumsq){
    res <-  res[, names(res) != "sumsq"]
  }
  if (!include.meansq){
    res <-  res[, names(res) != "meansq"]
  }
  prepare_output(res,
                 paste0("AV: ", AV),
                 paste0("Model: ", info$family[1]),
                 info$N,
                 info$labels)
}


#' @description Regression - Methode ueber basr::summary (lm und glm)
#' @param include.b Estimate
#' @param include.se  SE Standardfehler
#' @param include.beta  standartisiertes beta
#' @param include.ci,ci.level  95-CI mit ci-Level
#'
#' @rdname Ordnen
#' @export
Ordnen.lm <- function(x,
                      # custom.model.names = NULL,
                     # digits = 2,
                      include.b = TRUE,
                      include.se = TRUE,
                      include.beta = FALSE,
                      # include.eta = TRUE,
                      include.ci = FALSE,
                      # include.variance = TRUE,
                        include.r = TRUE,
                        include.ftest = FALSE,
                      #  include.aic = TRUE,
                      #  include.bic = include.aic,
                      ci.level = .95,
                      ...) {
  #cat("\n In Ordnen.lm " )
  info <- model_info(x)
  AV <-
    ifelse(is.na(info$labels[info$y]), info$y, info$labels[info$y])
  
  
  
  note <-  paste0("Model: ", info$family[1])
  if (include.ftest)
    note <- paste(note, APA(x, FALSE))
  if (include.r) {
    r2 <- R2(x)
    note <-
      paste(note, "\nr-squared:", rndr_r2(r2))
  }
  
  
  
  
  
  # res <- broom::tidy(x)
  coefs <- summary(x)$coef
  
  if (include.ci) {
    res <- cbind(coefs[, 1, drop = FALSE],
                 confint(x, level = ci.level),
                 coefs[,-1, drop = FALSE])
  } else {
    res <- coefs
  }
  
  if (include.beta) {
    b <- coefs[-1, 1]
    
    sx <- sapply(x$model[-1], function(x) {
      if (!is.numeric(x)) {
        cat("\nBeta macht bei ", class(x), "keinen Sinn!\n")
        x <- as.numeric(x)
      }
      sd(x, na.rm = TRUE)
    })
    sy <- sd(x$model[[1]], na.rm = TRUE)
    res <-
      cbind(res[, 1, drop = FALSE], beta = c(NA, b * sx / sy), res[, -1, drop =
                                                                     FALSE])
  }
  
  if (!include.se) {
    res <-  res[, colnames(res) != "Std. Error"]
  }
  
  if (!include.b) {
    res <-  res[, colnames(res) != "Estimate"]
  }
  
  colnames(res)[ncol(res)] <- "p.value" 
 
  prepare_output(data.frame(Source= rownames(res), res, stringsAsFactors = FALSE),
                 paste0("AV: ", AV),
                 note=note,    #  paste0("Model: ", info$family[1]),
                 info$N,
                 info$labels)
}



#' @rdname Ordnen
#' @param rr RR Relatives Risiko
#' @param include.b.ci,include.odds,include.rr.ci 95 Konfidenzintervalle
#' @export
Ordnen.glm <- function(x,
                       digits = 2,
                       include.b = TRUE,
                       include.se = TRUE,
                       include.b.ci = FALSE,
                       include.ci = TRUE,
                       include.odds = TRUE,
                       include.odds.ci= include.ci,
                       include.rr=FALSE,
                       include.rr.ci=include.ci,
                       include.test = "lrt",
                       #"wald"  bei SPSS wird der Wald-Test verwendet, ich verwende den LRT
                      
                       
                      # include.p = TRUE,
                      # include.stars = FALSE,
                       
                      # include.variance = FALSE,
                       include.r = TRUE,
                       include.pseudo = include.r,
                       # noch nicht fertig
                     #  include.ftest = FALSE,
                       include.loglik = TRUE,
                       # noch nicht fertig
                     #  include.custom = FALSE,
                      # include.aic = FALSE,
                     #  include.bic = FALSE, 
                       ci.level = .95,
                        ...) {
  
 # cat("\n In Ordnen.glm " )
  info <- model_info(x)
  
  note <-  paste0("Model: ", info$family[1])
  if (include.loglik)
    note <- paste(note, APA(x))
  if (include.pseudo) {
    r2 <- R2(x)
    note <-
      paste(note, "\npseudo r-squared:", rndr_r2pseudo(r2))
  }
  
   
   
  AV <-
    ifelse(is.na(info$labels[info$y]), info$y, info$labels[info$y])
  
  coefs <- data.frame(summary(x)$coef)
  names(coefs) <- c("b", "SE", "LR.Test", "p.value")
  
  if ( include.test == "lrt"){
    coefs$LR.Test <- (coefs$b/coefs$SE)^2
    names(coefs)[3]<- "Wald"
  }
  
  include.intercept <- rownames(coefs)[1] == "(Intercept)"
  
  if (include.ci) {
    # likelihood ratio test  oder  Wald test
    if (include.test == "lrt")
      cis <- confint(x, level = ci.level)
    else
      cis <- confint.default(x, level = ci.level)
    
    if (!is.matrix(cis))  
      cis <- matrix(cis, ncol = 2)
    
  }
  
  # SPSS gibt keine CIs hier aus
  if (include.b & include.b.ci ) {
    coefs <- cbind(coefs[, 1, drop = FALSE],
                   CI = rndr_CI(cis),
                   coefs[,-1, drop = FALSE])
    
    if (include.intercept)
      coefs$CI[1] <- ""
  } else {
    coefs <- coefs
  }
  
  if (include.odds) {
    if (include.odds.ci) {
      coefs$OR <- rndr_ods(as.vector(exp(coefs[, 1])))
      coefs$OR.CI <- rndr_ods_CI(exp(cis))
      
      if (include.intercept)  {
        coefs$OR[1] <- NA
        coefs$OR.CI[1] <- ""
      }
    }
    else{
      coefs$OR <- rndr_ods(as.vector(exp(coefs[, 1])))
      if (include.intercept)
        coefs$OR[1] <- NA
    }
  }
  
  
  
  # Relaltv-Risk   include.rr
  if (include.rr) {
    rr <-  sjstats::odds_to_rr(x)
    rr <- data.frame(RR = rndr_ods(rr[, 1]),
                     RR.CI = rndr_ods_CI(rr[, 2:3]))
    
    if (include.intercept)
      rr[1, 1:2] <- NA
    
    if (include.rr.ci)
      coefs <- cbind(coefs, rr)
    else
      coefs <- cbind(coefs, rr[, 1])
  }
  
  
  
  if (!include.se) {
    coefs <-  coefs[, colnames(coefs) != "SE"]
  }
  
  if (!include.b) {
    coefs <-  coefs[,  colnames(coefs) != "b"]
  }
  
  prepare_output(
    fix_format(cbind(source = rownames(coefs), coefs)),
    paste0("AV: ", AV),
    note=note,
    info$N,
    info$labels
  )
}






# Alte Version kommentar siehe unten

#' @rdname Ordnen
#' @export
Ordnen.merModLmerTest <- function(x,
                      # custom.model.names = NULL,
                      # digits = 2,
                      include.b = TRUE,
                      include.se = TRUE,
                      include.beta = FALSE,
                      # include.eta = TRUE,
                      include.ci = FALSE,
                      include.odds = FALSE,
                      # include.variance = TRUE,
                      #  include.r = TRUE,
                      #  include.ftest = FALSE,
                      #  include.aic = TRUE,
                      #  include.bic = include.aic,
                      ci.level = .95,
                      ...) {
  cat("\n In Ordnen.merModLmerTest \n")
  info <- model_info(x)
  AV <-
    ifelse(is.na(info$labels[info$y]), info$y, info$labels[info$y])
  coefs <- lmerTest::summary(x)$coefficients
  #lmerTest:::summary.lmerModLmerTest(x)
  stat.coef <- coefs[,-1, drop = FALSE]
  
  if (include.ci) {
    cis<-  confint(x, level = ci.level)
    k <- which(rownames(cis) == "(Intercept)")
    
    b.coef <- cbind(coefs[, 1, drop = FALSE],
                 cis[k:nrow(cis), ]  
                 )
  } else {
    b.coef <- coefs[, 1, drop = FALSE]
  }
  
  res<-
  if (include.odds) {
    odds <-
      apply(b.coef, 2, function(x)
        ifelse(x > 4.6, 100, round(exp(x), 2)))
    
    if(ncol(b.coef==1)) 
    colnames(odds) <-  "OR" 
    else   colnames(odds) <- c("OR", "low", "upr") 
    
    
  #  print(b.coef)
  #  print(odds)
    b.coef <- cbind(b.coef, odds)
   }
  
  if (include.beta)  cat("\nBeta macht bei ", class(x), "keinen Sinn!\n")
 
  res <- cbind(b.coef, stat.coef)
    colnames(res)[ncol(res)] <- "p.value" 
  
  if (!include.se) {
    res <-  res[, colnames(res) != "Std. Error"]
  }
  
  if (!include.b) {
    res <-  res[, colnames(res) != "Estimate"]
  }
  
  
  prepare_output(data.frame(Source= rownames(res), res, stringsAsFactors = FALSE),
                 paste0("AV: ", AV),
                 paste0("Model: ", info$family[1]),
                 info$N,
                 info$labels)
}



#' @rdname Ordnen
#' @description lmerTest::lmer returns an object of class 'lmerModLmerTest' (previously 'merModLmerTest') 
#' to clarify that 'lmerModLmerTest' extends  'lmerMod' – not 'merMod'. The merMod class includes generalized 
#' and nonlinear mixed models and lmerTest is only designed for linear mixed models.
#' @export
Ordnen.lmerModLmerTest<- function(x,
                                  # custom.model.names = NULL,
                                  # digits = 2,
                                  include.b = TRUE,
                                  include.se = TRUE,
                                  include.beta = FALSE,
                                  # include.eta = TRUE,
                                  include.ci = FALSE,
                                  include.odds = FALSE,
                                  # include.variance = TRUE,
                                  #  include.r = TRUE,
                                  #  include.ftest = FALSE,
                                  #  include.aic = TRUE,
                                  #  include.bic = include.aic,
                                  ci.level = .95,
                                  ...) {
#  cat("\n In Ordnen.lmerModLmerTest \n")
  info <- model_info(x)
  AV <-
    ifelse(is.na(info$labels[info$y]), info$y, info$labels[info$y])
  coefs <- lmerTest:::summary.lmerModLmerTest(x)$coefficients
  #
  stat.coef <- coefs[,-1, drop = FALSE]
  
  if (include.ci) {
    cis<-  confint(x, level = ci.level)
    k <- which(rownames(cis) == "(Intercept)")
    
    b.coef <- cbind(coefs[, 1, drop = FALSE],
                    cis[k:nrow(cis), ]  
    )
  } else {
    b.coef <- coefs[, 1, drop = FALSE]
  }
  
  res<-
    if (include.odds) {
      odds <-
        apply(b.coef, 2, function(x)
          ifelse(x > 4.6, 100, round(exp(x), 2)))
      
      if(ncol(b.coef==1)) 
        colnames(odds) <-  "OR" 
      else   colnames(odds) <- c("OR", "low", "upr") 
      
      
      #  print(b.coef)
      #  print(odds)
      b.coef <- cbind(b.coef, odds)
    }
  
  if (include.beta)  cat("\nBeta macht bei ", class(x), "keinen Sinn!\n")
  
  res <- cbind(b.coef, stat.coef)
  colnames(res)[ncol(res)] <- "p.value" 
  
  if (!include.se) {
    res <-  res[, colnames(res) != "Std. Error"]
  }
  
  if (!include.b) {
    res <-  res[, colnames(res) != "Estimate"]
  }
  
  
  prepare_output(data.frame(Source= rownames(res), res, stringsAsFactors = FALSE),
                 paste0("AV: ", AV),
                 paste0("Model: ", info$family[1]),
                 info$N,
                 info$labels)
}




#' @rdname Ordnen
#' @export
Ordnen.polr <- function(x,
                        digits = 2,
                        include.b = TRUE,
                        include.se = TRUE,
                        include.ci = FALSE,
                        include.odds = TRUE,
                        ...) {
  
  info <- model_info(x)
  AV <-
    ifelse(is.na(info$labels[info$y]), info$y, info$labels[info$y])
  
  res <- summary(x)
  
  Intercepts <- names(res$zeta)
  ## store table
  ctable <- coef(res)
  ## calculate and store p values
  colnames(ctable) <- c("b", "se", "t.value")
  p <- pnorm(abs(ctable[, "t.value"]), lower.tail = FALSE) * 2
  ## combined table
  
  ctable <- data.frame(
    Source = rownames(ctable),
    ctable,
    "p.value" = p,
    stringsAsFactors = FALSE
  )

  
  n1 <- nrow(ctable)
  n2 <- length(Intercepts)
  
  coefficients <- ctable[1:(n1 - n2),]
  intercepts  <- ctable[-c(1:(n1 - n2)),]
  source.coef <- coefficients[, 1, drop = FALSE]
  source.interc  <- intercepts[, 1, drop = FALSE]
  b.coef <- coefficients[, 2, drop = FALSE]
  b.interc <- intercepts[, 2, drop = FALSE]
  stat.coef <- coefficients[, 3:5, drop = FALSE]
  stat.interc <- intercepts[, 3:5, drop = FALSE]

  
  if (include.ci) {
    x <- update(x, Hess = TRUE)
    pr <- profile(x)
    ci <- confint(pr)
    colnames(ci) <- c("low", "upr")
    b.coef <- cbind(b.coef, ci)
    b.interc <- cbind(b.interc,  low = NA, upr = NA)
  }

  
  if (include.b) {
    source.coef <- cbind(source.coef, b.coef)
    source.interc <- cbind(source.interc, b.interc)
  }

  if (include.odds) {
    if(include.ci){
      source.coef$OR <- rndr_ods(source.coef$b )
      #  ifelse(> 4.6, 100,  round(exp(source.coef$b), 2))
      source.coef$OR.low <-rndr_ods(source.coef$low)
      #  ifelse(source.coef$low > 4.6, 100,  round(exp(source.coef$low), 2))
      source.coef$OR.upr <-rndr_ods(source.coef$upr)
      #  ifelse(source.coef$upr > 4.6, 100,  round(exp(source.coef$upr), 2))
      
      source.interc$OR <-rndr_ods(source.interc$b)
     #   ifelse(source.interc$b > 4.6, 100,  round(exp(source.interc$b), 2))
      source.interc$OR.low <- NA
      source.interc$OR.upr <- NA
    }
    else{
      source.coef$OR <-  rndr_ods(source.coef$b)
        
       # ifelse(source.coef$b > 4.6, 100,  round(exp(source.coef$b), 2))
      source.interc$OR <- rndr_ods(source.interc$b)
        
      #  ifelse(source.interc$b > 4.6, 100,  round(exp(source.interc$b), 2))
    }
  }
  

  
  if (include.se) {
    source.coef <- cbind(source.coef, stat.coef)
    source.interc <- cbind(source.interc, stat.interc)
  } else{
    source.coef <- cbind(source.coef, stat.coef[-1, drop = FALSE])
    source.interc <-
      cbind(source.interc, stat.interc[-1, drop = FALSE])
  }
  
  source.interc$Source <- paste0("Intercept(",  source.interc$Source, ")")
   
  prepare_output(rbind(source.interc, source.coef),
                 paste0("AV: ", AV),
                 paste0("Model: ", info$family[1]),
                 info$N,
                 info$labels)
  
}