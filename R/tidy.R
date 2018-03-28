#' @rdname Ordnen
#' @title Ordnen (Tidy)
#' @name Ordnen
#' @description Diese Funktion meine Version von tidy
#' @param x Objekt
#' @param ... weitere Objekte nicht benutzt
#' @return ein data.frame-Ojekt oder eine Liste von data.frames. Im Attribut N sind die Stichprobengroesse
#' und notes
#' @export
#Tidy#
Ordnen <- function(x, ...) {
  UseMethod("Ordnen")
}

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



#' ANOVA 
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
  
  if (include.eta ) {
    if(is(x, "lm")){
    k <- ncol(res)
    res <-
      cbind(res[, -k], etaSquared2(x, 2, FALSE), res[k])
    }else warning("include.eta geht nur bei aov")
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


 
#' Regression (linear)
#' @param include.b Estimate
#' @param include.se  SE Standardfehler
#' @param include.beta  standartisiertes beta
#' @param include.ci,ci.level  95%-CI mit ci-Level
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
                      #  include.r = TRUE,
                      #  include.ftest = FALSE,
                      #  include.aic = TRUE,
                      #  include.bic = include.aic,
                      ci.level = .95,
                      ...) {
  info <- model_info(x)
  AV <-
    ifelse(is.na(info$labels[info$y]), info$y, info$labels[info$y])
  
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
 
  prepare_output(data.frame(Source= rownames(res), res),
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
    odds <-
      data.frame(plyr::llply(b.coef , function(x)
        ifelse(x > 4.6, 100,  round(exp(
          x
        ), 2))))
    
    names(odds) <- gsub(".b", "", paste0("OR.", names(odds)))
    
    source.coef <- cbind(source.coef, odds)
    source.interc$OR <-
      ifelse(b.interc[, 1] > 4.6, 100,  round(exp(b.interc[, 1]), 2))
    if (length(names(odds)) > 1)
      source.interc <- cbind(source.interc, OR.low = NA, OR.upr = NA)
    
  }
  
  if (include.se) {
    source.coef <- cbind(source.coef, stat.coef)
    source.interc <- cbind(source.interc, stat.interc)
  } else{
    source.coef <- cbind(source.coef, stat.coef[-1, drop = FALSE])
    source.interc <-
      cbind(source.interc, stat.interc[-1, drop = FALSE])
  }
  
  rbind(source.interc, source.coef)
}