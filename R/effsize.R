#' Effsize
#'
#'
#' Measures of association
#' Pearson's r correlation Small 0.2,  Medium 0.5, Large 0.8
#' r2 coefficient of determination Small 0.04, Medium 0.25, Large 0.64
#' Quelle: http://www.ncbi.nlm.nih.gov/pmc/articles/PMC3444174/pdf/i1949-8357-4-3-279.pdf
#'
#' Gestolen von https://cran.r-project.org/web/packages/effsize/effsize.pdf
#' @name Effsize
#' @param x Objekt oder Formel
#' @param ... weitere Optionen
#' 
APA_Effsize<- function(x, ...){
  
  if(is_formula2(x) | is.numeric(x)) cohens.d(x, ...)
  else etaSquared2(x, ...)
  
}
 

#' @rdname APA2
#' @export
APA2.eff <- function(...) {APA2.efflist(...)}

# @rdname Ordnen
# @export
# Ordnen.eff <- function(x, ...) {
#   if (names(x)[2] %in% "formula")
#     efflist <- list(Effect = x)
#   Ordnen.efflist(efflist, ...)
# }


#' @rdname APA2
#' @export
#' @examples 
#' #----------------------------------------------------------
#' # Effekte / Mittelwerte
#' 
#'  Tabelle2(fit1, digits=2)  # mean SD
#'  require(effects)
#'  fit1 <- lm(chol0 ~  ak + rrs0 + med + g, hyper)
#'  eff<-allEffects(fit1)
#'   
#'  APA2(eff)
APA2.efflist <- function(x,
                         caption = "Effekte: ",
                         type = NULL,
                         ##c("fit", "lower",  "upper" ),
                         note = "",
                         digits = 2,
                         include.fit = TRUE,
                         include.n = FALSE,
                         include.ci = TRUE,
                         include.se = FALSE,
                         ...) {
  if (is.null(type)) {
    if (include.fit)
      type <- "fit"
    if (include.se)
      type <- c(type, "se")
    if (include.ci)
      type <- c(type, "lower", "upper")
    if (include.n)
      type <- c("N", type)
    type <- setdiff(c("N", "fit", "se", "lower", "upper"), type)
  }
  else{
    type <- setdiff(c("N", "fit", "se", "lower", "upper"), type)
  }
  
  
  res <- fix_eff_to_df(x, caption =  caption,
                          note = note 
                       )
  
  for (i in names(res)) {
      spalte = which(names(res[[i]]) %in% type)
    Output(fix_format( res[[i]][-spalte], digits = digits))
  }
  invisible(res)
}

 
 




# @rdname Ordnen
# @export
fix_eff_to_df <- function(x, caption, note ) {
  res_list <- NULL
  for (i in names(x)) {
    info <- model_info(x[[i]])
    AV <- ifelse(is.na(info$labels[info$y]), info$y, info$labels[info$y])
    ans <- as.data.frame(x[[i]])
    n<- ncol(ans)

    ans[1:(n-4) ] <- lapply(ans[1:(n-4)], as.character)

    myN <- aggregate_effect(x[[i]], info$y, info$x)
    #- aggregate verwirft Leere Eintraege
    if (nrow(ans) == nrow(myN)) {
      ans$N <- Format2(myN[, ncol(myN)], 0)
      attr(ans, "note") = ""
    }
    else{
      ans$N <- NA
      attr(ans, "note") = "Warnung: Die Stichprobe ist relativ klein sodass die Anzahl nicht berechnet werden kann."
    }
    attr(ans, "caption") =  paste0("AV: ", AV)
    attr(ans, "N") = info$N
    attr(ans, "labels") = info$labels

 

    res_list[[i]] <- ans
  }
  res_list
}



 
#' @rdname Effsize
#' @description Eta-Quadrat Kopie von lsr::etaSquared
#' @param type etaSquared2: Anova Type default ist 2
#' @param anova  etaSquared2: Ausgabe der ANOVA Tabelle
#' @return Vector
#' @export
#' @examples
#' 
#' # etaSquared 
#' 
#' fit1<-lm(y1~x1, anscombe)
#' #etaSquared2(aov (y1~x1, anscombe), anova=TRUE)
#' #etaSquared2(fit1, anova=TRUE)
#' etaSquared2(fit1 )
#'
etaSquared2 <-
  function (x, type = 2, anova = FALSE, ...)
    ##   <environment: namespace:lsr
  {
    if (!is(anova, "logical") | length(anova) != 1) {
      stop("\"anova\" must be a single logical value")
    }
    if (!is(x, "lm")) {
      stop("\"x\" must be a linear model object")
    }
    if (!is(type, "numeric") | length(type) != 1) {
      stop("type must be equal to 1,2 or 3")
    }
    if (type == 1) {
      ss <- anova(x)[, "Sum Sq", drop = FALSE]
      ss.res <- ss[dim(ss)[1],]
      ss.tot <- sum(ss)
      ss <- ss[-dim(ss)[1], , drop = FALSE]
      ss <- as.matrix(ss)
    }
    else {
      if (type == 2) {
        ss.tot <- sum((x$model[, 1] - mean(x$model[, 1])) ^ 2)
        ss.res <- sum((x$residuals) ^ 2)
        terms <- attr(x$terms, "factors")[-1, , drop = FALSE]
        l <- attr(x$terms, "term.labels")
        ss <- matrix(NA, length(l), 1)
        rownames(ss) <- l
        for (i in seq_along(ss)) {
          vars.this.term <- which(terms[, i] != 0)
          dependent.terms <- which(apply(terms[vars.this.term,
                                               , drop = FALSE], 2, prod) > 0)
          m0 <- lm(x$terms[-dependent.terms], x$model)
          if (length(dependent.terms) > 1) {
            m1 <- lm(x$terms[-setdiff(dependent.terms,
                                      i)], x$model)
            ss[i] <- anova(m0, m1)$`Sum of Sq`[2]
          }
          else {
            ss[i] <- anova(m0, x)$`Sum of Sq`[2]
          }
        }
      }
      else {
        if (type == 3) {
          mod <- drop1(x, scope = x$terms)
          ss <- mod[-1, "Sum of Sq", drop = FALSE]
          ss.res <- mod[1, "RSS"]
          ss.tot <- sum((x$model[, 1] - mean(x$model[,
                                                     1])) ^ 2)
          ss <- as.matrix(ss)
        }
        else {
          stop("type must be equal to 1, 2 or 3")
        }
      }
    }
    if (anova == FALSE) {
      ss <- rbind(ss, ss.res)
      eta2 <- ss / ss.tot
      eta2p <- ss / (ss + ss.res)
      k <- length(ss)
      E <- cbind(eta2, eta2p)
      E[k, 2] <- NA
      colnames(E) <- c("eta.sq", "eta.sq.part")
      rownames(E) <- rownames(ss)
      rownames(E)[k] <- "Residuals"
      
      # eta2 <- ss/ss.tot
      # eta2p <- ss/(ss + ss.res)
      # E <- cbind(eta2, eta2p)
      # rownames(E) <- rownames(ss)
      # colnames(E) <- c("eta.sq", "eta.sq.part")
    }
    else {
      ss <- rbind(ss, ss.res)
      eta2 <- ss / ss.tot
      eta2p <- ss / (ss + ss.res)
      k <- length(ss)
      # eta2p[k] <- NA
      df <- anova(x)[, "Df"]
      ms <- ss / df
      Fval <- ms / ms[k]
      p <- 1 - pf(Fval, df, rep.int(df[k], k))
      E <- cbind(ss, df, ms, Fval, eta2, eta2p, p)
      E[k, c(4, 6, 7)] <- NA
      colnames(E) <- c("SS", "df",
                       "MS", "F", "eta.sq", "eta.sq.part", "p")
      
      rownames(E) <- rownames(ss)
      rownames(E)[k] <- "Residuals"
    }
    return(E)
  }



 
#' @rdname Effsize
#' @description   Cohen's d and Hedges g effect size
#' Between groups
#' Cohen's d Small 0.2, Medium 0.5, Large 0.8, Very large 1.3
#' Odds ratio (OR) Small 1.5, Medium 2, Large 3
#' Relative risk or risk ratio (RR) R Small 2, Medium 3, Large 4
#' Cohen, J. (1988).  Statistical power analysis for the behavioral sciences (2nd ed.).  New York:Academic Press.
#' @param x,y formel oder x, y
#' @param ...
#'
#' @return vector
#' @export
#'
#' @examples
#'
#' #require(stpvers)
#' set.seed(45)                        ## be reproducible
#'  x <- rnorm(10, 10, 1)
#'  y <- rnorm(10, 5, 5)
#'
#' cohens.d(x, y)
#' varanax<-Melt2(m1+m2~nr,varana , key="time", value="m")
#' cohens.d(m~time, varanax )
#'
cohens.d <- function(x, ...) {
  UseMethod("cohens.d")
}

#' @rdname Effsize
cohens.d.default <- function(x, y, ...) {
  lx <- length(x) - 1
  ly <- length(y) - 1
  md  <-
    abs(mean(x) - mean(y))        ## mean difference (numerator)
  csd <- lx * var(x) + ly * var(y)
  csd <- csd / (lx + ly)
  csd <- sqrt(csd)                     ## common sd computation
  
  c(cohens.d = md / csd)                        ## cohen's d
}

#' @rdname Effsize
cohens.d.formula = function(x, data = list(), ...) {
  #mf <- model.frame(formula=x, data=data)
  mf <- aggregate(x, data, function(x) {
    x <- na.omit(x)
    c(l = length(x) - 1,
      m = mean(x),
      var = var(x))
  })[[2]]
  
  #return(mf[,"l"])
  md  <-
    abs(mf[1, "m"] - mf[2, "m"])        ## mean difference (numerator)
  csd <- mf[1, "l"] * mf[1, "var"] + mf[2, "l"] * mf[2, "var"]
  csd <- csd / (mf[1, "l"] + mf[2, "l"])
  csd <- sqrt(csd)                     ## common sd computation
  
  c(cohens.d = md / csd)                        ## cohen's d
  
}














#
#
# cohen_d2 <- function(d,f,
#                      pooled=TRUE,
#                      paired=FALSE,
#                      na.rm=FALSE,
#                      hedges.correction=FALSE,
#                      conf.level=0.95, ...){
#   #-- Aufbereiten ----------------------
#   if( ! any(c("numeric","integer") %in% class(d))){
#     stop("First parameter must be a numeric type")
#   }
#
#   if( any(c("character","factor") %in% class(f)) ){
#     if( "character" %in% class(f)){
#       f = factor(f)
#     }
#      if(length(levels(f))!=2){
#       stop("Factor should have only two levels")
#       }
#   }else{
#     ## it is treatment and control  (d und f sind numeric)
#     treatment = d
#     control = f
#     d = c(treatment, control)
#     f = factor(rep(c("Treatment","Control"),
#                    c(length(treatment),length(control))),
#                levels=c("Treatment","Control"),
#                ordered=T)
#   }
#
#
#   if(na.rm){
#     nas = is.na(d) | is.na(f);
#     d = d[!nas];
#     f = f[!nas];
#   }
#
#   #- Berechnen --------------------------
#
#   ns <- table(f)
#   n1 <- ns[1]
#   n2 <- ns[2]
#   m  <- c()
#   sd <- c()
#   for( l in levels(f) ){
#     m <- c(m, mean(d[f==l]))
#     sd <- c(sd, sd(d[f==l]))
#   }
#
#   delta.m <- m[1] - m[2]
#
#   if(pooled){
#     pool_sd <- sqrt(((n1-1)*sd[1]^2+(n2-1)*sd[2]^2)/(n1+n2-2))
#     d <- (delta.m) / pool_sd
#   }else{
#     d <- (delta.m) / sd(d)
#   }
#   df <- n1+n2-2
#
#   res <- list()
#   if(hedges.correction){
#     # Hedges, L. V. & Olkin, I. (1985). Statistical methods for meta-analysis. Orlando, FL: Academic Press.
#     d <- d * (1 - 3 / ( 4 * (n1+n2) - 9))
#     res$method = "Hedges's g"
#     res$name = "g"
#   }else{
#     res$method = "Cohen's d"
#     res$name = "d"
#   }
#
#   # The Handbook of Research Synthesis and Meta-Analysis (Cooper, Hedges, & Valentine, 2009)
#   ## p 238
#   S_d <- sqrt(((n1+n2)/(n1*n2) + .5*d^2/df) * ((n1+n2)/df))
#
#   Z <- -qt((1-conf.level)/2,df)
#
#   conf.int <- c(
#               d - Z*S_d,
#               d + Z*S_d
#             )
#   names(conf.int) <- c("inf","sup")
#
#   levels <- c(0.2, 0.5, 0.8)
#   magnitude = c("negligible","small","medium","large")
#   ## Cohen, J. (1992). A power primer. Psychological Bulletin, 112, 155-159. Crow, E. L. (1991).
#
#   res$estimate = d
#   res$conf.int = conf.int
#   res$var = S_d
#   res$conf.level = conf.level
#   res$magnitude = factor(magnitude[findInterval(abs(d),levels)+1],levels = magnitude,ordered=T)
#   #      variance.estimation = if(use.unbiased){ "Unbiased"}else{"Consistent"},
#   #      CI.distribution = if(use.normal){ "Normal"}else{"Student-t"}
#
#   class(res) <- "effsize"
#   return(res)
# }
#
#
# cohen_d_formula<- function(formula,
#                            data ,
#                            pooled=TRUE,
#                            paired=FALSE,
#                            na.rm=FALSE,
#                            hedges.correction=FALSE,
#                            conf.level=0.95,
#                            ...){
# res<- list()
# X<-Formula_Data(formula, data)
#
# myLabel<- GetLabelOrName(X$Y_data)
#
# for(i in X$yname){
#         d <-  X$Y_data[,i]
#         res[[i]] <- cohen_d2(X$Y_data[,i],
#                              X$X_data[,X$xname[1]],
#                                     pooled, paired,
#                                     na.rm,
#                                     hedges.correction,
#                                     conf.level)
#       }
# return(res)
#
# }




# APA_Effsize <- function(x, ...) {
#   cat("\nObsolet\n")
#   Text("Ich habe die Funktion geloescht da ich Sie nicht verwende!")
# }

# APA_Effsize <- function(x, ...) {
#   UseMethod("APA_Effsize")
# }


# @rdname APA_Effsize
# @export
# APA_Effsize.default <- function(x, ...) {
#     APA2.efflist(effects::allEffects(x), ...)
# }


# @rdname APA_Effsize
# @export
# APA_Effsize.formula<- function(...,
#                                type = "cohen.d"){
#   if(type =="cohen.d"){
#     APA2(..., type=type)
#   }else "noch nicht Implementiert"
# }
#
