#' APA_Effsize
#'
#' Cohen's d and Hedges g effect size
#' Between groups
#' Cohen's d Small 0.2, Medium 0.5, Large 0.8, Very large 1.3
#' Odds ratio (OR) Small 1.5, Medium 2, Large 3
#' Relative risk or risk ratio (RR) R Small 2, Medium 3, Large 4
#'
#' Measures of association
#' Pearson's r correlation Small 0.2,  Medium 0.5, Large 0.8
#' r2 coefficient of determination Small 0.04, Medium 0.25, Large 0.64
#' Quelle: http://www.ncbi.nlm.nih.gov/pmc/articles/PMC3444174/pdf/i1949-8357-4-3-279.pdf
#'
#' Gestolen von https://cran.r-project.org/web/packages/effsize/effsize.pdf
#' @param x Objekt oder Formel
#' @param type bis jetzt nur cohens d
#' @param ... weitere Optionen
#' @export
APA_Effsize <- function(x, ...) {
  UseMethod("APA_Effsize")
}


#' @rdname APA_Effsize
#' @export
APA_Effsize.default <- function(x, ...) {
    APA2.efflist(effects::allEffects(x), ...)
}


#' @rdname APA_Effsize
#' @export
APA_Effsize.formula<- function(...,
                               type = "cohen.d"){
  if(type =="cohen.d"){
    APA2(..., type=type)
  }else "noch nicht Implementiert"
}






cohen_d2 <- function(d,f,
                     pooled=TRUE,
                     paired=FALSE,
                     na.rm=FALSE,
                     hedges.correction=FALSE,
                     conf.level=0.95, ...){
  #-- Aufbereiten ----------------------
  if( ! any(c("numeric","integer") %in% class(d))){
    stop("First parameter must be a numeric type")
  }

  if( any(c("character","factor") %in% class(f)) ){
    if( "character" %in% class(f)){
      f = factor(f)
    }
     if(length(levels(f))!=2){
      stop("Factor should have only two levels")
      }
  }else{
    ## it is treatment and control  (d und f sind numeric)
    treatment = d
    control = f
    d = c(treatment, control)
    f = factor(rep(c("Treatment","Control"),
                   c(length(treatment),length(control))),
               levels=c("Treatment","Control"),
               ordered=T)
  }


  if(na.rm){
    nas = is.na(d) | is.na(f);
    d = d[!nas];
    f = f[!nas];
  }

  #- Berechnen --------------------------

  ns <- table(f)
  n1 <- ns[1]
  n2 <- ns[2]
  m  <- c()
  sd <- c()
  for( l in levels(f) ){
    m <- c(m, mean(d[f==l]))
    sd <- c(sd, sd(d[f==l]))
  }

  delta.m <- m[1] - m[2]

  if(pooled){
    pool_sd <- sqrt(((n1-1)*sd[1]^2+(n2-1)*sd[2]^2)/(n1+n2-2))
    d <- (delta.m) / pool_sd
  }else{
    d <- (delta.m) / sd(d)
  }
  df <- n1+n2-2

  res <- list()
  if(hedges.correction){
    # Hedges, L. V. & Olkin, I. (1985). Statistical methods for meta-analysis. Orlando, FL: Academic Press.
    d <- d * (1 - 3 / ( 4 * (n1+n2) - 9))
    res$method = "Hedges's g"
    res$name = "g"
  }else{
    res$method = "Cohen's d"
    res$name = "d"
  }

  # The Handbook of Research Synthesis and Meta-Analysis (Cooper, Hedges, & Valentine, 2009)
  ## p 238
  S_d <- sqrt(((n1+n2)/(n1*n2) + .5*d^2/df) * ((n1+n2)/df))

  Z <- -qt((1-conf.level)/2,df)

  conf.int <- c(
              d - Z*S_d,
              d + Z*S_d
            )
  names(conf.int) <- c("inf","sup")

  levels <- c(0.2, 0.5, 0.8)
  magnitude = c("negligible","small","medium","large")
  ## Cohen, J. (1992). A power primer. Psychological Bulletin, 112, 155-159. Crow, E. L. (1991).

  res$estimate = d
  res$conf.int = conf.int
  res$var = S_d
  res$conf.level = conf.level
  res$magnitude = factor(magnitude[findInterval(abs(d),levels)+1],levels = magnitude,ordered=T)
  #      variance.estimation = if(use.unbiased){ "Unbiased"}else{"Consistent"},
  #      CI.distribution = if(use.normal){ "Normal"}else{"Student-t"}

  class(res) <- "effsize"
  return(res)
}


cohen_d_formula<- function(formula,
                           data ,
                           pooled=TRUE,
                           paired=FALSE,
                           na.rm=FALSE,
                           hedges.correction=FALSE,
                           conf.level=0.95,
                           ...){
res<- list()
X<-Formula_Data(formula, data)

myLabel<- GetLabelOrName(X$Y_data)

for(i in X$yname){
        d <-  X$Y_data[,i]
        res[[i]] <- cohen_d2(X$Y_data[,i],
                             X$X_data[,X$xname[1]],
                                    pooled, paired,
                                    na.rm,
                                    hedges.correction,
                                    conf.level)
      }
return(res)

}

