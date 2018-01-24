cohens_d <- function(d,f,
                    pooled=TRUE,
                    paired=FALSE,
                    na.rm=TRUE,
                    hedges.correction=FALSE,
                    conf.level=0.95, ...){
  treatment = d
  control = f
  d = c(treatment, control)
  f = factor(rep(c("Treatment","Control"),
                 c(length(treatment),length(control))),
             levels=c("Treatment","Control"),
             ordered=T)



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

  #levels <- c(0.2, 0.5, 0.8)
  # magnitude = c("negligible","small","medium","large")
  ## Cohen, J. (1992). A power primer. Psychological Bulletin, 112, 155-159. Crow, E. L. (1991).

  res$estimate = d
  res$conf.int = conf.int
  # res$var = S_d
  #  res$conf.level = conf.level
  # res$magnitude = factor(magnitude[findInterval(abs(d),levels)+1],levels = magnitude,ordered=T)
  #      variance.estimation = if(use.unbiased){ "Unbiased"}else{"Consistent"},
  #      CI.distribution = if(use.normal){ "Normal"}else{"Student-t"}

  # class(res) <- "effsize"
  return(res)
}

cohen_d_formula<-
  function(formula, data , caption="", include.ci=TRUE, note="",
           pooled=TRUE, paired=FALSE ,na.rm=TRUE,
           hedges.correction=FALSE, conf.level=0.95, ... ){

    note <- paste(note, "Cohen's d",
                  paste(paste0(c(0.2,0.5,0.8,1.3), "=",
                               c("negligible","small","medium","large")), collapse=", "))
    X<-Formula_Data(formula, data, ...)
    f <-  X$X_data[,X$xname[1]]

    if( nlevels(f)!=2 ){
      return("Cohens D: Hier sind nur zwei Gruppen erlaubt!")
      }

    ANS<- errate_statistik2(formula, data, caption=caption, type="mean",
                            note=note, test=FALSE )[[1]]
    ANS$cohen.d <-NA

    for(i in 1:length(X$yname)){
      res <-   cohens_d(X$Y_data[,i] , f,
                               pooled, paired, na.rm,
                               hedges.correction, conf.level)

      if(include.ci) ANS$cohen.d[i]<- paste(
                      Format2(as.numeric(res$estimate),2),
                      ffCI(res$conf.int))
      else  ANS$cohen.d[i]<-Format2(as.numeric(res$estimate),2)
    }
    ANS
  }
