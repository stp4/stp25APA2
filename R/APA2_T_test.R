
#' @rdname t_test
#' @title T-Test
#' @name t_test
#' @description Berechnung von Mittelwerten und Mittelwertdifferenzen
#'
#' @param x Formula
#' @param data Data.frame
#' @param var.equal,paired,alternative an t.Test var.equal = FALSE  "two.sided" 
#' @param digits nachkommastellen
#' @param include.mean Mittelwerte mit SD
#' @param include.d Cohens d
#' @param include.mean.diff  Mittlere Differenzen
#' @param include.se Standardfehler
#' @param ... an Output
#' @return list
#' @export
#'
#' @examples 
#'  
#' #require(stpvers)
#' 
#' #Projekt("html")
#' 
#' APA_Ttest(m1+m2 ~ geschl, varana)
#' varanax<-Melt2(m1+m2~nr,varana , key="time", value="m")
#' 
#' # broom::tidy(with(varana, t.test(m1,m2 ) ))
#' # broom::tidy(  t.test(m~time, varanax, var.equal=FALSE)) )
#' 
#' APA_Ttest(m~time, varanax, paired = TRUE)
#' 
#' APA_Ttest(m~time, varanax, var.equal=TRUE)
#' APA_Ttest(m~time, varanax, var.equal=FALSE)
#' #broom::tidy(t.test(m~time, varanax, var.equal=TRUE))
#' #broom::tidy(t.test(m~time, varanax, var.equal=FALSE))
#' 
#' #End()

APA_Ttest <- function(x, 
                      data, 
                      var.equal=FALSE, 
                      paired=FALSE,
                      alternative =  "two.sided",
                      include.mean=TRUE,
                      include.d=TRUE,
                      include.mean.diff=TRUE,
                      include.test=TRUE,
                      digits = 2,
                      ...) {
  lhs <- all.vars(x[-3])  
  rhs <- all.vars(x[-2])
  result <- list()
  method <- alternative # Fehler abfangen
  for (r in rhs) {
    ANS <- NULL
    if(nlevels(data[[r]])==2) # T.test nur 2-Faktorstufen
    { for (l in lhs) {
      fm <- formula(paste(l, "~", r))
      res <- t.test(fm, data, 
                    var.equal = var.equal, 
                    paired = paired, 
                    alternative = alternative)
      
     # print(aggregate(fm, data, function(x) x))
      res_t <- broom::tidy(res)
      method <- as.character(res_t$method)
      res_sig  <- APA(res)
      res_mean <- Tabelle(fm, data, APA = TRUE, include.n = FALSE)[[1]]
      
      ans <- res_mean[1]
      if(include.mean) ans <- cbind(ans, res_mean[2:3])
      if(include.mean.diff){ 
        res_diff <- paste(Format2(res_t$estimate, digits = digits),
                        rndr_CI(res_t[c("conf.low", "conf.high")]))
        ans <-cbind(ans, Differenz = res_diff)
        }
      if(include.d) {
        ans <- cbind(ans,  cohens.d = Format2(cohens.d(fm, data),2))
        }
      ans <- cbind(ans, T.test = res_sig)
      
      if (is.null(ANS))
        ANS <- ans
      else
        ANS <- rbind(ANS, ans)
      
    }}
    else{
      Text( r, "hat ", nlevels(data[[r]])," levels und kann daher nicht mittels T-Test berechnet werden!")
    }
    # Text(method)
   ANS <- prepare_output(ANS, 
                        paste("UV =", r), 
                        method
                        
                        )
    result[[l]]<- ANS
  }
  
  Output(result, ...)
  invisible(result) 
}



#' @rdname t_test
#' @description Computes Cohen's d
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
cohens.d <- function(x, ...) {UseMethod("cohens.d")}


cohens.d.default <- function(x, y, ...) {
  lx <- length(x)- 1
  ly <- length(y)- 1
  md  <- abs(mean(x) - mean(y))        ## mean difference (numerator)
  csd <- lx * var(x) + ly * var(y)
  csd <- csd/(lx + ly)
  csd <- sqrt(csd)                     ## common sd computation
  
  c( cohens.d = md/csd )                        ## cohen's d
}

cohens.d.formula= function(x, data=list(), ...){
  #mf <- model.frame(formula=x, data=data)
  mf<-aggregate(x,data, function(x) {
    x<- na.omit(x)
    c(l=length(x)-1, m=mean(x), var=var(x))
  } )[[2]]
  
  #return(mf[,"l"])
  md  <- abs(mf[1,"m"] - mf[2,"m"])        ## mean difference (numerator)
  csd <- mf[1,"l"] * mf[1,"var"] + mf[2,"l"] * mf[2,"var"]
  csd <- csd/(mf[1,"l"] + mf[2,"l"])
  csd <- sqrt(csd)                     ## common sd computation
  
  c( cohens.d = md/csd )                        ## cohen's d
  
}











