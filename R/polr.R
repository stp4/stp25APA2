#' @rdname APA
#' @export
APA.polr   <- function(x, ...)
 {
  res <- as.data.frame(lmtest::lrtest(fit_polr))
  # likelihood ratio tests
  paste0("-LL=" ,
         round(as.numeric(logLik(x)), 1),
         ", ",
         rndr_Chisq(res$Chisq[2], abs(res$Df[2]), res[2, "Pr(>Chisq)", drop =
                                                        TRUE]))
  
}


#' @rdname APA2
#' @description Ordered Logistic or Probit Regression  
#' https://stats.idre.ucla.edu/r/dae/ordinal-logistic-regression/
#' @export
#' 
#' 
#' 
#' 
APA2.polr <- function(x, 
                       caption="", note="", include.b = TRUE,
                       include.se = TRUE,
                       include.ci=FALSE,
                       include.odds=TRUE, ...){
  res<- summary(x)
  Intercepts<- names(res$zeta)
  ## store table
  ctable <- coef(res)
  ## calculate and store p values
  colnames(ctable) <- c("b", "se", "t.value")
  p <- pnorm(abs(ctable[, "t.value"]), lower.tail = FALSE) * 2
  ## combined table  
  
  ctable <- data.frame(Source=rownames(ctable), 
                       ctable,   
                       "p.value" = p, stringsAsFactors=FALSE ) 
  
  n1<- nrow(ctable)
  n2<- length(Intercepts)
  
  coefficients<- ctable[1:(n1-n2),]
  intercepts  <- ctable[ -c(1:(n1-n2)),]
  source.coef <- coefficients[,1,drop = FALSE]
  source.interc  <- intercepts[,1,drop = FALSE]
  b.coef<- coefficients[, 2, drop = FALSE]
  b.interc<- intercepts[, 2, drop = FALSE]
  stat.coef<- coefficients[, 3:5, drop = FALSE]
  stat.interc<- intercepts[, 3:5, drop = FALSE]
  # 
  #OR<- exp(cbind(OR = coef(x), ci))
  # list(ctable,OR)
  
  if (include.ci){
    x <- update(x, Hess=TRUE)
    pr <- profile(x)
    ci<- confint(pr)
    colnames(ci) <- c("low", "upr")
    b.coef<- cbind(b.coef, ci) 
    b.interc<- cbind(b.interc,  low=NA, upr=NA)
  }
  
  if(include.b) {
    
    source.coef<- cbind(source.coef, b.coef)
    source.interc<- cbind(source.interc, b.interc)
  }
  if (include.odds){
    odds <- exp(b.coef)
    names(odds) <- gsub(".b", "", paste0("OR.", names(odds)))
    
    source.coef<- cbind(source.coef, odds)
    source.interc$OR <- exp(b.interc[,1])
    if(length(names(odds))>1) source.interc<- cbind(source.interc, OR.low=NA, OR.upr=NA)
    
  }
  
  if(include.se){
    source.coef<- cbind(source.coef, stat.coef)
    source.interc<- cbind(source.interc, stat.interc)
  }else{
    source.coef<- cbind(source.coef, stat.coef[-1,drop = FALSE])
    source.interc<- cbind(source.interc, stat.interc[-1,drop = FALSE])
  }
  
  #ctable
  Output(prepare_output(  fix_format( rbind(source.interc, source.coef
  )), caption, note )
  )
  
  invisible(list(rbind(intercepts=source.interc, coefficients=source.coef)))
}
