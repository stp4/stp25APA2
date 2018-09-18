#' @rdname APA
#' @description APA.survfit  Mediane berechnen.
#' @export
APA.survfit <- function(fit, ...) {
  if (length(names(fit)) > 11) {
    # Workaround fuer unterschiedlichen Output
    # Mediane berechnen
    mdn <- fix_to_data_frame(summary(fit)$table)
    mdn <- if (ncol(mdn) == 10)
      cbind(mdn[, c(1, 2, 5)], mdn[, c(8:10)])
    else
      cbind(mdn[, c(1, 4)], mdn[, c(7:9)])
    
    
    unlist(mdn["median"])
  }
  
  
}


#

#' @rdname APA
#' @description APA.survdiff Log-Rank-Test  berechnen.
#' @export
APA.survdiff <- function(fit) {
  df <- length(fit$n) - 1
  p.val <- 1 - pchisq(fit$chisq, df)
  paste0("Log Rank-Test ",
         rndr_X(fit$chisq, df), ", ",
         rndr_P(p.val))
}








#' @rdname Kaplan_Meier
#' @title Kaplan Maier
#' @name Kaplan_Meier
#' @description Summary -Funktion für Kaplan-Maier
#' Beispiel unter
#' (see \code{\link[stp25data]{hkarz}})
#'
#' \subsection{Mediane}{
#'    \code{m <- Surv(Time, status) ~ 1}
#'
#'    \code{res <- survfit(m, DF)}
#'
#'    \code{ APA2(res, caption="Kaplan-Meier")}
#'    }
#' \subsection{survival rate at a certain time }{
#' \code{APA2(summary(res, times=c(5, 10, 15)))}
#' }
#'
#'
#' @param x Objekt
#' @param ... weitere Objekte nicht benutzt
#' @return A \code{\link[tibble]{tibble}} with counted tagged NA values.
#' @examples
#' #require(survival)
#'  
#' #Projekt("html")
#' ### Magenkarzinom ###
#' mkarz <- GetData("C:/Users/wpete/Dropbox/3_Forschung/1 Statistik/BspDaten/SPSS/_Buehl/MKARZ.SAV")
#' # Text("Buehl Seite 553", style=3)
#' #Text("Die Datei mkarz.sav ist ein Datensatz mit 106 Patientan
#' #     mit Magenkarzinom über einen Zeitraum von 5 Jahren")
#'
#' mkarz %>% Tabelle2(survive="median", status, lkb)
#' mkarz$status<- ifelse(mkarz$status=="tot", 1, 0)
#'
#' #Head("Kaplan-Meier estimator without grouping", style=3)
#' #Text("
#' #     m0 <- Surv(survive, status) ~ 1
#' #     res0<- survfit(m0, mkarz)
#' #
#' #     ")
#' m0 <- Surv(survive, status) ~ 1
#' res0<- survfit(m0, mkarz)
#' APA2(res0)
#' #windows(8,4)
#' #par(mfrow=c(1,2))
#' #plot( res0 , ylab="Hazard", mark.time = T)
#' #plot( res0, fun="cumhaz",  ylab="Cumulative Hazard" )
#' #SaveData(caption="plot: mkarz")
#'
#'
#'
#'
#' m1 <- Surv(survive, status) ~ lkb
#' res1<- survfit(m1, mkarz)
#' fit1<- coxph(m1, mkarz)
#' logrank1<- survdiff(m1, mkarz)
#' model_info(logrank1)
#' APA2(res1, caption="Kaplan-Meier")
#' APA2(logrank1)
#' APA2(coxph(m1,mkarz))
#'
#' #End()
NULL



# stp25data::mkarz
#' @rdname APA2
#' @export
APA2.summary.survfit<- function(fit,
                                digits=NULL, # an fix_format()
                                percent=FALSE,
                                include = c(
                                  time = "time",
                                  n.risk ="n.risk",
                                  n.event ="n.event",
                                  surv = "survival",
                                  std.err = "std.err",
                                  lower = "lower 95% CI",
                                  upper ="upper 95% CI"),
                                ...){
#cat("\nin APA2.summary.survfit")
  #fit <- survfit((time=time,event=death)~group)
  #surv.prob <- summary(fit,time=c(0,10,20,30))$surv
  if(is.null(names(include))){
    vars <- vars_names <- include}
  else{
  vars <-  names(include)
  vars_names <- as.character(include)
  }

   result<-as.data.frame( fit[vars])
   if(percent) {

   #  print( result$survival )
   #  str(result$survival )
   result$surv<- result$surv*100
   result$lower<- result$lower*100
   result$upper<- result$upper*100
  }

  colnames(result)<-vars_names
  #

  result <-  fix_format(result,  exclude=1:3, digits =digits)

  #print(result)

  if( "strata" %in% names(fit) ) result<- cbind(Source=fit$strata,
                                              result)

     Output(result,...)
  invisible(result)
}

#' @rdname APA2
#' @export
APA2.survfit<- function(fit,
                        caption="NULL",
                        note="",
                        type=1, # 1ist Mediane 2 Mediane + Tabelle
                        digits=2,
                        ...){
  if(length(names(fit))>11){# Workaround fuer unterschiedlichen Output
    # Mediane berechnen
    mdn <- fix_to_data_frame(summary(fit)$table)

    if(!is.null(digits)){
Text(names(mdn))
      mdn["median"] <- round( mdn["median"], digits )
      mdn["0.95LCL"] <- round( mdn["0.95LCL"], digits )
      mdn["0.95UCL"] <- round( mdn["0.95UCL"], digits )


    }
    mdn <- if(ncol(mdn) == 10 )
                   cbind(mdn[,c(1,2,5)], mdn[,c(8:10)],
                         Mean= rndr_mean(mdn[,6],mdn[,7]))
           else cbind(mdn[,c(1,4)], mdn[,c(7:9)],
                      Mean= rndr_mean(mdn[,5],mdn[,6]))
    Output(mdn,
           caption=paste("Survival Mean ", caption), note=note)

    if(type!=1) with(fit, data.frame(Time=time,
                                     Risk= n.risk,
                                     Event=n.event,
                                     Survival= Format2(surv,2),
                                     SE= Format2(std.err,2 )
    )) %>% Output(caption=paste("Survival Table ", caption))
  }
  else{
    Text("Hier besser folgend kodieren: 0=alive, 1=dead. Other choices are TRUE/FALSE (TRUE = death).")
    return(fit)
  }


}




#' @rdname APA2
#' @export
APA2.survdiff<- function(fit,
                         caption="Test Survival Curve Differences",
                         note=""){
  df<-length(fit$n) - 1
  p.val <- 1 - pchisq(fit$chisq, df)

  data.frame(Source=c(names(fit$n), "Overall"),
             N= as.integer( c(fit$n, sum(fit$n) )),
             Observed= as.integer( c(fit$obs, sum(fit$obs) )),
             Expected =c( round(fit$exp,1), NA)
  ) %>% Output(caption,
               note=APA.survdiff(fit) )
}

#' @rdname APA2
#' @export
APA2.coxph<- function(fit,
                      caption="",
                      note="",
                      ...){

  sfit<- summary(fit)
  res<- rbind(
    "Wald test"=  fix_format_p(sfit$waldtest) ,
    "Score (logrank) test"= fix_format_p(sfit$sctest),
    "Likelihood ratio test"= fix_format_p(sfit$logtest ) ,

    "Concordance"= c(paste0(Format2(sfit$concordance[1],2)
                      , " (SE=", Format2(sfit$concordance[1],2),")"), NA, NA),
    "Rsquare"= c(Format2(sfit$rsq[1],2), NA, NA),
    "AIC"= c(Format2(AIC(fit),2), NA, NA),
    "BIC"= c(Format2(BIC(fit),2), NA, NA)
  )
  Output(fix_to_data_frame(res), caption, note)
}

#'  fix_irgendwas fix_format_p ist nur hier in Verwendung
#'  fix_format_p sucht automatisch nach den p-Werten die meist an der Letzten stelle sind
#'  und gibt einen Vector-String mit der LAenge drei aus.
#'  Nicht zu verwechseln mit \code{rndr_P()}
#' 
#'  fix_format_p Input(F, df,und p)  Output: (test, df, p.value)
#' 
#' 

fix_format_p<- function(x, df1=NULL,df2=null, p=NULL){
  if(is.vector(x) ){
    if(length(x==3))
      c(Format2(x[1], 2),  x[2],  ffpvalue(x[3]))
    else if(length(x==4))
      c(Format2(x[1], 2),  paste(x[2], ", ", x[3]),   ffpvalue(x[4]))
    
  }
  else  { }
}