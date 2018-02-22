#' @param type Mittelwert ("mean")
#' @param ... 
#'
#' @rdname APA
#' @export
#' @examples 
#' 
#' \dontrun{
#'  library(stpvars)
#'  APA(chol0+chol1 ~ g, hyper)
#'  }
#'  
APA.formula <- function(x,
                        data,
                        type = "mean",
                        digits = 2,
                        ...) {
  #type=c("mean","median")
  if (length(data) == 0)
    return("Fehlende Daten")
  type <-  match.arg(type)
  res <- aggregate(x, data, function(y)
    paste0(Format2(mean(y, na.rm = TRUE), digits), " (",
           Format2(mean(y, na.rm = TRUE), digits), ")"))
  
  apply(res, 1, function(y) {
    paste(y, collapse = " = ")
  })
}




#' @rdname APA2
#' 
#' @description Die Funktion \code{APA2.formula} estellt die Standard-Tabellen (analog wie die Hmisc:summary).
#'  Links stehen die Zielvariablen rechts die Gruppen.
#'  
#'  Fie Formel  \code{a1 + a2[4] +a3 ~ group1 + group2} ergibt zwei Auswertungen. Die Zahle in eckiger Klammer
#'  sind die Nachkommastellen. Achtung die Formeln sind auf 500 zeichen begrenzt (Limitation von der Funktion \code{deparse()})
#'  Einstellungen werden global erstellt:
#'  
#'      \code{set_my_options(prozent=list(digits=c(1,0), style=2))}
#'      
#'      \code{get_my_options()$apa.style$prozent}
#'      
#' @param fun,na.action,direction  eigene Funktion na.action=na.pass
#' (Auswertung ueeber die Funktionen \code{melt, cast}.)
#' @param type formula: \code{c("auto", "freq", "mean", "median", "ci", "freq.ci")} 
#' xtabs: type = c("fischer", "odds","sensitivity", "chisquare","correlation", "r")
#' 
#' @param test,include.test,corr_test,include.p,include.sig.star    Sig test bei  \code{type = auto} moegliche Parameter sind  test=TRUE, test="conTest" oder "sapiro.test" fuer den Test auf Normalverteilung, fuer SPSS-like \code{test=="wilcox.test"}  oder \code{test=="kruskal.test"}
#'  corr_test-ddefault ist  "pearson" c("pearson","spearman")
#' @param cor_diagonale_up bei Correlation art der Formatierung
#' @param order,decreasing Sortieren   Reihenfolge der Sortierung
#' @param use.level Benutzter level in Multi zB ja/nein
#' @param include.n,include.all.n,include.header.n,include.total N mit ausgeben
#' 
#' @param include.names,include.labels Beschriftung der zeilen
#' @param digits.mean,digits.percent Nachkommastellen
#' @param print.n,sig.star,pvalues,total veraltet jetzt include.n verwenden
#' @param output Ausgabe von Ergebiss ueber Output
#' @return liste mit data.frames
#' @export
#' @examples
#'
#' #-- APA2.formula --
#'
#' APA2(chol0+chol1 ~ g, hyper, print.n=FALSE)
#' APA2(~ g, hyper, caption="Einfache Tabelle")
#' APA2(chol0+chol1 ~ g, hyper, caption="Spalte mit Characteristik loeschen", print_col=-2)
#' APA2(gew + rrs0 ~ g, hyper, print.n=FALSE, test=TRUE)
#' APA2(~chol0+chol1~chol6+chol12, hyper, caption="Korrelation", test=TRUE)
#' APA2(~chol0+chol1+chol6+chol12, hyper, caption="Korrelation", test=TRUE, sig.star=FALSE)
#'
#' #End()
#'
APA2.formula <- function(x,
                         data = NULL, caption="", fun = NULL,
                         type =c("auto", "freq", "mean", "median",
                                  "ci", "multiresponse","cohen.d","effsize",
                                  "freq.ci","describe"),
                         note="", na.action=na.pass, test = FALSE,
                         sig.star = TRUE, p.value=FALSE,
                         pvalues =  if(p.value) p.value else FALSE,
                         print.n = NULL, #veraltet # head.n = print.n,
                         corr_test = "pearson", cor_diagonale_up=TRUE,
                         direction = "long",
                         order = FALSE, decreasing = TRUE,total=FALSE, #veraltet
                         use.level=1, # Multi

                     include.n=TRUE,
                     include.all.n=print.n,
                     include.header.n=TRUE,
                     include.total=total,
                     include.test=test,
                     include.p=pvalues,
                     include.sig.star=sig.star,
                     include.names=FALSE,
                     include.labels=TRUE,
                     #include.type=TRUE,


                         digits=NULL,
                         digits.mean=
                           if(!is.null(digits))c(digits,digits)# options()$stp25$apa.style$mittelwert$digits
                            else NULL,
                         digits.percent=
                            if(is.null(digits)) options()$stp25$apa.style$prozent$digits
                             else c(digits, 0),
                     output=TRUE,
                         ...) {


 # if(!is_vars_in_data(Formula, data)) {
 #       Text(Tab(),
 #                   "Variablen-Namen passen nicht oder die Daten sind nicht vorhanden!")
 #       return(NULL)
 #   }

 if(include.names & include.labels) {
     nms<-names(data)
     lbl <- GetLabelOrName(data)
     lbl<-paste(nms, lbl)
     names(lbl)<-nms
     data<-label_data_frame(data, lbl)
 }else if (!include.labels){
   nms<-names(data)
   names(nms)<-nms
   data<-label_data_frame(data, nms)
 }



type <-  match.arg(type, several.ok = TRUE)
if (!is.null(fun)) type <-  "recast"
if (length(type) > 2) type <- type[1] # Fehler abfangen
#cat("\n APA2(..., type =", type, ")\n")
result <- switch(type[1],
        recast = Recast2_fun(x,
                             data,
                             caption,
                             fun, note=note,
                             include.n=include.n,
                             direction=direction,
                             ...),
        multiresponse =  APA2_multiresponse(x, data,
                                            caption=caption, note=note,
                                            test=test,
                                            order=order, decreasing=decreasing,
                                            na.action=na.action, use.level=use.level ),
        cohen.d = cohen_d_formula( x, data, ...),
        # effsize = Effsize( x, data, ..., type="cohen.d"),
        describe = Describe2(x, data, stat=c("n", "mean","sd","min","max")),
        errate_statistik2(x,
                          data = as.data.frame(data),
                          caption=caption,
                          note=note,
                          na.action=na.action,
                          type = if(length(type)>1  | type[1]!="auto" ) type  else NULL,

                          include.n=include.n,
                          include.all.n=include.all.n,
                          include.header.n=include.header.n,
                          include.total=include.total,
                          include.test=include.test,
                          include.p=include.p,
                          include.sig.star=include.sig.star,

                         # test = test, sig.star = sig.star,
                         # pvalues = pvalues, print.n = print.n,
                          order = order, decreasing = decreasing,
                          corr_test = corr_test, cor_diagonale_up=cor_diagonale_up,
                         # total=total,
                          digits.mean=digits.mean,
                          digits.percent=digits.percent,
                          ...  #exclude

                         )
    ) #rueckgabewert data.frame oder liste mit dataframe oder liste html

#


#print(str(result))
if(output){
if (is.data.frame(result)) Output(result)
else if(is.list(result)) for( rst in result) Output(rst)
else Text(Tab(),class(result), " ", result)
}

invisible(result)
}




#- Interne Recast-Function
Recast2_fun <- function(Formula, data, caption="",
                        fun,
                        direction="long", note="",
                        include.n=FALSE,
                        ...){

  ANS <-  Recast2(Formula, data, fun,  drop = FALSE)
  if(include.n) {
    ans_n <- Recast2(Formula, data, fun=function(x) length(na.omit(x)),  drop = FALSE)
    ANS <- data.frame(ANS[-ncol(ANS)], n=ans_n$value, value=ANS[, ncol(ANS)])
  }
  ANS <- prepare_output(ANS, caption, note, nrow(data))

  if (direction != "long") prepare_output(
                            reshape2::dcast(ANS,
                                            as.formula(paste(
                                              "variable", paste(Formula[-2], collapse= ""))))
                            , caption, note, nrow(data))
  else ANS

}
