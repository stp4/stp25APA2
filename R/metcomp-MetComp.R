#' @rdname MetComp
#' @title Uebereinstimmung und Praezision von Messwerten
#' @name MetComp
#' @description Tukey Mean Difference oder auch Bland Altman Methode. Oft interessiert die Zuverlaessigkeit und Reproduzierbarkeit ein einer Diagnose. Die Beurteilung kann dabei durch einen Bewerter (Messverfahren) in wiederholter Form erfolgen und wird dann als Intra-Rater bezeichnet oder die Beurteilung eines Merkmals erfolgt durch mehrere Bewerter (Messverfahren). und hier spricht man von Inter-Rater.
#' Die Methode der Beurteilung der uebereinstimmung haengt von den jeweiligen Datentype ab.
#' Bei Nominalen wird abgezaehlt und die Rate der uebereinstimmung bewertet (Cohen-Koeffizient) Bei Ordinalen-Daten werden die gewichteten uebereinstimmungen ausgezaehlt (gewichteter Cohen-Koeffizient). Bei metrischen(stetigen) Daten werden die Differenzen beurteilt (Bland-Altman-Methode).
#'
#' Bland-Altman-Methode Bias (d) systematische Abweichung Messfehler (s) Standardabweichung der Differenz Limits of agreement (LOA) Intervall von 95 (entspricht d+-1.96 -> es wird eine Normalverteilung unterstellt).
#' Methoden Die generische Funktion MetComp() kann sowohl Kappa als auch Tukey-means berechnen. Kappa kann aber auch ueber die xtab() und APA2 berechnet werden. Wobei hier nur 2x2-Tabellen untersucht werden und bei Kappa() sind hingegen auch mehrere ordinale Kategorien erlaubt sind.
#' aehnliche Methode ist ICC die aber eher zur Reliabilitaetsanalyse gehoert.
#'
#' @param .data Daten
#' @param x Formula Objekt
#' @param ... weitere Objekte nicht benutzt
#' @return Ein bland_altman-Objekt mit den Daten (data) und der Statistik (stat).
#' @export
#' @examples
#' #library(stp25)
#' #library(tidyr)
#' #graphics.off()
#' #setwd("C:/Users/wpete/Dropbox/1_Projekte/000 Temp")
#' #Projekt("html", "bland_altman")
#' #- Understanding Bland Altman analysis
#' #Davide Giavarina
#' #Biochemia medica 2015;25(2) 141-51
#' #http://dx.doi.org/10.11613/BM.2015.015
#'
#' set.seed(0815)
#' DF<- data.frame(
#'   A=c(1, 5,10,20,50,40,50,60,70,80, 90,100,150,200,250,300,350,400,450,500,550,600,650,700,750,800,850,900, 950,1000),
#'   B=c(8,16,30,14,39,54,40,68,72,62,122, 80,181,259,275,380,320,434,479,587,626,648,738,766,793,851,871,957,1001, 980),
#'   group= sample(gl(2, 15, labels = c("Control", "Treat")))
#' )
#'
#' MetComp2(~A+B, DF, caption = "Giavarina")
#'
#' #Sachs Angewandte Statistik Seite 627
#'
#' DF2<- data.frame(A= factor(c(rep(1, 14), rep(1, 3),
#'                              rep(0, 5),rep(0, 18)),1:0, c("+", "-")),
#'                  B= factor(c(rep(1, 14), rep(0, 3),
#'                              rep(1, 5),rep(0, 18)),1:0, c("+", "-")) )
#'
#'
#' APA2(xtabs(~A+B, DF2), test=T)
#' MetComp2(~A+B, DF2)
#'
#'
#' DF <- transform(DF, C = round( A + rnorm(30,0,20)),
#'                 D = round( A + rnorm(30,0,10) + A/10 ),
#'                 E = round( A + rnorm(30,5,10) + (100-A/10) ))
#'
#' #head(DF)
#'  xt <-xtabs(~A+B, DF2)
#'  Klassifikation2(xt)
#'
#' x<- BlandAltman(~A+E , DF)
#' #x$groups
#' x %>% Output( )
#' ICC2(~A+E , DF)
#' #windows(8,3)
#' #plot(x)
#' #SaveData()
#' x<- BlandAltman(A+E+B~group, DF)
#'
#' #windows(8,3.2)
#' #plot(x)
#'
#' n<-1000
#' DF<- data.frame(
#'   A=rnorm(n, 100,50),
#'   B=rnorm(n, 100,50),
#'   C=NA,  D=NA,  E=NA,
#'   group= sample(gl(2, n/2, labels = c("Control", "Treat")))
#' )
#' DF <- transform(DF, C = round( A + rnorm(n,0,20)),
#'                 D = round( A + rnorm(n,0,10) + A/10 ),
#'                 E = round( A + rnorm(n,5,10) + (100-A/10) ))
#'
#' x<- BlandAltman(A+E~group, DF)
#'
#' #windows(8,3.2)
#' #plot(x)
#'
#'
#' #library(stp25)
#' #library(tidyr)
#' #graphics.off()
#' #setwd("C:/Users/wpete/Dropbox/1_Projekte/000 Temp")
#' #Projekt("html", "bland_altman")
#' #- Understanding Bland Altman analysis
#' #Davide Giavarina
#' #Biochemia medica 2015;25(2) 141-51
#' #http://dx.doi.org/10.11613/BM.2015.015
#'
#' set.seed(0815)
#'
#' n<-100
#' DF<- data.frame(
#'   A=rnorm(n, 100,50),
#'   B=rnorm(n, 100,50),
#'   C=NA,  D=NA,  E=NA,
#'   group= sample(gl(2, n/2, labels = c("Control", "Treat")))
#' )
#'
#' cutA<-mean(DF$A)
#' DF <- transform(DF, C = round( A + rnorm(n, -5, 20)),
#'                 D = round( A + rnorm(n,0,10) + A/10 ),
#'                 #E = round( A + rnorm(n,5,10) + (100-A/10) )
#'                 E = A + ifelse(A<cutA, A/5, -A/5 )+ rnorm(n, 0, 10)
#' )
#'
#'
#' x<- BlandAltman(~A+C, DF)
#' #windows(8,3.2)
#' #plot(x)
#' #SaveData(caption="A und C Messen das gleiche mit SD=20")
#'
#' x<- BlandAltman(~A+B, DF)
#' #windows(8,3.2)
#' #plot(x)
#' #SaveData(caption="A und B Messen unterschiedliche Parameter")
#'
#'
#' x<- BlandAltman(~A+D, DF)
#' #windows(8,3.2)
#' #plot(x)
#' #SaveData(caption="A und D Messen das unterschiedlich D hat im unteren
#'  #        Wertevereich deutlich geringere Werte")
#' x<- BlandAltman(~A+E, DF)
#' #windows(8,3.2)
#' #plot(x)
#' #SaveData(caption="A und E Messen das unterschiedlich es esistiert ein knik im Wertebereich 100")
#'
#' #End()
MetComp<-function(.data, x, ...) {
  UseMethod("MetComp")
}

#' @rdname MetComp
#' @export
MetComp.data.frame <- function(.data, x, ...) {
  X <- Formula_Data(x, .data )
  
  if(!all_identical2(X$Y_data)) return("Unterschiedliche Daten")
  
  if( is.numeric(X$Y_data[[1]]) )
    BAP(x, .data , ...)$stat
  else if( is.integer(X$Y_data[[1]]) )
    BAP(x, .data , ...)$stat
  else if( is.factor(X$Y_data[[1]]) ) {
    xtb <- xtabs(x, .data)
    Kappa(xtb, ...)
  }
}
#' @rdname MetComp
#' @export
MetComp.formula <- function(x, .data,  ...) {
  MetComp.data.frame(.data, x, ...)
}


#' @rdname MetComp
#' @export
MetComp2<-function(.data, x, ...) {
  UseMethod("MetComp2")
}

#' @rdname MetComp
#' @export
MetComp2.data.frame <- function(.data, x, ...) {
  X<-  Formula_Data(x, .data )
  
  if(!all_identical2(X$Y_data)) return("Unterschiedliche Daten")
  
  if( is.numeric(X$Y_data[[1]]) ) BAP2(x, .data , ...)
  else if( is.integer(X$Y_data[[1]]) ) BAP2(x, .data , ...)
  else if( is.factor(X$Y_data[[1]]) ) {
    xtb <- xtabs(x, .data )
    Kappa2( xtb, ...)
  }
}
#' @rdname MetComp
#' @export
MetComp2.formula <- function(x, .data, ...) {
  MetComp2.data.frame(.data, x, ...)
}