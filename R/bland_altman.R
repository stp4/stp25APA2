#' @rdname APA2
#' @export
APA2.bland_altman<- function(...) Output(x)






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



#' @rdname BlandAltman
#' @title BlandAltman
#' @name BlandAltman
#' @description Diese Funktion ist ein Platzhalter
#' (see \code{\link[haven]{tagged_na}})
#' und zeigt die ...
#' @param x Objekt
#' @param ... weitere Objekte nicht benutzt
#' @return A \code{\link[tibble]{tibble}} with counted tagged NA values.
#'
#' @examples
#' library(haven)
#'
#'

#' @export

BlandAltman<-function(.data, x, ...) {
  UseMethod("BlandAltman")
}

#' @rdname BlandAltman
#' @export
BlandAltman.data.frame <- function(.data, x, ...) {
  #-- hier Fehlt noch die Unterscheidung in 2 oder mehr Vergleiche
  BAP(x, .data, ...)
}
#' @rdname BlandAltman
#' @export
BlandAltman.formula <- function(x, .data, ...) {
  BlandAltman.data.frame(.data, x, ...)
}





#' @rdname BlandAltman
#' @export
Kappa <- function(xtb,  ..., CI=FALSE)
  vcd:::Kappa(xtb, ...) %>% fix_vdc_kappa(CI=CI)


#' @rdname BlandAltman
#' @export
Kappa2<- function(xtb, ..., CI = FALSE,
                  caption="Cohen's Kappa-Koeffizient"#,
                  # digits = max(getOption("digits") - 3, 3), level = 0.95
                  ){
 Output(
   fix_format(Kappa(xtb, ..., CI)),
   caption=caption)
  }
 #x<-vcd::Kappa(xtb, ...)    ####%>% stp25output:::Output.Kappa(CI=CI)
# #  @rdname Output
# #  @export
# Output.Kappa<- function (x
#                          CI = FALSE,
#
#                          caption="Cohen's Kappa-Koeffizient",
#                          ...){
#
#
# }



#-- Helper kapa Formatieren
fix_vdc_kappa<-   function (x, digits = max(getOption("digits") - 3, 3),
                         CI = FALSE,
                         level = 0.95,
                         ...)
{
  tab <- rbind(x$Unweighted, x$Weighted)


  z <- tab[, 1]/tab[, 2]
  tab <- cbind(tab, z, `Pr(>|z|)` = 2 * pnorm(-abs(z)))
  if (CI) {
    q <- qnorm((1 + level)/2)
    lower <- tab[, 1] - q * tab[, 2]
    upper <- tab[, 1] + q * tab[, 2]
    tab <- cbind(tab, lower, upper)
  }
  rownames(tab) <- names(x)[1:2]
  if(!CI) colnames(tab)<- c("Kappa", "SE", "z", "p.value")
  else colnames(tab)<- c("Kappa", "SE", "z", "p.value", "low.CI", "up.CI")

  #abweichung von vdc
  #if(tab[1,1] == tab[2,1]) tab <- tab[1,]

  data.frame(source=rownames(tab), tab)

}







#-- Helper Bland Altman
bland.altman.stats<-
function (dfr,
          two = 1.96, #mode = 1,
          conf.int = 0.95,
          digits=2)
{  ##<environment: namespace:BlandAltmanLeh>

  called.with <- nrow(dfr)
  dfr <- na.omit(dfr)
  based.on <- nrow(dfr)
  if (based.on < 2)
    warning("Warning in bland.altman.stats:less than 2 data pairs after deleting NAs.",
            call. = FALSE)
  if (ncol(dfr) > 2)
    warning("Warning in bland.altman.stats:Mehr als 2 Methoden.",
            call. = FALSE)
  diffs <- dfr[[1]] - dfr[[2]]
  means <-  rowMeans(dfr)
  diffs.percent<-diffs/means*100
  diffs.percent[is.infinite(diffs.percent)]<-0

  critical.diff <- two * sd(diffs)
  mean.diffs <- mean(diffs)
  sd.diffs<- sd(diffs)
  lower.limit <- mean.diffs - critical.diff
  upper.limit <- mean.diffs + critical.diff
  lines <- c(lower.limit = lower.limit,
             mean.diffs = mean.diffs,
             upper.limit = upper.limit)
  t1 <- qt((1 - conf.int)/2, df = based.on - 1)
  t2 <- qt((conf.int + 1)/2, df = based.on - 1)

  se.ci<- sqrt(sd(diffs)^2 * 3/based.on)
  se.mean<-sd(diffs)/sqrt(based.on)
  CI.lines <- c(lower.limit.ci.lower = lower.limit + t1 * se.ci,
                lower.limit.ci.upper = lower.limit + t2 * se.ci,
                mean.diff.ci.lower = mean.diffs + t1 * se.mean,
                mean.diff.ci.upper = mean.diffs + t2 * se.mean,
                upper.limit.ci.lower = upper.limit +  t1 * se.ci,
                upper.limit.ci.upper = upper.limit +  t2 * se.ci)
  #--- Prozent


  mean.percent<-mean(diffs.percent)
  ssd.persent<- sd(diffs.percent)
  critical.diff.percent <- two * ssd.persent
  se.ci.percent<- sqrt(ssd.persent^2 * 3/based.on)
  se.mean.percent<-ssd.persent/sqrt(based.on)
  lower.limit.percent = mean.percent-critical.diff.percent
  upper.limit.percent = ssd.persent+critical.diff.percent

  CI.lines.percent <- c(lower.limit.ci.lower = lower.limit.percent + t1 * se.ci.percent,
                lower.limit.ci.upper = lower.limit.percent + t2 * se.ci.percent,
                mean.diff.ci.lower = mean.percent + t1 * se.mean.percent,
                mean.diff.ci.upper = mean.percent + t2 * se.mean.percent,
                upper.limit.ci.lower = upper.limit.percent +  t1 * se.ci.percent,
                upper.limit.ci.upper = upper.limit.percent +  t2 * se.ci.percent)


  res<- list(lines = lines,  # wie oben ll mean ul
              CI.lines = CI.lines,
              lines.percent = c( mean.percent-critical.diff.percent,
                                 mean.percent,
                                 mean.percent+critical.diff.percent),
              CI.lines.percent = CI.lines.percent,

              stat= data.frame(Parameter=c("df (n-1)","difference mean (d)",
                                           "standard deviation (s)", "critical.diff (1.96s)",
                                           "d-1.96s", "d+1.96s"),
                               Unit= c(Format2(based.on - 1, 0),
                                       Format2( c(mean.diffs, sd.diffs,critical.diff,
                                                    lower.limit,upper.limit), digits)
                                       ),
                               Percent= c("",
                                        ffprozent.default(
                                           c(mean.percent, ssd.persent,
                                           critical.diff.percent,
                                           lower.limit.percent,
                                           upper.limit.percent)
                                            ,digits=1)),
                                SE= Format2(c(NA,se.mean, NA,NA, se.ci, se.ci),digits),
                               CI.low= Format2(c(NA,CI.lines[3],NA,NA,CI.lines[1],CI.lines[5]),digits),
                               CI.hig= Format2(c(NA,CI.lines[4],NA,NA,CI.lines[2],CI.lines[6]),digits)
                               ),
              data=cbind(dfr,
                         means,
                         diffs,
                         diffs.percent=diffs.percent)

              )
  class(res)<- c("bland_altman")
  return(res)
}


BAP2<-  function(x, .data, ...,
                 caption="Bland Altman Methode"){
  BAP(x, .data, ...) %>% Output(caption=caption)}


#--- Helper
BAP<- function(x, .data, ...){
  X<-Formula_Data(x, .data)

  ba.stats <- bland.altman.stats( X$Y_data )
  ba.stats$name <-  paste(X$yname, collapse=", ")
  ba.stats$name.diff <-  paste(X$yname[1:2], collapse=" - ")
  ba.stats$met_A <-X$yname[1]
  ba.stats$met_B <-X$yname[2]
  ba.stats$groups= X$X_data
  ba.stats
}

