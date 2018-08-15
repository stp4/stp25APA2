
# Einfacher Mittelwerte ---------------------------------------------------




#' @rdname Berechne
#' @param x Objekt Vector oder auch Formel
#' @param data bei verwendung von Formeln
#' @param na.rm Fehlende Werte
#' @param digits Dezimalstellen bei zB Mean2
#' @param ci Grenzen der Konfidenzintervalle
#' @param ... Weitere Argumente
#' @return Vector
#' @export
#' @examples
#' mean2(rnorm(100))
#' sd2(rnorm(100))
#' CI(rnorm(100))
#' StdErr(rnorm(100))
#' mean2(rnorm(100))
mean2 <-
  function(x, na.rm = TRUE, ...) {
    #- test_that
    mean_factor <- function(x, na.rm = TRUE) {
      if (nlevels(x) == 2)
        mean(as.numeric(x), na.rm = na.rm) - 1
      else
        mean(as.numeric(x), na.rm = na.rm)
    }
    if (is.numeric(x))
      mean(x, na.rm = na.rm)
    else if (is.factor(x)) {
      warning("Die Mittelwerte wurden durch Transformation auf Numeric berechnet!")
      mean_factor(x, na.rm = na.rm)
    }
    else {
      warning("Die Mittelwerte sind eventuel Falsch!")
      mean_factor(factor(x), na.rm = na.rm)
    }
  }


#' @rdname Berechne
#' @export
median2 <-
    function(x, na.rm=TRUE, ... ){  #- test_that
        #  cat(str(x))
        mean_factor<- function(x, na.rm){
            # cat(levels(x),"\n")
            if(nlevels(x)==2)    median(as.numeric(x), na.rm=na.rm) -1
            else   median(as.numeric(x), na.rm=na.rm)
        }


        if(is.numeric(x)) median(x, na.rm=na.rm)
        else if(is.factor(x)) {
            warning("Die Mittelwerte wurden durch Transformation auf Numeric berechnet!")
            mean_factor(x, na.rm=na.rm)
        }
        else {
            warning("Die Mittelwerte sind eventuel Falsch!")
            mean_factor(factor(x), na.rm=na.rm)

        }
    }


#' @rdname Berechne
#' @export
sd2 <-  function(x, na.rm=TRUE, ...){
    if(is.numeric(x)) sd(x, na.rm=na.rm)
    else {
      warning("Die Mittelwerte wurden durch Transformation auf Numeric berechnet!")
      sd(as.numeric(x), na.rm=na.rm)
    }
  }

#' @rdname Berechne
#' @export
CI <- function (x, ci = 0.95, na.rm=TRUE, ...) {
  #-- stolen from Rmisc
  a <- mean(x, na.rm=na.rm)
  s <- sd(x, na.rm=na.rm)
  n <- length(na.omit(x))
  error <- qt(ci + (1 - ci)/2, df = n - 1) * s/sqrt(n)
  return(c(upper = a + error, mean = a, lower = a - error))
}

#' @rdname Berechne
#' @export
StdErr <- function (x, ...) {
  a <- mean(x, ...)
  s <- sd(x, ...)
  n <- length(na.omit(x))
  error <- (s/sqrt(n))
  return(c(upper = a + error, mean = a, lower = a - error))
}





# Formatierte Mittelwerte aus data.frames ---------------------------------



#' @rdname Berechne
#' @export
Mean2 <- function(x, ...){UseMethod("Mean2")}

#' @rdname Berechne
#' @export
Mean2.formula <-  function(x, data, ...) {
  if (length(x) == 2) {
    strg <- c(NULL)
    for (i in all.vars(x)) {
      strg <- c(strg, Mean2(data[, i]))
    }
  } else {
    strg <-  aggregate(x, data, FUN = Mean2.default)
    apply(strg, 1, function(x) {
      paste(x, collapse = " = ")
    })
  }
  strg
}

#' @rdname Berechne
#' @export
Mean2.default <- function(x, digits = NULL) {
  #- test_that
  calc_mean <- function(x) {
    x <- na.omit(x)
    n <- length(x)
    rndr_mean(mean2(x), ifelse(n > 2, sd2(x), NA), digits)
  }
  
  
  if (length(x) <= 0)
    return("NaN")
  if (is.vector(x) | is.numeric(x)) {
    calc_mean(x)
  } else if (is.data.frame(x)) {
    if (ncol(x) == 1) {
      calc_mean(x[, 1])
    }
    else{
      unlist(lapply(as.data.frame(x), calc_mean))
    }
  } else{
    cat("Unbekanter Datentype", class(x))
    return("NaN")
  }
}



#' @rdname Berechne
#' @export
Median2 <- function(x, ...) {
    UseMethod("Median2")
}

#' @rdname Berechne
#' @export
Median2.formula<-  function(x, data, ...){
    if(length(x) == 2){
        strg<- c(NULL)
        for (i in all.vars(x) ){
            strg<- c(strg, Median2(data[,i]))
        }
    } else {
        strg <-  aggregate(x, data, FUN=Median2.default)
        apply(strg, 1, function(x) {
            paste(x, collapse=" = ")
        })}

    strg
}

#' @rdname Berechne
#' @export
Median2.default<- function(x, digits = NULL, 
                           median.style=get_my_options()$apa.style$mittelwert$median.style, 
                           ...) {
 # cat("in Median2\n")
 # print(head(x))
 # print(digits)
  
    #- test_that
    if (length(x) <= 0)
        return("NaN")
    if (is.vector(x) | is.numeric(x)) { 
        if (!is.numeric(x))
            x <- as.numeric(x)
        
        if (median.style == "IQR") {
          rndr_median(median(x), ifelse(n > 2, IQR(x), NA), digits)
        } else {
          rndr_median_quant(quantile(x, na.rm = TRUE), digits)
        }
              
           # rndr_median_quant(quantile(x, na.rm = TRUE), digits, ...)
    } else if (is.data.frame(x)) {
        if (ncol(x) == 1) {
            Median2(x[, 1])
        }
        else{
            unlist(lapply(as.data.frame(x), Median2))
        }
    } else{
        cat("Unbekanter Datentype", class(x))
        return("NaN")
    }
}

 



#' @rdname Berechne
#' @export
Meanci2<- function(x, digits=NULL, ...){
#  cat("\n in Meanci2\n")
 # print(head(x))
 # print(digits)

if (length(x)<=0) return("NaN")
if(!is.numeric(x)) x <- as.numeric(x)

N <- length(x)
x <- na.omit(x)
n <- length(x)
res <- Hmisc::smean.cl.normal(x, ...)
#print(res)
 # cat("\n-----\n")

if(is.null(digits)) digits <- countDigits(signif(res[1], 4))

paste(Format2(res[1], digits=digits[1]),
                ffCI(c(res[2], res[3]), digits=digits[1]))




}



#' @name Prozent
#' @rdname Prozent
#' @title Prozent Berechnen
#' @description Prozent fuer
#' @param x Objekt Vector oder auch Formel
#' @param exclude Fehlende Werte
#' @param continuous fuer cut
#' @param digits Dezimalstellen bei zB Mean2
#' @param max_factor_length wie viele Faktoren
#' @param continuous fuer cut
#' @param breaks fuer cut
#' @param labels fuer cut
#' @param count_factor wiess nicht
#' @param retur_tabel wiess nicht
#' @param ... Weitere Argumente
#' @return Vector
#' @examples
#' mean2(rnorm(100))
#' sd2(rnorm(100))
#' CI(rnorm(100))
#' StdErr(rnorm(100))
#' mean2(rnorm(100))
#' @export

#- das ist die Hauptfunktion
Prozent<-function (x, digits = 1,
                   continuous = 3, breaks = NULL, labels = NULL,
                   count_factor = c("yes","ja", "T", "TRUE", 1),
                   retur_tabel=FALSE
                   ) {  #- test_that


if (length(x)<=0) return("NaN") #-- fuer Melt2
 
calc_factor <- function(x) {
      if (length(x)<=0) {

        ans <-  if(retur_tabel) 0 else ffprozent.default(0, 0)
        names(ans) <-  names(x)
      }else{
        fq <- table(x)
        prc <- prop.table(fq) * 100
        ans <- if(retur_tabel) fq else ffprozent.default(prc, fq)
        names(ans) <- row.names(fq)
      }
      ans
    }
 

if (is.factor(x)) { calc_factor(x) }
else  if (is.logical(x) | is_all_0_1((x))) {  ffprozent.default( mean(x, na.rm = TRUE) * 100,  sum(x, na.rm=T)) }
else if (is.numeric(x)) {

    xf <- factor(x)

    if(nlevels(xf) > 7) xf<- cut(x, quantile(x, na.rm = TRUE) )
    calc_factor(xf)
    }
else {# hier kommt alles aus Recast2 an weil recast Character weitergibt
      xf <- factor(x)
      lvls <- levels(xf)
      n <- nlevels(xf)
      if (n == 2 |  n==1  ) {
         if(any(tolower(lvls) %in% c("ja", "yes", "true", "1","nein", "no", "false", "0"))){
             x<- ifelse(tolower(x) %in%  c("ja", "yes", "true", "1"), 1, 0)
             if(retur_tabel){ table(x) }
             else {ffprozent.default(mean(x, na.rm = TRUE) * 100, sum(x, na.rm = TRUE))}
         } else if (n == 2) {
             x <- 2 - as.numeric(xf)   # erster Faktor wird gez?hlt
             if(retur_tabel)  {table(x) }
             else {
                    res<-ffprozent.default(mean(x, na.rm = TRUE) * 100, sum(x, na.rm = TRUE))
                    paste(lvls[1], res)#-- namen zu Variable
             }
         } else { "nur eine Factorstufe"}
      }else{  "mehr als 2 Faktorstufen"}

 }
}


