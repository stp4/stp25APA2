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
  function(x, na.rm=TRUE, ... ){  #- test_that
        #  cat(str(x))
    mean_factor<- function(x, na.rm=TRUE){
           # cat(levels(x),"\n")
         if(nlevels(x)==2)    mean(as.numeric(x), na.rm=na.rm) -1
         else   mean(as.numeric(x), na.rm=na.rm)
    }


    if(is.numeric(x)) mean(x, na.rm=na.rm)
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






#' @rdname Berechne
#' @export
Mean2 <- function(x, ...){UseMethod("Mean2")}

#' @rdname Berechne
#' @export
Mean2.formula<-  function(x, data, ...){
    if(length(x) == 2){
        strg<- c(NULL)
        for(i in all.vars(x) ){
            strg<- c(strg, Mean2(data[,i]))
        }
        #strg
    } else {
   strg <-  aggregate(x, data, FUN=Mean2.default)
    apply(strg, 1, function(x) {
        paste(x, collapse=" = ")
    })}

    strg
}

#' @rdname Berechne
#' @export
Mean2.default<- function(x, digits=NULL){  #- test_that
    calc_mean<- function(x){
      if(is.null(digits)) ffmean(mean2(x), ifelse(n>2, sd2(x), NA))
      else  ffmean(mean2(x), ifelse(n>2, sd2(x), NA), digits)
    }
print(str(x))
if (length(x)<=0) return("NaN")
if( is.vector(x) | is.numeric(x)){calc_mean(x)
}else if( is.data.frame(x) ){
    if( ncol(x)==1 ){calc_mean(x[,1])}
    else{ unlist(lapply(as.data.frame(x), calc_mean))}
}else{ cat("Unbekanter Datentype",class(x) )
return("NaN") }
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
        for(i in all.vars(x) ){
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
Median2.default<- function(x, digits = NULL, ...) {
    #- test_that
    if (length(x) <= 0)
        return("NaN")
    if (is.vector(x) | is.numeric(x)) {
        if (!is.numeric(x))
            x <- as.numeric(x)
        if (is.null(digits))
            ffmedian(quantile(x, na.rm = TRUE), ...)
        else
            ffmedian(quantile(x, na.rm = TRUE), digits, ...)
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
#--------------------------------------------
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
#--------------------------------------------

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





#' @rdname Berechne
#' @description Prozent2 wird in APA.formula errate_statistik2 verwendet
#' freq = Prozent2(x_NA, exclude, digits=digits.percent, rtrn = "df", max_factor_length = max_factor_length)
#' @export
Prozent2APA <- function(x,
                        exclude = NA,
                        digits = 1,
                        max_factor_length = 35,
                        ...) {
  Non_Factor_To_Factor <- function(x) {
    if (is.logical(x)) {
      x <- factor(x, c(TRUE, FALSE))
    } else if (is.numeric(x)) {
      if (is_all_0_1(x))
        x <- factor(x, c(0, 1))
      else{
        x <- as.numeric(x)
        xf <- factor(x)
        if (nlevels(xf) > 7)
          x <- cut(x, quantile(x, na.rm = TRUE))
        else
          x <- xf
      }
    } else
      x <- rep(NA, length(x))
    x
  }
  
  if (!is.factor(x))
    x <- Non_Factor_To_Factor(x)
  
  x_NA <- x  # --mit nas
  N    <- length(x)
  x    <- na.omit(x)
  n    <- length(x)
  
  if (n == 0) {
    result <- ""
    ans <- rep(NA, nlevels(x_NA))
    names(ans) <- levels(x_NA)
  } else {
    if (is.null(exclude))
      x <- x_NA
    ans <- table(x, exclude = exclude)
    
    if (length(ans) > max_factor_length) {
      naLev <- levels(x)[-(1:max_factor_length)]
      Text("NA = ", paste(naLev, collapse = ", "))
      
      x <- factor(x, levels(x)[1:max_factor_length], exclude = NULL)
      x <- addNA(x)  #- addNA modifies a factor by turning NA into an extra level
      N <- length(x)
      n <- length(x)
      ans <- table(x)
    }
    
    Freq <- as.data.frame(ans)
    Precent <- as.data.frame(round(prop.table(ans) * 100, 3))
    result <- rndr_percent(Precent[, 2], Freq[, 2], digits = digits)
    # result <- ffprozent.default(Precent[, 2], Freq[, 2])
  }
  data.frame(
    Characteristics = names(ans),
    n = c(n, rep("", length(ans) - 1)),
    Statistics = result
  )
  
}





# Prozent2 <- function(x,  exclude=NA,
#                      #digits = 1,continuous = 3, breaks = NULL, labels = NULL,
#                      # count_factor = c("yes","ja", "T", "TRUE", 1),
#                      # retur_tabel=FALSE,
#                      rtrn="",
#                      max_factor_length=35,
#                      ...) {
#   Non_Factor_To_Factor<- function(x) {
#     if(is.logical(x)){
#       x<-factor(x, c(TRUE, FALSE))
#     }else if(is.numeric(x)){
#       if(is_all_0_1(x)) x<-factor(x, c(0, 1))
#       else{
#         x <- as.numeric(x)
#         xf <- factor(x)
#         if(nlevels(xf) > 7) x<-cut(x, quantile(x, na.rm = TRUE))
#         else x<-xf
#       }
#     }else x<-rep(NA, length(x))
#     x
#   }
#   
#   APA2_Prozent<- function() {
#     if (!is.factor(x)) {
#       x <- Non_Factor_To_Factor(x)
#     }
#     
#     x_NA <- x  # --mit nas
#     N    <- length(x)
#     x    <- na.omit(x)
#     n    <- length(x)
#     
#     if (n == 0) {
#       result <- ""
#       ans<- rep(NA, nlevels(x_NA))
#       names(ans) <- levels(x_NA)
#     }else {
#       if (is.null(exclude)) x <- x_NA
#       ans <- table(x, exclude = exclude)
#       
#       if(length(ans) > max_factor_length) {
#         naLev<- levels(x)[-(1:max_factor_length)]
#         Text("NA = ", paste(naLev, collapse=", "))
#         
#         x <- factor(x, levels(x)[1:max_factor_length], exclude = NULL)
#         x <- addNA(x)  #- addNA modifies a factor by turning NA into an extra level
#         N <- length(x)
#         n <- length(x)
#         ans<-table(x)
#       }
#       
#       Freq <- as.data.frame(ans)
#       Precent <- as.data.frame(round(prop.table(ans) * 100, 3))
#       
#       result <- ffprozent.default(Precent[, 2], Freq[, 2])
#       ##data.frame(Characteristics = "", n = as.character(n), Statistics =Meanci2(x, ...)),
#     }
#     
#     # print(ans)
#     if(rtrn=="df") data.frame(Characteristics = names(ans),
#                               n = c(n, rep("", length(ans) - 1)),
#                               Statistics = result)
#     else result
#   }
#   
#   
#   
#   
#   if(match.call()[[1]]=="Prozent2")  APA2_Prozent()
#   else Prozent(x, ...)
# }


# Normskalen
#
# Eine Kopie von <environment: namespace:psytabs>  norms(1:10, "IQ")
#- nicht benutzt bis jetzt
# Norms <-function (sumscores,
#                   statistics = "PR",
#                   from=min(sumscores, na.rm=TRUE),
#                   to=max(sumscores, na.rm=TRUE)) {
#     sumscores.range <- from:to
#     xecdf <- ecdf(sumscores)
#     sumscores.z <- (sumscores.range - mean(sumscores, na.rm = TRUE))/sd(sumscores,
#                                                                         na.rm = TRUE)
#     norm.table <- data.frame(Score = sumscores.range)
#     if (!is.na(statistics[1])) {
#       if ("PR" %in% statistics) {
#         sumscores.percentranks <- round(xecdf(sumscores.range) *
#                                           100, 1)
#         norm.table <- cbind(norm.table, PR = sumscores.percentranks)
#       }
#       if ("T" %in% statistics) {
#         sumscores.t <- round(50 + 10 * sumscores.z, 1)
#         norm.table <- cbind(norm.table, T = sumscores.t)
#       }
#       if ("Stanine" %in% statistics) {
#         sumscores.stanine <- trunc(5 + sumscores.z * 2)
#         sumscores.stanine[sumscores.stanine < 1] <- 1
#         sumscores.stanine[sumscores.stanine > 9] <- 9
#         norm.table <- cbind(norm.table, STANINE = sumscores.stanine)
#       }
#       if ("IQ" %in% statistics) {
#         sumscores.iq <- round(100 + 15 * sumscores.z, 1)
#         norm.table <- cbind(norm.table, IQ = sumscores.iq)
#       }
#       if ("Z" %in% statistics) {
#         sumscores.Z <- round(100 + 10 * sumscores.z, 1)
#         norm.table <- cbind(norm.table, Z = sumscores.Z)
#       }
#       if ("z" %in% statistics) {
#         norm.table <- cbind(norm.table, z = sumscores.z)
#       }
#     }
#     else {
#     NULL
#     }
#     return(norm.table)
#   }



#-- Neuen Apa Tabellen
stp25_stat_methode <- function(x,
                               mymet = c("freq",  # , "mean.ci", "median.ci","freq.ci", "cohen.d", "effsize"))
                                         "mean", "median",
                                         "multiresponse"),
                               funNames=c(
                                 mean="Mean2default",
                                 median="Median2default",
                                 freq="Prozent2default",
                                 multiresponse="Multi2default"
                               )
                                 ){
 # mth<-pmatch(x, mymet)
  #mymet[mth]
  mymet[match(x, mymet)]
}




#' @rdname Berechne
#' @export
Mean2default <- function(x, digits = 2, n = length(x)) {
  calc_mean <-
    function(x) {
      rndr_mean(mean(x), ifelse(n > 2, sd (x), NA), digits)
    }
  m <- if (is.numeric(x))
    calc_mean(x)
  else
    calc_mean(as.numeric(x))
  data.frame(lev = "(mean)",
             n = as.character(n),
             m = m)
}

#' @rdname Berechne
#' @export
Median2default <- function(x, 
                           digits = 2, 
                           n = length(x),
                           median.style=get_my_options()$apa.style$mittelwert$median.style
                           ) {
    #style=IQR  quantile rndr_median_range
  calc_median <-
    function(x) {
      if (median.style == "IQR") {
        rndr_median(median(x), ifelse(n > 2, IQR(x), NA), digits)
      } else {
        rndr_median_quant(quantile(x, na.rm = TRUE), digits)
      }
    }
    m <- if (is.numeric(x)) calc_median(x) else calc_median(as.numeric(x))

  data.frame(lev = "(median)",
             n = as.character(n),
             m = m)
}



#' @rdname Berechne
#' @export
#' @examples
#' set_my_options(prozent = list(digits = 1,
#' style = 2,
#' null_percent_sign= "."))
#'
#' Prozent2default(factor(c(1,2,3,3,3,5), 1:5))
#'
Prozent2default <-
  function(x,
           digits = 0,
           n = length(x),
           exclude = NA,
           max_factor_length = 25) {
    if (!is.factor(x))
      x <- factor(x)

    x_NA <- x  #  x mit nas
    N    <- length(x)
    if (n == 0) {
      result <- ""
      ans <- rep(NA, nlevels(x))
      names(ans) <- levels(x)
    } else {
      ans <- table(x, exclude = exclude)

      if (length(ans) > max_factor_length) {
        naLev <- levels(x)[-(1:max_factor_length)]
        Text("NA = ", paste(naLev, collapse = ", "))
        x <-
          factor(x, levels(x)[1:max_factor_length], exclude = NULL)
        x <-
          addNA(x)  #- addNA modifies a factor by turning NA into an extra level
        N <- length(x)
        n <- length(x)
        ans <- table(x)
      }
      result <- rndr_percent(prop.table(ans) * 100, ans)
    }
    data.frame(
      lev = names(ans),
      n = c(n, rep("", length(ans) - 1)),
      m = as.vector(result)
    )
  }


Multi2default<- function(x,
                         digits = 0,
                         n = length(x),
                         use.level=1 ){

  if(is.factor(x) & nlevels(x)==2){
    firstLevel<- levels(x)[use.level]
    x<- factor(ifelse(x == firstLevel, firstLevel, 0), c(firstLevel,0))
  }
  else if(is.logical(x)){
    x<- as.factor(x, c(TRUE, FALSE))
  }
  else if(is.numeric(x) & is.integer(x)) {
    x <- facror( ifelse(x == 1, 1, 0), 1:0)

  } else {
    return( data.frame(
        lev ="",
        n =n,
        m = "n.a.") )
  }

    Prozent2default(x, digits, n)[1,]
}
