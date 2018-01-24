#' render for html-output
#'
#' Formatiere Zahlen zu Character feur die Ausgabe. Im gegensatz zu Format2()
#' und ff() werden die Zahlen mit zB p=.002 ausgegeben.
#'
#' http://winvector.github.io/APAsig/APAsig.html
#' Format quality of a linear regerssion in
#' roughly in "APA Style" ( American Psychological Association ).
#' see: http://my.ilstu.edu/~jhkahn/apastats.html
#'      https://web2.uconn.edu/writingcenter/pdf/Reporting_Statistics.pdf
#' Not linking direclty to the style guide as they charge for it.
#'
#' @param ... alles an format
#' @return Objekt als Character
#' @export
#' @examples
#' rndr_(1.234, 3)
rndr_ <- function(...) Format2(...)


#' @rdname rndr_
#' @export
#' @description median
rndr_median <- function(m, iqr, digits)
  paste0(Format2(m, digits), " (IRQ ", Format2(iqr, digits), ")")



#' @rdname rndr_
#' @export
#' @description mean
rndr_mean <- function(m, s, digits) {
  #  print(c(m, s, digits))
  paste0(Format2(m, digits), " (", Format2(s, digits), ")")

}

#' @rdname rndr_
#' @export
#' @description Odds
rndr_ods <- function(x, digits = 2) {
  res <- round(x, digits)
  res[which(x > 20)] <- ">20"
  res[which(x < .01)] <- "<0.01"
  res
}
#' @rdname rndr_
#' @description rndr_percent()
#' Percentages are also most clearly displayed in parentheses with no decimal places:
#' Nearly half (49%) of the sample was married.
#' @export
#' @description Prozent
#' @examples
#' rndr_percent(c(.2568, 99, 0.02568) , c(4, 569, 25), digits = 1)
#' rndr_percent(10, 3, F, 2)

rndr_percent <- function(
  prozent,
  anzahl,
  percent = TRUE, # nur die Anzahl zurueckgeben (xtabs)
  digits = options()$stp25$apa.style$prozent$digits[1],
  percentage_str = options()$stp25$apa.style$prozent$percentage_str,
  style = options()$stp25$apa.style$prozent$style,
  null_percent_sign = options()$stp25$apa.style$prozent$null_percent_sign
) {

 if(is.null(percent)) percent <- style != 0

  if (is.vector(prozent)) {
    if (percent) {
      prz <- ifelse(
        prozent < 1 / (10 ^ digits),
        paste0("<", 1 / (10 ^ digits), "%"),
        paste0(
          formatC(
            prozent,
            format = "f",
            digits = digits ,
            decimal.mark = getOption("OutDec")
          ),
          percentage_str
        )
      )
      anz <- formatC(anzahl, format = "f", digits =  0)
      if (style == 1)
        res <- paste0(prz, " (", anz, ")")
      else
        res <- paste0(anz , " (", prz, ")")
    }

    else{
      res <- formatC(anzahl, format = "f", digits =  0)
    }

    if(!is.null(null_percent_sign))
      res[which(anzahl==0)] <- null_percent_sign

    return(res)
  } else{
    myattr <- attributes(anzahl) #-- colnames and rownames
    nrw <- nrow(anzahl)
    anzahl <- suppressWarnings(
      formatC(anzahl, format = "f", digits = 0))
    #------------------------------------------------
    if (percent) {
      prozent <- suppressWarnings(formatC(
        prozent,
        format = "f",
        digits = digits ,
        decimal.mark = getOption("OutDec")
      ))
      if (style == 1)
        res <- matrix(paste0(prozent, "% (", anzahl, ")"), nrow = nrw)
      else
        res <- matrix(paste0(anzahl, " (", prozent, "%)"), nrow = nrw)

    } else
      res <-  anzahl

    attributes(res) <- myattr

    if(!is.null(null_percent_sign))
      res[which(anzahl==0)] <- null_percent_sign

    return(res)
  }
}




#' @description APA Textausgabe
#' p-Werte \code{rndr_P()}
#' @rdname rndr_
#' @export
#' @examples
#'
#'rndr_P(c(1,.25,.08,0.05,0.01,0.0001))
rndr_P<-function(p_val, include.symbol=TRUE, include.bracket=FALSE) {

  if(include.symbol & !include.bracket){
   p<-  ifelse(p_val<.001, "p", "p=")
    paste0(p, ffpvalue(p_val))
    }
  else if(include.bracket) {
    p<-  ifelse(p_val<.001, "p", "p=")
    paste0(" (",  paste0(p, ffpvalue(p_val)),")")
    }
  else {ffpvalue(p_val)}
  }



#' @rdname rndr_
#' @export
rndr_Stars<-function(p_val) ffsigstars(p_val)




#' @rdname rndr_
#' @description rndr_corr
#' Correlations are reported with the degrees of freedom (which is N-2)
#' in parentheses and the significance level:
#' The two variables were strongly correlated, r(55) = .49, p < .01.
#' @export
#' @examples
#' rndr_corr(-.548369,0.005896,55)
rndr_corr <- function(r, p, df){
  paste0("r", rndr_df(df), "=", ffreta(r),", ", rndr_P(p))
}

#' @rdname rndr_
#' @export
rndr_r <- function(r, include.symbol=TRUE) {
  if(include.symbol)  paste0("r=", ffreta(p_val))
  else ffreta(r)
}


#' @rdname rndr_
#' @export
rndr_df<- function(df1, df2=NULL) {
  if(is.null(df2)) paste0("<sub>(", Format2(df1, 0), ")</sub>")
  else  paste0("<sub>(", Format2(df1, 0), ", ", Format2(df2, 0), ")</sub>")

}
#' @description CI-Wert\code{rndr_CI()}  rndr_CI(matrix(c(NA, 1:10, NA), ncol=2))
#' @rdname rndr_
#' @export
rndr_CI <- function(x, digits = 2,
                        #lead.zero = options()$stp25$apa.style$mittelwert$lead.zero,
                      sep=options()$stp25$apa.style$sep_element,
                      sep_1=options()$stp25$apa.style$brackets[1],
                      sep_2=options()$stp25$apa.style$brackets[2] ){

#Format2.matrix kann verschiedene digits aufloesen
res<- paste0(sep_1,
              Format2.matrix(x[,1], digits),
              sep, " ",
              Format2.matrix(x[,2], digits),
              sep_2)

res[which(is.na(x[,1]))]<-NA
res
}






#' @description F-Wert\code{rndr_F()}
#' @rdname rndr_
#' @export
rndr_F<-function(F_val, df1, df2, p=NULL){
  F_val<- paste0("F", rndr_df(df1,df2), "=", fftest(F_val))
  if(is.null(p)) F_val
  else paste0(F_val, symbol_seperator, rndr_P(p))
}



#' @description T-Wert\code{rndr_T()}
#' T Tests are reported like chi-squares, but only the degrees of freedom are
#'  in parentheses. Following that, report the t statistic (rounded to two decimal places)
#'   and the significance level.
#'  There was a significant effect for gender, t(54) = 5.43, p < .001, with men receiving higher scores than women.

#' @rdname rndr_
#' @export
#' @examples rndr_T(25.45, 45, .0045)
rndr_T<- function(F_val, df1 , p=NULL){
  F_val <-paste0("T", rndr_df(df1), "=", fftest(F_val))
  if(is.null(p)) F_val
  else paste0(F_val, symbol_seperator, rndr_P(p))
}


#' @description H-Wert\code{rndr_H()}
#' @rdname rndr_
#' @export
#' @examples rndr_H(25.45, 45, .0045)
rndr_H<- function(F_val, df1 , p=NULL){
  F_val <-paste0("H", rndr_df(df1), "=", fftest(F_val))
  if(is.null(p)) F_val
  else paste0(F_val, symbol_seperator, rndr_P(p))
}



#' @description W-Wert\code{rndr_W()}
#' @rdname rndr_
#' @export
rndr_W<- function(F_val, p=NULL){
  F_val <-paste0("W=", fftest(F_val))
  if(is.null(p)) F_val
  else paste0(F_val, symbol_seperator, rndr_P(p))
}

#' @description U-Wert\code{rndr_U()}
#' @rdname rndr_
#' @export
rndr_W<- function(F_val, p=NULL){
  F_val <-paste0("U=",fftest(F_val))
  if(is.null(p)) F_val
  else paste0(F_val, symbol_seperator, rndr_P(p))
}

rndr_shapiro<- function(F_val, p=NULL){
  F_val <-paste0("W=",fftest(F_val))
  if(is.null(p)) F_val
  else paste0(F_val, symbol_seperator, rndr_P(p))
}


#' @description Lm-Wert\code{rndr_ lm()}
#' @rdname rndr_
#' @export
rndr_lm<- function(F_val, df1, df2, p, r2, ar2){
  paste0("R2=",ffreta(r2), symbol_seperator,
         "ad.R2=",ffreta(ar2), symbol_seperator,
         rndr_F(F_val, df1, df2, p) )}


#capture.output(Hmisc::spearman2(pauli~g, data = rechentest))

#' @description Chi-Wert\code{rndr_X()}
#' @rdname rndr_
#' @export
rndr_X<-function(x, df1, df2=NULL, p=NULL){
  if(is.null(df2)) {
    if(!is.null(df1)) x <- paste0(symbol_chi2(), rndr_df(df1), "=" ,fftest(x))
    else x <- paste0(symbol_chi2(), "=",fftest(x))
  } else {
    x <- paste0(symbol_chi2(), rndr_df(df1),"=",fftest(x))
  }
  if(!is.null(p))  paste0(x, symbol_seperator, rndr_P(p))
  else x
}

#' @rdname rndr_
rndr_Chisq <- function(x, df, p) rndr_X(x, df, NULL, p )





#' @description Fischer-test\code{rndr_fischer()}
#' @rdname rndr_
#' @export
rndr_fischer<-function(or, p){
  paste0("OR=", fftest(or), symbol_seperator, rndr_P(p))

}

#' @rdname rndr_
#' @description CFA Confirmatorische Faktoranalyse
#'
#' Backhaus Multivariate Analysemethoden 11 AuflageSeite 383
#' GIF Goodness-of-Fit-Index >=.9
#' @examples  rndr_gfi_cfa(c(1,.9,.89))
rndr_gfi_cfa <- function(gfi) as.character(
  cut(gfi,
      c(-Inf, 0.89, Inf),
      c("nicht akzeptabel", "gut")))


#' @rdname rndr_
#' @description AGIF Adjusted-Goodness-of-Fit-Index
#' @examples rndr_agfi_cfa(c(1,.9,.89))
rndr_agfi_cfa <- function(agfi) as.character(
  cut(agfi,
      c(-Inf, 0.89, Inf),
      c("nicht akzeptabel", "gut")))

#' @rdname rndr_
#' @description SRMR
#' @examples rndr_rmsea_cfa(c(1,.9,.89))
rndr_rmsea_cfa <- function(srmr) as.character(
  cut(srmr,
      c(-Inf,  0.079, Inf),
      c("gut", "nicht akzeptabel")))

#' @rdname rndr_
#' @description  Moosbrugger, Kelava 2012 Testtheorie 2. Auflage Seite 339
#' CHISQ Chi-Quadrat/df 0 ,2, 3
#' @examples rndr_Chisq_cfa(c(0,2,3,2.01,3.4))
rndr_Chisq_cfa <- function(chsq, df=1) as.character(
  cut(chsq/df,
      c(-Inf, 2, 3, Inf),
      c("gut", "akzeptabel", "nicht akzeptabel"))
)

#' @rdname rndr_
#' @description RMSEA Root-Mean-Square-Error of Approximation 0, 0.050, 0.080
#' @examples rndr_rmsea_cfa(c(0, 0.050, 0.080, .051, .081) )
rndr_rmsea_cfa <- function(rmsea) as.character(
  cut(rmsea,
      c(-Inf, 0.050, 0.08, Inf),
      c("gut", "akzeptabel", "nicht akzeptabel")))

#' @rdname rndr_
#' @description CFI Comparative-Fit-Index .970-1.00, .950-.969
#' @examples rndr_cfi_cfa(c(.970,1.00, .950-.969,.8))
rndr_cfi_cfa <- function(nfi) as.character(
  cut(nfi,
      c(-Inf, .950, .970,  Inf),
      c("nicht akzeptabel","akzeptabel", "gut"),
      right=FALSE))

#' @rdname rndr_
#' @description NFI Normed-Fit-Index .950-1.00 , .900-.949
#' @examples rndr_nfi_cfa(c(.950, 1.00 , .900,  .949))

rndr_nfi_cfa <- function(nfi) as.character(
  cut(nfi,
      c( -Inf, .900,  0.950, Inf),
      c("nicht akzeptabel","akzeptabel", "gut"),
      right=FALSE))




symbol_chi2 <- function(output = stp25output:::which_output()) {
  if (output == "html")
    "&chi;<sup>2</sup>"
  else
    "X2"
}

symbol_kleiner_gleich <-
  function(output = stp25output:::which_output()) {
    if (output == "html")
      "&le;"
    else
      "=<"
  }
symbol_groesser_gleich <-
  function(output = stp25output:::which_output()) {
    if (output == "html")
      "&ge;"
    else
      "=>"
  }

symbol_alpha	<- function(output = stp25output:::which_output()){
  if (output == "html")
    "&alpha;"
  else
   "alpha"}
symbol_beta	<- function(output = stp25output:::which_output()) {
  if (output == "html")
    "&beta;"
  else
    "beta"
}
symbol_eta	<-
  function(output = stp25output:::which_output()) {
    if (output == "html")
      "&eta;"
    else
      "eta"
  }
symbol_kappa	<- function(output = stp25output:::which_output()) {
  if (output == "html")
    "&kappa;"
  else
    "kappa"
}

symbol_seperator <- ", "

