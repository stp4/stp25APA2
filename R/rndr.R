#' Render
#'
#' Formatiere von Zahlen nach dem APA-Style ( American Psychological Association ).
#' 
#' see:
#' http://winvector.github.io/APAsig/APAsig.html
#'  http://my.ilstu.edu/~jhkahn/apastats.html
#'      https://web2.uconn.edu/writingcenter/pdf/Reporting_Statistics.pdf
#' @param x Obkekt oder vektor
#' @param digits Nachkommastellen
#' @param ... alles an Format2()
#' @return Character-String
#' @export
#' @keywords internal
#' @examples
#' rndr_(1.234, 3)
#' 
rndr_ <- function(...) Format2(...)


# Mean --------------------------------------------------------------------



#' @rdname rndr_
#' @param m Mittelwert 
#' @param s,iqr SD,IRQ  (ein Wert)
#' @export
rndr_median <- function(m, iqr, digits=NULL, ...){
  if(is.null(digits))  digits <- options()$stp25$apa.style$mittelwert$digits
  paste0(Format2(m, digits[1],...), " (IRQ ", Format2(iqr, digits[1],...), ")")
  }


#' @rdname rndr_
#' @export
rndr_median_quant<- function(x, digits=NULL, ...){
  if(is.null(digits))  digits <- options()$stp25$apa.style$mittelwert$digits
  paste0(
    Format2(x[3], digits[1], ...),
    " (",
    Format2(x[2], digits[1], ...),
    symbol_seperator,
    Format2(x[4], digits[1], ...),
    ")"
  )
}

#' @rdname rndr_
#' @export
# noch nicht umgesetzt (Tabelle(..., APA=TRUE))
rndr_median_range <- function (m, iqr, mn, mx,
                               digits = NULL, ...) {
  if(is.null(digits))  digits <- options()$stp25$apa.style$mittelwert$digits
  paste0(
    Format2(m, digits[1],...), " (IRQ ", Format2(iqr, digits[1],...), 
    ", range ",
    Format2(mn, digits[1],...),
    " to ",
    Format2(mx, digits[1],...),
    ")"
  )
}

 
# ffmedian_long <-function (m, iq, mn, mx,
#                           digits = countDigits(m),
#                           ...)
# {
#   
#  
#   
#   paste0(
#     Format2(m, digits[1], ...), " (IQR ",
#     Format2(iq, digits[1], ...), ", range ",
#     Format2(mn, digits[1], ...), "-",
#     Format2(mx, digits[1], ...), ")"
#   )
#  
# }
# 
# 
#  
# ffmedian <-function (quant, digits=NA,
#                      style= options()$stp25$apa.style$median,
#                      sep=options()$stp25$apa.style$sep_element,
#                      ...)
# {
#   #input <- length(quant[3])
#   if(is.na(digits) | is.null(digits)) digits<- countDigits(quant[3])
#   if(is.null(style)){
#     paste0(Format2(quant[3], digits, ...),
#            " (", Format2(quant[2], digits, ...),
#            sep," ",
#            Format2(quant[4], digits, ...),
#            ")")}
#   else{
#     paste0(Format2(quant[3], digits, ...),
#            " (IQR=", Format2(abs(quant[2]-quant[4]), digits, ...),
#            ")")
#   }
# }
# 
# 






#' @rdname rndr_
#' @export
#' @examples 
#'  rndr_mean_range(1,2,3,4)
#'   rndr_mean (1,2 )
#'   rndr_median_range(1,2,3,4)
#'   rndr_median(1,2)
rndr_mean <- function(m, s, digits=NULL, ...) {
  if(is.null(digits))  digits <- options()$stp25$apa.style$mittelwert$digits
  paste0(Format2(m, digits[1], ...), " (", Format2(s, digits[1], ...), ")")

}
#' @rdname rndr_
#' @export
rndr_mean_range <- function(m, s, mn, mx, digits=NULL, ...) {
  if(is.null(digits))  digits <- options()$stp25$apa.style$mittelwert$digits 
   paste0(
    Format2(m, digits[1], ...), " (SD ",
    Format2(s, digits[1], ...), ", range ",
    Format2(mn, digits[1], ...), " to ", Format2(mx, digits[1], ...), ")"
  )
  
}
#  
# ffmean <-function (m, s = NULL,
#                    digits = rep_len(countDigits(m), 2),
#                    plusmin_sign=options()$stp25$apa.style$mittelwert$plusmin_sign,
#                    sym=options()$stp25$apa.style$mittelwert$plusmin_str,
#                    ...)
# {
#   if (!is.null(s)){
#     if(!plusmin_sign)
#       x <- paste0(Format2(m, digits[1],...), " (",
#                   Format2(s, digits[2], ...), ")")
#     else x <- paste0(Format2(m, digits[1],...), " (", sym,
#                      Format2(s, digits[2], ...), ")")
#   }  else {x <- Format2(m, digits[1], ...)}
#   x
# }
# 
#  
# ffmean_long <-function (m, s, mn, mx,
#                         digits = countDigits( m ),
#                         ...)
# {
#  
#   paste0(
#     Format2(m, digits, ...), " (SD ",
#     Format2(s, digits, ...), ", range ",
#     Format2(mn, digits, ...), "-",
#     Format2(mx, digits, ...), ")"
#   )
#   
#  
#   
# }
# 
# 
#  









#' @rdname rndr_
#' @export
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
#' @param n Anzahl
#' @param percent,style Formatierung als Prozent oder als Prozent(Anzahl)
#' @param percentage_str,null_percent_sign Formatierungs Optionen
#' @export
#' @description Prozent
#' @examples
#' rndr_percent(c(.2568, 99, 0.02568), c(4, 569, 25), digits = 1)
#' rndr_percent(10, 3, F, 2)

rndr_percent <- function(
  x,
  n=NULL,
  percent = TRUE, # nur die Anzahl zurueckgeben (xtabs)
  digits = options()$stp25$apa.style$prozent$digits[1],
  percentage_str = options()$stp25$apa.style$prozent$percentage_str,
  style = options()$stp25$apa.style$prozent$style,
  null_percent_sign = options()$stp25$apa.style$prozent$null_percent_sign
) {
  
#cat("\nrndr_percent\n")
  
 if(is.null(percent)) percent <- style != 0

  if (is.vector(x)) {
   # cat(" vector ")
    if (percent) {
      
        
      prz <- ifelse(
        x < 1 / (10 ^ digits),
        paste0("<", 1 / (10 ^ digits), "%"),
        paste0( formatC(x,
            format = "f",  digits = digits,
            decimal.mark = getOption("OutDec")),percentage_str))
      if(!is.null(n)){
      anz <- formatC(n, format = "f", digits =  0)
      
      if (style == 1)
        res <- paste0(prz, " (", anz, ")")
      else
        res <- paste0(anz, " (", prz, ")")
      
     
       
      } else { # in Kano verwendet
        null_percent_sign<- NULL #fehler abangen
        res <-  prz
      }
      
      
    }

    else{
      res <- formatC(n, format = "f", digits =  0)
    }

     if(!is.null(null_percent_sign))
      res[which(n==0)] <- null_percent_sign
#print(res)
    return(res)
    
  } else{
    cat("not a vector ")
    myattr <- attributes(n) #-- colnames and rownames
    nrw <- nrow(n)
    #n <- suppressWarnings(
    #  formatC(n, format = "f", digits = 0))
   # cat("x : ")
  #  print(str(n) )
    n_char <- apply(n, 2, function(x) {
     # cat("\nin fun: \n")
    #  print(x)
      formatC(x, format = "f", digits = 0) 
      })
   # cat(" nach formatC ")
    #------------------------------------------------
    if (percent) {
     # cat(" percent ")
      x_char <- apply(x, 2, function(y) formatC(
           y,
           format = "f",
           digits = digits,
           decimal.mark = getOption("OutDec")
         ))
      
      if (style == 1)
         res <- matrix(paste0(x_char, "% (", n_char, ")"), nrow = nrw)
       
      else
        res <- matrix(paste0(n_char, " (", x_char, "%)"), nrow = nrw)

    } else
      res <-  n_char

    
    #print(res)
    
    res<- data.frame(res, row.names= myattr$row.names,
                     stringsAsFactors = FALSE)
  #print(res)
    names(res) <- myattr$names
  
    
    
    if(!is.null(null_percent_sign))
      res[which(n==0)] <- null_percent_sign
    
    return(res)
  }
}



#' @rdname rndr_
#' @param p p-Wert
#' @param include.symbol,include.bracket Formatierungs Optionen
#' @export
#' @examples
#'
#'rndr_P(c(1,.25,.08,0.05,0.01,0.0001))
rndr_P<-function(p, include.symbol=TRUE, include.bracket=FALSE) {

  if(include.symbol & !include.bracket){
    pp_val<-  ifelse(p<.001, "p", "p=")
    paste0(pp_val, ffpvalue(p))
    }
  else if(include.bracket) {
    pp_val<-  ifelse(p<.001, "p", "p=")
    paste0(" (",  paste0(pp_val, ffpvalue(p)),")")
    }
  else {ffpvalue(p)}
  }



#' @rdname rndr_
#' @export
rndr_Stars<-function(p) ffsigstars(p)




#' @rdname rndr_
#' @description rndr_corr
#' Correlations are reported with the degrees of freedom (which is N-2)
#' in parentheses and the significance level:
#' The two variables were strongly correlated, r(55) = .49, p < .01.
#' @export
#' @examples
#' rndr_corr(-.548369,0.005896,55)
rndr_corr <- function(x, p, df){
  paste0("r", rndr_df(df), "=", ffreta(x),", ", rndr_P(p))
}

#' @rdname rndr_
#' @export
rndr_r <- function(x, include.symbol=TRUE) {
  if(include.symbol)  paste0("r=", ffreta(p_val))
  else ffreta(x)
}


#' @rdname rndr_
#' @param df,df1,df2 Freiheitsgrade
#' @export
rndr_df<- function(df1, df2=NULL) {
  if(is.null(df2)) paste0("<sub>(", Format2(df1, 0), ")</sub>")
  else  paste0("<sub>(", Format2(df1, 0), ", ", Format2(df2, 0), ")</sub>")

}
 
#' @rdname rndr_
#' @param ci Vektor mit zwei Werten
#' @param sep,sep_1,sep_2 intern seperator 
#' @export
#' @examples 
#'   rndr_CI(matrix(c(NA, 1:10, NA), ncol=2))
rndr_CI <- function(ci, digits = 2,
                        #lead.zero = options()$stp25$apa.style$mittelwert$lead.zero,
                      sep=options()$stp25$apa.style$sep_element,
                      sep_1=options()$stp25$apa.style$brackets[1],
                      sep_2=options()$stp25$apa.style$brackets[2] ){

#Format2.matrix kann verschiedene digits aufloesen
res<- paste0(sep_1,
              Format2.matrix(ci[,1], digits),
              sep, " ",
              Format2.matrix(ci[,2], digits),
              sep_2)

res[which(is.na(ci[,1]))]<-NA
res
}

#' @rdname rndr_
#' @export
rndr_mean_CI <- function(m, ci, digits) {
  #  print(c(m, s, digits))
  paste(Format2(m, digits), rndr_CI(ci, digits ))
  
}


#' @rdname rndr_
#' @description F-Wert \code{rndr_F()}
#' @param F_val Objekt aus einem Test
#' @export
rndr_F<-function(F_val, df1, df2, p=NULL){
  F_val<- paste0("F", rndr_df(df1,df2), "=", fftest(F_val))
  if(is.null(p)) F_val
  else paste0(F_val, symbol_seperator, rndr_P(p))
}


#' @rdname rndr_
#' @description T-Wert \code{rndr_T()}
#' T Tests are reported like chi-squares, but only the degrees of freedom are
#'  in parentheses. Following that, report the t statistic (rounded to two decimal places)
#'   and the significance level.
#'  There was a significant effect for gender, t(54) = 5.43, p < .001, with men receiving higher scores than women.
#' @export
#' @examples 
#' rndr_T(25.45, 45, .0045)
rndr_T<- function(F_val, df1, p=NULL){
  F_val <-paste0("T", rndr_df(df1), "=", fftest(F_val))
  if(is.null(p)) F_val
  else paste0(F_val, symbol_seperator, rndr_P(p))
}



#' @rdname rndr_
#' @export
#' @examples 
#' rndr_H(25.45, 45, .0045)
rndr_H<- function(F_val, df1, p=NULL){
  F_val <-paste0("H", rndr_df(df1), "=", fftest(F_val))
  if(is.null(p)) F_val
  else paste0(F_val, symbol_seperator, rndr_P(p))
}

#' @rdname rndr_
rndr_BP<- function(F_val, df1, p=NULL){
  F_val <-paste0("BP", rndr_df(df1), "=", fftest(F_val))
  if(is.null(p)) F_val
  else paste0(F_val, symbol_seperator, rndr_P(p))
}


#' @rdname rndr_
rndr_DW<- function(F_val, df1, p=NULL){
  F_val <-paste0("DW", rndr_df(df1), "=", fftest(F_val))
  if(is.null(p)) F_val
  else paste0(F_val, symbol_seperator, rndr_P(p))
}



#' @rdname rndr_
#' @export
rndr_W<- function(F_val, p=NULL){
  F_val <-paste0("W=", fftest(F_val))
  if(is.null(p)) F_val
  else paste0(F_val, symbol_seperator, rndr_P(p))
}



#' @rdname rndr_
#' @export
rndr_U<- function(F_val, p=NULL){
  F_val <-paste0("U=",fftest(F_val))
  if(is.null(p)) F_val
  else paste0(F_val, symbol_seperator, rndr_P(p))
}


#' @rdname rndr_
#' @export
rndr_shapiro<- function(F_val, p=NULL){
  F_val <-paste0("W=",fftest(F_val))
  if(is.null(p)) F_val
  else paste0(F_val, symbol_seperator, rndr_P(p))
}


 
#' @rdname rndr_
#' @export
rndr_lm<- function(F_val, df1, df2, p, r2, ar2){
  paste0("R2=",ffreta(r2), symbol_seperator,
         "ad.R2=",ffreta(ar2), symbol_seperator,
         rndr_F(F_val, df1, df2, p) )}

 



#' @rdname rndr_
#' @export
#' @examples 
#' #capture.output(Hmisc::spearman2(pauli~g, data = rechentest))
rndr_X<-function(x, df1, df2=NULL, p=NULL){
  if(is.null(df2)) {
    if(!is.null(df1)) x <- paste0(symbol_chi2(), rndr_df(df1), "=",fftest(x))
    else x <- paste0(symbol_chi2(), "=",fftest(x))
  } else {
    x <- paste0(symbol_chi2(), rndr_df(df1),"=",fftest(x))
  }
  if(!is.null(p))  paste0(x, symbol_seperator, rndr_P(p))
  else x
}


#' @rdname rndr_
#' @export
rndr_Chisq <- function(x, df, p) rndr_X(x, df, NULL, p )


#' @rdname rndr_
#' @export
rndr_Chisq_stars <- function(x, p) {
 # in Kano Benutzt
  paste0(fftest(x) , ffsigstars(p))
}


 
#' @rdname rndr_
#' @export
rndr_fischer<-function(x, p){
  paste0("OR=", fftest(x), symbol_seperator, rndr_P(p))

}

#' @rdname rndr_
#' @description CFA Confirmatorische Faktoranalyse
#'
#' Backhaus Multivariate Analysemethoden 11 AuflageSeite 383
#' GIF Goodness-of-Fit-Index >=.9
#' @examples  
#' rndr_gfi_cfa(c(1,.9,.89))
rndr_gfi_cfa <- function(x) as.character(
  cut(x,
      c(-Inf, 0.89, Inf),
      c("nicht akzeptabel", "gut")))


#' @rdname rndr_
#' @description AGIF Adjusted-Goodness-of-Fit-Index
#' @examples 
#' rndr_agfi_cfa(c(1,.9,.89))
rndr_agfi_cfa <- function(x) as.character(
  cut(x,
      c(-Inf, 0.89, Inf),
      c("nicht akzeptabel", "gut")))

#' @rdname rndr_
#' @description SRMR
#' @examples 
#' rndr_rmsea_cfa(c(1,.9,.89))
rndr_rmsea_cfa <- function(x) as.character(
  cut(x,
      c(-Inf,  0.079, Inf),
      c("gut", "nicht akzeptabel")))

#' @rdname rndr_
#' @description  Chisq_cfa:  Moosbrugger, Kelava 2012 Testtheorie 2. Auflage Seite 339
#' CHISQ Chi-Quadrat/df 0,2, 3
#' @examples 
#' rndr_Chisq_cfa(c(0,2,3,2.01,3.4))
rndr_Chisq_cfa <- function(x, df=1) as.character(
  cut(x/df,
      c(-Inf, 2, 3, Inf),
      c("gut", "akzeptabel", "nicht akzeptabel"))
)

#' @rdname rndr_
#' @description RMSEA Root-Mean-Square-Error of Approximation 0, 0.050, 0.080
#' @examples 
#' rndr_rmsea_cfa(c(0, 0.050, 0.080, .051, .081) )
rndr_rmsea_cfa <- function(x) as.character(
  cut(x,
      c(-Inf, 0.050, 0.08, Inf),
      c("gut", "akzeptabel", "nicht akzeptabel")))

#' @rdname rndr_
#' @description CFI Comparative-Fit-Index .970-1.00, .950-.969
#' @examples 
#' rndr_cfi_cfa(c(.970,1.00, .950-.969,.8))
rndr_cfi_cfa <- function(x) as.character(
  cut(x,
      c(-Inf, .950, .970,  Inf),
      c("nicht akzeptabel","akzeptabel", "gut"),
      right=FALSE))

#' @rdname rndr_
#' @description NFI Normed-Fit-Index .950-1.00, .900-.949
#' @examples 
#' rndr_nfi_cfa(c(.950, 1.00, .900,  .949))

rndr_nfi_cfa <- function(x) as.character(
  cut(x,
      c( -Inf, .900,  0.950, Inf),
      c("nicht akzeptabel","akzeptabel", "gut"),
      right=FALSE))


#' @rdname rndr_
#' @param output  nur intern HTML oder Konsole
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






#' @name Format2
#' @rdname Format2
#' @title Format2
#' @description Formatiere Zahlen zu Character f?r den Output \link{format}
#' @param x  vektor liste oder matrix
#' @param digits  bei data.frame auch mehere Werte
#' @param lead.zero = TRUE
#' @param OutDec = getOption("OutDec")
#' @param type = "digits"oder"signif
#' @param scientific = FALSE
#' @param pattern_pval nicht aendern P-Werte Namen
#' @param pattern_est  nicht aendern Parameter Namen
#' @param pattern_df nicht aendern DF Namen
#' @param apa.style  nicht aendern verwendeter Style als liste
#' @param nsmall  nicht aendern werden digite
#' @param  col_names  fuer fix_colnames col_names=NULL
#' @param  translate fuer fix_colnames(x, translate= TRUE)
#' @param ... alles an format
#' @return Objekt als Character
#' @export
#' @examples
#' ##library(stp5)
#' # Projekt(OutDec = ",")
#' # options()
#' x<- rnorm(10)
#' df <- data.frame(Item=c("a", "b"),
#'                  x=x[1:2],
#'                  x2=c(1.2,2.3),
#'                  beta=  c(.22,.13),
#'                  x3=c(.42,.03),
#'                  p.value=c(0.02456,0.0398))
#'
#' mx<- as.matrix(df[,-1])
#' #Projekt("html", OutDec = ".")
#' #Output(Format2(df, digits=c(1,2,2,2,2,3)))
#' #Format2(mx, digits=c(4,2,2,2,2,3))
#' Format2(x, digits=2, FALSE)
#' Format2(x, digits=3)
#' x<- list(a=1:5, b=rnorm(10))
#' length(x)
#' Format2(x, 2, FALSE)
#'
#' #End()
Format2 <- function(x, ...) {
  UseMethod("Format2")
}


#' @rdname Format2
#' @export
#' @description Format2.matrix kann über digits
#' verschiedene nachkommastellen runden
#' @examples
#' x<-matrix(rnorm(10), ncol=2)
#' Format2.matrix(x[,1],c(1:5))
Format2.matrix <- function(x, digits, ...){
  if(!is.matrix(x)) x <- matrix(x)
  
  if(length(digits)==1) apply(x, 2, Format2, digits=digits, ...)
  else matrix(mapply(Format2, x, digits,...), ncol=ncol(x))
}

#  Format2.data.frame(x, ...)
#' @rdname Format2
#' @description Format2.data.frame
#' @export
#' @examples
#' #  str(Format2(data.frame(a1=1:10/100,a2=1:10, a3=1:10)))
Format2.data.frame <- function(x,
                               digits = NULL,
                               lead.zero = TRUE,
                               type = "digits",
                               scientific = FALSE,
                               ...) {
  input <- lengths(x)
  
  nc <- length(x)
  if (!nc)
    return(x)
  for (i in 1:ncol(x)) {
    if (length(digits) > 1)
      mydigits <- digits[i]
    else
      mydigits <- digits
    
    if (length(type) > 1)
      mytype <- type[i]
    else
      mytype <- type
    
    if (length(lead.zero) > 1)
      mylead.zero <- lead.zero[i]
    else
      mylead.zero <- lead.zero
    
    if (length(scientific) > 1)
      myscientific <- scientific[i]
    else
      myscientific <- scientific
    
    x[, i] <- Format2(
      x[, i],
      digits = mydigits,
      lead.zero = mylead.zero,
      type = mytype,
      scientific = myscientific,
      ...
    )
  }
  ## Pruefe ob die Lange passt
  if(!identical(input, lengths(x))) {
    print(input)
    print( lengths(x) )
    stop("Format2.data.frame")
  }
  x
}
#' @rdname Format2
#' @export
Format2.list <- function(x,
                         digits = NULL,
                         lead.zero = TRUE,
                         type = "digits",
                         scientific = FALSE,
                         ...) {
  input <- lengths(x)
  
  nc <- length(x)
  if (!nc)
    return(x)
  for (i in 1:nc) {
    if (length(digits) > 1)
      mydigits <- digits[i]
    else
      mydigits <- digits
    
    if (length(type) > 1)
      mytype <- type[i]
    else
      mytype <- type
    
    if (length(lead.zero) > 1)
      mylead.zero <- lead.zero[i]
    else
      mylead.zero <- lead.zero
    
    if (length(scientific) > 1)
      myscientific <- scientific[i]
    else
      myscientific <- scientific
    x[[i]] <- Format2(
      x[[i]],
      digits = mydigits,
      lead.zero = mylead.zero,
      type = mytype,
      scientific = myscientific
      ,
      ...
    )
  }
  
  
  ## Pruefe ob die Lange passt
  if(!identical(input, lengths(x))) {
    print(input)
    print( lengths(x) )
    stop("Format2.list")
  }
  x
}
#' @rdname Format2
#' @export
Format2.default <- function(x,
                            digits = NULL,
                            lead.zero = TRUE,
                            OutDec = getOption("OutDec"),
                            type = "digits",  #"signif"
                            
                            scientific = FALSE,
                            nsmall =  ifelse(is.null(digits), 0L,  digits),
                            #-- wenn erster wert 0 dann trotzdem digits
                            ...)
{
  #cat("\n in Format2.default\n")
  #  print(class(x))
  #print(x)
  #print(digits)
  #cat("\n--------\n")
  input <- lengths(x)
  
  
  format_number_to_char<- function(x){
    r <- format(x, format = "f", nsmall = nsmall,
                scientific = scientific, ...)
    # cat("\nin format_number_to_char")
    # print(r)
    # cat("\n--------\n")
    r[stringr::str_detect(r,"NA")]<-""
    if (!lead.zero) return(replace_lead_zero(r))
    else  return(r)
  }
  
  nc <- length(x)
  if (!nc)
    return(x)
  if (is.character(x))
    return(x)
  if (is.factor(x))
    return(as.character(x))
  
  if (!is.null(digits)){
    if (type == "digits")
      x <- format_number_to_char(round(x, digits = digits))
    else
      x <- sapply(signif(x, digits = digits), format_number_to_char)
  }else{
    if (type == "digits")
      x <- format_number_to_char(x)
    else
      x <- sapply(x, format_number_to_char)
  }
  
  
  ## Pruefe ob die Lange passt
  if(!identical(input, lengths(x))) {
    print(input)
    print( lengths(x) )
    stop("Format2.default")
  }
  
  x
  
}


replace_lead_zero<- function(r){
  r <- gsub("0\\.", "\\.", r)
  gsub("0,", ",", r)
  
}


# x<- list(a=1:5, b=rnorm(10))
# length(x)
# Format2(x,2, F)


# format.pval(c(0.10, 0.0001, 1e-27), eps = .001, digits = 3)


#' @rdname Format2
#' @export
ffpvalue <- function (x,
                      digits =    options()$stp25$apa.style$p$digits,
                      lead.zero = options()$stp25$apa.style$p$lead.zero,
                      with.stars = options()$stp25$apa.style$p$with.stars,
                      ##default FALSE
                      pval = x,
                      # OutDec = getOption("OutDec"),
                      lim_sig = ifelse(options()$prompt[1] == "HTML> ", "&lt;", "<"),
                      ...) {
  
  input <- lengths(x)
  
  format_000 <- function(x) ifelse(x == ".000",
                                   paste0(lim_sig, ".",
                                          paste0(rep(0, digits - 1),
                                                 collapse = ""), "1"),x)
  
  x <- Format2(x, digits, lead.zero)
  
  if(is.vector(x)) x <- format_000(x)
  else if( is.matrix(x)) {
    xnames <- dimnames(x)
    x <- apply(x, 2, format_000)
    dimnames(x) <- xnames
  }
  else if( is.data.frame(x)) {
    xnames <- names(x)
    x<- data.frame(lapply(x, format_000))
    names(x)<- xnames
  }
  
  
  if (with.stars) {
    paste0(x, ffsigstars(pval))
  }
  
  ## Pruefe ob die Lange passt
  if(!identical(input, lengths(x))) {
    print(input)
    print( lengths(x) )
    stop("ffpvalue")
  }
  
  x
}


#' @rdname Format2
#' @export
ffsigstars <- function (x,
                        stars.value =   options()$stp25$apa.style$p$stars.value,
                        stars.symbols = options()$stp25$apa.style$p$stars.symbols)
{
  
  # ffsigstars(c(1,.1,.001))
  input <- lengths(x)
  
  p_sternchen<-function(x)  {
    stern<-as.character(cut(round(x, 3),
                            c(-Inf, stars.value, Inf),
                            c(stars.symbols, "")))
    stern[is.na(stern)] <- ""
    stern
  }
  
  if( is.vector(x)){
    xnames<-names(x)
    x <- p_sternchen(x)
    names(x)<- xnames
  }
  else if(is.data.frame(x)) {
    xnames<-names(x)
    x <- data.frame(lapply(x, p_sternchen))
    names(x)<- xnames
  }
  else if(is.matrix(x)) {
    
    xnames <- dimnames(x)
    x <- apply(x, 2, p_sternchen)
    dimnames(x) <- xnames
    
  }
  ## Pruefe ob die Lange passt
  #
  if(!identical(input, lengths(x))) {
    
    print(input)
    print( lengths(x) )
    stop("ffsigstars")
  }
  x
}

# x<-c(.01,.0002,.03,.04,.06)
# ffsigstars(x)
# ffsigstars(data.frame(x=x, x2=x, x3=x, x4=x))
# ffsigstars(as.matrix(data.frame(x=x, x2=x, x3=x, x4=x)))


#-------------------------------------------------------------------------------------

# Prozent -----------------------------------------------------------------



#' @rdname Format2
#' @export
ffprozent <-
  function(prz, frq = NULL, ...){ UseMethod("ffprozent")}

ffprozent.ftable<-  function(...) rndr_percent(...) ## APA_Xtabs
ffprozent.table<-  function(...) rndr_percent(...) ## APA_Xtabs
# @rdname Format2
# @export
# rndr_percent<- function(prozent, anzahl,
#                         percent = FALSE,
#                       #  digits=c(0, 0),
#                         digits = options()$stp25$apa.style$prozent$digits,
#                         #lead.zero = options()$stp25$apa.style$prozent$lead.zero,
#                         style = options()$stp25$apa.style$prozent$style,
#
#
#                         null_percent_sign = options()$stp25$apa.style$prozent$null_percent_sign,
#                         ...) {
# ##cat("\n", class(prozent), ", ",class(anzahl), "\n")
#   # in xtabs genutzt
#   myattr <- attributes(anzahl) #-- colnames and rownames
#   nrw <- nrow(anzahl)
#   anzahl <- suppressWarnings(
#     formatC(anzahl, format = "f", digits =  digits[2]))
#   #------------------------------------------------
#   if (percent | style>0) {
#     prozent <- suppressWarnings(
#       formatC(prozent, format = "f", digits = digits[1],
#               decimal.mark = getOption("OutDec")))
#    if(style==1)
#     res <- matrix(paste0(prozent, "% (", anzahl, ")"), nrow = nrw)
#     else res <- matrix(paste0(anzahl, " (",prozent, "%)"), nrow = nrw)
#
#   } else
#     res <-  anzahl
#   attributes(res) <- myattr
#
#   return(res)
#
# }
#


#' @rdname Format2
#' @export
ffprozent.default <- function (prz,
                               frq = NULL,
                               digits = options()$stp25$apa.style$prozent$digits,
                               lead.zero = options()$stp25$apa.style$prozent$lead.zero,
                               style = options()$stp25$apa.style$prozent$style,
                               null_percent_sign = options()$stp25$apa.style$prozent$null_percent_sign,
                               ...)
{
  strg <- NULL
  if (is.null(frq) & !is.null(prz)) {
    # "prozent"
    strg <- paste0(
      Format2(prz, digits[1], lead.zero[1], type = "digits"), "%")
  }else if (!is.null(frq) & !is.null(prz)) {
    if (style == 1){
      #"prozent_anzahl"
      strg <- paste0(
        Format2(prz, digits[1], lead.zero[1], type = "digits"),"% (",
        Format2(frq, digits[2], lead.zero[2], type = "digits"),")")
    } else {
      #"anzahl_prozent"
      strg <-  paste0(
        Format2(frq, digits[2], lead.zero[2], type = "digits")," (",
        Format2(prz, digits[1], lead.zero[1], type = "digits"),"%)")
    }
  } else{
    #"null"
    strg <- null_percent_sign
  }
  
  if(!is.null(null_percent_sign))
    strg[ prz<=0 ] <- null_percent_sign
  
  if(is.matrix(prz))
    strg <- matrix(strg,
                   nrow = nrow(prz), ncol =  ncol(prz),
                   dimnames=dimnames(prz))
  return(strg)
}


#' @rdname Format2
#' @export
ffprozent.data.frame<- function(prz, frq=NULL, ...)
  ffprozent.default(as.matrix(prz),
                    as.matrix(frq), ... )
#' @rdname Format2
#' @export
ffprozent.matrix<- function (prz, frq =NULL, ...){
  if( !is.null(frq) & !is.matrix(frq))
    frq <- as.matrix(frq) #-- Fehler abfangen
  
  ffprozent.default(prz,
                    frq, ... )
}





#-------------------------------------------------------------------------------------

#' @rdname Format2
#' @export
ffCI <-function (CIs, digits=2,# = options()$stp25$apa.style$mittelwert$digits,
                 lead.zero = options()$stp25$apa.style$mittelwert$lead.zero,
                 sep=options()$stp25$apa.style$sep_element,
                 sep_1=options()$stp25$apa.style$brackets[1],
                 sep_2=options()$stp25$apa.style$brackets[2], ...)
{
  input <- lengths(CIs)
  #cat("\n in ffCI\n")
  x <- paste0(sep_1,
              Format2(CIs[1], digits, lead.zero[1]),
              sep, " ",
              Format2(CIs[2], digits, lead.zero[1]),
              sep_2)
  
  
  # if(!identical(input, lengths(x))) {
  #   print(x)
  #   print(input)
  #   print( lengths(x) )
  #
  #   warning("ffCI noch nicht getestet")
  # }
  x
}

#-------------------------------------------------------------------------------------
#' @rdname Format2
#' @export
ffreta <- function(x
                   ,digits = options()$stp25$apa.style$r$digits
                   ,lead.zero = options()$stp25$apa.style$r$lead.zero){
  Format2(x, digits, lead.zero, type= "digits")
}
#------------------------------------------------------------------------------------
#' @rdname Format2
#' @export
fftest<- function (x, digits = options()$stp25$apa.style$Fstat$digits,
                   lead.zero = options()$stp25$apa.style$Fstat$lead.zero)
{
  Format2(x, digits[1], lead.zero[1], type= "digits")
}




#  rdname Format2
# #  export
# Format_P<-function(...) rndr_P(...)
#
# #  rdname Format2
# #  export
# #
# Format_F<-function(...) rndr_F(...)
# #  rdname Format2
# #  export
# Format_X<-function(...) rndr_X(...)
#
#
# #  rdname Format2
# #  export
# Format_T<- function(...)rndr_T(...)
# #capture.output(Hmisc::spearman2(pauli~g, data = rechentest))
#







#' @rdname Format2
#' @description countDigits Interne Function wird in Meanci2() verwendet
#' @export
#' @examples
#' countDigits(1.2345)
countDigits <- function(x) {
  x<- signif(x, 3)
  x <- strsplit(as.character(x),"\\.")[[1]][2]
  if (is.na(x))
    0
  else
    nchar(x)
}


# description Interne Function wird in Meanci2() verwendet
#  countDigits(1.2345)
# countDigits <- function(x) {
#   x <- signif(x, 3)
#   x <- strsplit(as.character(x),"\\.")[[1]][2]
#   if (is.na(x)) 0 else nchar(x)
# }





# --- noch nicht benutzte Funktionen ----------------------

# adapted from John Fox's numbers2words function

make.digits <- function(x) {
  # This is a function breaks an input number x into the positive (left)
  # and negative(right) elements and returns these as numbers
  x <- toString(x)
  negative <- substr(x,1,1)=="-"
  if (negative) x <- substring(x,2)
  
  if (length(grep('.',x, fixed=TRUE))==0) {
    left <- x %>% strsplit("") %>% unlist
    right <- NULL
  }
  else {
    y <- x %>% strsplit(".", fixed=TRUE)
    left <- y[[1]][1] %>% strsplit("") %>% unlist
    right <- y[[1]][2] %>% strsplit("") %>% unlist
  }
  list(left,right, negative)
}



# Insert commas where needed in large numbers
make.proper <- function(x, sep=",") {
  if (is.numeric(x)) x <- format(x, scientific=FALSE)
  digits <- make.digits(x)
  outlength <- ceiling(length(digits[[1]])/3)-1+length(digits[[1]])
  right <- digits[[2]]
  left <- rep("", outlength)
  left[outlength:1 %% 4==0] <- sep
  left[outlength:1 %% 4!=0] <- digits[[1]]
  if (length(right>0)) paste(c(left, ".", right), collapse="")
  else  paste(left, collapse="")
}