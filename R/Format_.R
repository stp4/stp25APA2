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
Format2.matrix <- function(x, digits,...){
  if(!is.matrix(x)) x <- matrix(x)

  if(length(digits)==1) apply(x, 2, Format2, digits=digits,...)
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
ffmean <-function (m, s = NULL,
                   digits = rep_len(countDigits(m), 2),
                   plusmin_sign=options()$stp25$apa.style$mittelwert$plusmin_sign,
                   sym=options()$stp25$apa.style$mittelwert$plusmin_str,
                   ...)
{

  input <- lengths(m)




 #   options()$stp25$apa.style$mittelwert$plusmin_str else ""
  if (!is.null(s)){
    if(!plusmin_sign)
      x <- paste0(Format2(m, digits[1],...), " (",
            Format2(s, digits[2], ...), ")")
    else x <- paste0(Format2(m, digits[1],...), " (", sym,
                     Format2(s, digits[2], ...), ")")
  }  else {x <- Format2(m, digits[1], ...)}

  ## Pruefe ob die Lange passt
  if(!identical(input, lengths(x))) {
    print(input)
    print( lengths(x) )
    stop("ffmean")
  }

  x
}

#' @rdname Format2
#' @export
ffmean_long <-function (m, s, mn, mx,
                   digits = countDigits( m ),
                   ...)
{
input <- lengths(m)
 x<- paste0(
    Format2(m, digits, ...), " (SD ",
    Format2(s, digits, ...), ", range ",
    Format2(mn, digits, ...), "-",
    Format2(mx, digits, ...), ")"
  )

  ## Pruefe ob die Lange passt
  if(!identical(input, lengths(x))) {
    print(input)
    print( lengths(x) )
    stop("ffmean_long")
  }
 x

}


#   ffmean_long(1,2,3,4)
#   ffmean (1,2 )
#   ffmedian_long(1,2,3,4)
#   ffmedian(1,2)
#' @rdname Format2
#' @export
ffmedian_long <-function (m, iq, mn, mx,
                        digits = countDigits(m),
                        ...)
{

  input <- lengths(m)

 x <-  paste0(
    Format2(m, digits, ...), " (IQR ",
    Format2(iq, digits, ...), ", range ",
    Format2(mn, digits, ...), "-",
    Format2(mx, digits, ...), ")"
  )

  ## Pruefe ob die Lange passt
  if(!identical(input, lengths(x))) {
    print(input)
    print( lengths(x) )
    stop("ffmedian_long")
  }
  x
}


#-------------------------------------------------------------------------------------
#' @rdname Format2
#' @export
ffmedian <-function (quant, digits=NA,
                     style= options()$stp25$apa.style$median,
                     # Optionen fuer median sind
                     # noch nicht fertig
                     # style ist quasi die einzige Option

                     #lead.zero = options()$stp25$apa.style$mittelwert$lead.zero,
                     sep=options()$stp25$apa.style$sep_element,
                     ...)
{
 #input <- length(quant[3])
 if(is.na(digits) | is.null(digits)) digits<- countDigits(quant[3])
 if(is.null(style)){
    paste0(Format2(quant[3], digits, ...),
           " (", Format2(quant[2], digits, ...),
           sep," ",
           Format2(quant[4], digits, ...),
           ")")}
 else{
    paste0(Format2(quant[3], digits, ...),
            " (IQR=", Format2(abs(quant[2]-quant[4]), digits, ...),
            ")")
 }
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







