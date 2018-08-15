#' @rdname Berechne
#' @title Berechne Mittelwerte
#' @name Berechne
#' @description Die Lagemasse werden ueber die Standard-Funktionen berechnet unterschied ist nur dass
#' Faktoren zu Zahlen transformiert werden und das \code{na.rm=TRUE} gesetzt wird.
#' CI = Hmisc::smean.cl.normal
#' @param ... weitere Objekte
#' @return  ein dataframe Objekt oder ein Character-String
#' @export

berechne<- function(...){UseMethod("berechne")}

#' @rdname Berechne
#' @param fm formel
#' @param x Namen der  measure.vars
#' @param type mean, median
#' @param measure.name,measure,by Variablen name des Ergebnisses. 
#' In Tabelle() measure.name  ="value"
#' @param digits Nachkommastellen
#' @param fun Function an plyr::ddply
#' @export
#' 
#' @examples 
#' 
#' #  Tabelle( hyper, chol0,chol1,chol6,chol12,by=~ g)
#' 
#' res<-stp25APA2:::berechne.default(hyper, 
#'                                   Cs(chol0,chol1,chol6,chol12), 
#'                                   by=~med+g, 
#'                                   measure="mean")
#'names(res)
#'  # res
#'  
berechne.default <- function(data,
                             x,
                             by = "1",
                             measure ,
                             type ,
                             fun = function(x) length( na.omit(x)),
                             fm = NULL,
                             digits = NULL,
                             measure.name = NULL 
                             
                             ) {

#print(measure.name)
  mdn <- function() {
    aggregate(
      fm,
      data,
      FUN = function(x) {
        if (type == "auto_long")
          rndr_median_range(
            median(x, na.rm = TRUE),
            IQR(x, na.rm = TRUE),
            min(x, na.rm = TRUE),
            max(x, na.rm = TRUE),
            digits = digits
          )
        else
          rndr_median_quant(quantile(x), digits = digits)
        
      }
    )
  }
  
  mn <- function() {
    aggregate(
      fm,
      data,
      FUN = function(x) {
        if (type == "auto_long")
          rndr_mean_range(
            mean(x, na.rm = TRUE),
            sd(x, na.rm = TRUE),
            min(x, na.rm = TRUE),
            max(x, na.rm = TRUE),
            digits = digits
          )
        else
          rndr_mean(mean(x, na.rm = TRUE),
                    sd(x, na.rm = TRUE), digits = digits)
        
      }
    )
  }
  
  frq <- function() {
    aggregate(
      fm,
      data ,
      FUN = function(x) {
        r <- table(x)
        paste(r, collapse = "/")
      }
    )
  }
  
  custom_fun <- function() {
   res <-  aggregate(fm, data, FUN = fun, simplify = TRUE)
    #fun mit meheren rueckgabewerten
    if (is.matrix(res[[ncol(res)]])) {
      measure.name <<- NULL
      cbind(res[-ncol(res)],  res[[ncol(res)]])
    } else
      res
   }
  
 
  
  
  if (is.null(fm)){
    fm <- makeFormula(x, by)
    }
 
    
    res <- switch (
      measure,
     
      factor = frq() ,
      numeric = mn(), 
      median = mdn(),
      integer = mn(),
      mean = mn(),
      custom_fun = custom_fun(),
      NULL
    )
    
 
  
  if(!is.null(measure.name)) 
      names(res)[ncol(res)] <- measure.name[1]
 

 
 res
}







#' @rdname Berechne
#' @param .data Daten
#' @param na.rm NAs
#' @param conf.interval CIs
#' @param .drop anplyr::ddply
#' @export
#' @examples
#' berechne(hyper, "chol0" )
#' names(hyper)
#' hyper %>% berechne(chol0,chol1,chol6,chol12, by=~med+g)
berechne.data.frame <- function(.data,
                                ...,
                                by = "1",
                                type = 1,
                                na.rm=TRUE,
                                conf.interval=.95, .drop=TRUE
) {

 # Text("berechne.data.frame: Achtung die Funktion wird bals geloescht!")

  measure <-
    sapply(lazyeval::lazy_dots(...), function(x) {
      as.character(x[1])
    })
  # erlaubt:  varana %>% berechne(4, 5, by= ~geschl )
  meAsNum<- grep("^[[:digit:]]", measure)
  if(length(meAsNum) !=0 ) {
    measure[meAsNum] <- names(.data[ as.numeric(measure[meAsNum]) ])
  }

  if (stpvers::is_formula2(by)) by <- all.vars(by)


  res<- NULL
  for (i in measure){
    res<-rbind(res,
               berechneMean(.data, i, by,  na.rm=na.rm,
                            conf.interval=conf.interval, .drop=.drop
               ))
  }

  res$variable<- factor(res$variable, unique(res$variable))



  res
}

#' @rdname Berechne
#' @export
berechneMean <- function(data=NULL,
                         measurevar,
                         by=NULL, na.rm=TRUE,
                         conf.interval=.95, .drop=TRUE) {

  Text("berechneMean: Achtung die Funktion wird bals geloescht!")
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  if (length(measurevar) != 1 ) return(measurevar)

  datac <- plyr::ddply(data, by,
                       .fun = function(xx, col) {
                         c(variable = NA,
                           N    = length2(xx[[col]], na.rm=na.rm),
                           mean = mean   (xx[[col]], na.rm=na.rm),
                           sd   = sd     (xx[[col]], na.rm=na.rm),
                           min  = min    (xx[[col]], na.rm=na.rm),
                           max  = max    (xx[[col]], na.rm=na.rm)
                         )
                       },
                       measurevar, .drop=.drop
  )

  # Rename the "mean" column
  #  datac <- plyr::rename(datac, c("mean" = measurevar))
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean

  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval:
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  datac$ci.low <-    datac$mean-datac$ci
  datac$ci.hig <-    datac$mean+datac$ci
  datac$variable<-stp25aggregate::GetLabelOrName(data[measurevar])
  return(datac)
}

# - New version of length which can handle NA's: if na.rm==T, don't count them -----
length2 <- function (x, na.rm=FALSE) {
  if (na.rm) sum(!is.na(x))
  else       length(x)
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
  
  x_NA <- x  # --mit na
  x    <- na.omit(x)
  n    <- length(x)
  
  if (n == 0) {
    result <- ""
    ans <- rep(NA, nlevels(x_NA))
    names(ans) <- levels(x_NA)
  } else {
    
    if (is.null(exclude))   x <- x_NA
    
    
    ans <- table(x, exclude = exclude)
    
    # seltener fall das sehr viele levels vorhanden sind
    if (length(ans) > max_factor_length) {
      naLev <- levels(x)[-(1:max_factor_length)]
      Text("NA = ", paste(naLev, collapse = ", "))
      
      x <- factor(x, levels(x)[1:max_factor_length], exclude = NULL)
      x <- addNA(x)  #- addNA modifies a factor by turning NA into an extra level
      n <- length(x)
      ans <- table(x)
    }
    
    result <- rndr_percent(prop.table(ans) * 100, ans,  digits = digits)
  }
  
  data.frame(
    Characteristics = names(ans),
    n = c(n, rep("", length(ans) - 1)),
    Statistics = result, 
    stringsAsFactors=FALSE
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



#Loechkandidat nach stp25formula
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
Mean2default <- function(x,
                         digits = 2,
                         n = length(x),
                         mean.style = get_my_options()$apa.style$mittelwert$mean.style) {
  calc_mean <-
    function(x) {
      if (is.null(mean.style)) {
        rndr_mean(mean(x), ifelse(n > 2, sd (x), NA), digits)
      }
      else if (mean.style == "1") {
        rndr_mean(mean(x), ifelse(n > 2, sd (x), NA), digits)
      }
      else if (mean.style == "2" |
               mean.style == "long") {
        rndr_mean_range(
          mean(x, na.rm = TRUE),
          ifelse(n > 2, sd (x), NA),
          min(x, na.rm = TRUE),
          max(x, na.rm = TRUE),
          digits = digits
        )
      } else {
        rndr_mean(mean(x), ifelse(n > 2, sd (x), NA), digits)
      }
    }
  
  m <- if (is.numeric(x))
    calc_mean(x)
  else
    calc_mean(as.numeric(x))
  
  data.frame(
    lev = "(mean)",
    n = as.character(n),
    m = m,
    stringsAsFactors = FALSE
  )
}



# Mean2default <- function(x, digits = 2, n = length(x)) {
#   calc_mean <-
#     function(x) {
#       rndr_mean(mean(x), ifelse(n > 2, sd (x), NA), digits)
#     }
#   m <- if (is.numeric(x))
#     calc_mean(x)
#   else
#     calc_mean(as.numeric(x))
#   data.frame(lev = "(mean)",
#              n = as.character(n),
#              m = m, 
#              stringsAsFactors=FALSE)
# }

#' @rdname Berechne
#' @export
Median2default <- function(x,
                           digits = 2,
                           n = length(x),
                           median.style = get_my_options()$apa.style$mittelwert$median.style) {
  #style=IQR  quantile rndr_median_range
  calc_median <-
    function(x) {
      if (is.null(median.style)) {
        rndr_median_quant(quantile(x, na.rm = TRUE), digits)
      }
      else if (median.style == 1) {
        rndr_median_quant(quantile(x, na.rm = TRUE), digits)
      }
      else if (median.style == "IQR") {
        rndr_median(median(x), ifelse(n > 2, IQR(x), NA), digits)
      }
      else if (median.style == "2" | median.style == "long") {
        rndr_median_range(
          median(x, na.rm = TRUE),
          IQR(x, na.rm = TRUE),
          min(x, na.rm = TRUE),
          max(x, na.rm = TRUE),
          digits = digits
        )
      }
      else {
        rndr_median_quant(quantile(x, na.rm = TRUE), digits)
      }
    }
  m <-
    if (is.numeric(x))
      calc_median(x)
  else
    calc_median(as.numeric(x))
  
  data.frame(
    lev = "(median)",
    n = as.character(n),
    m = m,
    stringsAsFactors = FALSE
  )
}

# Median2default <- function(x, 
#                            digits = 2, 
#                            n = length(x),
#                            median.style=get_my_options()$apa.style$mittelwert$median.style
#                            ) {
#     #style=IQR  quantile rndr_median_range
#   calc_median <-
#     function(x) {
#       if (median.style == "IQR") {
#         rndr_median(median(x), ifelse(n > 2, IQR(x), NA), digits)
#       } else {
#         rndr_median_quant(quantile(x, na.rm = TRUE), digits)
#       }
#     }
#     m <- if (is.numeric(x)) calc_median(x) else calc_median(as.numeric(x))
# 
#   data.frame(lev = "(median)",
#              n = as.character(n),
#              m = m, 
#              stringsAsFactors=FALSE)
# }



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
    # cat( "\nProzent2default\n Levels: " )
    if (!is.factor(x))
      x <- factor(x)
    #print(levels(x))
    #cat("\n data: ")
    # print(x[1:5])
    
    
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
      
      result <- rndr_percent(as.vector(prop.table(ans) )* 100, as.vector(ans))
      #  cat("\nnach rendr")
      # print(result)
    }
    data.frame(
      lev = names(ans),
      n = c(n, rep("", length(ans) - 1)),
      m = as.vector(result), 
      stringsAsFactors=FALSE
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
    x<-  factor(x)
    ## xyz <- c(TRUE, FALSE, TRUE,TRUE, FALSE)
    ## factor(xyz)
    
  }
  else if(is.numeric(x) & is.integer(x)) {
    x <- factor( ifelse(x == 1, 1, 0), 1:0)
    
  } else {
    return( data.frame(
      lev ="",
      n =n,
      m = "n.a.", 
      stringsAsFactors=FALSE) )
  }
  
  Prozent2default(x, digits, n)[1,]
}
