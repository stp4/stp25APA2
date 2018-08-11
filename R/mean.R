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
