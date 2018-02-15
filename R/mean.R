#' @rdname Berechne
#' @title Berechne Mittelwerte
#' @name Berechne
#' @description Die Lagemasse werden ueber die Standard-Funktionen berechnet unterschied ist nur dass
#' Faktoren zu Zahlen transformiert werden und das \code{na.rm=TRUE} gesetzt wird.
#' CI = Hmisc::smean.cl.normal
#' @param ... weitere Objekte
#' @return  ein dataframe Objekt oder ein Character-String
#' @export
#' @examples
#' berechne(hyper, "chol0" )
#' names(hyper)
#' hyper %>% berechne(chol0,chol1,chol6,chol12, by=~med+g)
berechne<- function(...){UseMethod("berechne")}

#' @rdname Berechne
#' @param fm formel
#' @param x Namen der  measure.vars
#' @param type mean, median
#' @param measure.name,measure,by Variablen name des Ergebnisses
#' @param digits Nachkommastellen
#' @param fun Function an plyr::ddply
#' @export
berechne.default <- function(data, x, by="1", measure , type = 2,
                             fun=NULL, fm=NULL, digits=NULL,
                             measure.name="value") {

  if(is.null(fm)) fm <- makeFormula(x, by)
  # cat("\nin berechne\n")
  #  print(fm)
  mdn <- function() {
    aggregate(
      fm,
      data,
      FUN = function(x) {

        if(is.null(digits)){
          if (type == 2 | type == "long")
            ffmedian_long(
              median(x, na.rm = TRUE),
              IQR(x, na.rm = TRUE),
              min(x, na.rm = TRUE),
              max(x, na.rm = TRUE)
            )
          else
            ffmedian(quantile(x))
        }else{
          if (type == 2 | type == "long")
            ffmedian_long(
              median(x, na.rm = TRUE),
              IQR(x, na.rm = TRUE),
              min(x, na.rm = TRUE),
              max(x, na.rm = TRUE), digits=digits
            )
          else
            ffmedian(quantile(x), digits=digits)
        }
      }
    )
  }
  mn <- function() {
    #     cat("\nin Mea\n")
    #    print(fm)
    #    print(head(data))

    aggregate(
      fm,
      data,
      FUN = function(x) {
        #print(x)
        #print(digits)
        #print(mean(x, na.rm = TRUE))
        if(is.null(digits)){
          if (type == 2 | type == "long")
            ffmean_long(mean(x, na.rm = TRUE),
                        sd(x, na.rm = TRUE),
                        min(x, na.rm = TRUE),
                        max(x, na.rm = TRUE))
          else
            ffmean(mean(x, na.rm = TRUE),
                   sd(x, na.rm = TRUE))}
        else{

          #cat("digits: \n")
          # print(digits)
          if (type == 2 | type == "long")
            ffmean_long(mean(x, na.rm = TRUE),
                        sd(x, na.rm = TRUE),
                        min(x, na.rm = TRUE),
                        max(x, na.rm = TRUE), digits=digits)
          else
            ffmean(mean(x, na.rm = TRUE),
                   sd(x, na.rm = TRUE),digits=digits)


        }

      }
    )
  }
  frq <- function() {
    aggregate(
      fm,
      data ,
      FUN = function(x) {
        r <- table(x)
        # pr<-prop.table(r)
        # pr <-  if(length(x)>99)  paste0(round(pr/100), "%") else round(pr,2)
        # r<- paste0(r, "(", pr, ")")
        paste(r, collapse = "/")
      }
    )
  }
  if(!is.null(fun)){
    type <- 0
    measure <- "custom_fun"

  }
  custom_fun <- function() {
    aggregate(
      fm,
      data ,
      FUN = fun
    )
  }

  # print(head(data[1:2]))
  if(type != 3) {
    if( type[1] =="median") {
      measure<- type[1]
      type<- 2 ##Lange version
    }
    res <- switch (measure,
                   median = mdn(),
                   factor = frq() ,
                   numeric = mn(),
                   integer = mn(),
                   mean = mn(),
                   custom_fun=custom_fun(),
                   NULL)

  } else{
    if(is.factor(x))
      x <- if( nlevels(x) ==2 ) as.numeric(x)-1  else as.numeric(x)

    res <- aggregate(fm, data,
                     FUN = function(x) mean(x, na.rm = TRUE))

  }
  names(res)[ncol(res)] <- measure.name
  res
}

#' @rdname Berechne
#' @param .data Daten
#' @param na.rm NAs
#' @param conf.interval CIs
#' @param .drop anplyr::ddply
#' @export
berechne.data.frame <- function(.data,
                                ...,
                                by = "1",
                                type = 1,
                                na.rm=TRUE,
                                conf.interval=.95, .drop=TRUE
) {

  Text("berechne.data.frame: Achtung die Funktion wird bals geloescht!")

  measure <-
    sapply(lazyeval::lazy_dots(...), function(x) {
      as.character(x[1])
    })
  # erlaubt:  varana %>% berechne(4, 5, by= ~geschl )
  meAsNum<- grep("^[[:digit:]]", measure)
  if(length(meAsNum) !=0 ) {
    measure[meAsNum] <- names(.data[ as.numeric(measure[meAsNum]) ])
  }

  if (is_formula2(by)) by <- all.vars(by)


  res<- NULL
  for(i in measure){
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
  datac$variable<-GetLabelOrName(data[measurevar])
  return(datac)
}

# - New version of length which can handle NA's: if na.rm==T, don't count them -----
length2 <- function (x, na.rm=FALSE) {
  if (na.rm) sum(!is.na(x))
  else       length(x)
}
