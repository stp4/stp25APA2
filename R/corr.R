#' @rdname APA
#' @description APA.biVar: Korrelation mit Hmisc::spearman2
#' @export
#' @examples
#' 
#' # Correlation
#' APA(Hmisc::spearman2(mpg ~ factor(vs), mtcars))
#' 
APA.biVar <- function(x, ...) {
  ## Hmisc::spearman2 Wilkox-Test

  x <- unlist(as.list(x))
  names(x) <- c("rho2",
                "F", "df1", "df2", "P", "Adjusted.rho2", "n")
  paste0("rho2=", Format2(x[1], 2), ", ",
         rndr_F(x[2],
                x[3],
                x[4],
                x[5]))
  
}




#' @rdname APA_
#' @description APA_Correlation: Korrelationstabelle (Interkorrelationen mit hilfe
#' der Funktion \code{\link[Hmisc]{rcorr}}. Erlaubt ist die getrennte Auswertung ueber groups bzw mit by.
#' 
#' @export
#' @examples
#' n<- 2*8
#' e<- rnorm(n)*10
#' data<- data.frame(a=rnorm(n) + e,
#'                   b=rnorm(n), c=rnorm(n), d=rnorm(n) + e,
#'                   g=gl(2, 8, labels = c("Control", "Treat")))
#' #
#' APA_Correlation(~a+b+c, data, caption="~a+b+c")
#'
#' APA_Correlation(~a+b+c, data, caption="mit Output: ~a+b+c")
#'
#'
#'  # library(arm)
#'  # windows(7,7)
#'  # corrplot(data[Cs(a,b,c,d)], abs=TRUE, n.col.legend=7)
#'
#'  # install.packages("PerformanceAnalytics")
#'
#'  #library("PerformanceAnalytics")
#'  #?chart.Correlation#(decathlon2.active[, 1:6], histogram=TRUE, pch=19)
#'  #data(managers)
#'  #chart.Correlation(managers[,1:8], histogram=TRUE, pch="+")

APA_Correlation <-
  function(x, ...,
           caption = "Korrelation", note = NULL,
           output = which_output(),
           col_names = NULL,
           cor_diagonale_up = TRUE,
           stars = TRUE,
           p.value = FALSE,
 
           include.mean = FALSE,
           include.n = TRUE
           
           ) {
    print(include.n)
    res <- corr_tabel2(x, ...,
                       cor_diagonale_up = cor_diagonale_up,
                       stars = stars,
                       p.value = p.value,
                       # mean = FALSE, # Veraltet
                       include.mean = include.mean,
                       include.n = include.n
                       
                       )
    note <- if (is.null(note))
      attr(res, "note")
    res <- prepare_output(
      res,
      caption = caption, note = note, N = attr(res, "N"),
      labels = NA
    )
    Output(fix_format(res), output=output)
    invisible(res)
  }



#' interne Funktion fuer die Korrelationen
#' 
#' Die Funktion \code{corr_tabel}: ist die interne Funktion die die Berechnungen erstellt.
#' 
#' @return corr_tabel: liste mit "r",        "n",        "P",        "mean",     "row_name"
#' @keywords internal
corr_tabel <- function(x, ...) {
  UseMethod("corr_tabel")
}


#' @rdname corr_tabel
#' @description corr_tabel.data.frame ist die Data.Frame- methode bei der die Variablennamen ueber das ...
#' Argument uebergeben werden. Die Funktion arbeitet intern mit der corr_tabel.formula Funktion.
#' @param .data ist das erste Obkjekt
#' @param by  wenn das erste Argument die Daten sind kann hier die
#' Gruppe gewaelt werdendefault = NULL
#' @examples
#'
#' #-- corr_tabel.data.frame
#' # corr_tabel(data, a, b, d)
#'
corr_tabel.data.frame <- function(.data,
                                  ...,
                                  by = NULL,
                                  groups = NULL,
                                  type = c("pearson", "spearman"),
                                  cor_diagonale_up = TRUE,
                                  stars = TRUE,
                                  p.value = FALSE,
                                  include.mean = FALSE,
                                  include.n = FALSE) {
  vars <-  sapply(lazyeval::lazy_dots(...), function(x) {
    as.character(x[1])})

  if (is.null(by))
    Formula <- formula(paste("~", paste(vars, collapse = "+")))
  else {
    if (is_formula2(by))
      Formula <- formula(paste(
        paste(vars, collapse = "+"),
        "~",
        paste(all.vars(by), collapse = "+")
      ))
    else
      Formula <- formula(paste(paste(vars, collapse = "+"), "~",
                               paste(by, collapse = "+")))
  }
  
  corr_tabel.formula(
    Formula, .data, groups,
    type,
    cor_diagonale_up = cor_diagonale_up,
    stars = stars, p.value = p.value,
    include.mean = include.mean,
    include.n = include.n
  )
}

#' @rdname corr_tabel
#' @param Formula Ist das erste Element
#' @param data   daten fuer Formula-Methode
#' @param exclude,subset,na.action  fuer Formula Data (geht aber nicht)
#' @param groups groups spezielle Gruppen  wie zB Korrelation zwischen den Geschlechtern al
#' @param type Cor-Typ   default = "pearson" , type=c("pearson","spearman")
#' @param cor_diagonale_up diagonale   Linksbuendig = TRUE
#' @param stars,p.value Sternchen oder p-Werte
#' @param include.mean Mittelwert mit SD
#' @param include.n Anzahl N
#' @examples
#' #-- corr_tabel.formula
#'
#' #corr_tabel(a+b~d+c, data)
#' #corr_tabel(a+b+c ~ d, data)
#'
#' #corr_tabel(a+b+c~d, data, groups = ~g)
#'
corr_tabel.formula <-
  function (Formula,
            data,
            groups = NULL,
            type = c("pearson", "spearman"),
            exclude = NA,
            subset,
            na.action = na.pass,
            cor_diagonale_up = TRUE,
            stars = TRUE,
            p.value = FALSE,
            include.mean = FALSE,
            include.n = TRUE)
  {
    type <-  match.arg(type)
    #-- Vorbereiten der Daten (na.omit, subset)
    X <- Formula_Data(Formula, data, subset, na.action)
    N <- nrow(X$Y_data)
    
    row_name <- GetLabelOrName(X$Y_data)
    ## wenn nur ein element dann ergibt sich ein Fehler
    if (ncol(X$Y_data) == 1)
      X$Y_data  <- dplyr::as_data_frame(X$Y_data)
    if (!is.null(groups)) {
      cat(" in !is.null(groups)")
      groups <- all.vars(groups)
      if (length(groups) == 1) {
        groups <- data[[groups]]
        if (!is.factor(groups)) {
          warning("Achtung nur eine Faktor kann Gruppen bilden!")
          return(head(data))
        }
        
        groups <- droplevels(groups)
        lvls <- levels(groups)
        g1 <- which(groups == lvls[1])
        
        # vor corr_2"
        ans <- corr_2(X$Y_data[g1, ], X$X_data[g1, 1], type)
        names(ans)[2:4] <- paste0(lvls[1], "_", names(ans)[2:4])
        
        for (i in 2:(length(lvls))) {
          g2 <- which(groups == lvls[i])
          ans2 <-  corr_2(X$Y_data[g2, ], X$X_data[g2, 1], type)
          names(ans2)[2:4] <-
            paste0(lvls[i], "_", names(ans2)[2:4])
          ans <- cbind(ans, ans2[-1])
        }
        
      }
      else{
        return("Achtung nur eine Gruppe kann berechnet werden!")
      }
      ans[, 1] <- factor(ans[, 1], names(row_name), row_name)
      attr(ans, "note") <- type
      attr(ans, "N") <- N
    }
    else if (!is.null(X$xname)) {
      # Beginn der Funktion -----------------------------------------------------
      ans <- corr_2(X$Y_data, X$X_data[1], type)
      
      if (length(X$xname) > 1) {
        names(ans)[2:4] <- paste0(X$xname[1], "_", names(ans)[2:4])
        for (i in  2:(length(X$xname))) {
          ans2 <-  corr_2(X$Y_data, X$X_data[i], type)
          names(ans2)[2:4] <-
            paste0(X$xname[i], "_", names(ans2)[2:4])
          ans <- cbind(ans, ans2[-1])
        }
        attr(ans, "note") <- type
        attr(ans, "N") <- N
      }
      ans[, 1] <- factor(ans[, 1], names(row_name), row_name)
    }
    else{
      ans <- corr_1(X$Y_data, type = type) ## liste
      
      ans$mean <-
        t(berechne.default(X$Y_data, 
                           X$yname, 
                           measure = "mean", 
                           type = 1,
                           measure.name = "value"))
      
      ans$row_name <- row_name
    }
    
      if(!include.n){
    
    #  cat("\n", include.n, "\n\n-----\n")
    # grep("_N", c("jdhdz", "hdgdt_N", "sfsg_N"))
    #  print( grep("_N", names(ans)) )
     ans<- ans[, - grep("_N", names(ans))]
      }
    
    ans
  }


#' @rdname corr_tabel
#' @description corr_tabel2 ist die Ausgabe von corr_tabel als Formatierte Tabelle
#' @return corr_tabel2: data.frame()
#' @examples
#' #--corr_tabel2
#'
#' corr_tabel2(data, a, b, d)
#'
corr_tabel2 <- function(...,
                        cor_diagonale_up = TRUE,
                        stars = TRUE,
                        p.value = FALSE,
                        # mean = FALSE, # Veraltet
                        include.mean = FALSE,
                        include.n = TRUE)
{
  cat("incorr_tabel2:\n")
  print(include.n)
  ans <- corr_tabel(..., include.n=include.n)
  print(class(ans))
  print(ans)
  #cat("\ninclude.mean =", include.mean, "\n")
  res <- data.frame()
  if (class(ans) == "rcorr") {
    #-- Hmisc also wie aus APA2
    format_diagonale <- function(mycorrtable,
                                 d = 1,
                                 l = "") {
      diag(mycorrtable) <- d
      if (cor_diagonale_up)
        mycorrtable[lower.tri(mycorrtable)] <- l
      else
        mycorrtable[upper.tri(mycorrtable)] <- l
      mycorrtable
    }
    n <- Format2(ans$n, 0)
    # colnames(n)<- paste0(colnames(n), "_", "n" )
    r <- format_diagonale(Format2(ans$r, 2))

    if (stars) {
      p <- apply(ans$P, 2, function(x)
        cut(
          x,
          c(-Inf, options()$stp25$apa.style$p$stars.value, Inf),
          c(options()$stp25$apa.style$p$stars.symbols, "")
        ))

      r <- format_diagonale(matrix(
        paste0(Format2(ans$r, 2), p),
        nrow = nrow(ans$r),
        dimnames = dimnames(ans$r)
      ))
    }
    if (p.value) {
      p <- paste0(" (p=", ffpvalue(ans$P), ")")
      r <- format_diagonale(matrix(
        paste0(Format2(ans$r, 2), p),
        nrow = nrow(ans$r),
        dimnames = dimnames(ans$r)
      ))
    }

    res <- data.frame(Source = rownames(ans$r), r)
    attr(res, "note") <-  attr(ans, "note")
    attr(res, "N") <-  attr(ans, "N")
    my_num <- paste0("(", 1:length(ans$row_name), ")")

    res[, 1] <-
      factor(res[, 1], names(ans$row_name), paste(my_num, ans$row_name)) # Labels
    colnames(res)[2:ncol(res)] <- my_num

   # if(!include.n){

    #  cat("\n", include.n, "\n\n-----\n")
      # grep("_N", c("jdhdz", "hdgdt_N", "sfsg_N"))
    #  print( grep("_N", names(ans)) )
    #  ans<- ans[, - grep("_N", names(ans))]
   # }

    if (include.mean)
      res <- cbind(res[1], "M (SD)" = ans$mean, res[2:ncol(res)])
    return(res)
  }
  else{
    return(ans)
  }
}


#-- Helper ---------------

#' @rdname corr_tabel
#' @examples
#' #-- corr_1
#' # stp25APA2:::corr_1(data[1:3])
corr_1 <- function(x, type = "pearson") {
  names_x <- names(x)
  #if(ncol(x)<2) (warnings("Zu wenig Variablen"))
  x <- apply(x, 2, as.numeric)
  ans <- Hmisc::rcorr(x, type = type)
  attr(ans, "note") <- type
  attr(ans, "N") <- nrow(x)
  ans
}

#' @rdname corr_tabel
#' @examples
#' #-- corr_2
#' # stp25APA2:::corr_2(data[1:3], data[4])
#'
corr_2 <- function(x, y = NULL, type = "pearson") {
  names_x <- names(x)
  x <- apply(x, 2, as.numeric)

  if (is.data.frame(y))
    y <- apply(y, 2, as.numeric)
  else
    y <- as.numeric(y)

  y <- as.matrix(y)
  ans <- Hmisc::rcorr(x, y, type = type)

  k <- ncol(ans$r)
  r <- ans$r[-k, k]
  p <- ans$P[-k, k]

  if (!is.null(names(r)))
    names_x <- names(r)

  ans <- data.frame(
    Characteristics = names_x,
    N = Format2(ans$n[-k, k], 0),
    r =  round(r, 3),
    p.value = round(p, 4),
    stringsAsFactors = F
  )
  attr(ans, "note") <- type
  attr(ans, "N") <- nrow(x)
  ans
}

#' @rdname corr_tabel
#' @description Die Interne Fznction \code{Corr2} wird in APA2 verwendete Korrelation
#' @examples
#'
#' #-- Corr2
#' # APA2(a+b+c~d, data )
#' #stp25APA2:::Corr2(data[1:3], data[4], "pearson", TRUE)
#'
Corr2 <- function(x,
                 y = NULL,
                 type="pearson",
                 stars,
                 ...) {
  # cat("\nin funktion Corr2\n")
  names_x <- names(x)
  x <- apply(x, 2, as.numeric)
  y <- as.matrix(y)
  ans <- Hmisc::rcorr(x, y, type=type)
  k <- ncol(ans$r)

  r<-ans$r[-k, k]
  p <- ans$P[-k, k]
  r <- rndr_r(r, include.symbol=FALSE)
  r2<- if(stars) paste0(r, rndr_Stars(p))
        else   paste0(r, rndr_P(p, include.bracket=TRUE))
  data.frame(Characteristics = NA,
             N = ans$n[-k, k],
             Wert1 = ans$r[-k, k],
             Wert2 = round(p, 3),
             summary = r,
             summary2 = r2,
             stringsAsFactors = F)
}

#' @rdname corr_tabel
#' @examples
#'
#' #-- Corr1
#' # APA2(~a+b+cd, data)
#' #stp25APA2:::Corr1(data[1:3], dimnames=TRUE)
#' #stp25APA2:::Corr1(data[1:3], dimnames=TRUE, include.p=TRUE)
Corr1<- function(y,
                 n= nrow(y),
                 type="pearson",
                 include.p=FALSE,
                 include.stars=TRUE,
                 cor_diagonale_up=TRUE,
                # digits=2,
                 dimnames=FALSE# nur interner gebrauch zum Testen
){

  if(any(sapply(y, class) == "factor")){
    res <- rep("Gemischte Skalenniveaus use as.numeric", n)}
  if(nrow(na.omit(y)) < 4){
    res <- rep("must have >4 observations", n)
  }else{


    mycor <- Hmisc::rcorr(as.matrix(y), type = type)
    #res <- round(mycor$r, digits)
    res <- rndr_r(mycor$r, include.symbol=FALSE)

    if(include.p){
     # p <- mycor$P
     # pval <- apply(p, 2, ffpvalue)
     # pval <- p=",pval,")
      pval <- rndr_P(mycor$P, include.bracket=TRUE)
      res <- matrix(paste0(res, pval), ncol = ncol(res))
    }
    if(include.stars){
     # p <- mycor$P
    #  pval <- apply(p, 2, ffsigstars)
      pval <- rndr_Stars(mycor$P)
      res <- matrix(paste0(res, pval), ncol = ncol(res))
    }
    diag(res) <- "1"
    if(cor_diagonale_up)
      res[lower.tri(res)] <- ""
    else
      res[upper.tri(res)] <- ""
  }#- end else nrow(na.omit(y)) < 4

  if(dimnames) dimnames(res)<- dimnames(mycor$r)
  res
}
