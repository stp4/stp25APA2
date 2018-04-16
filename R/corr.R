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




#' @param ... Formel mit Daten wird von prepare_data2()aufgedroeselt
#'
#' @param caption,note Ueberschrift
#' @param output Html oder Text
#' @param col_names nicht benutzt
#'
#' @rdname APA_
#' @description APA_Correlation: Korrelationstabelle (Interkorrelationen mit der Hilfe
#' der Funktion  Hmisc::rcorr. Erlaubt ist die getrennte Auswertung ueber by=~b
#' 
#' @export
#' @examples
#' 
#' n <- 2 * 20
#' e <- rnorm(n)
#' dat <- Label(
#'   data.frame(
#'     a = rnorm(n) + e / 2,
#'     b = rnorm(n) + e,
#'     c = rnorm(n),
#'     d = rnorm(n) + e * 10,
#'     g = gl(2, 20, labels = c("Control", "Treat"))
#'   ),
#'   a = "Alpha",
#'   b = "Beta",
#'   c = "Gamma"
#' )
#' 
#' 
#' APA_Correlation( ~ a + b + c, dat)
#' APA_Correlation(a ~ c, dat)
#' APA_Correlation(a + b + c ~ d, dat)
#' APA_Correlation(a + b ~ c +  d, dat)
#' APA_Correlation(a + b + c ~ d, dat, groups = ~ g)
#' APA_Correlation( ~ a + b + c, dat)
#' APA_Correlation( ~ a + b + c, dat, include.p = TRUE)
#' APA_Correlation( ~ a + b + c, dat, include.p = TRUE, include.stars = FALSE)
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
  function(...,
           caption = "Korrelation", note = NULL,
           output = which_output(),
           col_names = NULL,
           cor_diagonale_up = TRUE,
           type = c("pearson", "spearman"),
           
           include.mean = FALSE,
           include.n = TRUE,
           stars = TRUE,
           p.value = FALSE,
           include.stars=stars,
           include.p=p.value
           
           ) {
    
   res<-  Hmisc_rcorr(...,
                cor_diagonale_up = cor_diagonale_up,
                include.stars = include.stars,
                include.p  = include.p,
                include.mean = include.mean,
                include.n = include.n,
                type=type)
    
    note <- if (!is.null(note))
         attr(res, "note")<- note
   Output(res, caption=caption, output=output)
   
  invisible(res)
    
    # res <- corr_tabel2(x, ...,
    #                    cor_diagonale_up = cor_diagonale_up,
    #                    
    #                    stars = include.stars,
    #                    p.value = include.p,
    #                     
    #                    include.mean = include.mean,
    #                    include.n = include.n
    #                    
    #                    )
    # note <- if (is.null(note))
    #   attr(res, "note")
    # res <- prepare_output(
    #   res,
    #   caption = caption, note = note, N = attr(res, "N"),
    #   labels = NA
    # )
    # 
    # Output(fix_format(res), output=output)
    # invisible(res)
  }




#' @param cor_diagonale_up Diagonale oben oder unter
#' @param include.stars Sternchen als p-Werte
#' @param include.p  Explizite p-Werte
#' @param include.mean Mittelwerte
#' @param include.n  Anzahl an gueltigen Werten
#' @param type enweder "pearson" oder "spearman"
#' 
#' @rdname APA_
#'
Hmisc_rcorr <- function(...,
                     cor_diagonale_up = TRUE,
                     include.stars = TRUE,
                     include.p  = FALSE,
                     include.mean = FALSE,
                     include.n = TRUE,
                     type = c("pearson", "spearman")) {
  X <- prepare_data2(...)
  type <-  match.arg(type)
  condition <- X$condition.vars
  measure.vars <- X$measure.vars
  measure_data <- apply(X$data[measure.vars], 2, as.numeric)
  group.vars <- X$group.vars
  N <- nrow(X$data[measure.vars])
  row_name <- GetLabelOrName(X$data[measure.vars])
  ans <- NULL
  
  if (!is.null(condition)) {
    # a + b ~ c | d
    
    if (length(condition) < 1)
      warning("kann nur eine Gruppe aufdroeseln!")
    condition <- X$data[[condition[1]]]
    
    if (!is.factor(condition)) {
      warning("Achtung nur eine Faktor kann Gruppen bilden!")
      return(head(data))
    }
    
    group_data <- apply(X$data[group.vars], 2, as.numeric)
    condition <- droplevels(condition)
    lvls <- levels(condition)
    print(seq_len(length(lvls)))
    for (i in seq_len(length(lvls))) {
      g2 <- which(condition == lvls[i])
      
      ans_corr <-
        Hmisc::rcorr(measure_data[g2, ], group_data[g2, ], type = type)
      k <- ncol(ans_corr$r)
      r <- ans_corr$r[-k, k]
      p <- ans_corr$P[-k, k]
      
      ans_corr <- data.frame(
        Characteristics = X$row_name ,
        N = round(ans_corr$n[-k, k], 0),
        r =  rndr_r(r, FALSE),
        p.value = rndr_P(p, FALSE),
        stringsAsFactors = FALSE
      )
            names(ans_corr)[-1] <-
        paste0(lvls[i], "_", names(ans_corr)[-1])
      if (i == 1) {
        ans <- ans_corr
      }
      else{
        ans <- cbind(ans, ans_corr[-1])
      }
    }
      }
  else if (!is.null(group.vars)) {
    #   a + b ~ c + d
    group_data <- apply(X$data[group.vars], 2, as.numeric)

    for (i in  1:(length(group.vars))) {
      ans_corr <- Hmisc::rcorr(measure_data, group_data[, i], type = type)
      k <- ncol(ans_corr$r)
      r <- ans_corr$r[-k, k]
      p <- ans_corr$P[-k, k]
      
      ans_corr <- data.frame(
        Characteristics = X$row_name ,
        N = round(ans_corr$n[-k, k], 0),
        r =  rndr_r(r, FALSE),
        p.value = rndr_P(p, FALSE),
        stringsAsFactors = FALSE
      )
      names(ans_corr)[-1] <-
        paste0(group.vars[i], "_", names(ans_corr)[-1])
      if (i == 1) {
        ans <- ans_corr
      }
      else{
        ans <- cbind(ans, ans_corr[-1])
      }
    }
  }
  else{
    # Korrelations_Matrix  ~ a+ b + c
    ans_list <- Hmisc::rcorr(measure_data, type = type)
    n <- Format2(ans_list$n, 0)
    
    if (include.stars & !include.p) {
      p <- apply(ans_list$P, 2,
                 function(x)
                   cut(
                     x,
                     c(-Inf, options()$stp25$apa.style$p$stars.value, Inf),
                     c(options()$stp25$apa.style$p$stars.symbols, "")
                   ))
      
      r <- format_diagonale(matrix(
        paste0(rndr_r(ans_list$r, FALSE), p),
        nrow = nrow(ans_list$r),
        dimnames = dimnames(ans_list$r)
      ),
      cor_diagonale_up)
    } else if (include.p & !include.stars) {
      p <-  rndr_P(ans_list$P, TRUE, TRUE)
      
      r <- format_diagonale(matrix(
        paste0(rndr_r(ans_list$r, FALSE), p),
        nrow = nrow(ans_list$r),
        dimnames = dimnames(ans_list$r)
      ),
      cor_diagonale_up)
    } else if (!include.p & !include.stars) {
      r <- format_diagonale(rndr_r(ans_list$r, FALSE), cor_diagonale_up)
    } else {
      p <- apply(ans_list$P, 2,
                 function(x)
                   paste0(rndr_Stars(x), rndr_P(x, TRUE, TRUE)))
      r <-  apply(ans_list$r, 2,
                  function(x)
                    rndr_r(x, FALSE))
      
      r <- format_diagonale(matrix(
        paste0(rndr_r(ans_list$r, FALSE), p),
        nrow = nrow(ans_list$r),
        dimnames = dimnames(ans_list$r)
      ),
      cor_diagonale_up)
      # warning("Ich weigere mich p-Werten mit Sternchen zu erstellen!!!")
    }
    
    ans <-  data.frame(Source = rownames(ans_list$r),
                       r,
                       stringsAsFactors = FALSE)
    # Labels
    my_num <- paste0("(", 1:length(X$row_name), ")")
    ans[, 1] <-   paste(my_num, X$row_name)
    colnames(ans)[2:ncol(ans)] <- my_num
    
    
    if (include.n)
      cat("\n include.n ist nicht implementiert.\n")
    
    if (include.mean) {
      ans_mean <-
        t(
          berechne.default(
            X$data[measure.vars],
            X$measure.vars,
            measure = "mean",
            type = 1,
            measure.name = "value"
          )
        )
      ans <- cbind(ans[1], "M (SD)" = ans_mean, ans[2:ncol(ans)])
    }
  }
  prepare_output(ans, note = type, N = N)
}


#-- Hmisc also wie aus APA2
format_diagonale <- function(mycorrtable,
                             cor_diagonale_up,
                             d = 1,
                             l = "") {
  diag(mycorrtable) <- d
  if (cor_diagonale_up)
    mycorrtable[lower.tri(mycorrtable)] <- l
  else
    mycorrtable[upper.tri(mycorrtable)] <- l
  
  mycorrtable
}



#' @rdname APA2
#' @description  Corr1() wird in in APA2 beim Einzelvergeich verwendet.
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



#' @rdname APA2
#' @description Die Interne Funktion  Corr2()  wird in APA2 verwendete um Korrelation zu berechnen.
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
             stringsAsFactors = FALSE)
}
