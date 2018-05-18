
#' \code{APA.xtabs}: Chi-Quadrat aus Kreuztabellen
#' 
#' @rdname APA
#' @export
APA.xtabs <- function(x, ... ) {
  x<-summary(x)
  rndr_Chisq(x$statistic, x$parameter, x$p.value)
}


#' @rdname APA
#' @export
APA.table <- function(x, ...) APA.xtabs(x, ...)


#' @rdname APA2
#' @export
#' @examples
#' #-- APA2.summary.table
#'
#' a <- letters[1:3]
#' APA2(summary(table(a, sample(a))))
APA2.summary.table <- function(x, ...) {
  Text(paste0(
    "Chisq(df=",
    x$parameter,
    ")=",
    Format2(x$statistic, 2),
    ", p=",
    ffpvalue(x$p.value)
  ))
}


#' @rdname APA2
#' @export
APA2.table <- function(x,
                       caption = "" ,
                       note = "",
                       digits = NULL,
                       test = FALSE,
                       type = c("fischer",
                                "odds",
                                "sensitivity",
                                "chisquare",
                                "correlation",
                                "r"),
                       include.total = FALSE,
                       # total/sum,
                       include.total.columns = include.total,
                       # total/sum,
                       include.total.sub = include.total,
                       # total/sum,
                       include.total.rows = include.total,
                       # total/sum,
                       include.percent = TRUE,
                       margin = NULL,
                       add.margins = seq_along(dim(x)),
                       ...) {
  APA2.xtabs(
    x,
    caption,note, digits,
    test, type,
    include.total,
    include.total.columns,
    include.total.sub,
    include.total.rows,
    include.percent,
    margin, add.margins,
    ...
  )
}


#' @rdname APA2
#' @description \strong{Kreuztabellen} 
#' APA2.xtabs Formatiert xtabs() 2x2 Tabellen werden mit Haufigkeit wahlweise mit Prozent
#' (verhalten wird ueber \code{margin = 2}) geseuert. Berechnet werden mittels
#'  \link{fisher.test} die Odds-Ratio mit 95%CI und die p-Werte. Alternatibe ist die Funktion
#'  \link{oddsratio} aus dem Paket \code{vcd}. Weiters laesst sich mit der Option \code{type = 2}
#'  eine sensitivitaetsanalyse erstellen.
#'  Bei NxM-Tabellen wird als Test-Statistik Pearson und der
#'  Kontingentkoeffizient berechnet alternativ steht auch  der Phi-Coefficient zur verf?gung
#'  auch hier mit \code{type = 2}. die Berechnung erfolgt hier mit \link{assocstats}
#'  aus dem Packet \code{vcd}.
#'
#' @param include.percent Prozent TRUE/FALSE
#' @param margin,add.margins Zeilenprozent, Spaltenprozent
#' @param include.total.columns,include.total.sub,include.total.rows xtabs: default = FALSE total/sum
#' @export
#' @examples
#' 
#' # library(stp25data)
#' # Projekt("html")
#' hkarz$LAI<- factor(hkarz$lai, 0:1, c("pos", "neg"))
#' hkarz$Tzell<- cut(hkarz$tzell, 3, c("low", "med", "hig"))
#'
#'
#' xtab <- xtabs(~ gruppe+LAI, hkarz)
#' APA2(xtab, caption="Harnblasenkarzinom", test=FALSE)
#' APA2(xtab, type="sens", test=TRUE, caption = "type=sens")
#' APA2(xtab, type="sens", caption = "geht nur mit teat=TRUE +  type=sens")
#' APA2(xtabs(~ gruppe+Tzell, hkarz), caption="APA_Xtabs: 2x3 Tabelle", test=FALSE)
#' APA2(xtabs(~ gruppe+LAI+Tzell, hkarz), caption="APA_Xtabs: 2x2x3 Tabelle", test=FALSE)
#'
#'
#' APA2(xtab, include.total.columns=TRUE, caption = "include.total.columns")
#' APA2(xtab, include.total.sub=TRUE, caption = "include.total.sub")
#'
#'
#'
#'
#' xtab <- xtabs(~ gruppe+Tzell, hkarz)
#' APA2(xtab, test=FALSE, caption="APA2: 2x3 Tabelle")
APA2.xtabs  <- function(x,
                        caption = "" ,
                        note = "",
                        output = which_output(),
                        col_names = NULL,
                        digits = NULL,
                        test = FALSE,
                        type = c("fischer",
                                 "odds",
                                 "sensitivity",
                                 "chisquare",
                                 "correlation",
                                 "r"),
                        include.total = FALSE,
                        # total/sum,
                        include.total.columns = include.total,
                        # total/sum,
                        include.total.sub = include.total,
                        # total/sum,
                        include.total.rows = include.total,
                        # total/sum,
                        include.percent = TRUE,
                        margin = NULL,
                        add.margins = seq_along(dim(x)),
                        ...) {
 # cat("\n in APA2.xtabs")
  res <- list(xtab = NULL,
              fischer = NULL,
              sensitivity = NULL)
  type <-  match.arg(type, several.ok = TRUE)
  test <- if (test)  type  else  0
  digits <- if (is.null(digits)) options()$stp25$apa.style$prozent$digits else  c(digits[1], 0)
  dimension <- length(dimnames(x))
  
  if (dimension == 1) {
    cat("Proportion")
    Text("Funktion noch nicht fertig")
  } else if (dimension == 2) {
    #cat("\n in dim=2 Zeilen/Spalten-Summen")
    if (include.total |
        (include.total.columns & include.total.rows))
      add.margins <- seq_along(dim(x))
    else if (include.total.columns) {
      add.margins <- 2
      if (is.null(margin))
        margin <- 1
    }
    else if (include.total.rows) {
      add.margins <- 1
      if (is.null(margin))
        margin <- 2
    }
    else
      add.margins <- FALSE
    
  #  cat("\n length(x)=",length(x))
    if (length(x) != 4) {
      res <- xtabl_NxM(
        x,
        caption = caption,
        digits = digits,
        type = test,
        percent = include.percent,
        margin = margin,
        add.margins = add.margins,
        ...
      )
    }
    else{
      res <- xtabl_2x2(
        x,
        caption = caption,
        digits = digits,
        type = test,
        percent = include.percent,
        margin = margin,
        add.margins = add.margins,
        ...
      )
    }

  } else if (dimension == 3) {
  if (include.total |
        (include.total.columns & include.total.rows))
      add.margins <- seq_along(dim(x))
    else if (include.total.columns) {
      add.margins <- 3
      if (is.null(margin))
        margin <- 1
    }
    else if (include.total.rows) {
      add.margins <- 1
      if (is.null(margin))
        margin <- 3
    }
    else
      add.margins <- FALSE
    #die subs kommen extra dazu
    if (include.total.sub)
      add.margins <- c(2, add.margins)
    res$xtab <- Format_xtabs(x, margin,
                             add.margins, include.percent, digits)
    
    
    Output(res$xtab, caption, note, output = output)
    
    if (test != 0)
      Text("Funktion noch nicht fertig. eventuell summary oder likelihood.test")
  } else {
    cat("NxMx... - Tabelle")
    Text("Funktion noch nicht fertig")
  }
  
  
  invisible(res)
}






#' @rdname APA_
#' @description APA_Xtabs Kreuztabellen
#' @export
APA_Xtabs <- function(x,
                      data,
                      caption = "" ,
                      note = "",
                      output = which_output(),
                      col_names = NULL,
                      ...) {
  if (stpvers::is_formula2(x))
    x <- stats::xtabs(x, data) #- altlast abfangen
  
  if (class(x)[1] == "glm") {
    x <- Klassifikation(x, ...)$xtab
  }
  
  APA2(x, caption, note, output = output)
  invisible(x)
}




# Format ------------------------------------------------------------------
#Input: Anzahl + Einstellungen
#Output: Anzahl+Prozent
Format_xtabs <- function(x,
                         margin,
                         add.margins = seq_along(dim(x)),
                         percent,
                         digits,
                         ...) {
  #Text("Format_xtabs percent: ", percent)
  #cat("\n In Format_xtabs ")
  if (!add.margins) {
    # cat(" In Format_xtabs !add.margins ")
    anzahl  <- ftable(x)
    prozent <- ftable(prop.table(x, margin = margin)) * 100
  } else if (is.null(margin) | length(dim(x)) > 2) {
    # cat(" In Format_xtabs is.null(margin)")
    anzahl  <- ftable(addmargins(x, add.margins))
    prozent <- ftable(addmargins(prop.table(x, margin = margin)
                                 , add.margins)) * 100
  }
  else {
    #- Spezialfall
    add.margins <- if (margin == 1)
      2
    else
      1
    anzahl  <- ftable(addmargins(x, add.margins))
    prozent <- ftable(addmargins(prop.table(x, margin = margin),
                                 add.margins) * 100)
  }
  
  res <-  rndr_percent_matrix(prozent, anzahl, percent, digits)
  
  colnames(res) <- dimnames(x)[[2]]
  res <- cbind(Item = dimnames(x)[[1]], res)
  names(res)[1] <- paste(names(dimnames(x)), collapse = "/")
  
  res
}


# Fischer -----------------------------------------------------------------


fisher_Statistik <- function(x, digits = 2) {
  fisher <- fisher.test(x)
  res <- data.frame(
    OR  = Format2(fisher$estimate, digits),
    CI  = ffCI(fisher$conf.int),
    #       paste0("(", Format2(fisher$conf.int[1],3), ", " , Format2(fisher$conf.int[2],3),")")
    p   = ffpvalue(fisher$p.value)
  )
  names(res) <- c("OR", "95% CI" , "p-Value")
  
  res
}

# Chi ---------------------------------------------------------------------


chisq_Statistik <- function(xtabs, type,
                            x = summary(xtabs),
                            dins = length(dimnames(xtabs))
                            ) {
  stat <- vcd::assocstats(xtabs)
  ans<- list()

  if ("chisquare" %in% type)
   {

    ans[["Chisq"]] <- data.frame(
              Test = rownames(stat$chisq_tests),
              Chi2 = fftest(stat$chisq_tests[, 1]),
              df   = Format2(stat$chisq_tests[, 2], 0),
              p    = ffpvalue(stat$chisq_tests[, 3]))}

 if (any(c("correlation", "r") %in% type))
  ans[["Correlation"]] <- data.frame(
        Test = c("Phi-Coefficient","Contingency Coefficient","Cramer's V"),
        r = Format2(c(stat$phi,stat$contingency,stat$cramer), 3)
      )
  ans
}


#' @rdname APA2
#' @export
#' @examples
#'
#' # Fit Log-Linear Models by Iterative Proportional Scaling
#' library(MASS)
#'
#' fit<-loglm(~ Type + Origin, xtabs(~ Type + Origin, Cars93))
#' APA2(fit)
APA2.loglm <- function(x,
                       caption = "Likelihood",
                       note = "",
                       output = which_output(),
                       col_names = NULL,
                       ...) {
  #-- Orginal MASS::print.loglm
  ts.array <- rbind(c(x$lrt, x$df,
                      if (x$df > 0L)
                        1 - pchisq(x$lrt, x$df)
                      else
                        1),
                    c(x$pearson, x$df,
                      if (x$df > 0L)
                        1 - pchisq(x$pearson, x$df)
                      else
                        1))
  dimnames(ts.array) <- list(c("Likelihood Ratio",
                               "Pearson"),
                             c("Chi2", "Df", "p.value"))
  res <-
    prepare_output(fix_data_frame2(Test = rownames(ts.array), ts.array), caption, note)
  
  Output(res, output=output)
  invisible(res)
}











# interne Functions NxM --------------------------------------

xtabl_NxM <- function (xtab,
                       caption,
                       digits,
                       type,
                       percent,
                       margin,
                       add.margins,
                       ...)
{
#  cat("\nin xtabl_NxM")
  x_tab <- Format_xtabs(xtab, margin,
                       add.margins,
                       percent, digits)
  #print( x_tab )
  x_tab <- prepare_output(x_tab, caption=caption)
  Output(x_tab)
  res <- list(xtab=x_tab)

  if (type[1] != 0) {
    ans <- chisq_Statistik(xtab, type = type)
    for (i in names(ans)){
      res[[i]] <- ans[[i]]
      #Error in if (translate) nms <- Names2Language(nms)
      Output(ans[[i]], caption = paste(i, caption), fix_colnames = FALSE)}
  }
  
  res
}


# interne Functions  2x2 Tabelle -------------------------------------------------------------
xtabl_2x2 <- function(xtab,
                      caption,
                      digits,
                      type,
                      percent,
                      margin,
                      add.margins,
                      lvs = c("+", "-"),
                      ...) {
 # cat("\n In xtabl_2x2 ")
#Text("include.percent: ", percent)
  x_tab <- Format_xtabs(xtab, margin,
                        add.margins,
                        percent, digits)
#  cat("\n nach Format_xtabs")
#  print(class(x_tab))
#  print(str(x_tab))
#
  x_tab <- prepare_output(x_tab, caption=caption)
# print(x_tab)
  Output(x_tab)
 # cat("\n nach Output")
  res <- list(xtab = x_tab)
  # type kann mehr sein
  #-- c("fischer", "odds","sensitivity", "chisquare" )
  if ("fischer" %in%  type ){
    x_fisher <- prepare_output(
                   fisher_Statistik(xtab),
                   caption = paste("Fisher's Exact Test ", caption) )
    Output(x_fisher, row.names = FALSE)
    res$fisher <- x_fisher
  }
  if ( "sensitivity" %in% type ) {
    x_diagnostic <- prepare_output(
                      Klassifikation.xtabs(xtab, lvs),
                      caption = paste("Sensitivity Test", caption))
    res$diagnostic <- x_diagnostic
    Output(x_diagnostic)
  }
  res
}
