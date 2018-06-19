# Kreuztabellen


# APA ---------------------------------------------------------------------




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


# APA2 --------------------------------------------------------------------
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
#' 
APA2.table <- function(...) APA2.xtabs(...)

# APA2.table <- function(x,
#                        caption = "" ,
#                        note = "",
#                        digits = NULL,
#                        test = FALSE,
#                        type = c("fischer",
#                                 "odds",
#                                 "sensitivity",
#                                 "chisquare",
#                                 "correlation",
#                                 "r"),
#                        include.total = FALSE,
#                        include.total.columns = include.total,
#                        include.total.sub = include.total,
#                        include.total.rows = include.total,
#                        include.percent = TRUE,
#                        margin = NULL,
#                        add.margins = seq_along(dim(x)),
#                        ...) {
#   APA2.xtabs(
#     x,
#     caption,note, digits,
#     test, type,
#     include.total,
#     include.total.columns,
#     include.total.sub,
#     include.total.rows,
#     include.percent,
#     margin, add.margins,
#     ...
#   )
# }


#' @rdname APA2
#'
#' @param caption,note,output,col_names,print_col,labels an Output
#' @param digits Nachkommastellen 
#' @param test,type fischer chi usw
#' @param include.total,include.total.columns,include.total.sub,include.total.rows Zeilen Prozenz usw

#' @param include.percent,include.count ausgabe 
#' @param margin,add.margins alternative zu include.total
#'
#' @return list(xtab, test)
#' @export
APA2.xtabs  <- function(x,
                        caption = "" ,
                        note = "",
                        output = which_output(),
                        col_names = NULL,
                        print_col = NULL,
                        digits = NULL,
                        test = FALSE,
                        type = c("0",
                                 "fischer",
                                 "odds",
                                 "sensitivity",
                                 "chisquare",
                                 "correlation",
                                 "r"),
                        include.total = FALSE,
                        include.total.columns = include.total,
                        include.total.sub = include.total,
                        include.total.rows = include.total,
                        include.percent = TRUE,
                        include.count = TRUE,
                        margin = NA,
                        add.margins = NA,
                        labels=NULL,   #GetLabelOrName()
                        ...) {
  res <- NULL
  type <- match.arg(type, several.ok = TRUE)
  # test <- if (test)  type  else  0

    if (is.null(digits))
       digits <- options()$stp25$apa.style$prozent$digits[1]
  
  
  x_tab <- Xtabelle(
    x,
    include.total,
    include.total.columns,
    include.total.sub,
    include.total.rows,
    margin,
    add.margins,
    include.count,
    include.percent,
    digits
  )
  
  x_tab <- prepare_output(x_tab, caption = caption)
  Output(x_tab, output = output)
  res <- list(xtab = x_tab, test = NULL)
  
  if (test){
    dimension <- length(dimnames(x))
  
  # margin und add.margins ueber include.total festlegen
  if (dimension == 1) {
    # Proportion
    Text("Funktion noch nicht fertig")
  } else if (dimension == 2) {
    if (length(x) != 4)
      res$test <- test_xtabl_NxM(x, type, output)
    else
      res$test <- test_xtabl_2x2(x, type, output)
  } else if (dimension == 3) {
    res$test <- test_xtabl_NxMxO(x, type, output)
  } else {
    cat("NxMxO... - Tabelle")
    Text("Funktion noch nicht fertig")
  }}
  
  
  invisible(res)
}








 
# APA2.xtabs  <- function(x,
#                         caption = "" ,
#                         note = "",
#                         output = which_output(),
#                         col_names = NULL,  # nicht benutzt print_col = NULL
#                         digits = NULL,
#                         test = FALSE,
#                         type = c("0","fischer",
#                                  "odds",
#                                  "sensitivity",
#                                  "chisquare",
#                                  "correlation",
#                                  "r"),
#                         include.total = FALSE,
#                         include.total.columns = include.total,
#                         include.total.sub = include.total,
#                         include.total.rows = include.total,
#                         include.percent = TRUE,
#                         margin = NULL,
#                         add.margins = FALSE) {
# 
#   res <- NULL
#   type <- match.arg(type, several.ok = TRUE)
#  # test <- if (test)  type  else  0
#   digits <- if (is.null(digits)) options()$stp25$apa.style$prozent$digits else  c(digits[1], 0)
#   dimension <- length(dimnames(x))
#   
#   # margin und add.margins ueber include.total festlegen
#   if (dimension == 1) {
#     # Proportion
#         Text("Funktion noch nicht fertig")
#   } else if (dimension == 2) {
#     # Kreuztabellen fuer NXM-Tabellen 
#           if (include.total | (include.total.columns & include.total.rows))
#             add.margins <- seq_along(dim(x))
#           # margin= NULL
#           else if (include.total.columns) {
#             add.margins <- 2
#             if (is.null(margin))
#               margin <- 1
#           }
#           else if (include.total.rows) {
#             add.margins <- 1
#             if (is.null(margin))
#               margin <- 2
#           }
#           else
#             add.margins <- FALSE
#          # Erstellen der Tabellen
#           if (length(x) != 4) {
#             res <- xtabl_NxM( x, caption, output,
#                               digits, type,
#                               include.percent, margin, add.margins)
#           }
#           else{
#             cat("\n 2x2-Tabelle\n")
#             res <- xtabl_2x2(x, caption, output,
#                              digits, type,
#                              include.percent, margin, add.margins)
#           }
#   } else if (dimension == 3) {
#     # Kreuztabellen fuer NXMxO-Tabellen 
#         if (include.total | (include.total.columns & include.total.rows))
#            add.margins <- seq_along(dim(x))
#         else if (include.total.columns) {
#           add.margins <- 3
#           if (is.null(margin))
#             margin <- 1
#         }
#         else if (include.total.rows) {
#           add.margins <- 1
#           if (is.null(margin))
#             margin <- 3
#         }
#         else
#           add.margins <- FALSE
#         #die subs kommen extra dazu
#         if (include.total.sub)
#            add.margins <- c(2, add.margins)
#         
#         res <- xtabl_NxMxO(x, caption, output,
#                            digits, type,
#                            include.percent, margin, add.margins)
# 
#     
#   } else {
#     cat("NxMxO... - Tabelle")
#     Text("Funktion noch nicht fertig")
#   }
#   
#   
#   invisible(res)
# }






#' @rdname APA_
#' @description \strong{Kreuztabellen} APA_Xtabs, APA2.xtabs : 
#' Die Funktion Formatiert xtabs() mit NxM und NxMxO Tabellen.
#' 
#' \itemize{
#'  \item{"Haufigkeit/Prozent 1"}{
#'    Die Prozent werden ueber include.percent mit margin erstellt. Der Parameter  
#'    add.margins wird automatisch vergeben.
#'    include.total, include.total.columns, include.total.sub, include.total.rows, include.percent,
#'    include.count.
#'    Feineinstellungen erfolgt ueber \code{margin = 2}).
#'    }
#'  \item{"Sensitivitaets Analyse"}{Nur bei 2x" Tabellen ueber test=TRUE}
#'  \item{"Sig.-Tests"}{Bei 2x" Tabellen Fischer sonst Chi-test.
#'  Die Berechnung erfolgt hier mit \link{assocstats}. 
#'  Weiter Einstellungen sind Correlationen, Pearson, Kontingentkoeffizient 
#'  berechnet alternativ steht auch  der Phi-Coefficient}
#'  
#' }
#' 

#'    
#' @export
#' @examples
#' 
#' #  APA_Xtabs ################################
#' # Projekt("html")
#' 
#'  dat<-GetData("
#' sex treatment  neg  pos
#' f   KG         10	   9
#' f   UG         14	   5
#' m   KG         23	   7
#' m   UG         18	  14",Tabel_Expand = TRUE, id.vars = 1:2, value="befund")
#' 
#' x1<-xtabs( ~ sex + treatment , dat)
#' x2<-xtabs( ~ sex + befund + treatment , dat)
#' APA2(x1)
#' APA2(x1, include.total=TRUE, percent=F)
#' APA2(x2, include.total=TRUE)
#' 
#' APA2(x1, include.total.columns=T)
#' APA2(x2, include.total.columns=T)
#' 
#' APA2(x1, include.total.rows=T)
#' APA2(x2,include.total.rows=T)
#' APA2(x2,include.total.sub=T)
#' APA2(x2,include.total.sub=T, include.total.rows=T)
#' APA2(x2,
#' include.total.columns = T,
#' include.total.rows = T)
#' 
#' 
#' 
#' 
#' 
#' 
#' hkarz$LAI<- factor(hkarz$lai, 0:1, c("pos", "neg"))
#' hkarz$Tzell<- cut(hkarz$tzell, 3, c("low", "med", "hig"))
#'
#'
#' xtab <- xtabs(~ gruppe+LAI, hkarz)
#' APA2(xtab, 
#' caption="Harnblasenkarzinom", test=FALSE)
#' APA2(xtab, type="sens", 
#' test=TRUE, caption = "type=sens")
#' APA2(xtab, type="sens", 
#' caption = "geht nur mit teat=TRUE +  type=sens")
#' APA2(xtabs(~ gruppe+Tzell, hkarz), 
#' caption="APA_Xtabs: 2x3 Tabelle", test=FALSE)
#' APA2(xtabs(~ gruppe+LAI+Tzell, hkarz), 
#' caption="APA_Xtabs: 2x2x3 Tabelle", test=FALSE)
#'
#'
#' APA2(xtab, 
#' include.total.columns=TRUE, caption = "include.total.columns")
#' APA2(xtab, 
#' include.total.sub=TRUE, caption = "include.total.sub")
#'
#'
#'
#'
#' xtab <- xtabs(~ gruppe+Tzell, hkarz)
#' APA2(xtab, test=FALSE, caption="APA2: 2x3 Tabelle")
#' 
#' 
#'  #################################
#'  
#'  
APA_Xtabs <-   function(x, ...) {
  UseMethod("APA_Xtabs")
}


#' @rdname APA_
#' @export
APA_Xtabs.glm <- function(x,
                          caption = "",
                          output = which_output(),
                          thresh = 0.5,
                          ...) {
  x <- Klassifikation(x, thresh, caption)$xtab
  APA2(x, output = output, ...)
  
}
  


#' @rdname APA_
#' @param addNA,exclude,drop.unused.levels An xtabs() default = FALSE
#' @export
APA_Xtabs.formula <- function(x,
                              data = NULL,
                              caption = "",
                              output = which_output(),
                              labels = FALSE,
                              addNA = FALSE, 
                              exclude = if(!addNA) c(NA, NaN),
                              drop.unused.levels = FALSE,
                              
                              include.prop.chisq = TRUE,
                              include.chisq = FALSE, include.correlation = FALSE,
                              include.fisher = FALSE,
                              include.mcnemar = FALSE,
                              include.resid = FALSE,
                              include.sresid = FALSE,
                             ## include.asresid = FALSE,
                              include.sensitivity = FALSE,
                              
                              test=  include.prop.chisq | include.chisq | include.fisher | include.mcnemar,
                               
                              ...) {
  fm_x <- x
  x <- stats::xtabs(x, data,  
                    addNA = addNA, exclude = exclude,
                    drop.unused.levels = drop.unused.levels)
  
  if (is.logical(labels)) {
    if (labels) {
      dnn <- dimnames(x)
      names(dnn) <-
        GetLabelOrName(data[all.vars(fm_x)])
      dimnames(x) <- dnn
    }
  } else if (is.character(labels)) {
    dnn <- dimnames(x)
    names(dnn)[1:length(labels)] <-
      labels
    dimnames(x) <- dnn
  }else if(is.list(labels)){
    dimnames(x) <- labels
  }
  
  
  # noch nicht fertig ----
  CST <- NULL  #chisq.test
  
  if(include.chisq | include.correlation){
    
    
    # chisq.test(x ) #Pearson's Chi-squared test with Yates
    # chisq.test(x, correct = FALSE) #Pearson's Chi-squared test
    # summary(x) # Test for independence of all factors
    # 
    
    CST <- chisq.test(x, correct = FALSE)
    
    
  }  
  
  if(include.resid){
    if(is.null( CST))  CST <- chisq.test(x, correct = FALSE)
  resid <-  formatC(CST$observed-CST$expected, digits = 2,
                    format = "f" ) }
  
  if(include.sresid){
    if(is.null( CST))  CST <- chisq.test(x, correct = FALSE)
  sresid  <-  formatC(CST$residual , digits = 2,
                      format = "f" ) 
  }
  
  if (include.mcnemar)
  {
    McN <- mcnemar.test(x, correct = FALSE)
  }
  
  
  APA2(x, caption = caption, output = output, ...)
}

#' @rdname APA_
#' @export
APA_Xtabs.data.frame <- function(data = NULL,
                                 formula,
                                 caption = "",
                                 output = which_output(),
                                 labels = FALSE,
                                 ...) {
  APA_Xtabs.formula(formula, data, caption, output, labels, ...)
}

#' @rdname APA_
#' @export
APA_Xtabs.default <- function(x, ...) {
  Text("Keine Methode fuer", class(x), "vorhanden.")
}
 



# Test Chi und Fisher ---------------------------------------------------------------------


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
        Test = c("Phi-Coefficient",
                 "Contingency Coefficient",
                 "Cramer's V"),
        r = Format2(c(stat$phi,
                      stat$contingency,
                      stat$cramer), 3)
      )
  ans
}





# interne Functions --------------------------------------



which_margin <- function(x,
                         include.total = FALSE,
                         include.total.columns = include.total,
                         include.total.sub = include.total,
                         include.total.rows = include.total) {
  mydim <- dim(x)
  mylength <- length(mydim)
  margin <-
    if (mylength == 2) {
      if (include.total)
        list(add = seq_along(mydim), prop = NULL)
      else if (include.total.columns &
               !include.total.rows)
        list(add = mylength, prop = 1)
      else if (include.total.rows &
               !include.total.columns)
        list(add = 1, prop = 2)
      else
        list(add = seq_along(mydim), prop = NULL)
    }
  else{
    if (include.total)
      list(add = seq_along(mydim), prop = NULL)
    else if (include.total.columns &
             !include.total.rows &
             !include.total.sub)
      list(add = mylength, prop = 1:2)
    else if (include.total.rows  &
             !include.total.columns  &
             !include.total.sub)
      list(add = 1, prop = 2:3)
    else if (include.total.sub &
             !include.total.rows  &
             !include.total.columns)
      list(add = 2, prop = c(1, 3))
    else if (include.total.rows  &
             include.total.sub &
             !include.total.columns)
      list(add = 1:2, prop = 3)
    else if (include.total.rows  &
             include.total.columns  &
             !include.total.sub)
      list(add = c(1, 3), prop = c(2))
    else
      list(add = seq_along(mydim), prop = NULL)
  }
  
  
  #cat("\nmargin: "); print(margin)
  margin
  
}

Xtabelle <- function(x,
                     include.total = FALSE,
                     include.total.columns = include.total,
                     include.total.sub = include.total,
                     include.total.rows = include.total,
                     margin = NA,
                     add.margins = NA,
                     count = TRUE,
                     percent = TRUE,
                     
                     digits = 0)  {
  mrgn <-
    which_margin(x,
                 include.total,
                 include.total.columns,
                 include.total.sub,
                 include.total.rows)
  
  if (!is.na(margin))
    mrgn$prop <- margin
  if (!is.na(add.margins))
    mrgn$add <- add.margins
  
  f_count <- ftable(addmargins(x, mrgn$add))
  f_percent <-
    ftable(addmargins(prop.table(x, mrgn$prop) * 100, mrgn$add))
  
  
  if (count & percent) {
    
    #print(digits)
    rndr_percent_ftable(f_percent, f_count, digits=digits)
  }
  else if (!percent)  {stp25output::fix_to_data_frame(f_count)}
  else{
    rndr_percent_ftable(f_percent, digits=digits)
  }
 
}




test_xtabl_NxMxO <- function(x, type, output, ...) {
  list(likelihood.test = NULL)
}


test_xtabl_NxM <- function (x, type, output, ...) {
  ans <- chisq_Statistik(x, type = type)
  for (i in names(ans))
    Output(
      ans[[i]],
      caption = i,
      fix_colnames = FALSE,
      output = output
    )
  ans #liste mit Test
}

test_xtabl_2x2 <- function(x, type, output, lvs = c("+", "-"), ...) {
  
  res <- list(fisher=NULL, sensitivity=NULL)
  # type kann mehr sein  #-- c("fischer", "odds","sensitivity", "chisquare" )
  if ("fischer" %in%  type) {
    x_fisher <- prepare_output(fisher_Statistik(x),
                               caption = "Fisher's Exact Test ")
    Output(x_fisher, row.names = FALSE, output = output)
    res$fisher <- x_fisher
  }
  if ("sensitivity" %in% type) {
    x_diagnostic <- prepare_output(Klassifikation.xtabs(x, lvs),
                                   caption = "Sensitivity Test")
  
    Output(x_diagnostic, output = output)
    
    res$sensitivity <- x_diagnostic
  }
  res
}

# #Input: Anzahl + Einstellungen
# #Output: Anzahl+Prozent
# Format_xtabs <- function(x,
#                          margin,
#                          add.margins = seq_along(dim(x)),
#                          percent,
#                          digits,
#                          ...) {
#   
#   Text( paste("margin:", paste(margin, collapse=", ")," <br> ",
#                 "add.margins: ", paste(add.margins, collapse=", ")))
#   # print(margin)
#   # print(add.margins)
#   
#   if (!add.margins[1]) {
#     anzahl  <- ftable(x)
#     prozent <- ftable(prop.table(x, margin = margin)) * 100
#     
#   } else if (is.null(margin[1]) | length(dim(x)) > 2) {
#     anzahl  <- ftable(addmargins(x, add.margins))
#     prozent <- ftable(addmargins(prop.table(x, margin = margin)
#                                  , add.margins)) * 100
#   } else {
#     #- Spezialfall
#     add.margins <- if (margin == 1)  2  else  1
#     
#     anzahl  <- ftable(addmargins(x, add.margins))
#     prozent <- ftable(addmargins(prop.table(x, margin = margin),
#                                  add.margins) * 100)
#   }
#   
#   res <-
#     as.matrix(rndr_percent_matrix(prozent, anzahl, percent, digits))
#   
#   ans <- stp25output::fix_to_data_frame(anzahl)
#   n <- ncol(ans)
#   ans[, (n - ncol(res) + 1):n] <- res
#   ans
#   
# }
#  #Text("Format_xtabs percent: ", percent)
# cat("\n In Format_xtabs \n")
#  
#  
#  print(x)
#  cat("\n margin")
#  print(margin)
#  cat("\n add.margins")
#  print(add.margins)
#  cat("\n percent")
#  print(percent)
#  cat("\n digits")
#  print(digits)
#  
#  my_colnames<- dimnames(x)[[2]]
#  my_itemsnames <- dimnames(x)[[1]]
#  if (!add.margins) {
#    cat("  !add.margins ")
#    anzahl  <- ftable(x)
#    prozent <- ftable(prop.table(x, margin = margin)) * 100
#  } else if (is.null(margin) | length(dim(x)) > 2) {
#     cat("  add.margins:")
#    my_colnames<- dimnames(x)[[2]]
#    my_itemsnames <- dimnames(x)[[1]]
#    
#    print(add.margins)
#    anzahl  <- ftable(addmargins(x, add.margins))
#    print(anzahl)
#    prozent <- ftable(addmargins(prop.table(x, margin = margin)
#                                 , add.margins)) * 100
#    
#    my_colnames <- c(my_colnames, "Sum")
#    
#    my_itemsnames<- c(my_itemsnames, "Sum" )
#  }
#  else {
#    #- Spezialfall
#    add.margins <- if (margin == 1)
#      2
#    else
#      1
#    anzahl  <- ftable(addmargins(x, add.margins))
#    prozent <- ftable(addmargins(prop.table(x, margin = margin),
#                                 add.margins) * 100)
#  }
#  
#  
# # print(prozent)
# # print(anzahl)
#  res <-  rndr_percent_matrix(prozent, anzahl, percent, digits)
# # print(res)
# # cat("\n  colnames: ")
#  
#   print(my_colnames)
#   print( colnames(res))
#  colnames(res) <- my_colnames
#  
#  print(res)
#  cat("\n  cbind: ")
#  
#  
#  res <- cbind(Item = my_itemsnames, res)
#  print(res)
#  cat("\n  names: ")
#  names(res)[1] <- paste(names(dimnames(x)), collapse = "/")
#  
#  
#  print(res)
#  res





#Reihenfolge Input  x, caption, output,digits, type,percent, margin, add.margins


# xtabl_NxMxO <- function(xtab,
#                        caption,
#                        output,
#                        digits,
#                        type,
#                        percent,
#                        margin,
#                        add.margins){
#  
#   x_tab <- Format_xtabs(xtab, 
#                          margin,
#                          add.margins,
#                          percent, 
#                          digits)
# 
#   x_tab <- prepare_output(x_tab, caption=caption)
#   Output(x_tab, output = output) 
#   res <- list(xtab=x_tab)
#   
#   if (type[1] != "0" | isTRUE(type[1]) ){
#     res$likelihood.test=NULL
#     Text("Funktion noch nicht fertig. eventuell summary oder likelihood.test")
#   }
#   
#   res
# }
# 
# xtabl_NxM <- function (xtab,
#                        caption,
#                        output,
#                        digits,
#                        type,
#                        percent,
#                        margin,
#                        add.margins)
# {
#   x_tab <- Format_xtabs(xtab, 
#                         margin, add.margins, percent, 
#                         digits)
#   # Kreuztabelle
#   x_tab <- prepare_output(x_tab, caption=caption)
#   Output(x_tab, output=output)
#     res <- list(xtab=x_tab)
#   #Chi- Statistik
#   if (type[1] != "0" | isTRUE(type[1]) ) {
#     ans <- chisq_Statistik(xtab, type = type)
#     for (i in names(ans)){
#       res[[i]] <- ans[[i]] #Error in if (translate) nms <- Names2Language(nms)
#       Output(ans[[i]], caption = paste(i, caption), fix_colnames = FALSE, output=output)}
#   }
#   
#   res #liste mitX-Table und Test
# }
# 
#  
# 
#  
# xtabl_2x2 <- function(xtab,
#                       caption,
#                       output,
#                       digits,
#                       type,
#                       percent,
#                       margin,
#                       add.margins,
#                       lvs = c("+", "-")
#                       ) {
#  
#   # Kreutzabelle
#   x_tab <- Format_xtabs(xtab, 
#                         margin, add.margins, percent, 
#                         digits)
#   x_tab <- prepare_output(x_tab, caption=caption)
#   Output(x_tab, output=output)
#   res <- list(xtab = x_tab)
#   
#   # Sig.Test
#   # type kann mehr sein  #-- c("fischer", "odds","sensitivity", "chisquare" )
#   if (type[1] != "0" | isTRUE(type[1]) ){
#   if ("fischer" %in%  type ){
#     x_fisher <- prepare_output(
#                    fisher_Statistik(xtab),
#                    caption = paste("Fisher's Exact Test ", caption) )
#     Output(x_fisher, row.names = FALSE, output=output)
#     res$fisher <- x_fisher
#   }
#   if ( "sensitivity" %in% type ) {
#     x_diagnostic <- prepare_output(
#                       Klassifikation.xtabs(xtab, lvs),
#                       caption = paste("Sensitivity Test", caption))
#     res$diagnostic <- x_diagnostic
#     Output(x_diagnostic, output=output)
#   }}
#   res
# }
