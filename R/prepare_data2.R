#' @rdname prepare_data
#' @description Funktion zum Aufbereiten der Daten. Die Daten werden als
#' tibble::as_tibble(.data) weitergegeben.
#' @param x  formel
#' @param data daten
#' @param subset  nicht implementiert
#' @param na.action  nicht zum aendern
#'
#' @return Liste mit Namen und Daten
#' data,
#' measure.vars ,
#' group.vars,
#' condition.vars,
#' formula,
#' by,
#' measure,
#' row_name,
#' col_name,
#' measure.class,
#' group.class,
#' condition.class,
#' digits
#' @export

prepare_data2 <- function(...)
  UseMethod("prepare_data2")

#' @rdname prepare_data
#' @description Funktion zum Aufbereiten der Daten Output List mit neuen Daten.
#' @param x  formel
#' @param data daten
#' @param subset  nicht implementiert
#' @param na.action  nicht zum aendern
#' @export
#' @examples
#' # require(stpvers)
#' prepare_data2(~ m1 + m2 + m3 + m4, varana)
#' prepare_data2(~ log(m1) + m2 + m3 + m4, varana)
#' prepare_data2(~ m1[1] + m2 + m3 + m4, varana)
#' prepare_data2(~ m1[1] + m2 + m3[4,median] + m4, varana)
#'
#' #-- Helper
#' data <- data.frame(x = NA, y = NA, z = NA)
#' stp25APA2:::clean_dots_formula(x ~ y, data)
#' stp25APA2:::clean_dots_formula(. ~ x + y, data)
#' stp25APA2:::clean_dots_formula(x + y ~ ., data)
#' stp25APA2:::clean_dots_formula(~., data)
#' stp25APA2:::formula_split(a+b~x|y)
prepare_data2.formula <-
  function(x,
           data,
           subset,
           na.action = na.pass,
           groups = NULL) {

if(!is.null(groups)){
  #is_formula2(x)
      x<- formula(
        paste(
        deparse(x), "|", gsub("~", "", deparse(groups))))
      }
    
 if(!tibble::is_tibble(data))
      data <- tibble::as_tibble(data)

  measure.vars = NULL
  group.vars = NULL
  condition.vars = NULL
  formula = NULL
  by = NULL
  measure = NULL
  row_name = NULL
  col_name = NULL
  measure.class = NULL
  group.class = NULL
  condition.class = NULL
  digits = NULL
  all_vars <- NULL

  x <- clean_dots_formula(x, data)
  lng_fm <- length(x)
  frml <- formula_split(x)
  formula <- frml$formula

  measure.vars <- all.vars(x[[2L]])
  condition.vars <- all.vars(frml$condition)
  #- Formel vom Type 'a+b[2]~c' kann auch 'a+b[2,median] ~ . ' sein
  if (any(all.names(formula[[2L]]) %in% '[')) {
#cat("\in drösl\n")
      #  bei var[2,median] kommt der Median durch, error wegen  width.cutoff = 60L
    y_hsd <- gsub(" ", "", paste(deparse(formula[[2L]]), collapse=""))
    y_hsd <- strsplit(y_hsd, "\\+")[[1]]
    
    #  y_hsd <-
    #    strsplit(deparse(formula[[2L]], width.cutoff = 500L), " \\+ ")[[1]]
#print(y_hsd)
      measure.vars <- gsub("\\[.+\\]", "", y_hsd) # bereinigen von Klammern
      
#print(measure.vars)
      #- die länge hat sich geändert
      measure.class <- get_classes(data[measure.vars])
      measure <- measure.class
      digits<- ifelse(measure=="factor",
                      options()$stp25$apa.style$prozent$digits[1],
                      options()$stp25$apa.style$mittelwert$digits[1])
      # Dedect Position
      pos <- grep('\\[', y_hsd)
      # dedect_string afer ,  var[2,median]  gsub("[^[:alpha:]]", "", "var[2,median]")
      dedect_string<- gsub("[^[:alpha:]]", "",
                        stringr::str_extract(y_hsd[pos], "\\[.+"))
      #  print(dedect_string)
      dedect_string <- stp25_stat_methode(dedect_string) # return:"mean"   "freq"   "median" NA
      #  print(dedect_string)
      dedect_number <- as.integer(gsub("[^0-9]", "",
                            stringr::str_extract(y_hsd[pos], "\\[.")))

      if(!is_empty2(dedect_string)){
        for (i in  seq_len(length(pos)))
          if(!is_empty2(dedect_string[i]))
            measure[pos[i]] <- dedect_string[i]
          }

      if(!is_empty2(dedect_number)){
        for (i in seq_len(length(pos)))
          if(!is_empty2(dedect_number[i]))
            digits[pos[i]] <- dedect_number[i]
          digits<- as.numeric(digits)
          }

      if (length(formula) == 2) {
        formula <- to_formula(measure.vars, NULL)
        all_vars <- c(measure.vars, condition.vars)
      } else {
        x_hsd <- strsplit(deparse(formula[[3L]]), " \\+ ")[[1]]
        group.vars <- gsub("\\[.+\\]", "", x_hsd)
        col_name <- GetLabelOrName(data[group.vars])
        formula <- to_formula(measure.vars, group.vars)
        all_vars<- c(measure.vars, group.vars, condition.vars)
      }

      data<-data[all_vars]
   } else {# A+B+C~G  oder log(a) + b + c
      formula <- Formula::Formula(formula)
      data2 <- stats::model.frame(formula, data = data, na.action = na.action)
      X_data <- Formula::model.part(formula, data = data2, rhs = 1)
      Y_data <- Formula::model.part(formula, data = data2, lhs = 1)

      xname <- names(X_data)
      yname <- names(Y_data)

      #-- Einzelvergeich kein Y_data und wir werten ueber Y_data aus daher
      if (!length(yname)) {
        if(length(grep("\\(", formula))>0) {
         # yname <-  all.vars(formula[[2L]])
          xname <- all.vars(formula[[2L]])
          names(X_data) <-xname

          row_name <- GetLabelOrName(X_data)
         # names(Y_data) <- yname
          formula <- to_formula( xname, NULL)
        }
        data <- X_data
        measure.vars <- xname
      } else {
          if(length(grep("\\(", formula))>0) {
                 yname <-  all.vars(formula[[2L]])
                 xname <- all.vars(formula[[3L]])
                 names(X_data) <-xname
                 names(Y_data) <- yname
                 formula <- to_formula(yname, xname)
        }

          if(is.null(condition.vars))
               data <- cbind(X_data, Y_data)
          else data <- cbind(X_data, Y_data, data[condition.vars])

          measure.vars <- yname
          group.vars <- xname
        }
    }

    stp25DataObjekt(
      data,
      measure.vars,group.vars,condition.vars,
      formula,by,
      measure,
      row_name,col_name,
      measure.class,group.class,condition.class,
      digits)
}


#' @rdname prepare_data
#' @description Funktion zum Aufbereiten der Daten Output List mit neuen Daten.
#' @param data Daten
#' @param subset  nicht implementiert
#' @param na.action  nicht zum aendern
#' @param .data daten
#' @param ... Variablen
#' @param by Gruppe
#' @param groups Block codition
#' @export
#' @examples
#' 
#' prepare_data2(varana, m1, m2, m3, m4)
#' prepare_data2(varana, 4:7)
#' prepare_data2(varana, m1[1], m2, m3, m4)
#' prepare_data2(varana, m1[1], m2, m3[4,median], m4)
#' prepare_data2(varana, m1 , m2, m3=median, m4)
#' prepare_data2(varana, m1, m2, m3 , m4, by =  ~ geschl)
#' prepare_data2(varana, m1[4, median], m2, m3 , m4[5], by =  ~ geschl)
prepare_data2.data.frame <- function(.data,
                                     ...,
                                     by = "1",
                                     groups = NULL,
                                     subset,
                                     na.action = na.pass
                                     ) {
if(!tibble::is_tibble(.data))
    .data <- tibble::as_tibble(.data)

 measure.vars = NULL
 group.vars = NULL
 condition.vars <- if(is_formula2(groups)) all.vars(groups) else groups
 formula = NULL
 row_name = NULL
 col_name = NULL
 measure.class = NULL
 group.class = NULL
 condition.class = NULL
 digits = NULL
 all_vars <- NULL

 measure <-
    sapply(lazyeval::lazy_dots(...), function(x)
      as.character(x[1]))

  measure <- makeNamesNum(measure, .data)

  if (length(measure) == 0) { # prepare_data2(varana)
     measure.vars <- names(.data)
     }
  else if (any(grepl("\\[", measure))) { # prepare_data2(varana, m1[4,median])
     return( prepare_data2.formula(
              to_formula(measure, by, condition.vars), .data))
  }
  else { # prepare_data2(varana, m1=median, m2, m3)
    if (length(names(measure)) != 0) {
        measure.vars <-
          ifelse(names(measure) == "", measure, names(measure))
        measure.class <- get_classes(.data[measure.vars])
        measure <- ifelse(names(measure) == "", measure.class, measure)
    }
    else {
        measure.vars <- measure
        measure <- measure.class <- get_classes(.data[measure.vars])
    }

    fm <- paste(measure.vars, collapse = "+")

    if (by == "1") {
      all_vars <- c(measure.vars, condition.vars)
      .data <- .data[all_vars]
      formula <- formula(paste("~", fm))
    }
    else if (is_formula2(by)) {
      group.vars <- all.vars(by)
      all_vars <- c(measure.vars, group.vars, condition.vars)
      .data <- .data[all_vars]
      group.class <- get_classes(.data[group.vars])
      formula <- to_formula(measure.vars, group.vars, condition.vars)

      col_name<-GetLabelOrName(.data[group.vars])
    }
    else {
      group.vars <- by
      by <- formula(paste("~", paste(by, collapse = "+")))
      all_vars <- c(measure.vars, group.vars, condition.vars)
      .data <- .data[all_vars]
      group.class <- get_classes(.data[group.vars])
      formula <- to_formula(measure.vars, group.vars, condition.vars)

      col_name<-GetLabelOrName(.data[group.vars])
    }
  }
 stp25DataObjekt(
   data=.data,
   measure.vars,group.vars,condition.vars,
   formula,by,
   measure,
   row_name,col_name,
   measure.class,group.class,condition.class,
   digits)
}

#-- helper erstelle eine Formel
to_formula <- function(measure.vars, group.vars, condition.vars=NULL) {

  if (is.null(group.vars) ){
    fm <- paste0("~", paste(measure.vars, collapse = "+"))}
  else if (group.vars[1] == "1" ){fm <- paste0("~", paste(measure.vars, collapse = "+"))}
  else {
    if (is_formula2(group.vars))
      fm <- paste0(paste(measure.vars, collapse = "+"),
                   "~",
                   paste(all.vars(group.vars), collapse = "+"))
    else
      fm <- paste0(paste(measure.vars, collapse = "+"),
                   "~",
                   paste(group.vars, collapse = "+"))
  }
 # cat("\n Formel: ", fm, "\n")
  formula(fm)
}


#--helper extrahiere Classe
get_classes <-
  function(data)
    sapply(data, function(x)
      setdiff(class(x), "labelled"))


#' @rdname prepare_data
#' @description stp25DataObjekt Ueberprüft die Objektstrucktur also ob
#'  measure usw vorhanden ist.
#'  Die Uebergebenen Daten werden falls notwendig transformiert.
stp25DataObjekt <- function(data = NULL,
                            measure.vars = NULL,
                            group.vars = NULL,
                            condition.vars = NULL,
                            formula = NULL,
                            by = NULL,
                            measure = NULL,
                            row_name = NULL,
                            col_name = NULL,
                            measure.class = NULL,
                            group.class = NULL,
                            condition.class = NULL,
                            digits = NULL) {
  if (!is.null(measure.vars)) {
    if (is.null(measure.class))
      measure.class <- get_classes(data[measure.vars])
    if (is.null(measure))
      measure  <- measure.class
    if (is.null(row_name))
      row_name <- GetLabelOrName(data[measure.vars])
    if (is.null(digits))
      digits <- ifelse(
        measure == "factor",
        options()$stp25$apa.style$prozent$digits[1],
        options()$stp25$apa.style$mittelwert$digits[1]
      )
  }

  if (!is.null(group.vars)) {
    if (is.null(group.class))
      group.class <- get_classes(data[group.vars])

    if (is.null(col_name))
      col_name <- GetLabelOrName(data[group.vars])

  } else{
    group.class <- col_name <- NULL
  }

  if (!is.null(condition.vars) & length(condition.vars > 0)) {
    if (is.null(condition.class))
      condition.class <- get_classes(data[condition.vars])
  } else{
    condition.class <- condition.vars <- NULL
  }

  if (is.null(by)) {
    if (is.null(group.vars))
      by <- "1"
    else{
      by <- formula(paste("~", paste(group.vars, collapse = "+")))
    }
  }


  if (is.null(formula)) {
    formula <- to_formula(measure.vars, group.vars, condition.vars)
  }


  measure_class <- ifelse(measure == "mean",
                          "numeric",
                          ifelse(
                            measure == "mean",
                            "numeric",
                            ifelse(measure == "freq", "factor",
                                   measure)
                          ))
  #cat("\nmeasure_class")
 # print(measure_class)
  if (!all(measure.class == measure_class)) {
    x <- which(!measure.class == measure_class)
    for (i in x) {
    #  print(i)
      if (measure_class[i] == "numeric") {
        data[[i]] <- as.numeric(data[[i]])
      }
      else if (measure_class[i] == "factor") {
        data[[i]] <- factor(data[[i]])
      }
      measure.class[i] <- measure_class[i]
    }

  }




  res = list(
    data = data,
    measure.vars = measure.vars,
    group.vars = group.vars,
    condition.vars = condition.vars,
    formula = formula,
    by = by,
    measure = measure,
    row_name = row_name,
    col_name = col_name,
    measure.class = measure.class,
    group.class = group.class,
    condition.class = condition.class,
    digits = digits,
    N = nrow(data)
  )
  class(res) <- c("stp25data", class(res))
  res
}

#' @rdname prepare_data
#' @description Print Methode
#' @export
print.stp25data<- function(x){
  cat("\nformula: ")
  print(x$formula)
  cat("\nmeasure.vars: ", paste(x$measure.vars, collapse=", "))
  cat("\nmeasure: ", paste(x$measure , collapse=", "))
  cat("\nmeasure.class: ", paste(x$measure.class , collapse=", "))
  cat("\ndigits: ", paste(x$digits, collapse=", "))
  cat("\nrow_name: ", paste(x$row_name, collapse=", "))
cat("\nby: ")
print(x$by)
  cat("\ngroup.vars: ", paste(x$group.vars, collapse=", "), "\n")
  
#  cat("\ncol_name: ", paste(x$col_name, collapse=", "),"\n")
  print(head(x$data))
}




# #test_prepare_data2 <- function()
#  # {
#  
#
# prepare_data2(varana, m1, m2, m3, m4)
# prepare_data2(~ m1 + m2 + m3 + m4, varana)
# prepare_data2(~ log(m1) + m2 + m3 + m4, varana)
# prepare_data2(varana, 4:7)
#
# prepare_data2(varana, m1[1], m2, m3, m4)
# prepare_data2(varana, m1[1], m2, m3[4,median], m4)
# prepare_data2(varana, m1 , m2, m3=median, m4)
# prepare_data2(~ m1[1] + m2 + m3 + m4, varana)
# prepare_data2(~ m1[1] + m2 + m3[4,median] + m4, varana)
#
# prepare_data2(varana, m1, m2, m3 , m4, by =  ~ geschl)
# prepare_data2(varana, m1[4, median], m2, m3 , m4[5], by =  ~ geschl)
#
#
# prepare_data2(varana, m1 = median, m2, m3 , m4, by =  ~ geschl)
#
#
#
#
# prepare_data2(~ m1 + m2 + m3 + m4, varana)
# prepare_data2(~ log(m1) + m2 + m3 + m4, varana)
# #}
#




