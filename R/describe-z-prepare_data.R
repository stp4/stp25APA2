#' @rdname prepare_data
#' @title Daten und Formula-Objekte
#' @description  Hilfsfunktionen zum Aufbereiten der Daten.
#' @name prepare_data
#'
NULL


#' @rdname prepare_data
#' @description Formula_Data: Funktion zum aufbereiten der formula. Output List mit neuen Daten.
#' Die Funktion Formula kan nur \code{Formula(y1 +y2 +y3 ~ .)} aufarbeinten aber nicht
#' \code{Formula(. ~ y1 +y2 +y3  )} daher wird über \code{clean_dots_formula()} dir Formel vorbereitet
#' @param  x   Formel
#' @param  data Daten als data.frame
#' @param  subset geht mit data$variable
#' @param  na.action nicht aendern default ist na.pass
#' @param  ... nicht benutzt
#' @return Formula_Data: list mit "X_data","Y_data","xname","yname",
#' "Z_data",  "zname", "formula", "condition", "formula.orginal", "digits", "type"
#' @export
#' @examples
#' names(Formula_Data(m1[3]+m2~geschl, varana))
#'
Formula_Data<- function(x,
                        data,
                        subset,
                        na.action=na.pass,
                        ...){
fm <- Formula_Names(x, data)
#print( fm$condition )
if(is.null(fm$condition)) {
  data_condition<- NULL
  zname <-NULL
  condition<- NULL
  }
else {
  zname <- all.vars(fm$condition)
  condition<- fm$condition
  data_condition <- data[zname] #1 #which(names(data) %in% )
  }
#formula<- fm$formula
formula <- Formula::Formula(fm$formula)
# all.vars(s~a+g)
 if( !check_data(data, all.vars(formula)) )
 {
   print(names(data))
   print(all.vars(formula))
   warning("Namen und Daten passen nicht zusammen oder es gibt NA's")}

if (!missing(subset) && length(subset))
      data <- stats::model.frame(formula,
                                 data = data,
                                 subset = subset,
                                 na.action = na.action)

else data <- stats::model.frame(formula,
                                data = data,
                                na.action =
                                na.action)

 X_data <- Formula::model.part(formula, data = data, rhs = 1)
 Y_data <- Formula::model.part(formula, data = data, lhs = 1)

 xname  <- names(X_data)
 yname  <- names(Y_data)

 if ( !length(yname) ){#-- Einzelvergeich
        #-  kein Y_data und wir werten ueber Y_data aus daher
        Y_data <- X_data
        yname  <- names(Y_data)
        xname  <- NULL
        X_data <- NULL

 }



 # if (is.null(xname)) {
 #   by <- "1"
 #   data <-   Y_data
 #   group.class<- NULL
 #
 # }
 # else{
 #    data=cbind(X_data, Y_data)
 #    group.class <- sapply( X_data,  function(x)  setdiff(class(x), "labelled"))
 #    by <- formula(paste("~", paste(xname, collapse="+")))
 #
 # }
 #row_name <- stp25aggregate::GetLabelOrName(Y_data)
 #measure.class <- sapply( Y_data,  function(x)  setdiff(class(x), "labelled"))
 #measure <- rep.int("default", length(yname))






 list( X_data=X_data,
       Y_data=Y_data,
       xname=xname,
       yname=yname,
       Z_data=data_condition,
       zname=zname ,
       formula=formula,
       condition=condition,
       formula.orginal=x,
       digits=fm$digits,
       type=fm$type


#       data = data,                 # X_data, Y_data, Z_data=data_condition
 #      measure.vars = yname,  # xname
  #     group.vars = xname,      # yname
   #    condition.vars=zname,          # zname
    #   formula=formula,
     #  by=by,                        # an berechne default
      # measure = measure,
      # row_name = row_name,
      # measure.class = measure.class,
     #  group.class = group.class,
    #   condition.class = NULL

       )
}


#' @rdname prepare_data
#' @description Data_Vars: Funktion zum aufbereiten der Daten Output List mit neuen Daten.
#' @param  .data Daten als data.frame
#' @param  ... Variablen
#' @param  by Gruppen meist als formula ~ x
#' @return  list  data, measure.vars, group.vars, by, measure,
#' row_name, measure.class, group.class
#' @export
#' @examples
#'
#' varana2<- head(varana)
#' varana2 %>% Data_Vars(. ~ .)
#' varana2 %>% Data_Vars(alter, m1, m2, by =  ~ geschl)
#' varana2 %>% Data_Vars(alter, m1, m2, by = "geschl")
#'
#' varana2 %>% Data_Vars(1, 2, 3)
#' varana2 %>% Data_Vars(1:3)
#'
Data_Vars <- function(.data, ..., by = "1") {



  get_classes <-
    function(data)
      sapply(data, function(x)
        setdiff(class(x), "labelled"))

  # 1 ... auslesen
  measure <-
    sapply(lazyeval::lazy_dots(...), function(x) {
      as.character(x[1])
    })
cat("\nmeasure\n")
    print(measure)
    cat("\n")
  # 2 schauen ob was daherkommt - Alle auswerten mit Tabelle(data, .~.)
  if (length(measure) == 0){
    measure <- names(.data)
  } else if (length(measure) == 1 & grepl("~", measure[1])) {
    measure <- names(.data)
  }else if (any(grepl("\\[", measure))) {#  digits ##Data_Vars(varana, m1[4], m2, m3 , m4)
    if (by == "1")
      fm <- paste("~", paste(measure, collapse = "+"))
    else {
      if (stpvers::is_formula2(by))
        fm <- paste(paste(measure, collapse = "+"),"~",
                paste(all.vars(by), collapse = "+"))
      else
        fm <- paste(paste(measure, collapse = "+"), "~",
                paste(by, collapse = "+"))
    }

    return(Formula_Data(formula(fm), .data))
  }


  # 3 teste measure auf Zahlen
  #



  measure <-
    makeNamesNum(measure, .data) # meAsNum <- grepl("^[[:digit:]]", measure)

  if (length(names(measure)) == 0)
  { measure.vars <- measure
    measure <- rep("default", length(measure.vars) )
  }
  else
  {  measure.vars <- ifelse(names(measure) == "", measure, names(measure))
     measure <- ifelse(names(measure) == "", "default", measure)
  }


  check_data_result <-
    if (by == "1")
      check_data(.data, measure.vars)
  else if (stpvers::is_formula2(by))
    check_data(.data, c(measure.vars, all.vars(by)))
  else
    check_data(.data, c(measure.vars, by))

  if (!check_data_result)
    return(head(.data))



  measure.class <-  get_classes(.data[measure.vars])
  row_name <- stp25aggregate::GetLabelOrName(.data[measure.vars])

  fm<-paste(measure.vars, collapse="+")

  if (by == "1") {
    .data <-  .data[measure.vars]
    group.vars <- NULL
    group.class <- NULL
    fm <- formula(paste("~", fm))
  }
  else if (stpvers::is_formula2(by)) {
    group.vars <- all.vars(by)
    .data <-  .data[c(measure.vars, group.vars)]
    group.class <- get_classes(.data[group.vars])

    fm <- formula(paste(fm,"~", paste(group.vars, collapse="+") ))
  }
  else {
    group.vars <- by
    .data <- .data[c(measure.vars, by)]
    group.class <- get_classes(.data[group.vars])
    by<- formula(paste( "~", paste(by, collapse = "+")))
    fm <- formula(paste(fm, "~", paste(by, collapse = "+")))
  }


  # list(
  #   data = .data,
  #   measure.vars = measure.vars,
  #   group.vars = group.vars,
  #   by = by,
  #   measure = measure,
  #   row_name = row_name,
  #   measure.class = measure.class,
  #   group.class = group.class
  # )


  list(
    data = .data,                 # X_data, Y_data, Z_data=data_condition
    measure.vars = measure.vars,  # xname
    group.vars = group.vars,      # yname
    condition.vars=NULL,          # zname
    formula=fm,
    by=by,                        # an berechne default
    measure = measure,
    row_name = row_name,
    measure.class = measure.class,
    group.class = group.class,
    condition.class = NULL,
    digits=NULL
  )




}








#' @rdname prepare_data
#' @description Formula_Names: Namen und Formula-Objekte vorbereiten.
#' @return list mit den Items yname fuer das was vorne steht
#' und xname fuer das was hinten stehtlist(yname, xname, zname, formula, condition, digits, type)
#' @export
#' @examples
#' names(Formula_Names(m1[3]+m2~geschl, varana))
#'
Formula_Names<- function(x, data=NULL, ... ){
  #- default einstellungen
  digits_pos <- options()$stp25$apa.style$mittelwert$digits
  type<- "auto"
  frml <- formula_split(x)
 # print(frml)
  x<- frml$formula
  #- Formel vom Type 'a+b[2]~c' kann auch  'a+b[2] ~ . ' sein
  if (any(all.names(x[[2L]]) %in% '[')) {
    y_names_vars <- all.vars(x[[2L]])
    #hier war ein Fehler wegen  width.cutoff = 60L
    y_hsd<-  strsplit(deparse(x[[2L]],
                              width.cutoff = 500L), " \\+ ")[[1]]

    y_names <-gsub("\\[.+\\]", "", y_hsd) # bereinigen von Klammern

    if(length(x)==2){
      x <-  formula(paste("~",
                          paste(y_names, collapse="+")))
    }else{
      x_hsd<-  strsplit(deparse(x[[3L]]), " \\+ ")[[1]]
      x_names <-gsub("\\[.+\\]", "", x_hsd)

      x <- formula(paste(
        paste(y_names, collapse="+")
        ,"~",
        paste(x_names, collapse="+")))
    }

    pos <- grep('\\[',y_hsd)

    stat<-gsub("[^[:alpha:]]", "",
               stringr::str_extract(y_hsd[pos],
                                    ",.+"))

    dig <-as.integer(gsub("[^0-9]", "",
                          stringr::str_extract(y_hsd[pos],
                                               "\\[.")))

    digits_pos <- lapply(1:length(y_names), function(j) digits_pos)
    type<- lapply(1:length(y_names), function(j) type)

    # print(stat)
    for (i in seq_len(length(pos))){
      digits_pos[[pos[i]]] <- rep(dig[i], 2)
      type[[pos[i]]] <-  stat[i]
    }
  } #- end

  x <-  clean_dots_formula(x, data)

  ##  mf input: formula output: formula
  if (length(x)==2)
    list(yname = all.vars(x[[2L]]),
         xname = NULL,
         zname = all.vars(frml$condition),
         formula=x,
         condition=frml$condition,
         digits =digits_pos,
         type=type) #unlist(strsplit(gsub(" ", "",  deparse(x[[2L]])), split = "\\+")),
  else if((length(x)==3))
    list(yname = all.vars(x[[2L]]),
         xname = all.vars(x[[3L]]),
         zname = all.vars(frml$condition),
         formula=x,
         condition=frml$condition,
         digits =digits_pos,
         type=type)
  # list(yname = unlist(strsplit(gsub(" ", "", deparse(x[[2L]])), split = "\\+")),
  #     xname = unlist(strsplit(gsub(" ", "", deparse(x[[3L]])), split = "\\+")))
  else {
    warning("Weis nicht was tun - sorry!")
    list(yname=NULL, xname=NULL, zname=NULL, formula=x, condition=frml$condition,
         digits =digits_pos, type=type)
  }
}

#' @rdname prepare_data
#' @param formula Formula
#' @description formula_split stolen from mosaic ggformula
#' @return formula_split: liste  formula, condition, facet_type
#'
formula_split <- function(formula) {
  # split A | B into formula <- A; condition <- B
  fs <-
    stringr::str_split(deparse(formula), "\\|")[[1]]
  # try to split, else leave formula unchanged and set condition to NULL
  if ( (length(fs) != 2) ||
       ! tryCatch({
         formula_string <- fs[1]
         condition_string <- fs[2]
         if (! grepl("~", condition_string)) {
           condition_string <- paste0("~", condition_string)
           condition <- as.formula(condition_string, env = environment(formula))
           facet_type <- "facet_wrap"
         } else {
           condition <- as.formula(condition_string, env = environment(formula))
           facet_type <- "facet_grid"
         }
         formula <- as.formula(formula_string, env = environment(formula))
         TRUE
       }, error = function(e) {warning(e); FALSE}
       )
  ) {
    condition <- NULL
    facet_type <- "none"
  }
  list(formula = formula, condition = condition, facet_type = facet_type)
}




#' @rdname prepare_data
#' @description clean_dots_formula: Formel bereinigen
#' @return clean_dots_formula: formula - Objekt
#' @examples
#'
#' data <- data.frame(x = NA, y = NA, z = NA)
#' clean_dots_formula(x ~ y, data)
#' clean_dots_formula(. ~ x + y, data)
#' clean_dots_formula(x + y ~ ., data)
#' clean_dots_formula(~., data)
clean_dots_formula <- function(formula,
                               data = data,
                               names_data = names(data)
                               ) {

myvars <- all.vars(formula)
if (any(myvars %in% ".")) {
        if (length(myvars) == 1) {
            return(formula(paste(" ~ ", paste(names_data, collapse = "+"))))
        } else if (myvars[1] == ".") {
            var_dots <- names_data[!names_data %in% myvars[-1]]
            return(formula(paste(paste(var_dots, collapse = "+"),
                                 " ~ ", paste(myvars[-1], collapse = "+"))))
        } else if (myvars[length(myvars)] == ".") {
            var_dots <- names_data[!names_data %in% myvars[-length(myvars)]]
            return(formula(paste(paste(myvars[-length(myvars)],
                                       collapse = "+"), " ~ ", paste(var_dots, collapse = "+"))))
        }

} else {
        return(formula)
    }

}




#' @rdname prepare_data
#' @description makeFormula: Formel erstellen
#' @param  measurevar Variable  in makeFormula
#' @param groupvars Variable in makeFormula
#' @return makeFormula: formula - Objekt
#' @examples
#'
#' makeFormula("a", "b")
#' makeFormula("a", c("b","c"))
#' makeFormula("a", ~b+c)
#' makeFormula(c("a", "d"), c("b","c"))
#'
makeFormula<- function(measurevar, groupvars ){

  if (stpvers::is_formula2(groupvars)) groupvars<-paste0(all.vars(groupvars), collapse = "+")
  else groupvars <- paste0(groupvars, collapse = "+")


  if (stpvers::is_formula2(groupvars)) {
    measurevar<- all.vars(measurevar)
    if(length(measurevar)!=1)
      measurevar<-  paste("cbind(", paste0(measurevar, collapse = ", "), ")")
  }
  else {
    if(length(measurevar)!=1)
      measurevar<- paste("cbind(", paste0(measurevar, collapse = ", "), ")")
  }

  formula(paste(measurevar, "~", groupvars))
}




#' @rdname prepare_data
#' @description check_data: prueft ob variablen vorhanden sind bzw ob Faelle NA sind.
#' @param  vars in check_data
#' @return check_data: Logical und wen FALSE ueber cat die  Objekt die falsch sind
#' @examples
#'
#' check_data(varana, c("m1", "m2") )
#' check_data(varana, c("m1", "sex") )
#'
#
check_data <- function (data, vars = NULL)
{
  if (!is.null(vars)) {
    vars_match <- names(data) %in% vars
    names_match <- names(data)[vars_match]

    #  print( vars  %in% names_match)
    if (all(vars %in% names_match)) {
      gibts_na <- sapply(data[vars],   function(x)
        any(!is.na(x)))
      if (all(gibts_na))
        TRUE
      else {
        cat("\nInput: ", vars, "\nVorhanden mit NA: ", vars[!gibts_na], "\n")
        FALSE
      }
    }
    else{
      cat("\nInput: ", vars, "\nVorhanden: ", names_match, "\n")
      FALSE
    }
  }
  else  {
    TRUE
  }
}









#' @rdname prepare_data
#' @description makeNamesNum: aus Nummern die Namen extrahieren
#' @param  data Daten als data.frame
#' @param  measure Variablen
#' @param  meAsNum  logical welche sind Zahlen
#' @return   string( )
#' @examples
#'
#' measure <- c("geschl", "1" , "3:5", 1)
#'  makeNamesNum(measure,  data=varana)
#'
makeNamesNum <- function(measure, data,
                         meAsNum=grepl("^[[:digit:]]", measure))
{
  if (sum(meAsNum) == 0) return(measure)
  measure_number <- NULL
  for (i in seq_len(length(meAsNum))) {
    if (meAsNum[i]) {
      if (grepl("[^[:digit:]]", measure[i])) {
        n <- stringr::str_split(measure[i], ":", 2)

        measure_number <- c(measure_number,
                            names(data)[seq(n[[1]][1], n[[1]][2])])
      }
      else
        measure_number <-
          c(measure_number, names(data)[as.numeric(measure[i])])
    }
    else
      measure_number <- c(measure_number, measure[i])
  }
  unique(measure_number)
}





