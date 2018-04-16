#' @name Tabelle
#' @rdname Tabelle
#' @title Tabelle
#' @description  Einfache deskriptive Tabelle die in medizinischen Arbeiten
#' verwendet werden.
#' Die Funktion arbeitet Intern mit \code{aggregate} bzw. mit  berechne.default() also aggregate(formula, data,FUN).
#' @return Tabelle: data.frame oder list mit data.frame
#' Tabelle2: HTML
#' @author Wolfgang Peter
#' @export
#' @examples
#'
#' library(dplyr)
#' library(tidyr)
#' #names(varana)
#' #  set_my_options(mittelwert=list(median.style="Quantil")) # ++IQR
#' varana2 <- varana %>%
#'   gather(Zeit, Merkfgk, m1:m4) %>%
#'   mutate(Zeit = factor(Zeit, Cs(m1, m2, m3 , m4), Cs(t0, t1, t2, t3))) %>%
#'   Label(Merkfgk = "Merkfaehigkeit")
#'
#' Tabelle(Merkfgk ~ Zeit, varana2)
#'
#' varana2 %>% Tabelle(Merkfgk, by =  ~ Zeit)
#' varana %>% Tabelle(m1, m2, m3 , m4)
#'
#' #- Achtung hier wird eine Liste Ausgegeben
#' varana %>% Tabelle(
#'   4:7,
#'   by =  ~ geschl,
#'   fun = function(x)
#'     c(
#'       n = length(na.omit(x)),
#'       m = mean(x),
#'       sd = sd(x)
#'     )
#' )
#'
Tabelle <- function(...) {
  UseMethod("Tabelle")
}

#' @rdname Tabelle
#' @description Tabelle2:  html-Output Tabelle(...) %>% Output()
#' @export
#' @examples
#' varana2 %>% Tabelle2(Merkfgk, by=~ Zeit)
Tabelle2 <- function(...) {
  x <- Tabelle(...)
  Output(x)
  invisible(x)
}


#' @rdname Tabelle
#'
#' @param ...   Die auszuwertenden Variablen  sex, age="mean", usw
#' @param type 1 oder 2 1 ist kurzes Format 2 int lang
#' @param formula An dcast Gruppe ~ .id ist zum Zeilen und Spalten vertauschen
#' @param fun Eigene Function am Berechne
#' @param digits Kommastellen
#' @param caption,note Uberschrift an Output
#'  
#' @param test,include.test  Signifikanz Test include.test= c(TRUE, "t.test","aov","wilcox.test","kruskal.test")
#' @param na.action,exclude an Formula
#'   
#' @param include.n,include.nr,include.total Anzahl ausgeben
#'  
#' @param max_factor_length Fehler bei langen Faktoren abfangen
#'  
#' @param APA APA2 Style TRUE/FALSE
#'
#' @export
Tabelle.default <- function(...,
                            formula = NULL,
                            fun = NULL,
                            type = c(
                              "2",
                              "1",
                              "auto",
                              "freq",
                              "mean",
                              "median",
                              "ci",
                              "multiresponse",
                              "cohen.d",
                              "effsize",
                              "freq.ci",
                              "describe", "correlation"
                            ),
                            caption = "Charakteristik",
                            note = "",
                            
                            digits = NULL,
                            APA = FALSE,
                            test = FALSE,
                            na.action = na.pass,
                            exclude = NA,
                            
                            include.n = TRUE,
                            include.nr = FALSE,
                            include.total = FALSE,
                            
                            include.test = test,
                            
                          #  include.p = TRUE,
                           # include.stars = FALSE,
                          #  include.mean=FALSE,  # fuer Correlation
                          #  corr_test = "pearson",
                          #  cor_diagonale_up = TRUE,
                            max_factor_length = 35#,
                          #  order = FALSE,
                          #  decreasing = FALSE,
                         #   useconTest = FALSE,
                           # normality.test = FALSE
                          ) {
  type <-  match.arg(type, several.ok = TRUE)[1]
  if (APA) {
    
  #  cat("\ninAPA=TRUE")
    errate_statistik3(
      ...,
      type = type,        # "multiresponse"
      caption = caption,
      note = note,
    #  digits = digits,    
    #  test = test,        
      na.action = na.action,
      exclude = exclude,
      
      include.n = include.n,
      include.nr = include.nr,
      include.total = include.total,
      include.test = include.test, # "wilcox.test","u.test",  "kruskal.test","h.test",  
                                   # "chisq.test","t.test",  "aov", "anova", 
                                   # "SPSS", "Hmisc" 
                                   #         "shapiro.test" "KS.test"
                                  #  Kolmogorov-Smirnov-Anpassungstest mit SPSS
   #   include.p = include.p,
   #   include.stars = include.stars,
    #  include.mean=include.mean,
   #   corr_test = corr_test,
    #  cor_diagonale_up = cor_diagonale_up,
      max_factor_length = max_factor_length#,
    #  order = order,
    #  decreasing = decreasing,
   #   useconTest = useconTest,
    #  normality.test = normality.test
    )
  }
  else if(type=="correlation"){
    
    stop("Benutze Bitte die Funktion APA_Correlation()!")
  }
  else
  {
    if (is.null(formula)) {
      calculate_tabelle2(
        prepare_data2(...),
        type = type,
        # nur ein Type erlaubt
        caption = caption,
        note = note,
        fun = fun,
        digits = digits
      )
    }
    else {
      res <- calculate_tabelle2(
        prepare_data2(...),
        type = type[1],
        caption = caption,
        note = note,
        fun = fun,
        digits = digits
      )
      prepare_output(reshape2::dcast(res,
                                     formula, function(x) {
                                       paste(x, collapse = "|")
                                     }),
                     caption, note)
    }
  }
}








#' @rdname Tabelle
#' @export
Describe <-  function(x, ...) {
  # @importFrom dplyr tbl_df
  fix_format(broom::tidy(x))
}



# Mittelwerte -------------------------------------------------------------

#' @rdname Tabelle
#' @export
Tabelle.lm <- function(x,
                       digits = 2,
                       fun = function(x) {
                         c(
                           n = length(x),
                           M = mean(x, na.rm = TRUE),
                           SD = sd(x, na.rm = TRUE)
                         )
                       }) {
  res_list <- NULL
  myeff <- effects::allEffects(x)
  
  for (i in names(myeff)) {
    info <- model_info(myeff[[i]])
    ans <- aggregate_effect(myeff[[i]], info$y, info$x, fun)
    
    AV <- ifelse(is.na(info$labels[info$y]),
                 info$y, info$labels[info$y])
    
    ans <-
      data.frame(plyr::llply(ans, function(x)
        if (is.numeric(x))
          round(x, digits)
        else
          x),
        stringsAsFactors=FALSE)
    res_list[[i]] <- prepare_output(ans,
                                    paste0("AV: ", AV), "",
                                    info$N,  info$labels)
  }
  res_list
}




aggregate_effect <- function(eff,
                             y,
                             x,
                             fun = function(x)
                               length(x)) {
  fm <- formula(paste(y, "~", paste(x, collapse = "+")))
  df <- eff$data
  #-- Faktoren fuer N berechnung vorbereiten
  for (j in names(eff$variables)) {
    if (!eff$variables[[j]]$is.factor)
      df[, j] <- cut(df[, j], length(eff$variables[[j]]$levels))
  }
  
  res <- try(aggregate(fm, df, fun, drop = FALSE))
  if (class(res) == "try-error")
    data.frame(NULL)  #  wegen ncol im weiteren progammverlauf
  else
    do.call(data.frame, res)
}


calculate_tabelle2 <- function(X,
                               type,
                               # wie soll Ausgewertet werden Mittelwerte: type=3, type="mean"
                               caption,
                               note,
                               fun = NULL
                               ,
                               digits = digits) {
  res <- NULL
  
  for (i in seq_len(length(X$measure))) {
    # if (X$measure[i] == "default")
    #  X$measure[i] <- X$measure.class[i]
    
    #-- Labels ----------------------------------
    if (type == 3 | type == "mean") {
      # cat("\nBerechne Mittelwerte\n")
    } else{
      if (X$measure[i] == "factor")
        X$row_name[i] <- paste0(X$row_name[i], " (",
                                paste0(levels(X$data[[X$measure.vars[i]]]), collapse = "/"), ")")
      else if (X$measure[i] == "median")
        X$row_name[i] <- paste0(X$row_name[i], " (median)")
      else
        X$row_name[i] <- paste0(X$row_name[i], " (mean)")
      
    }
    
    res[[X$measure.vars[i]]]  <-
      berechne.default(
        X$data,
        X$measure.vars[i],
        X$by,
        X$measure[i],
        type,
        fun = fun,
        digits =  if (is.null(digits))
          X$digits[i]
        else
          digits,
        measure.name = "value"
      )
    
  }
  
  df <- plyr::ldply(res)
  df[, 1] <- factor(df[, 1], names(X$row_name), X$row_name)
  
  prepare_output(df, caption, note, nrow(X$data), NA)
}


Describe2 <- function(fml,
                      data,
                      caption = "",
                      note = "",
                      stat = c("n", "mean", "sd", "min", "max"),
                      #  na.rm = TRUE,
                      ...) {
  vars <- which(names(data) %in% all.vars(fml))
  data <- data[vars]
  result <-  as.data.frame(
                    psych::describe(data),
                        stringsAsFactors=FALSE)[stat]
 result[-1] <- Format2(result[-1])
  prepare_output(cbind(Item = GetLabelOrName(data), result),
                 caption,
                 note,
                 nrow(data))
}
