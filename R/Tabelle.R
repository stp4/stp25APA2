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






# @rdname Tabelle
# @examples
# 
# \dontrun{
# prepare_data2(m1+m2~m3 , varana)
# corr_tabel_X(~m1+m2, varana)
# corr_tabel_X(m1+m2~m3, varana)
# corr_tabel_X(m1+m2~m3|geschl, varana)
# }
# corr_tabel_X <-
#   function (...,
#             type = c("pearson", "spearman"),
#             exclude = NA,
#            # subset,
#             na.action = na.pass,
#             cor_diagonale_up = TRUE,
#             stars = TRUE,
#             p.value = FALSE,
#             include.mean = FALSE,
#             include.n = TRUE
#            )
#   {
#     type <-  match.arg(type)
#     #-- Vorbereiten der Daten (na.omit, subset)
#     ANS <- NULL
#     X <- prepare_data2(..., na.action = na.action)
#     # return(X )
#     N <- nrow(X$data)
#     measure.vars <- X$measure.vars
#     group.vars <- X$group.vars
#     row_name <- X$row_name
#     Y_data <- X$data[measure.vars]
#     X_data <- X$data[group.vars]
#     
#     condition.vars <- X$condition.vars
#     cat("\n corr_tabel_X")
#     print(measure.vars)
#     print(group.vars)
#     ## wenn nur ein element dann ergibt sich ein Fehler
#     if (ncol(X$data[measure.vars]) == 1)
#       X$data[measure.vars]  <-
#       dplyr::as_data_frame(X$data[measure.vars])
#     
#     
#     if (!is.null(condition.vars)) {
#       cat("\n in  (condition.vars)")
#       #groups <- all.vars(groups)
#       if (length(condition.vars) == 1) {
#         condition <- X$data[[condition.vars]]
#         if (X$condition.class  != "factor") {
#           warning("Achtung nur eine Faktor kann Gruppen bilden!")
#           return(head(X$data))
#         }
#         
#         condition <- droplevels(condition)
#         # return(condition)
#         lvls <- levels(condition)
#         g1 <- which(condition == lvls[1])
#         
#         # vor corr_2"
#         
#         
#         ans <- stp25APA2:::corr_2(Y_data[g1,], X_data[g1, 1], type)
#         names(ans)[2:4] <- paste0(lvls[1], "_", names(ans)[2:4])
#         
#         for (i in 2:(length(lvls))) {
#           g2 <- which(condition == lvls[i])
#           ans2 <-
#             stp25APA2:::corr_2(Y_data[g2,], X_data[g2, 1], type)
#           names(ans2)[2:4] <-
#             paste0(lvls[i], "_", names(ans2)[2:4])
#           ans <- cbind(ans, ans2[-1])
#         }
#         
#       }
#       else{
#         return("Achtung nur eine Gruppe kann berechnet werden!")
#       }
#       ans[, 1] <- factor(ans[, 1], names(row_name), row_name)
#       attr(ans, "note") <- type
#       attr(ans, "N") <- N
#     }
#     else if (!is.null(group.vars)) {
#       cat("\n mit Gruppe")
#       # Beginn der Funktion -----------------------------------------------------
#       ans <- corr_2(Y_data, X_data[1], type)
#       
#       if (length(X$xname) > 1) {
#         names(ans)[2:4] <- paste0(group.vars[1], "_", names(ans)[2:4])
#         for (i in  2:(length(group.vars))) {
#           ans2 <-  corr_2(Y_data, X_data[i], type)
#           names(ans2)[2:4] <-
#             paste0(group.vars[i], "_", names(ans2)[2:4])
#           ans <- cbind(ans, ans2[-1])
#         }
#         attr(ans, "note") <- type
#         attr(ans, "N") <- N
#       }
#       ans[, 1] <- factor(ans[, 1], names(row_name), row_name)
#     }
#     else{
#       cat("\n else  mit Gruppe")
#       ans <- corr_1(Y_data, type = type) ## liste
#     #  print(ans)
#       ans$mean <-
#         t(berechne.default(Y_data , 
#                            measure.vars, 
#                            measure = "mean", 
#                            type = 1,
#                            measure.name = "value"))
#       
#       ans$row_name <- row_name
#     }
#     ordne_corr_tabel(ans)
#   }


# 
# ordne_corr_tabel <- function(ans,
#                              cor_diagonale_up = TRUE,
#                             stars = TRUE,
#                              p.value = FALSE,
#                              # mean = FALSE, # Veraltet
#                              include.mean = FALSE,
#                              include.n = TRUE)
# {
#   #cat("\ninclude.mean =", include.mean, "\n")
#   res <- data.frame()
#   if (class(ans) == "rcorr") {
#     #-- Hmisc also wie aus APA2
#     format_diagonale <- function(mycorrtable,
#                                  d = 1,
#                                  l = "") {
#       diag(mycorrtable) <- d
#       if (cor_diagonale_up)
#         mycorrtable[lower.tri(mycorrtable)] <- l
#       else
#         mycorrtable[upper.tri(mycorrtable)] <- l
#       mycorrtable
#     }
#     n <- Format2(ans$n, 0)
#     # colnames(n)<- paste0(colnames(n), "_", "n" )
#     r <- format_diagonale(Format2(ans$r, 2))
#     
#     if (stars) {
#       p <- apply(ans$P, 2, function(x)
#         cut(
#           x,
#           c(-Inf, options()$stp25$apa.style$p$stars.value, Inf),
#           c(options()$stp25$apa.style$p$stars.symbols, "")
#         ))
#       
#       r <- format_diagonale(matrix(
#         paste0(Format2(ans$r, 2), p),
#         nrow = nrow(ans$r),
#         dimnames = dimnames(ans$r)
#       ))
#     }
#     if (p.value) {
#       p <- paste0(" (p=", ffpvalue(ans$P), ")")
#       r <- format_diagonale(matrix(
#         paste0(Format2(ans$r, 2), p),
#         nrow = nrow(ans$r),
#         dimnames = dimnames(ans$r)
#       ))
#     }
#     
#     res <- data.frame(Source = rownames(ans$r), r)
#     attr(res, "note") <-  attr(ans, "note")
#     attr(res, "N") <-  attr(ans, "N")
#     my_num <- paste0("(", 1:length(ans$row_name), ")")
#     
#     res[, 1] <-
#       factor(res[, 1], names(ans$row_name), paste(my_num, ans$row_name)) # Labels
#     colnames(res)[2:ncol(res)] <- my_num
#     
#     if (!include.n) {
#       cat("\n", include.n, "\n\n-----\n")
#       # grep("_N", c("jdhdz", "hdgdt_N", "sfsg_"))
#       #ans<- ans[- grep("_N", names(ans))]
#     }
#     
#     if (include.mean)
#       res <- cbind(res[1], "M (SD)" = ans$mean, res[2:ncol(res)])
#     return(res)
#   }
#   else{
#     return(ans)
#   }
# }
# 
# 
# 
# 


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
