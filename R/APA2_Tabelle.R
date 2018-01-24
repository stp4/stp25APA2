#' @name Tabelle
#' @rdname Tabelle
#' @title Tabelle
#' @description  Einfache deskriptive Tabelle die in medizinischen Arbeiten
#' verwendet werden.
#' Die Funktion arbeitet Intern mit \link{aggregate} bzw  berechne.default()  und ist in
#' kombination mit dem \code{pipe} Operator gedacht.
#' Im gegensatz zur alten \link{APA2} Funktion werden nur deskriptive
#' Statistiken berechnet und jeder Parameter steht in einer Zeile.
#' @return Tabelle: data.frame mit value value ist dabei entwerder ein
#' String (type=1 oder 3) oder eine Zahl
#' Tabelle2: entweder HTML oder text
#' @author Wolfgang Peter
#' @export
#' @examples
#' #'
#' library(dplyr)
#' library(tidyr)
#' names(varana)
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
Tabelle <- function(...
  #x, ..., by = "1",
                    #formula = NULL, fun=NULL,
                   # type=2, caption="Charakteristik", note=""
                    ) {
  UseMethod("Tabelle")
}

#' @rdname Tabelle
#' @description Tabelle2:  Tabelle(...) %>% Output()
#' @export
#' @examples
#' varana2 %>% Tabelle2(Merkfgk, by=~ Zeit)
Tabelle2 <- function(...) {
  x <- Tabelle(...)
  Output(x)
  invisible(x)
}


#' @rdname Tabelle
#' @param .data  Daten data.frame
#' @param ...   Die auszuwertenden Variablen  sex, age="mean", usw
#' @param type 1 oder 2 1 ist kurzes Format 2 int lang
#' @param caption,note Wir in attribute gespeichert Ueberschrift und Note.
#' @param formula An dcast Gruppe ~ .id ist zum Zeilen und Spalten vertauschen
#' @param fun Eigene Function am Berechne
#' @param digits Kommastellen
#' @param APA APA2 versuion
#' @export
Tabelle.default <- function(...,
                            formula = NULL, fun=NULL,
                            type = c("2", "1",   "auto", "freq", "mean", "median",
                                   "ci", "multiresponse", "cohen.d", "effsize",
                                   "freq.ci", "describe"),
                            caption = "Charakteristik", note = "",

                            digits=NULL,
                            APA=FALSE,
                            test = FALSE,
                            na.action = na.pass,
                            exclude = NA,

                            include.n = TRUE,
                          #  include.all.n = FALSE,
                            include.nr = FALSE, # formals include.header.n
                            # print.n,
                          #  include.header.n = TRUE,
                            include.total = FALSE,
                            # total,
                            include.test = test,
                            # test,
                            include.p = TRUE,
                            include.sig.star = FALSE,
                            corr_test = "pearson",
                            cor_diagonale_up = TRUE,
                            max_factor_length = 35,
                            order = FALSE,
                            decreasing = FALSE,
                            useconTest = FALSE,
                            normality.test = FALSE

                            ) {
  type <-  match.arg(type, several.ok = TRUE)

  if(APA){

    errate_statistik3(..., type=type,
                      caption=caption, note=note,
                      digits=digits,
                      test=test,
                      na.action=na.action,
                      exclude=exclude,

                      include.n=include.n,
                    #  include.all.n =include.all.n,
                      # print.n,
                      include.nr=include.nr,
                      include.total=include.total,
                      # total,
                      include.test=include.test,
                      # test,
                      include.p=include.p,
                      include.sig.star=include.sig.star,
                      corr_test=corr_test,
                      cor_diagonale_up=cor_diagonale_up,
                      max_factor_length=max_factor_length,
                      order=order,
                      decreasing=decreasing,
                      useconTest=useconTest,
                      normality.test=normality.test
                      )


  }
  else
 { if (is.null(formula)) {
    calculate_tabelle2(
      prepare_data2(...),
      type=type[1], # nur ein Type erlaubt
      caption=caption, note= note, fun=fun, digits=digits

    )
  }
  else {

    res <- calculate_tabelle2(
      prepare_data2(...),
      type = type[1],
      caption = caption,  note = note,
      fun = fun,
      digits = digits
    )
    prepare_output(
      reshape2::dcast(res,
                      formula, function(x) {
                        paste(x, collapse = "|")
                      }),
      caption, note)




  } }

}


 # if(is.null(X)) X <- Data_Vars(.data, ..., by = by)

  # if (is.null(formula)) {
  #   calculate_tabelle(X, type=type, caption=caption, note= note, fun=fun, digits=digits)
  # }
  # else {
  #   reshape2::dcast(
  #     calculate_tabelle(X, type=type, caption=caption, note= note, fun=fun, digits=digits),
  #   formula, function(x) {
  #     paste(x, collapse = "|")
  #   })
  # }


#     measure <-
#       sapply(lazyeval::lazy_dots(...), function(x) {
#         as.character(x[1])
#       })
#
#     #- Alle auswerten mit Tabelle(data, .~.)
#     if(length(measure)<2){
#       if( length(measure)==0) measure<- names(.data)
#       else if(grepl("~", measure[1])) measure<- names(.data)
#       else {}
#      }
# #print(measure)
#
#   measure.vars <- ifelse(names(measure) == "", measure, names(measure))
#   check_data_result <-  if (by=="1") check_data(.data, measure.vars)
#                           else if (is_formula2(by)) check_data(.data, c(measure.vars, all.vars(by)))
#                           else  check_data(.data, c(measure.vars, by))
#
#    if(!check_data_result) return( head(.data))
#
#
#
#     mycls <- sapply(.data[measure.vars],  function(x)  setdiff(class(x), "labelled"))
#     row_name <- GetLabelOrName(.data[measure.vars])
#     measure <- ifelse(names(measure) == "", "default", measure)
#
#     data <-  if (by=="1") .data[measure.vars]
#              else if (is_formula2(by)) .data[c(measure.vars, all.vars(by))]
#              else  .data[c(measure.vars, by)]
#
#     calculate_tabelle(data,
#                       measure.vars, by,
#                       measure, row_name, mycls, type,
#                       caption, note)


#@rdname Tabelle
#@param x formula a~h+j
#@export
# Tabelle.formula<- function(x, .data,
#                            by = "1", #not uesed in Tabelle.formula
#                            formula = NULL, fun=NULL,
#                            type = 2,
#                            digits=NULL,
#                            caption = "Charakteristik", note = "") {
#
#   Tabelle.default(X=Formula_Data(x, .data),
#                   type=type, caption=caption, note= note, fun=fun,
#                   digits=digits)
# }



#' @rdname Tabelle
#' @export
Tabelle.lm <- function(x, ..., caption, note) {
  res<- mittelwert_fit(x, ...)
  for(i in names(res)) {
    Output( fix_format( res[[i]] ) )
  }

  invisible(res)
}




#' @rdname Tabelle
#' @export
Describe<-  function(x, ...) {
  # @importFrom dplyr tbl_df
  fix_format(broom::tidy(x))
}









# Mittelwerte -------------------------------------------------------------


mittelwert_fit <- function(x,
                           fun = function(x) {
                             c(
                               n = length(x),
                               M = mean(x, na.rm = TRUE),
                               SD = sd(x, na.rm = TRUE)
                             )
                           }
                           , ...) {
  myeff <- effects::allEffects(x)
  res_list <- NULL
  for (i in names(myeff)) {
    info <- model_info(myeff[[i]])
    ans <- aggregate_effect(myeff[[i]], info$y, info$x, fun)
    AV <- ifelse(is.na(info$labels[info$y]),
                      info$y, info$labels[info$y])
    res_list[[i]] <- prepare_output(ans, paste0("AV: ", AV), "",
                                    info$N,  info$labels)
  }
  res_list
}



aggregate_effect<- function(eff,
                            y,
                            x, fun=function(x) length(x)){
  fm <- formula(paste(y, "~", paste(x, collapse = "+")))
  df <- eff$data

  #-- Faktoren fuer N berechnung vorbereiten
  for (j in names(eff$variables)) {
    if (!eff$variables[[j]]$is.factor)
      df[, j] <-
        cut(df[, j], length(eff$variables[[j]]$levels))
  }

  res<- try(aggregate(fm, df, fun, drop = FALSE))
  if(class(res) =="try-error")   data.frame(NULL)  #  wegen ncol im weiteren progammverlauf
  else do.call(data.frame, res)

}







# -   Hilfsfunktion ---------------------

#
# calculate_tabelle<- function(X,
#                              type, # wie soll Ausgewertet werden Mittelwerte: type=3, type="mean"
#                              caption, note,
#                              fun=NULL
#                              , digits=digits){
#   res <- NULL
#
#   for (i in 1:length(X$measure)) {
#     if (X$measure[i] == "default")
#            X$measure[i] <- X$measure.class[i]
#
#     #-- Labels ----------------------------------
#     if (type == 3 | type == "mean") {
#             cat("\nBerechne Mittelwerte\n")
#     } else{
#
#       if (X$measure.class[i] == "factor")
#         X$row_name[i] <- paste0(X$row_name[i], " (",
#                           paste0(levels(X$data[[X$measure.vars[i]]]), collapse = "/"), ")")
#       else if (X$measure[i] == "median")
#         X$row_name[i] <- paste0(X$row_name[i], " (median)")
#       else
#         X$row_name[i] <- paste0(X$row_name[i], " (mean)")
#
#     }
#
#     #-------------------------
#     res[[X$measure.vars[i]]]  <-
#       berechne.default(X$data,
#                        X$measure.vars[i], X$by,
#                        X$measure[i], type, fun=fun,
#                        digits=  if(is.null(digits)) X$digits[[i]][1] else digits
#       )
#   }
#
#   df <- plyr::ldply(res)
#   df[, 1] <- factor(df[, 1], names(X$row_name), X$row_name)
#   # prepare_output
#   prepare_output(df, caption, note, nrow(X$data), NA)
# }


calculate_tabelle2<- function(X,
                             type, # wie soll Ausgewertet werden Mittelwerte: type=3, type="mean"
                             caption, note,
                             fun=NULL
                             , digits=digits){
  res <- NULL

  for (i in 1:length(X$measure)) {
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
#print("in berechne.default")
    #-------------------------
    res[[X$measure.vars[i]]]  <-
      berechne.default(X$data,
                       X$measure.vars[i], X$by,
                       X$measure[i], type, fun=fun,
                       digits =  if(is.null(digits)) X$digits[i]
                                else digits
      )

  }
 #print("ex berechne.default")
# print(res)
  df <- plyr::ldply(res)
  df[, 1] <- factor(df[, 1], names(X$row_name), X$row_name)
  # prepare_output
  prepare_output(df, caption, note, nrow(X$data), NA)
}



# calculate_tabelle<- function(data,
#                      vars,
#                      by,
#                      measure,  # "default" oder "median"
#                      row_name,
#                      mycls,
#                      type, # wie soll Ausgewertet werden Mittelwerte: type=3, type="mean"
#                      caption, note,
#                      fun=NULL
#                      ){
#
#
#
#
#
#   res <- NULL
#   # sleepstudy %>% Tabelle2(Reaction, Days)
#       # measure = c("default", "default")
#       # row_name =c(Reaction="Reaction", Days="Days")
#       # mycls = c(Reaction= "numeric",Days= "factor" )
#       # type = 2
#
#   for (i in 1:length(measure)) {
#     if (measure[i] == "default")
#                   measure[i] <- mycls[i]
#     #-- Labels ----------------------------------
#     if (type == 3 | type == "mean") {
#       cat("\nBerechne Mittelwerte\n")
#     } else{
#       if (mycls[i] == "factor")
#         row_name[i] <- paste0(row_name[i], " (",
#                               paste0(levels(data[[vars[i]]]), collapse = "/"), ")")
#       else if (measure[i] == "median")
#         row_name[i] <- paste0(row_name[i], " (median)")
#       else
#         row_name[i] <- paste0(row_name[i], " (mean)")
#
#     }
#     #-------------------------
#     res[[vars[i]]]  <-
#       berechne.default(data, vars[i], by, measure[i], type, fun=fun)
#   }
#
#   df <- plyr::ldply(res)
#   df[, 1] <- factor(df[, 1], names(row_name), row_name)
#  # prepare_output
#   prepare_output(df, caption, note, nrow(data), NA)
# }
#
#

#' @rdname Tabelle
#' @description Hielfsfunktion fuer Tabellen
#' @param type welche Auswertung
#' @param test,include.test,corr_test,useconTest,normality.test Signifikanz Test
#' @param na.action,exclude An Formula
#' @param include.n,include.nr  Anzahl
#' @param include.total Total
#' @param include.p,include.sig.star p-Werete
#' @param cor_diagonale_up Korrelation
#' @param max_factor_length Fehler Abfangen bei langen Factoren
#' @param order,decreasing noch nicht Implementiert
errate_statistik3 <-
  function (...,
            type = NULL,
            caption="", note="", digits=NULL,
            #"auto",
            test = FALSE,
            na.action = na.pass,
            exclude = NA,

            include.n = TRUE,
            include.nr = FALSE,
            # print.n,
       # zu wenig verwendet und eventuell nicht eindeutig     include.header.n = TRUE,
            include.total = FALSE,
            # total,
            include.test = test,
            # test,
            include.p = TRUE,
            include.sig.star = FALSE,
            corr_test = "pearson",
            cor_diagonale_up = TRUE,
            max_factor_length = 35,
            order = FALSE,
            decreasing = FALSE,
            useconTest = FALSE,
            normality.test = FALSE)
  {

# cat("\ntype: ",type,     # = NULL,
#    "\ntest: ",test,
#    "\ninclude.n: ", include.n, #= TRUE,
#    "\ninclude.nr: ",include.nr, #= NULL,
#
#    "\ninclude.total: ",include.total, #= FALSE,,
#    "\ninclude.test: ",   include.test,"\n" )   #= test
#

    Mittelwert_Einzel <- function(i, x) {
      # Item|lev|n|m
      x_NA <- x
      N    <- length(x)
      x    <- na.omit(x)
      n    <- length(x)
      rr <- NULL #Result
      if (all(is.na(x)))
        X$measure[i] <- "all_NA"
      res <- switch(
        X$measure[i],
        numeric = Mean2default(x, X$digits[i], n),
        integer = Mean2default(x, X$digits[i], n),
        factor =  Prozent2default(x, X$digits[i], n, exclude, max_factor_length),
        logical = Prozent2default(x, X$digits[i], n, exclude, max_factor_length),
        freq =    Prozent2default(x, X$digits[i], n, exclude, max_factor_length),
        mean =    Mean2default(x, X$digits[i], n),
        median=   Median2default(x, X$digits[i], n),
        multi=    Multi2default(x, X$digits[i], n),

        #  median = mydf(n, Median2(x, digits =digits.mean[1],...), "(median)"),
        "NA"
      )
      if(X$measure[i]=="factor"){
        x0 <- data.frame(Item = X$row_name[i],
                         lev = "",
                         n = res$n[1] ,
                         m = "")
        res$n <-""
        x1<-cbind(Item = "&nbsp;&nbsp;", res)
        rr <- rbind( x0, x1 )
      } else rr <- cbind(Item = c(X$row_name[i], rep("", nrow(res) - 1)), res)

      rr
    }

    Mittelwert_Gruppe <- function(i, j, x=NULL) {
     # print(str(i))
     # print(str(j))
      # item|lev|g1_n|g1_m|g2_n|g2_m|g3_n|...
      groups <- droplevels(X$data[[j]])
      tabel_header <- paste0("&nbsp;", names(table(groups)) )

       # if (include.header.n)
        #  paste0("&nbsp;", names(table(groups)), " (n=", table(groups), ")")
      ans <- NULL
      for (lev in 1:nlevels(groups)) {
        xx <- x[which(groups == levels(groups)[lev])]
        rr <- Mittelwert_Einzel(i, xx)

        if (is.null(ans))
          ans <- rr
        else
          ans <- cbind(ans, rr[-c(1, 2)])
      }
     tabel_header <- rep(tabel_header, each = 2)
     names(ans)[-c(1, 2)] <- paste0(tabel_header, "_", names(ans)[-c(1, 2)])
     ans
    }


    Total_Gruppe <- function(i, j) {
      groups <- droplevels(X$data[[j]])
      res <- t(as.matrix(table(groups)))
      res_n<- NULL
      for(i in 1:ncol(res)){
        res_n <-  cbind(res_n, cbind(n="", res[,i]))

      }

      colnames(res_n) <- paste0("&nbsp;",
                                rep(colnames(res), each=2),"_",
                                rep(c("n", "m"), length.out=ncol(res)))

      cbind(data.frame(Item = "Total",lev = "(N)"), res_n)
    }

    Test <- function(i, j) {
      if (X$measure.class[i] == "numeric" & X$group.class[j] == "factor") {
        fm_aov <- formula(paste(measure.vars[i], "~", j))
        #  print(fm_aov)
        conTest(fm_aov, X$data, include.test)
      } else  if (X$measure.class[i] == "factor" & X$group.class[j] == "factor") {
        fm_chi <- formula(paste("~", measure.vars[i], "+",  j))
        print(fm_chi)
        catTest(fm_chi, X$data, include.test)
      }


      else
        "NA"
    }


    #-- Vorbereiten der Daten
    ANS <- NULL
        X <- prepare_data2(..., na.action = na.action)
    group.vars   <- X$group.vars
    measure.vars <- X$measure.vars
    N            <- nrow(data)

    if(type[1] == "multiresponse")
      X$measure <- rep("multi", length(X$measure))
#-- Einzelvergleich -------------------------------
    if (is.null(group.vars)) {
      if (include.nr)
        ANS <- data.frame(Item = "Total",lev = "(N)", n = "", m = X$N)

      for (i in 1:length(measure.vars))
          ANS <- rbind(ANS, Mittelwert_Einzel(i, X$data[[i]]))

      ANS$Item <- paste(ANS$Item, ANS$lev) # Spalte Characteristics entfernen
      if (include.n) ANS <- prepare_output(ANS[-2], caption, note, N)
      else  ANS <- prepare_output(ANS[-c(2,3)], caption, note, N)
    }
#-- Gruppenvergleich
    else{
      for (j in group.vars) {
        #- jeder Eintrg getrennt
        if(X$group.class[j]=="factor"){ # Kontrolle
          caption <- paste(X$col_name[j], caption)
          ans_in  <- NULL
          if(include.nr) {
            ans_in <- Total_Gruppe(i, j)
            if(include.total) #: Item|lev|n||All|G1_n|G1_m|G2_n|G2_m|.._n|.._m|
             ans_in <- cbind(ans_in[1:2],
                                  "Total_n"="", "Total_m"=X$N,ans_in[3:ncol(ans_in)])
           if(include.test) #   Item|lev|G1|G2|...|statistics
             ans_in$statistics<-""

         #  print(ans_in)
            }


          for (i in 1:length(measure.vars)) {

            ans <- Mittelwert_Gruppe(i, j, X$data[[measure.vars[i]]])
            # ans: # item|lev|g1_n|g1_m|g2_n|g2_m|g3_n|...
          #  print(ans)
            if(include.total){
              total<- Mittelwert_Einzel(i, X$data[[measure.vars[i]]])[-c(1,2)]
              names(total)[] <- paste0("Total_",names(total) )
              ans <- cbind(ans[1:2], total, ans[3:ncol(ans)])
            }

            if (include.test) {
              ans$statistics <- ""
              ans$statistics[1] <- Test(i, j)

            }
            ans_in <- rbind(ans_in, ans)
          }
          if(!include.n) {
            if (ncol(ans_in) %% 2)
              ans_in <- ans_in[c(1, 2, seq(4, ncol(ans_in), by=2), ncol(ans_in))]
            else
              ans_in <- ans_in[c(1, 2, seq(4, ncol(ans_in), by=2))]

            names(ans_in) <- gsub("_m", "", names(ans_in) )
          }

          ans_in$Item <- paste(ans_in$Item, ans_in$lev)
          ANS[[j]] <- prepare_output(ans_in[-2], caption, note, N)
        }
        else {
          # Das geht nicht bzw vieleicht als eigene Funktion
         ANS <- corr_tabel_X(...)
        }
      }
    }
   ANS
}


#' @rdname Tabelle
#' @description Hielfsfunktion fuer Tabellen
#' @export
#' @examples
#' prepare_data2(m1+m2~m3 , varana)
#' corr_tabel_X(~m1+m2, varana)
#' corr_tabel_X(m1+m2~m3, varana)
#' corr_tabel_X(m1+m2~m3|geschl, varana)
corr_tabel_X <-
  function (...,
            type = c("pearson", "spearman"),
            exclude = NA, subset, na.action = na.pass,
            cor_diagonale_up = TRUE,
            sig.star = TRUE, p.value = FALSE,
            include.mean = FALSE,
            include.n = TRUE)
  {
    type <-  match.arg(type)
    #-- Vorbereiten der Daten (na.omit, subset)
    ANS <- NULL
    X <- prepare_data2(..., na.action = na.action)
    # return(X )
    N <- nrow(X$data)
    measure.vars<- X$measure.vars
    group.vars<- X$group.vars
    row_name <- X$row_name
    Y_data<-X$data[measure.vars]
    X_data <-X$data[group.vars]

    condition.vars<- X$condition.vars


    ## wenn nur ein element dann ergibt sich ein Fehler
    if (ncol(X$data[measure.vars]) == 1)
      X$data[measure.vars]  <- dplyr::as_data_frame(X$data[measure.vars])


    if (!is.null(condition.vars)) {
      cat(" in !is.null(condition.vars)")
      #groups <- all.vars(groups)
      if (length(condition.vars) == 1) {


        condition <- X$data[[condition.vars]]
        if (X$condition.class  != "factor") {
          warning("Achtung nur eine Faktor kann Gruppen bilden!")
          return(head(X$data))
        }

        condition <- droplevels(condition)
        # return(condition)
        lvls <- levels(condition)
        g1 <- which(condition == lvls[1])

        # vor corr_2"


        ans <- stp25APA2:::corr_2( Y_data[g1,], X_data[g1, 1], type)
        names(ans)[2:4] <- paste0(lvls[1], "_", names(ans)[2:4])

        for (i in 2:(length(lvls))) {
          g2 <- which(condition == lvls[i])
          ans2 <-  stp25APA2:::corr_2(Y_data[g2,], X_data[g2, 1], type)
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
    else if (!is.null(group.vars)) {
      # Beginn der Funktion -----------------------------------------------------
      ans <- corr_2(Y_data, X_data[1], type)

      if (length(X$xname) > 1) {
        names(ans)[2:4] <- paste0(group.vars[1], "_", names(ans)[2:4])
        for (i in  2:(length(group.vars))) {
          ans2 <-  corr_2(Y_data, X_data[i], type)
          names(ans2)[2:4] <-
            paste0(group.vars[i], "_", names(ans2)[2:4])
          ans <- cbind(ans, ans2[-1])
        }
        attr(ans, "note") <- type
        attr(ans, "N") <- N
      }
      ans[, 1] <- factor(ans[, 1], names(row_name), row_name)
    }
    else{


      ans <- corr_1( Y_data, type = type) ## liste
      print(ans)
      ans$mean <-
        t(berechne.default(Y_data , measure.vars, measure = "mean", type = 1))

      ans$row_name <- row_name
    }
    ordne_corr_tabel(ans)
  }



ordne_corr_tabel <- function(ans,
                             cor_diagonale_up = TRUE,
                             sig.star = TRUE,
                             p.value = FALSE,
                             # mean = FALSE, # Veraltet
                             include.mean = FALSE,
                             include.n = TRUE)
{

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

    if (sig.star) {
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

    if(!include.n){

      cat("\n", include.n, "\n\n-----\n")
      # grep("_N", c("jdhdz", "hdgdt_N", "sfsg_"))
      #ans<- ans[- grep("_N", names(ans))]
    }

    if (include.mean)
      res <- cbind(res[1], "M (SD)" = ans$mean, res[2:ncol(res)])
    return(res)
  }
  else{
    return(ans)
  }
}


