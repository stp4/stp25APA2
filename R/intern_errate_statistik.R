#' @rdname APA2
#' @description Errate korekte Auswertung und Extrahieren der Variablen aus Formula.
#' @examples 
#' 
#' varana <- varana %>% Label(m1="Mesung1", m2="BMI")
#' x<-APA2( ~m1,varana)
#' x<-APA2( ~m1+m2,varana)
#' 
#' x<-APA2( m1~geschl,varana)
#' x<-APA2( m1+m2~alter,varana)
#' x<-APA2( m1+m2+geschl~alter,varana, include.test = TRUE)
#' x<-APA2( ~m1+m2+m3+m4,varana, test=TRUE)
#' 
errate_statistik2 <- function(Formula,
                              data,
                              caption = "caption",
                              
                              note = "note",
                              type = NULL,
                              #"auto",
                              # test = FALSE,
                              na.action = na.pass,
                              exclude = NA,
                              #neu damit besser leslich
                              include.n = TRUE,
                              include.all.n = NULL,
                              # print.n,
                              include.header.n = TRUE,
                              include.total = FALSE,
                              # total,
                              include.test = FALSE,
                              # test,
                              include.p = TRUE,
                              include.stars = FALSE,
                              corr_test = "pearson",
                              cor_diagonale_up = TRUE,
                              max_factor_length = 35,
                              # total=FALSE,
                              order = FALSE,
                              decreasing = FALSE,
                              useconTest = FALSE,
                              normality.test = FALSE,
                              digits.mean = options()$stp25$apa.style$m$digits,
                              digits.percent = options()$stp25$apa.style$prozent$digits[1],
                              test_name = "Hmisc",
                              # auto_wrap = NULL, #-- neu Zeilenumbruch
                              ...)
{
  
  #-- Statistik Berechnen ---------------------------------
  Stat_Mean_Freq <- function(x, ...,
                             default_numeric = "mean") {
    index_zaeler <<- index_zaeler + 1
    if (is.list(digits.mean))
      digits.mean <-
        digits.mean[[index_zaeler]] # lebt nur in dieser Funktion
    if (is.list(type))
      type <- type[[index_zaeler]] # lebt nur in dieser Funktion
    # Formula_ data muss ~ m1[3]+ m2 aufdroeseln
    # und digits uebergeben,
    # und Formel zusammenbauen
    
    type_switch <- tolower(type)
    #Funktion definieren fuer  'auto'
    if (is.na(type_switch[1]) | any(type_switch %in% "auto")) {
      if (any(type_switch %in% "median"))
        default_numeric <- "median"
      if (is.factor(x))
        type_switch <- "freq"
      else if (is.logical(x))
        type_switch <- "freq_logical"
      else if (is.numeric(x))
        type_switch <- default_numeric
      else{
        x <- as.numeric(x)
        type_switch <- default_numeric
      }
    }
    
    x_NA <- x
    N    <- length(x)
    x    <- na.omit(x)
    n    <- length(x)
    
    
    mydf <- function(n, m, name = "")
      data.frame(Characteristics = "",
                 n = as.character(n),
                 Statistics = m,
                 stringsAsFactors=FALSE)
    
    if (all(is.na(x)))
      type_switch <- "all_NA"
    
    #cat(type_switch,"\n")
    
    result <- switch(
      type_switch,
      mean = mydf(n, Mean2(x, digits = digits.mean, ...), "(mean)"),
      median = mydf(n, Median2(x, digits = digits.mean[1], ...), "(median)"),
      ci = mydf(n, Meanci2(x, digits = digits.mean, ...), "(CI)"),
      meanci = mydf(n, Meanci2(x, digits = digits.mean, ...), "(CI)"),
      freq = Prozent2APA(x_NA, exclude, digits.percent, max_factor_length),
      freq_logical = Prozent2APA(x_NA, exclude, digits.percent, max_factor_length)[1,],
      freq.ci = Prop_Test(x_NA)[, c("Characteristics", "n", "Statistics")],
      n = mydf(n, n),
      all_NA =  mydf(0, "n.a."),
      mydf(n, class(x)) # nur eine Zeile ausgeben# Fehler abfangen
    )
    
 
    
    if (include.all.n)
      result
    else
      result[,-2, drop = FALSE]
  }
  
  #-- Liste zu Dataframe -----------------
  return_data_frame <- function(ans) {
    ANS <- NULL
      for (var in names(ans)) {
        var_name <- ifelse(is.null(attr(X$Y_data[, var], "label")),
                           var,
                           attr(X$Y_data[, var], "label"))
        n_var <- length(ans[[var]]$Characteristics) - 1
        ans[[var]] <-
          cbind(Item = c(var_name, rep("", n_var)), ans[[var]])
        if (is.null(ANS)) {
          ANS <- ans[[var]]
        } else {
          ANS <- rbind(ANS, ans[[var]])
        }
      }
    ANS
  }
  
  
  #- Start der Funktion ------------------------------------
  X      <- Formula_Data(Formula, data, na.action = na.action)
  N      <- nrow(data)
  
  
  if (!is.logical(include.test)) {
    if (include.test == "conTest")
      useconTest <- TRUE
    else if (include.test == "shapiro.test")
      normality.test <- TRUE
    else {
      test_name <- include.test
      useconTest <- TRUE
    }
    include.test <- TRUE
  }
  
  if (is.null(type))  type <- X$type
  if (is.null(digits.mean))  digits.mean <- X$digits
  if (!is.null(X$condition)) {warning("errate_statistik2: condition weden noch nicht unterstuetzt")}
  
  # Beginn der Auswertung -----------------------------------------------------
  if (is.null(include.all.n)) {
    # Automatisch N auswahlen
    if (is.null(X$X_data)) {
      if (!any(is.na(X$Y_data)))
        include.all.n <- FALSE
      else
        include.all.n <- TRUE
    }
    else{
      if (!any(is.na(cbind(X$X_data, X$Y_data))))
        include.all.n <- FALSE
      else
        include.all.n <- TRUE
    }
  }
  if (order & (length(X$yname) > 1)) {
    my_order <- order(
                 apply(X$Y_data, 2,
                    function(x) if (is.numeric(x) | is.factor(x)) mean2(x) else  0),
                 decreasing = decreasing)
    X$Y_data <- X$Y_data[, my_order, drop = FALSE]
  }
  
  # Einzelvergeich Pruefen ob Gruppe (also ~a+b+c oder a+b+c~d+e) -------------
  
  if (is.null(X$xname)) {
    index_zaeler <- 0
    ANS <- return_data_frame(
      lapply(X$Y_data, Stat_Mean_Freq))
    if (include.test & !normality.test) {
        mycorrtable <- Corr1(X$Y_data, nrow(ANS),
                             corr_test, include.p, include.stars, cor_diagonale_up)
        note <- paste("Korrelation nach" , Hmisc::upFirst(type))
        if (nrow(ANS) != nrow(mycorrtable)) ANS <-  cbind(ANS, Error = "gemischtes Skalenniveau")
        else ANS <- cbind(ANS, mycorrtable)
      } else if (include.test & normality.test) {
        ANS <- cbind(ANS,
                     "shapiro test" = unlist(
                                      lapply(X$Y_data,
                                        function(x) {
                                            if (is.numeric(x)) {
                                                APA(shapiro.test(x))
                                            } else {
                                                rbind(paste(APA(shapiro.test(as_numeric(x))),  class(x)),
                                                      rep("", nlevels(x) - 1))
                                                      }})))
      } else {NULL}
    ANS <- prepare_output(ANS, caption, note, N)
    return(ANS)
    
    #- GRUPPENVERGLEICH ---------------------------------------------------
  } else {
    ANS_list <- list() #antwortliste
    for (ix in X$xname) {
       ANS <- NULL
      #--  Mehere Gruppenvariablen aufschluesseln
      caption <- paste(ix, caption)
      Xi <- X$X_data[, ix]  # Gruppe ist X'
      x_name <- ifelse(is.null(attr(X$X_data, "label")), ix, attr(X$X_data, "label")) ## hmisc::LAbel
      y_name <-  sapply(X$xname, function(y)
                                  ifelse(is.null(attr(X$Y_data, "label")),
                                  y, attr(X$Y_data, "label")))
      my_levels <- levels(Xi)
      #-- Test ob Gruppen cat("\n\nAchtung Gruppe ist kein Factor!\n\n")
      if (is.null(my_levels)) {
        #--Gruppe ist Numeric also Correlation
        if (corr_test %in% c("pearson", "spearman")) {
          note <- paste(note, "Korrelation nach", Hmisc::upFirst(corr_test))
          ANS <- Corr2(X$Y_data, Xi, corr_test, include.stars)
          ANS[, 1] <- rownames(ANS)
          colnames(ANS)[1] <- x_name
          ANS <-
            if (include.test)
              ANS[, c(1, 2, 6)]
          else
            ANS[, c(1, 2, 5)]
        }
      } else{
        #-- Gruppe ist Faktor  also Freq oder Mean
        Xi <- factor(Xi)
        #-- sicherstellen das keine leeren Faktorstufen esistieren
        tabel_header <-
          if (include.header.n)
            paste0(names(table(Xi)), " (n=", table(Xi), ")")
        else
          names(table(Xi))
        my_levels <- levels(Xi)
        #-- alle Faktor-Stufen Auswerten mean/Freq
        for ( lev in seq_len(length(my_levels)) ) {
          index_zaeler <- 0
          my_subset <- which(Xi == my_levels[lev])
            ans <- return_data_frame(lapply(X$Y_data[my_subset, , drop = FALSE], Stat_Mean_Freq))
          
          colnames(ans)[include.all.n + 3] <- tabel_header[lev]
          if (is.null(ANS))
            ANS <- ans
          else if (include.all.n)
            ANS <- cbind(ANS, ans[,-c(1:2)])
          else
            ANS <- cbind(ANS, ans[3])
        }
        
        if (include.total | include.n) {
          Total <-
            errate_statistik2(
              Formula = formula(paste0(
                "~", paste(X$yname, collapse = "+")
              )),
              data = X$Y_data,
              type = type,
              include.test = FALSE,
              include.all.n = TRUE,
              include.header.n = FALSE,
              include.total = FALSE,
              max_factor_length = max_factor_length
            )
          
          nncol <- ncol(Total)
          names(Total)[c(nncol - 1, nncol)] <- c("N", "Total")
          names_ans <- names(ANS)
          
          if (include.total) {
            if (include.all.n | include.n) {
              ANS  <-  cbind(ANS[1:2],
                             Total[c(nncol - 1, nncol)],
                             ANS[3:ncol(ANS)])
              names(ANS)[-c(1:4)] <- names_ans[-c(1:2)]
            }
            else{
              ANS  <-  cbind(ANS[1:2],
                             Total[nncol],
                             ANS[3:ncol(ANS)])
              names(ANS)[-c(1:3)] <- names_ans[-c(1:2)]
            }
          }
          else{
            ANS <- cbind(ANS[1:2], N = Total[, nncol - 1], ANS[3:ncol(ANS)])
            names(ANS)[-c(1:3)] <- names_ans[-c(1:2)]
          }
        }
        
        if (include.test) {
          inference_test_result <- c()
          for (y in X$yname) {
            fm_aov <- formula(paste(y, "~", ix))
            fm_xtab <- formula(paste("~", ix, "+", y))
            
            if (is.factor(X$Y_data[, y])) {
              if (useconTest) {
                X$Y_data[, y] <- as_numeric(X$Y_data[, y])
                cctest       <-
                  conTest(fm_aov, cbind(X$X_data, X$Y_data), test_name)
              } else{
                cctest    <- catTest(fm_xtab, cbind(X$X_data, X$Y_data))
              }
              
              inference_test_result <-
                c(inference_test_result,
                  cctest,
                  rep("", nlevels(data[, y]) - 1))
            } else{
              # Zielvariable Zahl
              X$Y_data[, y] <- as_numeric(X$Y_data[, y])
              data_aov   <- cbind(X$X_data, X$Y_data)
              cctest     <- conTest(fm_aov, data_aov, test_name)
              
              inference_test_result <-
                c(inference_test_result, cctest)
            }
          }
          ANS$sig.Test <- inference_test_result
        }
      }
      ANS <- prepare_output(ANS, caption, note, N)
      ANS_list[[ix]]  <-  (ANS)
    }
    return(ANS_list)
  }
}







#' @rdname Tabelle
#' 
#' @examples 
#' 
#'  Tabelle(
#' m1[median] + m2[median] + m3[median] + m4[median] ~ geschl,
#' varana,
#' APA = TRUE,
#' include.n = FALSE,
#' test = TRUE
#' )
#' 
#'  c(
#' "wilcox"=Tabelle(alter ~ geschl, varana, APA=TRUE, test="wilcox")[[1]]$statistics[1],
#' "h.test"=Tabelle(alter ~ geschl, varana, APA=TRUE, test="h.test")[[1]]$statistics[1],
#' "anova"=Tabelle(alter ~ geschl, varana, APA=TRUE, test="anova")[[1]]$statistics[1],
#' "t.test"=Tabelle(alter ~ geschl, varana, APA=TRUE, test="t.test")[[1]]$statistics[1],
#' "hmisc"=Tabelle(alter ~ geschl, varana, APA=TRUE, test="Hmisc")[[1]]$statistics[1]
#' )
#'   
#'   
#'    Tabelle(alter ~ geschl, varana, APA=TRUE)
#' 
#' Tabelle(alter ~ geschl, varana, include.n=FALSE, APA=TRUE)
#' Tabelle(alter ~ geschl, varana, include.nr=TRUE, APA=TRUE)
#' Tabelle(alter ~ geschl, varana, include.total=TRUE, APA=TRUE)
#' Tabelle(alter ~ geschl, varana, include.test=TRUE, APA=TRUE)
errate_statistik3 <-
  function (...,
            type = NULL, # "muliresponse" "pearson", "spearman"
            caption = "",
            note = "",
          #  digits = NULL,
         #  test = FALSE,
            na.action = na.pass,
            exclude = NA,
            
            include.n = TRUE,     # Anzahl bei Gruppe je Spalte
            include.nr = FALSE,   # erste zeile Anzahl der Gruppen
            include.total = FALSE,# Total Anzahl + Statistik bei Gruppe
            
            include.test = test,
            
          #  include.p = TRUE,
         #   include.stars = FALSE,
         #   include.mean=FALSE,  # fuer Correlation
         #   corr_test = "pearson",
          #  cor_diagonale_up = TRUE,
         
            max_factor_length = 35
         #   order = FALSE,
         #   decreasing = FALSE,
           # useconTest = FALSE,
        #    normality.test = FALSE
         )
  {
    
    mySep<- ' '
    mySep2  <- '  '  
      if(stp25output::which_output() == "hlml"){  
        mySep <- '&nbsp;' 
        mySep2 <- '&nbsp;&nbsp;'
        }
    
    Mittelwert_Einzel <- function(i, x) {
     
      x_NA <- x
      N    <- length(x)
      x    <- na.omit(x)
      n    <- length(x)
      rr <- NULL #Result
      
      # gelöscht wegen Fehler bei NAs
     # if (all(is.na(x)))
    #    X$measure[i] <- "all_NA"
       
      res <- switch(
        X$measure[i],
        numeric = Mean2default(x, X$digits[i], n),
        integer = Mean2default(x, X$digits[i], n),
        factor =  Prozent2default(x, X$digits[i], n, exclude, max_factor_length),
        logical = Prozent2default(x, X$digits[i], n, exclude, max_factor_length),
        freq =    Prozent2default(x, X$digits[i], n, exclude, max_factor_length),
        mean =    Mean2default(x, X$digits[i], n),
        median =  Median2default(x, X$digits[i], n),
        multi =   Multi2default(x, X$digits[i], n),
        
        "NA"
      )
    #  cat("\nin Mittelwert_Einzel")
    #  print(res)
      
      if (X$measure[i] == "factor") {
        x0 <- data.frame(
          Item = X$row_name[i],
          lev = "",
          n = res$n[1] ,
          m = "",stringsAsFactors=FALSE
        )
        res$n <- ""
        x1 <- cbind(Item = mySep2, res)
        rr <- rbind(x0, x1)
      } else
        rr <-
        cbind(Item = c(X$row_name[i], rep("", nrow(res) - 1)), res)
     
      rr
    }
    
    Mittelwert_Gruppe <- function(i, j, x = NULL) {
      groups <- droplevels(X$data[[j]])
      tabel_header <- paste0(mySep, names(table(groups)))
      
      ans <- NULL
      for (lev in seq_len(nlevels(groups))) {
        xx <- x[which(groups == levels(groups)[lev])]
        rr <- Mittelwert_Einzel(i, xx)
        
        if (is.null(ans))
          ans <- rr
        else
          ans <- cbind(ans, rr[-c(1, 2)])
      }
      tabel_header <- rep(tabel_header, each = 2)
      names(ans)[-c(1, 2)] <-
        paste0(tabel_header, "_", names(ans)[-c(1, 2)])
      ans
    }
    
    
    Total_Gruppe <- function(i, j) {
      groups <- droplevels(X$data[[j]])
      res <- t(as.matrix(table(groups)))
      res_n <- NULL
      for (i in seq_len(ncol(res)) ) {
        res_n <-  cbind(res_n, cbind(n = "", res[, i]))
      }
      
      colnames(res_n) <- paste0(mySep,
                                rep(colnames(res), each = 2),
                                "_",
                                rep(c("n", "m"), length.out = ncol(res)))
      
      cbind(data.frame(Item = "Total", 
                       lev = "(N)",
                       stringsAsFactors=FALSE
                       ), res_n)
    }
    
    Test <- function(i, j) {
      fm_chi <- formula(paste("~", measure.vars[i], "+",  j))
      fm_aov <- formula(paste(measure.vars[i], "~", j))
      # fm_aov <- formula(paste(measure.vars[i], "~", j))
      if (X$measure.class[i] == "numeric" &
          X$group.class[j] == "factor") {
        conTest(fm_aov, X$data, which_test)
      } else if (X$measure.class[i] == "median" &
                 X$group.class[j] == "factor") {
        conTest(fm_aov, X$data, which_test)
      } else  if (X$measure.class[i] == "factor" &
                  X$group.class[j] == "factor") {
        if (is.logical(which_test))
          catTest(fm_chi, X$data, which_test)
        else{
          X$data[[measure.vars[i]]] <- as.numeric(X$data[[measure.vars[i]]])
          conTest(fm_aov, X$data, which_test)
        }
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
    
    if(is.character(include.test)){
     which_test <-  match.arg(include.test, c("wilcox.test","u.test",
                                              "kruskal.test","h.test",
                                              "chisq.test","t.test", 
                                              "aov", "anova",
                                              "SPSS", "Hmisc",
                                              "shapiro.test", "ks.test" #Kolmogorov-Smirnov-Anpassungstest mit SPSS
                                              ))
     if (any(which_test %in% c("shapiro.test", "KS.test"))) {
       include.test <- FALSE # Einzeltest
     } else {
       include.test <- TRUE
     }
    }  else {
      which_test <- TRUE}
    
    if (type[1] == "multiresponse")
      X$measure <- rep("multi", length(X$measure))
    
    #-- Einzelvergleich -------------------------------
    if (is.null(group.vars)) {
    #  cat(" Einzelvergleich ")
      if (include.nr)
        ANS <-
          data.frame(
            Item = "Total",
            lev = "(N)",
            n = "",
            m = X$N,
            stringsAsFactors=FALSE
          )
   
      
      for (i in seq_len(length(measure.vars)))
      { 
        mymeans<- Mittelwert_Einzel(i, X$data[[i]])
        if( which_test == "shapiro.test" ) mymeans$shapiro.test <- APA(shapiro.test(
                                                                  na.omit(as_numeric(X$data[[i]]))))
        if( which_test == "ks.test" ){ 
          x<- na.omit(as_numeric(X$data[[i]]))
       #   x<- hyper$chol0 
       #    APA( ks.test(x,"pnorm",mean(x),sd(x)) )
       #   str(ks.test(x,"pnorm",mean(x),sd(x)))
          
          mymeans$ks.test <- APA( ks.test(x,"pnorm",mean(x),sd(x)))
        
        
        }
        ANS <- rbind(ANS, mymeans)
        
      }
     
      ANS$Item <-
        paste(ANS$Item, ANS$lev) # Spalte Characteristics entfernen
      if (include.n)
        ANS <- prepare_output(ANS[-2], caption, note, N)
      else
        ANS <- prepare_output(ANS[-c(2, 3)], caption, note, N)
    }
    #-- Gruppenvergleich
    else{
      for (j in group.vars) {
        #- jeder Eintrg getrennt
        if (X$group.class[j] == "factor") {
          # Kontrolle
          caption <- paste(X$col_name[j], caption)
          ans_in  <- NULL
          if (include.nr) {
            ans_in <- Total_Gruppe(i, j)
            if (include.total)
              #: Item|lev|n||All|G1_n|G1_m|G2_n|G2_m|.._n|.._m|
              ans_in <- cbind(ans_in[1:2],
                              "Total_n" = "",
                              "Total_m" = X$N,
                              ans_in[3:ncol(ans_in)])
            
           if ( include.test ) ans_in$statistics <- ""
            
            
            # if (is.logical(include.test)) {
            #   if (include.test){ ans_in$statistics <- "" }
            # } else if (is.character(include.test)) { ans_in$statistics <- "" }
            
          }
          
          for (i in seq_len(length(measure.vars))) {
            ans <- Mittelwert_Gruppe(i, j, X$data[[measure.vars[i]]])
           
            if (include.total) {
              total <- Mittelwert_Einzel(i, X$data[[measure.vars[i]]])[-c(1, 2)]
              names(total)[] <- paste0("Total_", names(total))
              ans <- cbind(ans[1:2], total, ans[3:ncol(ans)])
            }
            
           if (include.test){
               ans$statistics <- ""
               ans$statistics[1] <- Test(i, j)
           }
            
            
            # if (is.logical(include.test)) {
            #   if (include.test){ 
            #     ans$statistics <- ""
            #     ans$statistics[1] <- Test(i, j) }
            # } else if (is.character(include.test)) { 
            #    ans$statistics <- ""
            #    ans$statistics[1] <- Test(i, j)
            #   }
            
          
            ans_in <- rbind(ans_in, ans)
          }
          if (!include.n) {
            if (ncol(ans_in) %% 2)
              ans_in <-
                ans_in[c(1, 2, seq(4, ncol(ans_in), by = 2), ncol(ans_in))]
            else
              ans_in <-
                ans_in[c(1, 2, seq(4, ncol(ans_in), by = 2))]
            
            names(ans_in) <- gsub("_m", "", names(ans_in))
          }
          
          ans_in$Item <- paste(ans_in$Item, ans_in$lev)
          ANS[[j]] <- prepare_output(ans_in[-2], caption, note, N)
        }
        else {
          # Das geht nicht bzw vieleicht als eigene Funktion
          
          stop("Benutze die Funktion APA_Correlation")
        }
      }
    }
    ANS
  }