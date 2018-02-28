# Errate korekte Auswertung
# Extrahieren der Variablen aus Formula.


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
  
  #-- Statistik Berechnen------
  Stat_Mean_Freq <- function(x, ...,
                             default_numeric = "mean") {
    #  cat("\nin Stat_Mean_Freq include.all.n =", include.all.n, "\n")
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
                 Statistics = m)
    
    if (all(is.na(x)))
      type_switch <- "all_NA"
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
      result[,-2]
  }
  
  #-- Liste zu Dataframe -------------------
  return_data_frame <- function(ans, var_vektor = "") {
    # print(ans)
    ANS <- NULL
    if (class(ans) == "list") {
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
    } else {
      var_name <- ifelse(is.null(attr(X$Y_data[, var_vektor], "label")),
                         var_vektor,
                         attr(X$Y_data[, var_vektor], "label"))
      
      n_var <- length(ans$Characteristics) - 1
      ANS <- cbind(Item = c(var_name, rep("", n_var)), ans)
    }
    ANS
  }
  
  X      <- Formula_Data(Formula, data, na.action = na.action)
  N      <- nrow(data)
  
  if (is.null(type))
    type <- X$type
  if (is.null(digits.mean))
    digits.mean <- X$digits
  if (!is.null(X$condition)) {
    warning("errate_statistik2: condition weden noch nicht unterstuetzt")
  }
  
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
    my_order <- order(apply(X$Y_data, 2,
                            function(x)
                              if (is.numeric(x) | is.factor(x))
                                mean2(x)
                            else
                              0)
                      , decreasing = decreasing)
    X$Y_data <- X$Y_data[, my_order]
  }
  
  # Einzelvergeich Pruefen ob Gruppe (also ~a+b+c oder a+b+c~d+e) -------------
  
  if (is.null(X$xname)) {
    index_zaeler <- 0
    # cat("\nerate_statistik2: einzelvergleich\n")
    #  kein X$Y_data und wir werten ueber X$Y_data aus daher
    if (length(X$yname) == 1) {
      ANS <- return_data_frame(Stat_Mean_Freq(X$Y_data[, 1]),
                               var_vektor = X$yname)
      # Beschriftung mit labels
      ANS[, 1] <- as.character(ANS[, 1])
      ANS[1, 1] <- GetLabelOrName(X$Y_data[1])
      
      if (include.test & normality.test) {
        #- kann nur Normalverteilungstest sein
        if (is.numeric(X$Y_data[, 1])) {
          shapiro_test <- stats::shapiro.test(X$Y_data[, 1])
          shapiro_test <- rndr_shapiro(shapiro_test)
        }
        else{
          shapiro_test <- rbind(class(X$Y_data[, 1]),
                                rep("", nlevels(X$Y_data[, 1]) - 1))
          
        }
        ANS <- cbind(ANS, "shapiro test" = shapiro_test)
      }
    } else{
      ANS <- return_data_frame(lapply(X$Y_data, Stat_Mean_Freq))
      if (include.test & !normality.test) {
        mycorrtable <- Corr1(X$Y_data,
                             nrow(ANS),
                             corr_test,
                             include.p,
                             include.stars,
                             cor_diagonale_up)
        note <- paste("Korrelation nach" , Hmisc::upFirst(type))
        
        if (nrow(ANS) != nrow(mycorrtable))
          ANS <-  cbind(ANS, Error = "gemischtes Skalenniveau")
        else
          ANS <- cbind(ANS, mycorrtable)
        
        
      } else if (include.test & normality.test) {
        ANS <- cbind(ANS,
                     "shapiro test" = unlist(lapply(X$Y_data,
                                                    function(x) {
                                                      if (is.numeric(x)) {
                                                        APA(shapiro.test(x))
                                                      } else {
                                                        rbind(paste(APA(shapiro.test(
                                                          as_numeric(x)
                                                        ))
                                                        ,  class(x)),
                                                        rep("", nlevels(x) -
                                                              1))
                                                      }
                                                    })))
      } else{
        NULL
      }
    }
    ANS <- prepare_output(ANS, caption, note, N)
    return(ANS)
    
    #- GRUPPENVERGLEICH -
  } else{
    ANS_list <- list() #antwortliste
    for (ix in X$xname) {
      #--  Mehere Gruppenvariablen aufschluesseln
      #cat("\nerate_statistik2: Gruppenvergleich aufschluesseln\n")
      caption <- paste(ix, caption)
      Xi <- X$X_data[, ix]  # Gruppe ist X'
      x_name <-
        ifelse(is.null(attr(X$X_data, "label")), ix, attr(X$X_data, "label")) ## hmisc::LAbel
      y_name <-
        sapply(X$xname, function(y)
          ifelse(is.null(attr(
            X$Y_data, "label"
          )),
          y, attr(X$Y_data, "label")))
      ANS <- NULL
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
        for (lev in 1:length(my_levels)) {
          index_zaeler <- 0
          my_subset <- which(Xi == my_levels[lev])
          
          if (length(X$yname) == 1)
            ans <- return_data_frame(Stat_Mean_Freq(X$Y_data[my_subset, 1]),
                                     var_vektor = X$yname)
          else
            ans <- return_data_frame(lapply(X$Y_data[my_subset, ], Stat_Mean_Freq))
          
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
