#' @name PCA
#' @rdname PCA
#' @title PCA
#' @description Gestohlen von psych::print.psych.fa \link{print.psych}.
#' Die Ergebnisse stimmen mit SPSS weitgehend ueberein. Ueberprueft habe ich das
#' anhand der Daten vom Stampfl.
#' mehr unter \link{fkv}
#'
#' Interpretation:
#'
#' KorrelationsMatrix: Korrelation von mindestens r=.30
#'
#' Erklaerte Gesamtvarianz (Eigenwerte)
#' Erklärte Gesamtvarianz => Kummulierte %
#'
#'
#'
#' Measures of Appropriateness of Factor Analysis
#' KMO- und Bartlett-Test (partiellen Korrelationen zwischen Itempaaren)
#' KMO: Minimum   0.50
#'
#' Bartlett's test of sphericity (überprueft die Nullhypothese, ob die Korrelationsmatrix eine Identitaetsmatrix ist.)
#' gut ist wenn p < .050
#'
#' Anzahl an Faktoren
#' Faustregel nur Eigenwerte größer als eins (Kaiser-Guttman-Kriterium)
#' oder Scree-Plot hier wird der charakteristische Knick als kriterium gewaehlt
#'
#'
#' Rotierte Komponentenmatrix (varimax)
#'
#'
#' Quelle: \url{http://statistikguru.de/spss/hauptkomponentenanalyse/auswerten-und-berichten.html}
#'
#' Deutsch
#'
#' Es wurde eine Hauptkomponentenanalyse durchgeführt, um die wichtigsten,
#' unabhängigen Faktoren zu extrahieren. Das Kaiser-Meyer-Olkin-Kriterium war .867 und der Bartlett-Test hochsignifikant (p < .001), was eine ausreichend hohe Korrelation zwischen Items darstellt, um eine Hauptkomponentenanalyse durchzuführen. Nur Faktoren mit Eigenwerten ≥ 1 wurden in Betracht gezogen (Guttman, 1954; Kaiser, 1960).
#' Eine Überprüfung des Kaiser–Kriteriums und Scree-Plots rechtfertigte
#' die Extraktion von zwei Faktoren, jeweils mit Eigenwerten über 1,
#' die eine Gesamtvarianz von 60.45 aufklären. Unter den Lösungen lieferte die Varimax rotierte zweifaktor-Lösung die Lösung, die am besten zu interpretieren war, bei der die meisten Items nur auf einen der beiden Faktoren hohe Ladungen zeigten.
#'
#' English
#'
#' We performed a Principal Component Analysis (PCA) to extract the most important independent factors. The Kaiser–Meyer–Olkin measure of sampling adequacy was .867, representing a relatively good factor analysis, and Bartlett’s test of Sphericity was significant (p < .001), indicating that correlations between items were sufficiently large for performing a PCA. Only factors with eigenvalues ≥ 1 were considered (Guttman, 1954; Kaiser, 1960).
#' Examination of Kaiser’s criteria and the scree-plot yielded empirical justification for retaining two factors with eigenvalues exceeding 1 which accounted for 60.45 of the total variance. Among the factor solutions, the varimax-rotated two-factor solution yielded the most interpretable solution, and most items loaded highly on only one of the two factors.
#'
#' @param data Output from a psych function
#' @param nfactors Anzahl Faktoren default  nfactors=1
#' @param residuals Anzeige der Residuen  default: FALSE
#' @param rotate Rotation default = "varimax"
#' @param n.obs  default= NA
#' @param covar  default= FALSE
#' @param scores  default= TRUE
#' @param missing  default= FALSE
#' @param impute  Imputation default = "median" NA ist keine Imputation
#' @param oblique.scores  default= TRUE
#' @param method  default= "regression"
#' @param type Was soll ausgegeben werden default alles also
#' type=c("pca", "plot", "kmo")
#' @param  w Grafik Fenster w=5
#' @param  h Grafik Fenster h=5
#' @param sort  default= TRUE,
#' @param ... weitere Argumente wie:
#' digits = 2 Number of digits to use in printing
#' all = FALSE if all=TRUE, then the object is declassed and all output from the function is printed
#' cut = NULL Cluster loadings < cut will not be printed. For the factor analysis functions (fa and factor.pa etc.), cut defaults to 0, for ICLUST to .3, for omega to .2.
#' sort = TRUE Cluster loadings are in sorted order
#' @return HTML oder Test Output
#' @export
#' @examples
#' pc <- principal(Harman74.cor$cov,4,rotate="varimax")
#' mr <- fa(Harman74.cor$cov,4,rotate="varimax")  #minres factor analysis
#' pa <- fa(Harman74.cor$cov,4,rotate="varimax",fm="pa")  # principal axis factor analysis
#'
#' round(factor.congruence(list(pc,mr,pa)),2)
#' pc2 <- principal(Harman.5,2,rotate="varimax",scores=TRUE)
#' round(cor(Harman.5,pc2$scores),2)  #compare these correlations to the loadings
#' biplot(pc2,main="Biplot of the Harman.5 socio-economic variables")
#' APA2(pc)
#' APA2(mr)
#' APA2(pa)
#' APA2(pc2)
#'
#'
#' # -- Buehl Faktoranalyse Seite 475
#' ##some(DF <- GetData("Raw data/FKV.SAV"))
#'  #-  Freiburger Fragebogen zur Krankheitsverarbeitung
#' ##DF <- DF[,-1]
#' #APA2( ~., DF, test=T)
#' library(arm)
#' windows(5,5)
#' corrplot(DF, abs=TRUE, n.col.legend=7)#  corrplot {arm}
#' SaveData( )
#' Principal2(DF, 5, cut=.35)
#' APA2(DF[,c(5,16,22,9,26,6,35,33,12,34,4)])
#' End(FALSE)
#'
PCA2 <- function(x, ...) {
  UseMethod("PCA2")
}

#' @rdname PCA
#' @export
PCA2.default <- function(x, ...) {
  Principal2(x, ...)
}

#' @rdname PCA
#' @export
#'
Principal2<-function(data, ...,
                     type=c("pca", "plot", "kmo"), w=5, h=5,
                     caption="Standardized loadings (pattern matrix) based upon correlation matrix",
                     note="",
                     main=""
                     ){

  res <- Principal(data, ...)

  if("pca" %in% tolower(type)){
    res$Loadings %>% Output(caption=caption)
    res$Test %>% Output(caption=caption)
  }
    if( "plot" %in% tolower(type)){
      x.matrix <- as.matrix(data)
      windows(w,h)
      psych::VSS.scree(cor(x.matrix), main=main)
      SaveData("scree_plot", caption="scree plot")
    }
    if("kmo" %in% tolower(type)){
      res$KMO %>% Output(caption=caption)}
}


# Principal2_alt <- function(data,
#                        nfactors = 1,
#                        residuals = FALSE,
#                        rotate = "varimax",
#                        n.obs = NA,
#                        covar = FALSE,
#                        scores = TRUE,
#                        missing = FALSE,
#                        impute = "median",
#                        oblique.scores = TRUE,
#                        method = "regression",
#                        type=c("pca", "plot", "kmo"), w=5, h=5,
#                        sort = TRUE,
#                        N=nrow(data),
#                        caption="Standardized loadings (pattern matrix) based upon correlation matrix",
#                       ...){
#
#
#    if(!is_all_identical2(data)){
#        Text("Die Skalenniveaus sind gemischt daher ist die Berechnung nicht m?glich.")
#        return(NULL)
#    } else if( any(sapply(data, is.factor))){
#        Text("Die Daten sind Factoren daher umwandeln nach Numeric.")
#        data <- dapply2(data, fun= function(x) as.numeric(x))
#    }
#
#     #-- Imputieren wegen NAs
#     if(anyNA(data)){
#         if(!is.na(impute)){
#           Text("Imputation fehlender Werte (", impute, ") N=", N)
#             data <- data.frame(
#               lapply(data,
#                           function(x)
#                               if(anyNA(x)) {
#                                  if(impute=="median"){
#                                    x[which(is.na(x))] <-  median(x, na.rm=T)
#                                    return(x)
#                                  }else if(impute=="mean"){
#                                    x[which(is.na(x))] <-  mean(x, na.rm=T)
#                                    return(x)
#                                    }
#                                  }else {x}
#                       ))
#         }else{
#
#             data<- na.omit(data)
#             Text("Faelle mit fehlenden Werten auschliessen N=", N, "n=", nrow(data))
#     }}
#
#     resPCS<-NULL
#
#
#     if("pca" %in% tolower(type)){
#         resPCS <- psych::principal(data,
#                             nfactors = nfactors,
#                             residuals = residuals,
#                             rotate = rotate)
#
# #  Text(class(resPCS))
#         APA2(resPCS,
#              note= rotate,
#              sort=sort,
#              caption=caption, ...)
#     }
#     if( "plot" %in% tolower(type)){
#         x.matrix <- as.matrix(data)
#         windows(w,h)
#         psych::VSS.scree(cor(x.matrix), main="")
#         SaveData("scree_plot", caption="scree plot")
#     }
#     if("kmo" %in% tolower(type)){
#       #-- Kaiser, Meyer, Olkin Measure of Sampling Adequacy
#         resK<-  psych::KMO(data)
#         resK$MSA
#         resBar <- psych::cortest.bartlett(data) #-  Bartlett-Test  auf  Sphärizität,
#         Output(
#             data.frame(
#                 Measures =c("Kaiser-Meyer-Olkin Measure",
#                             "Bartlett's test of sphericity") ,
#                 Statistic=c( Format2(resK$MSA,2),
#                              rndr_X(resBar$chisq,
#                                     resBar$df, NULL, resBar$p.value)
#                             # paste0("Chisq(df=",resBar$df, ")=",
#                                     #Format2(resBar$chisq,2),
#                              #       " p=",ffpvalue(resBar$p.value))
#                              )
#             )
#             , caption="Measures of Appropriateness of Factor Analysis")
#     }
#   invisible(resPCS)
# }
#
#
#

#' @rdname PCA
#' @export
Principal <- function(data,
                       nfactors = 1,
                       cut=.35,
                       residuals = FALSE,
                       rotate = "varimax",
                       n.obs = NA,
                       covar = FALSE,
                       scores = TRUE,
                       missing = FALSE,
                       impute = "median",
                       oblique.scores = TRUE,
                       method = "regression",
                     #  type=c("pca", "plot", "kmo"), w=5, h=5,
                       sort = TRUE,
                       N=nrow(data),
                       caption="Standardized loadings (pattern matrix) based upon correlation matrix",
                       ...){


  if(!is_all_identical2(data)){
    Text("Die Skalenniveaus sind gemischt daher ist die Berechnung nicht m?glich.")
    return(NULL)
  } else if( any(sapply(data, is.factor))){
    Text("Die Daten sind Factoren daher umwandeln nach Numeric.")
    data <- dapply2(data, fun= function(x) as.numeric(x))
  }

  #-- Imputieren wegen NAs
  if(anyNA(data)){
    if(!is.na(impute)){
      Text("Imputation fehlender Werte (", impute, ") N=", N)
      data <- data.frame(
        lapply(data,
               function(x)
                 if(anyNA(x)) {
                   if(impute=="median"){
                     x[which(is.na(x))] <-  median(x, na.rm=T)
                     return(x)
                   }else if(impute=="mean"){
                     x[which(is.na(x))] <-  mean(x, na.rm=T)
                     return(x)
                   }
                 }else {x}
        ))
    }else{

      data<- na.omit(data)
      Text("Faelle mit fehlenden Werten auschliessen N=", N, "n=", nrow(data))
    }}






    resPCS <- extract_principal(
                  psych::principal(data,
                               nfactors = nfactors,
                               residuals = residuals,
                               rotate = rotate))

    #-- Kaiser, Meyer, Olkin Measure of Sampling Adequacy
    resK<-  psych::KMO(data)
   # resK$MSA
    resBar <- psych::cortest.bartlett(data) #-  Bartlett-Test  auf  Sphärizität,
    resKMO <- prepare_output(
                data.frame(
                Measures =c("Kaiser-Meyer-Olkin Measure",
                            "Bartlett's test of sphericity") ,
                Statistic=c(Format2(resK$MSA,2),
                            rndr_X(resBar$chisq,
                            resBar$df, NULL, resBar$p.value)))
             , caption="Measures of Appropriateness of Factor Analysis")

   list( Loadings=resPCS$loadings,
        Eigenvalue=resPCS$eigen,
        Test=resPCS$test,
        KMO=resKMO)
}




extract_principal<-
  function (x,
            caption="",
            note="",
            digits = 2,   #Number of digits to use in printing
            all = FALSE,  #if all=TRUE, then the object is declassed
            #and all output from the function is printed
            cut = .30,   #Cluster loadings < cut will not be printed.
            #For the factor analysis functions (fa and factor.pa etc.),
            #cut defaults to 0, for ICLUST to .3, for omega to .2.
            sort = TRUE,  #Cluster loadings are in sorted order
            suppress.warnings = TRUE,
            ...)
  {
    res <- NULL
   # res_loadings<-NULL
    #res_stdloadings<- NULL
    #res_einen<-NULL

# Vorbereitung ------------------------------------------------------------


    colnames(x$loadings) <- paste0("PC", 1:(length(colnames(x$loadings))))

    if (!is.matrix(x) && !is.null(x$fa) && is.list(x$fa))
      x <- x$fa
    if (!is.null(x$fn)) {
      if (x$fn == "principal") {
        caption <- paste(caption, "Principal Components Analysis")
      }
      else {
        caption <- paste(caption, "Factor Analysis using method = ", x$fm)
      }
    }
    load <- x$loadings
    if (is.null(cut))   cut <- 0
    nitems <- dim(load)[1]
    nfactors <- dim(load)[2]
    if (sum(x$uniqueness) + sum(x$communality) > nitems) { covar <- TRUE
    }else { covar <- FALSE}

    loads <- data.frame(item = seq(1:nitems),
                        cluster = rep(0, nitems), unclass(load))
    u2.order <- 1:nitems

# Sortieren ---------------------------------------------------------------


    if (sort) {
      loads$cluster <- apply(abs(load), 1, which.max)
      ord <- sort(loads$cluster, index.return = TRUE)
      loads[1:nitems, ] <- loads[ord$ix, ]
      rownames(loads)[1:nitems] <- rownames(loads)[ord$ix]
      items <- table(loads$cluster)
      first <- 1
      item <- loads$item
      for (i in 1:length(items)) {
        if (items[i] > 0) {
          last <- first + items[i] - 1
          ord <- sort(abs(loads[first:last, i + 2]),
                      decreasing = TRUE,
                      index.return = TRUE)
          u2.order[first:last] <- item[ord$ix + first - 1]
          loads[first:last, 3:(nfactors + 2)] <- load[item[ord$ix + first - 1], ]
          loads[first:last, 1] <- item[ord$ix + first - 1]
          rownames(loads)[first:last] <- rownames(loads)[ord$ix + first - 1]
          first <- first + items[i]
        }
      }
    }

    if (max(abs(load) > 1) && !covar)
      note <- "Warning: A Heywood case was detected"

    ncol <- dim(loads)[2] - 2
    rloads <- round(loads, digits)
    fx <- format(rloads, digits = digits)
    nc <- nchar(fx[1, 3], type = "c")
    fx.1 <- fx[, 1, drop = FALSE]
    fx.2 <- fx[, 3:(2 + ncol)]
    load.2 <- as.matrix(loads[, 3:(ncol + 2)])
    fx.2[abs(load.2) < cut] <- paste(rep(" ", nc), collapse = "")
    if (sort) {
      fx <- data.frame(V = fx.1, fx.2)
      if (dim(fx)[2] < 3)
        colnames(fx) <- c("V", colnames(x$loadings))
    }
    else {
      fx <- data.frame(fx.2)
      colnames(fx) <- colnames(x$loadings)
    }

    if (nfactors > 1) {
      if (is.null(x$Phi)) {
        h2 <- rowSums(load.2^2)
      }
      else {
        h2 <- diag(load.2 %*% x$Phi %*% t(load.2))
      }
    }
    else {
      h2 <- load.2^2
    }

    if (!is.null(x$uniquenesses)) {
      u2 <- x$uniquenesses[u2.order]
    }
    else {
      u2 <- (1 - h2)
    }

    vtotal <- sum(h2 + u2)

    if (isTRUE(all.equal(vtotal, nitems))) {
      # cat("Standardized loadings (pattern matrix) based upon correlation matrix\n")
      com <- x$complexity[u2.order]

      if (!is.null(com)) {
        res$loadings <- cbind_pca(Items=rownames(fx),
                                  fx,
                                  h2=Format2(h2, 2),
                                  u2=Format2(u2, 2),
                                  com=Format2(com, 2),
                                  caption=caption, note=note)
      }
      else {
        res$loadings<-
          cbind_pca(Items=rownames(fx),
                    fx,
                    h2=Format2(h2,2),
                    u2=Format2(u2,2),
                    caption=caption, note=note)
      }
    }
    else {
      res$loadings<-cbind_pca(Items=rownames(fx),
                              fx,
                              h2=Format2(h2,2),
                              u2=Format2(u2,2),
                              #H2 = Format2(h2/(h2 + u2),2),
                              # U2 = Format2(u2/(h2 + u2),2),
                               caption="Unstandardized loadings (pattern matrix) based upon covariance matrix",
                              note=note)
    }

    if (is.null(x$Phi)) {
      if (nfactors > 1) {
        vx <- colSums(load.2^2)
      }
      else {
        vx <- sum(load.2^2)
      }
    }
    else {
      vx <- diag(x$Phi %*% t(load) %*% load)
    }
    names(vx) <- colnames(x$loadings)
    varex <- rbind(`SS loadings` = vx)
    varex <- rbind(varex, `Proportion Var` = vx/vtotal)
    if (nfactors > 1) {
      varex <- rbind(varex, `Cumulative Var` = cumsum(vx/vtotal))
      varex <- rbind(varex, `Proportion Explained` = vx/sum(vx))
      varex <- rbind(varex, `Cumulative Proportion` = cumsum(vx/sum(vx)))
    }

    res$eigen <- cbind_eigen(varex)

    if (!isTRUE(all.equal(vtotal, nitems))) {
      # cat("\n Standardized loadings (pattern matrix)\n")
      fx <- format(loads, digits = digits)
      nc <- nchar(fx[1, 3], type = "c")
      fx.1 <- fx[, 1, drop = FALSE]
      fx.2 <- round(loads[, 3:(2 + ncol)]/sqrt(h2 + u2), digits)
      load.2 <- loads[, 3:(ncol + 2)]/sqrt(h2 + u2)
      fx.2[abs(load.2) < cut] <- paste(rep(" ", nc), collapse = "")
      fx <- data.frame(V = fx.1, fx.2)
      if (dim(fx)[2] < 3)
        colnames(fx) <- c("V", colnames(x$loadings))
      if (nfactors > 1) {
        h2 <- h2/(h2 + u2)
      }
      else {
        h2 <- h2/(h2 + u2)
      }
      u2 <- (1 - h2)
      res$stdloadings <- cbind_pca(Items=rownames(fx),
                                   fx,
                                   h2=Format2(h2,2),
                                   u2=Format2(u2,2),
                                   caption="Standardized loadings (pattern matrix)")
      if (is.null(x$Phi)) {
        if (nfactors > 1) {
          vx <- colSums(load.2^2)
        }
        else {
          vx <- diag(t(load) %*% load)
          vx <- vx * nitems/vtotal
        }
      }
      else {
        vx <- diag(x$Phi %*% t(load) %*% load)
        vx <- vx * nitems/vtotal
      }
      names(vx) <- colnames(x$loadings)
      varex <- rbind(`SS loadings` = vx)
      varex <- rbind(varex, `Proportion Var` = vx/nitems)
      if (nfactors > 1) {
        varex <- rbind(varex, `Cumulative Var` = cumsum(vx/nitems))
        varex <- rbind(varex, `Cum. factor Var` = cumsum(vx/sum(vx)))
      }
      res$eigen <- cbind_eigen(varex)
    }




    res$test <- prepare_output(
      data.frame(
        Measures=c("n.obs","Mean item complexity","RMSR",
                   "empirical chi square"),
        Statistics=c(x$n.obs,
                   round(mean(x$complexity), 1),
                   Format2(x$rms, 2),
                   rndr_X(x$chi, NULL, NULL, x$EPVAL))),
      caption= paste("Test of the hypothesis that ", nfactors,
                     if (nfactors == 1)
                       " component is"
                     else " components are", " sufficient."),
      N=x$n.obs)

    res
  }

cbind_eigen <- function(x,
                        caption="Erklaerte Gesamtvarianz (Eigenwerte)",
                        digits=3)  {
  prepare_output(round(x, digits), caption=caption)
}

cbind_pca <- function(Items, fx, h2, u2, com,
                      caption="", note=""){
  names(fx)[1]<- "Nr"

  prepare_output(cbind(Item=Items, fx, h2),
                 caption=caption, note=note)
}
