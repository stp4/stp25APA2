#' @rdname APA2
#' @export
APA2.fa<-function(...) APA2.principal(...)



#' @rdname APA2
#' @export
APA2.principal<-
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

    res_loadings<-NULL
    res_stdloadings<- NULL
    res_einen<-NULL

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
    # cat("\nCall: ")
    # print(x$Call)

    load <- x$loadings
    if (is.null(cut))   cut <- 0
    nitems <- dim(load)[1]
    nfactors <- dim(load)[2]
    if (sum(x$uniqueness) + sum(x$communality) > nitems) { covar <- TRUE
    }else { covar <- FALSE}

    loads <- data.frame(item = seq(1:nitems),
                        cluster = rep(0, nitems), unclass(load))
    u2.order <- 1:nitems

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
      note<- "Warning: A Heywood case was detected"

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
        res_loadings <- cbind_pca(Items=rownames(fx),
                             fx,
                             h2=Format2(h2,2)#,
                             #- u2=Format2(u2,2), # auskommentiert weil spss das nicht ausgibt
                             #- com=Format2(com,2)
                            , caption=caption, note=note)
        Output(res_loadings)
      }
      else {

        res_loadings<-
          cbind_pca(Items=rownames(fx),
                fx,
                h2=Format2(h2,2)#,
                #- u2=Format2(u2,2)
                , caption=caption, note=note)
        Output(res_loadings)

      }
    }
    else {
      # cat("Unstandardized loadings (pattern matrix) based upon covariance matrix\n")
      res_loadings<-cbind_pca(Items=rownames(fx),
                          fx,
                          h2=Format2(h2,2)#,
                          #  u2=Format2(u2,2),
                          #  H2 = Format2(h2/(h2 + u2),2),
                          #  U2 = Format2(u2/(h2 + u2),2)
                         ,caption="Unstandardized loadings (pattern matrix) based upon covariance matrix",
                         note=note)
      Output(res_loadings)
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
    # cat("\n")


    res_einen<- round(varex, digits)
    Output(res_einen,
           caption="Erklaerte Gesamtvarianz (Eigenwerte)")


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



      res_stdloadings <- cbind_pca(Items=rownames(fx),
                               fx,
                               h2=Format2(h2,2)#,
                               #  u2=Format2(u2,2)
                               ,caption="Standardized loadings (pattern matrix)")
      Output(res_stdloadings)
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
      #  cat("\n")

      res_einen<- round(varex, digits)
      Output(res_einen)
    }
    if (!is.null(x$Phi)) {
      if (!is.null(x$fn)) {
        if (x$fn == "principal") {
          cat("\n With component correlations of \n")
        }
        else {
          cat("\n With factor correlations of \n")
        }
      }
      colnames(x$Phi) <- rownames(x$Phi) <- colnames(x$loadings)
      Output(round(x$Phi, digits))
    }
    else {
      if (!is.null(x$rotmat)) {
        U <- x$rotmat
        ui <- solve(U)
        Phi <- t(ui) %*% ui
        Phi <- cov2cor(Phi)
        if (!is.null(x$fn)) {
          if (x$fn == "principal") {
            cat("\n With component correlations of \n")
          }
          else {
            cat("\n With factor correlations of \n")
          }
        }
        colnames(Phi) <- rownames(Phi) <- colnames(x$loadings)
        Output(round(Phi, digits))
      }
    }
    result2<-list()
    if (!is.null(x$complexity))
      # Text("Mean item complexity = ", round(mean(x$complexity), 1))
      result2[["mean"]]<- c("Mean item complexity", round(mean(x$complexity), 1))

    objective <- x$criteria[1]
    if (!is.null(objective)) {
      if (!is.null(x$fn)) {
        if (x$fn == "principal") {
          # Text("Test of the hypothesis that ", nfactors,
          #   if (nfactors == 1)
          #      " component is"
          #  else " components are", " sufficient.")

          result2[["head"]]<-  paste("Test of the hypothesis that ", nfactors,
                                     if (nfactors == 1)
                                       " component is"
                                     else " components are", " sufficient.")

        }
        else {
          Text("Test of the hypothesis that", nfactors,
               if (nfactors == 1)
                 "factor is"
               else "factors are", "sufficient.")

          result2[["head"]]<-  paste( "Test of the hypothesis that", nfactors,
                                      if (nfactors == 1)
                                        "factor is"
                                      else "factors are", "sufficient.")

        }
      }
      if (x$fn != "principal") {
        if (!is.null(x$null.dof)) {
          Text("The degrees of freedom for the null model are ",
               x$null.dof, " and the objective function was ",
               round(x$null.model, digits), ...)
        }
        if (!is.null(x$null.chisq)) {
          Text(" with Chi Square of ", round(x$null.chisq,
                                             digits))
        }
        Text("The degrees of freedom for the model are",
             x$dof, " and the objective function was ", round(objective,
                                                              digits), " ", ...)
      }
      if (!is.null(x$rms)) {
        #Text("The root mean square of the residuals (RMSR) is ",
        #   round(x$rms, digits), "\n")

        result2[["RMSR"]]<-   c("RMSR", Format2(x$rms, 2))
      }
      if (!is.null(x$crms)) {
        Text("The df corrected root mean square of the residuals is ",
             round(x$crms, digits), " ", ...)
      }
      if ((!is.null(x$nh)) && (!is.na(x$nh))) {
        Text("The harmonic number of observations is ",
             round(x$nh))
      }
      if ((!is.null(x$chi)) && (!is.na(x$chi))) {
        # Text(" with the empirical chi square ", round(x$chi,
        #                                              digits), " with prob < ", ffpvalue(x$EPVAL),
        #    " ", ...)

        result2[["chi"]]  <- c("empirical chi square",
                               rndr_X(x$chi, NULL, NULL, x$EPVAL)
                               #paste("X=",Format2(x$chi, 2),
                                #     "p=", ffpvalue(x$EPVAL))
                               )
      }
      if (x$fn != "principal") {
        if (!is.na(x$n.obs)) {
          Text("The total number of observations was ",
               x$n.obs, " with MLE Chi Square = ", round(x$STATISTIC,
                                                         digits),
               " with prob < ", signif(x$PVAL,  digits), "\n", ...)
        }
        if (!is.null(x$TLI))
          Text("Tucker Lewis Index of factoring reliability = ",
               round(x$TLI, digits + 1))
      }
      if (!is.null(x$RMSEA)) {
        Text("RMSEA index = ", round(x$RMSEA[1], digits +
                                       1), " and the", (1 - x$RMSEA[4]) * 100, "% confidence intervals are ",
             round(x$RMSEA[2:3], digits + 1), ...)
      }
      if (!is.null(x$BIC)) {
        Text("BIC = ", round(x$BIC, digits))
      }
    }
    if (!is.null(x$fit))
      Text("Fit based upon off diagonal values =", round(x$fit.off,
                                                         digits))
    if ((!is.null(x$fn)) && (x$fn != "principal")) {
      if (!is.null(x$R2)) {
        stats.df <- t(data.frame(sqrt(x$R2), x$R2, 2 * x$R2 -
                                   1))
        rownames(stats.df) <- c("Correlation of scores with factors  ",
                                "Multiple R square of scores with factors ",
                                "Minimum correlation of possible factor scores ")
        colnames(stats.df) <- colnames(x$loadings)
      }
      else {
        stats.df <- NULL
      }
      badFlag <- FALSE
      if ((is.null(x$R2)) || (any(max(x$R2, na.rm = TRUE) >
                                  (1 + .Machine$double.eps)))) {
        badFlag <- TRUE
        if (!suppress.warnings) {
          Text("\n WARNING, the factor score fit indices suggest that the solution is degenerate. Try a different method of factor extraction.\n")
          warning("the factor score fit indices suggest that the solution is degenerate\n")
        }
      }
      else {
        if (!is.null(stats.df)) {
          cat("\nMeasures of factor score adequacy             \n")
          print(round(stats.df, digits))
        }
        if (!is.null(x$stats$R2) && !badFlag) {
          cat("\nMeasures of factor score adequacy             ",
              colnames(x$loadings))
          cat("\nCorrelation of scores with factors           ",
              round(sqrt(x$R2), digits))
          cat("\nMultiple R square of scores with factors      ",
              round(x$R2, digits))
          cat("\nMinimum correlation of factor score estimates ",
              round(2 * x$R2 - 1, digits))
        }
      }
    }

    Output(data.frame(
            Measures=c(result2[["mean"]][1],
                       result2[["RMSR"]][1],
                       result2[["chi"]][1] ),
            Statistic=c(result2[["mean"]][2],
                        result2[["RMSR"]][2],
                        result2[["chi"]][2] )
    ), caption=result2[["head"]])

    result <- list(Vaccounted = varex)
    invisible(result)
  }
