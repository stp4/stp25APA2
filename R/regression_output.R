#' @rdname APA_Table
#' @description  APA_Table(..., type="long") ist ein Workaround von texreg
#'
#' include.pseudo = FALSE Preudo R
#'
#'  Cox und Snell R2: [ 0.2 = akzeptabel, 0.4 = gut ]
#'
#'  Nagelkerke R2: [ 0.2 = akzeptabel, 0.4 = gut, 0.5 = sehr gut]
#'
#'  McFaddens R2: [ 0.2 = akzeptabel, 0.4 = gut ]
#'
#' include.ftest = FALSE  noch nicht fertig
#' include.loglik = FALSE noch nicht fertig
#'
#' include.CI=FALSE leicht unterschiedlich zu confint
#' texreg berechnet über die SE und qnorm (Normal Distribution)
#' confint bei lm über qt (student-T-Distribution)
#'
#'  z = qnorm(1 - ((1 - ci.level)/2))
#'   coef + (z * se) und coef - (z * se)
#'
#'
#' rgroup = c("Parameter", "Goodness of fit")
#' col_names = c("b", "SE", "p")
regression_output  <-
  function (fits,
            # Liste mit lm, glm, usw
            caption = "",
            note = "",
            custom.model.names = NULL,
            digits = 2,
            p.value = TRUE,
            # Sternchen oder p-Werte
            col_names = NULL,
            rgroup = c("Parameter", "Goodness of fit"),
            # Parameter Goodness of fit

            include.pseudo = FALSE,
            #Preudo R
            include.ftest = FALSE,
            # noch nicht fertig
            include.loglik = FALSE,
            # noch nicht fertig
            include.CI = FALSE,
            ...)
  {
    #cat("\ninclude.pseudo: ")
    #  print(include.pseudo)
    #  cat("\n")
    # Extract Parameter -------------------------------------------------------
    models <- texreg:::get.data(fits)
    gof.names <-
      texreg:::get.gof(models) #return:  gof.names[1] "R$^2$"      "Adj. R$^2$" "Num. obs."  "RMSE"
    models <- texreg:::correctDuplicateCoefNames(models)
    gofs <- texreg:::aggregate.matrix(
      models,
      gof.names,
      custom.gof.names = NULL,
      digits = 2,
      returnobject = "gofs"
    )
    m <- texreg:::aggregate.matrix(
      models,
      gof.names,
      custom.gof.names = NULL,
      digits = 2,
      returnobject = "m"
    )
    m <- texreg:::rearrangeMatrix(m)


    if (include.CI) {

      #Das mit den CIs noch ändern


      models2 <- texreg:::get.data(fits)
      #models2 <-
      #texreg:::ciforce(models2, ci.force = TRUE, ci.level = 0.95)
        #Kopie ciforce ohne die Fehlerprüfung
        ci.level<- .95
        note <- "95%-CI based on asymptotic normality"
        for (i in 1:length(models2)) {
          if (length(models2[[i]]@se) > 0) {
            z <- qnorm(1 - ((1 - ci.level)/2))
            upper <- models2[[i]]@coef + (z * models2[[i]]@se)
            lower <- models2[[i]]@coef - (z * models2[[i]]@se)
            models2[[i]]@ci.low <- lower
            models2[[i]]@ci.up <- upper
            models2[[i]]@se <- numeric(0)
            models2[[i]]@pvalues <- numeric(0)
          }
        }

       models2 <- texreg:::correctDuplicateCoefNames(models2)

      m_cis <- texreg:::aggregate.matrix(
        models2,
        gof.names,
        custom.gof.names = NULL,
        digits = 2,
        returnobject = "m"
      )
      m_cis <- texreg:::rearrangeMatrix(m_cis)

    }



    #- fuer Output Zwi Ueberschriftenebenen
    modnames <- gsub("_",
                     " ",
                     texreg:::modelnames(fits, models, custom.model.names))
    if (include.pseudo) {
      whichR2 <- sapply(fits, function(fitx) {
        if (any(class(fitx) %in% "lm")) {
          if (any(class(fitx) %in% "glm"))
            3 # Cox + Nagek
          else
            0
        } else
          2  # Magrinal + Cond
      })

      resR2 <- NULL

      if (any(whichR2 == 2)) {
        for (i in fits) {
          if (any(class(i) %in% "lm"))
            R2i <- c(NA, NA)
          else
            R2i <- R2(i)
          names(R2i) <-
            c("Pseudo R2 (Marginal)", "Pseudo R2 (Conditional)")
          if (is.null(resR2))
            resR2 <- R2i
          else
            resR2 <- rbind(resR2, R2i)
        }
        gofs <- rbind(gofs, t(resR2))
      }

      if (any(whichR2 == 3)) {
        for (i in fits) {
          if (!any(class(i) %in% "glm"))
            R2i <- c(NA, NA, NA)
          else
            R2i <- R2(i)
          # McFadden's pseudo r-squared

          # r2ML Cox & Snell
          # Maximum likelihood pseudo r-squared

          # r2CU Nagelkerke
          # Cragg and Uhler's pseudo r-squared
          names(R2i) <- c("McFadden R2", "Cox & Snell R2", "Nagelkerke")
          if (is.null(resR2))
            resR2 <- R2i
          else
            resR2 <- rbind(resR2, R2i)
        }
        gofs <- rbind(gofs, t(resR2))
      }
    }


    # Gof ---------------------------------------------------------------------
    # sonderzeichen entfernen #"[^[:alnum:] :()]"[^[:alnum:]]
    rownames(gofs) <- gsub("[^[:alnum:] :().]", "", rownames(gofs))
    Numobs <-
      which(grepl("Num", rownames(gofs))) #  which(rownames(gofs)=="Numobs")

    if (length(fits) == 1) {
      gofs <- c(gofs[-Numobs, ], Num.obs = gofs[Numobs, ])
      gofs <- matrix(gofs, ncol = 1 , dimnames = list(names(gofs)))
    }
    else{
      gofs <- rbind(gofs[-Numobs, ], Num.obs = gofs[Numobs, ])
    }



    gofs[1:(nrow(gofs) - length(Numobs)), ] <- Format2(gofs[1:(nrow(gofs) -
                                                                 length(Numobs)),], 2)

    # p-Werte -----------------------------------------------------------------
    est_vars <- seq(1, ncol(m), by = 3)
    se_vars  <- seq(2, ncol(m), by = 3)
    p_vars   <- seq(3, ncol(m), by = 3)

    p_stars  <- ffsigstars(m[, p_vars])
    p_val    <- ffpvalue(m[, p_vars])

    m[, c(est_vars, se_vars)] <-
      Format2.matrix(m[, c(est_vars, se_vars)], digits)

    if (include.CI) {
      ci_vars <- 2:3
      for (i in 1:length(est_vars)) {

        m[, se_vars[i]] <- rndr_CI(m_cis[, ci_vars], digits)
        ci_vars <- ci_vars + 3
      }
      if (is.null(col_names))
        col_names <- c("b", "95%-CI ", "p")
    } else{
      if (is.null(col_names))
        col_names <- c("b", "SE", "p")
    }




    # Sternchen
    if (p.value) {
      n_param <- 3
      m[, p_vars] <- p_val
      colnames(m) <-  c(t(
        outer(modnames, paste0("_", col_names), FUN=paste0)))
    }
    else{
      m[, est_vars] <- mapply(paste0, m[, est_vars], p_stars)
      n_param <- 2
      m <- m[, -p_vars]
      colnames(m) <- c(t(
        outer(modnames,
              paste0("_", col_names)[1:2],FUN=paste0)))
    }


    # Gofs --------------------------------------------------------------------
    ngofs <- nrow(gofs)
    emptygofs <- rep(NA, ngofs * (n_param - 1))
    newgofs <- rownames(gofs)

    for (i in 1:length(modnames))
      gofs <- append(gofs, emptygofs, after = ngofs * (1 + n_param * (i -
                                                                        1)))

    gofs <- matrix(gofs , nrow = ngofs)
    rownames(gofs) <- newgofs

    result <- prepare_output(fix_to_data_frame(rbind(m, gofs)),
                             caption , note)

    Output(result, rgroup = rgroup, n.rgroup = nrow(m))
    invisible(result)
  }
