#' @rdname APA2
#' @export
#' @description    A Poisson regression was run to predict the number of publications an academic publishes in the last 12 months based on the experience of the academic and the number of hours an academic spends each week working on research. For every extra hour worked per week on research, 1.044 (95% CI, 1.004 to 1.085) times more publications were published, a statistically significant result, p = .030.
#' https://statistics.laerd.com/spss-tutorials/poisson-regression-using-spss-statistics.php
APA2.lm<- function(...) APA_Table(...)

#' @rdname APA2
#' @export
APA2.anova <- function(fit, ...) {
  Output(fix_format(fit), ...)
}


#' @rdname APA2
#' @export
APA2.htest <- function(x, caption = "", ...) {
    # t.test
    if (names(x$statistic) == "t")
        Output(
            fix_data_frame2(
                Source = x$data.name,
                T = x$statistic,
                df = x$parameter,
                p.value = x$p.value
            ),
            caption = paste(x$method, caption)
            ,
            ...
        )
    else
        Output(
            fix_data_frame2(
                Source = x$data.name,
                W = x$statistic,
                p.value = x$p.value
            ),
            caption = paste(x$method, caption)
            ,
            ...
        )
}


#' @rdname APA2
#' @export
APA2.pairwise.htest <-
    function(x, caption = "", ...) {
        #pairwise.t.test
        #-- ?pairwise.wilcox.test
        Output(
            data.frame(
                Source = row.names(x$p.value),
                Format2(
                    as.data.frame(x$p.value),
                    digits = 3 ,
                    lead.zero = FALSE
                )
            ),
            caption = paste(x$data.name, x$method, "(p-Value)", caption)
            ,
            ...
        )
    }





#' @rdname APA2
#' @export
APA_lme <- function(fit,
                     caption = "" ,
                     note = "",
                     type = "III",

                     anova = TRUE,
                     anova_type = "F",
                     #  F-werte (wie SPSS) oder Chi (car::Anova)
                     ...) {
    if(!is.character(caption)) {
        Text("Achtung: bei Arbument ... ist eine >caption< mit anzugeben!")
        return(NULL)
        }
    APA_regression <<- TRUE
    fit_sum <- summary(fit)
    fit_param <- as.data.frame(fit_sum$tTable)  ##  xtTab
    names(fit_param)[1] <- c("Estimate")

    if (anova_type == "F") {
        #cat(Anov)
        #  cat("Verwende die Funktion anova stadt car::Anova\n")
        #Text("Fehler in den Factorstufen es d?rfen keine leeren Factoren vorkommen")
        #-- Unbekannter aufgrung von anzahl an Factorstufen
        Anov <- anova(fit)
        Anov <- as.data.frame(Anov)

        goodnes <- cbind(
            Obs = fit_sum$dims[["N"]],
            round(r.squared(fit)[, 4:6], 2) ,
            BIC = round(fit_sum$BIC, 2),
            logLik = round(c(fit_sum$logLik), 2)
        )

    } else{
        Anov <- car::Anova(fit, type = type)

        goodnes <- cbind(
            Obs = fit_sum$dims[["N"]],
            round(r.squared(fit)[, 4:6], 2) ,
            BIC = round(fit_sum$BIC, 2),
            logLik = round(c(fit_sum$logLik), 2)
        )
    }


    Output(fix_data_frame2(Source=rownames(fit_param), fit_param),
        caption = paste("Regression Model:", caption),
        note = note

    )


    if (anova)
        Output(fix_data_frame2(Source = rownames(Anov), Anov),
            caption = paste("ANOVA:", caption),
            note = note
        )

    Output(goodnes,
        caption = paste("Goodness-of-fit", caption),
        note = "R-Quadrat entspricht Marginal und Conditional"
    )
}










#APA_TukeyHSD <- function(fit, ...){
#  if (options()$prompt[1] == "HTML> ")  HTML(fit)
#  else print(fit)
#
#}




#' @rdname APA2
#' @export
APA_mediate <- function(l, ...) {
    Print2_mediate <- function(fit,
                               print = TRUE,
                               caption = "Mediation",
                               ...) {
        print_summary_mediate2 <-
            function(x,
                     print = print,
                     caption = caption,
                     ...) {
                clp <- 100 * x$conf.level
                cat("\n")
                cat("Causal Mediation Analysis \n")
                if (x$boot) {
                    cat("Confidence Intervals Based on Nonparametric Bootstrap\n\n")
                } else {
                    cat("Quasi-Bayesian Confidence Intervals\n")
                }

                if (!is.null(x$covariates)) {
                    cat(
                        "(Inference Conditional on the Covariate Values Specified in `covariates')\n\n"
                    )
                }

                isLinear.y <-
                    (
                        (class(x$model.y)[1] %in% c("lm", "rq")) ||
                            (
                                inherits(x$model.y, "glm") &&
                                    x$model.y$family$family == "gaussian" &&
                                    x$model.y$family$link == "identity"
                            ) ||
                            (
                                inherits(x$model.y, "survreg") &&
                                    x$model.y$dist == "gaussian"
                            )
                    )

                printone <- !x$INT && isLinear.y

                if (printone) {
                    # Print only one set of values if lmY/quanY/linear gamY without interaction
                    smat <- c(x$d1, x$d1.ci, x$d1.p)
                    smat <-
                        rbind(smat, c(x$z0, x$z0.ci, x$z0.p))
                    smat <-
                        rbind(smat, c(x$tau.coef, x$tau.ci, x$tau.p))
                    smat <-
                        rbind(smat, c(x$n0, x$n0.ci, x$n0.p))
                    rownames(smat) <-
                        c(
                            "Mediation Effect",
                            "Direct Effect",
                            "Total Effect",
                            "Proportion via Mediation"
                        )
                } else {
                    smat <- c(x$d0, x$d0.ci, x$d0.p)
                    smat <-
                        rbind(smat, c(x$d1, x$d1.ci, x$d1.p))
                    smat <-
                        rbind(smat, c(x$z0, x$z0.ci, x$z0.p))
                    smat <-
                        rbind(smat, c(x$z1, x$z1.ci, x$z1.p))
                    smat <-
                        rbind(smat, c(x$tau.coef, x$tau.ci, x$tau.p))
                    smat <-
                        rbind(smat, c(x$n0, x$n0.ci, x$n0.p))
                    smat <-
                        rbind(smat, c(x$n1, x$n1.ci, x$n1.p))
                    smat <-
                        rbind(smat, c(x$d.avg, x$d.avg.ci, x$d.avg.p))
                    smat <-
                        rbind(smat, c(x$z.avg, x$z.avg.ci, x$z.avg.p))
                    smat <-
                        rbind(smat, c(x$n.avg, x$n.avg.ci, x$n.avg.p))
                    rownames(smat) <-
                        c(
                            "Mediation Effect_0",
                            "Mediation Effect_1",
                            "Direct Effect_0",
                            "Direct Effect_1",
                            "Total Effect",
                            "Proportion via Mediation_0",
                            "Proportion via Mediation_1",
                            "Mediation Effect (Ave.)",
                            "Direct Effect (Ave.)",
                            "Proportion via Mediation (Ave.)"
                        )
                }
                colnames(smat) <-
                    c(
                        "Estimate",
                        paste(clp, "% CI Lower", sep = ""),
                        paste(clp, "% CI Upper", sep =
                                  ""),
                        "p-value"
                    )



                #cat("\n")
                cat("Sample Size Used:", x$nobs, "\n")
                # cat("\n")
                cat("Simulations: ", x$sims, "\n\n")


                #invisible(x)
                if (print) {
                    if (options()$prompt[1] == "HTML> ")
                        HTML(
                            printCoefmat(smat, digits = 3) ,
                            caption = caption,
                            captionalign = "top",
                            Border = 0
                        )
                    else
                        printCoefmat(smat, digits = 3)
                } else {
                    smat
                }



            }

        med_summary <- summary(fit, ...)
        print_summary_mediate2(med_summary , print = print, caption = caption)
    }
    Print2(l, caption = caption, ...)
}







#' @rdname APA2
#' @export
APA_aovlist <- function(...) {
    if (options()$prompt[1] == "HTML> ")
        HTML(summary(...))
    else
        print(summary(...))
}

#' @rdname APA2
#' @export
APA_anova.lme <- function(fit, ...) {
    #-- siehe auch APA.aov
    #-- hier auf jeden fall noch ein Test-Skript bauen und ev eta2
    #-- die Fun ist fuer Messwiederholung
    #-- fit <- lme(y~x)
    #-- APA2(anova.lme(fit))
    res <- as.data.frame(fit)
    #res[, 3] <- ff(res[, 3], 2)
   # res[, 4] <- ffpvalue(res[, 4])
   # APA2(cbind(Source = rownames(res), res), type = "data.frame", ...)


    Text("APA_anova.lme")



    Output( fix_data_frame2(Source = rownames(res), res),
            ...)

}




#' @rdname APA2
#' @export
APA_lmerMod <- function(fit, ...) {
    Text(
        "
        ----
        Achtung: Paket library(lmerTest) laden.
        Bzw die update() Funktion nicht verwenden.
        ----
        "
    )

}




#' @rdname APA2
#' @export
APA_merModLmerTest <- function(fit,
                                caption = "" ,
                                note = "",

                                anova = TRUE,
                                random.effects=TRUE,
                                # anova_type="F",   #  type = "III",
                                #  F-werte (wie SPSS) oder Chi (car::Anova)
                                ...) {
cat("\nAPA_merModLmerTest noch nicht getestet!\n")
    #     he lmerTest package overloads the lmer function, so you can just
    #     re-fit the model using exactly the same code, but the summary()
    #     will now include approximate degrees of freedom and p-values. This
    #     implementation is extremely easy to use, but can be a little maddening
    #     if you forget whether your model is a an object of type lmerMod
    #     or merModLmerTest.

    # require2(lmerTest)
    res <- lmerTest::summary(fit)
    anovaTabel <- as.data.frame(anova(fit))

    goodnes <- cbind(
        Obs = res$devcomp$dims["N"],
        round(r.squared.merMod(fit)[, 4:6], 2),
        # BIC = round(res$BIC,2),
        logLik = round(c(as.numeric(res$logLik)), 2),
        REML =  round(res$devcomp$cmp["REML"], 2))


Output(fix_data_frame2(Source = rownames(res$coefficients),
                       res$coefficients[, 1:4],
                       p.value = res$coefficients[, 5]
                      ),
        caption = paste("Regression Model", caption),
        note = note)

if(random.effects) {
        myranef<- ranef(fit)
        for( i in  names(myranef)){
         x <- myranef[[i]]
         names(x)[1]<- "Intercept"
         x <- fix_data_frame2(Source = rownames(x), x)
         names(x)[1]<- i
         Output(x, caption="Random Effect")

         chiTest <-  rand(fit)$rand.table
         Output(
         fix_data_frame2(Source = rownames(chiTest), chiTest )
         , caption="Likelihood Ratio Test")


        }
    }


if (anova)
        Output( fix_data_frame2(
                Source = rownames(anovaTabel),
                anovaTabel[, c(1,3:6)]),
            caption = paste("ANOVA:", caption),
            note = "Analysis of Variance Table of type III")

    Output(goodnes,
        caption = paste("Goodness-of-fit", caption),
        note = "R-Quadrat entspricht Marginal und Conditional"
    )

}


