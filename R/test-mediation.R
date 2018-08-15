#' @rdname Mediation
#' @title Mediation Funktion
#' @name Mediation
#' @description Einfache Mediation und Moderationsanalyse mit Sobel-Test
#' Mediation ist gegeben wenn alle Modelle signifikant (1) und (4)
#'
#'   (1) Y~X
#'   (2) Y~X+M  (wenn hier M signifikant ist => partielle Mediation)
#'   (4) M~X
#'
#'   Moderation ist gegeben wenn die Interaktion (X:M) signifikant ist
#'
#'   (3) Y~ X + M + X:M
#' @param model_y_x Model y~x
#' @param model_y_xm Model y~x+m
#' @param model_m_x Model m~x
#' @param model_inter_xm Model y~x*m
#' @param X Einflussvaria´ble
#' @param M Moderator
#' @param type fuer Regression APA_Tabelle()
#' @param caption Ueberschrift
#' @param ... not used
#' @return String (HTML)
#' @export
#' @examples
#'
#' library(effects)
#' library(stp25)
#' graphics.off()
#' setwd("C:/Users/wpete/Dropbox/3_Forschung/10_temp")
#' Projekt("html", "Mediation")
#'
#' Text(
#'   "
#'   Mediation ist gegeben wenn alle Modelle signifikant (1) und (4)
#'
#'   (1) Y~X
#'   (2) Y~X+M  (wenn hier M signifikant ist => partielle Mediation)
#'   (4) M~X
#'
#'   Moderation ist gegeben wenn die Interaktion (X:M) signifikant ist
#'
#'   (3) Y~ X + M + X:M
#'   "
#' )
#'
#' # Using the Jessor data discussed in Preacher and Kelley (2011), to illustrate
#' # the methods based on summary statistics.
#'
#' # An  example data (from Hayes)
#' DF <- rbind(
#'   c(-5.00, 25.00, -1.00),
#'   c(-4.00, 16.00, 2.00),
#'   c(-3.00, 9.00, 3.00),
#'   c(-2.00, 4.00, 4.00),
#'   c(-1.00, 1.00, 5.00),
#'   c(.00, .00, 6.00),
#'   c(1.00, 1.00, 7.00),
#'   c(2.00, 4.00, 8.00),
#'   c(3.00, 9.00, 9.00),
#'   c(4.00, 16.00, 10.00),
#'   c(5.00, 25.00, 13.00),
#'   c(-5.00, 25.00, -1.00),
#'   c(-4.00, 16.00, 2.00),
#'   c(-3.00, 9.00, 3.00),
#'   c(-2.00, 4.00, 4.00),
#'   c(-1.00, 1.00, 5.00),
#'   c(.00, .00, 6.00),
#'   c(1.00, 1.00, 7.00),
#'   c(2.00, 4.00, 8.00),
#'   c(3.00, 9.00, 9.00),
#'   c(4.00, 16.00, 10.00),
#'   c(5.00, 25.00, 13.00))
#' colnames(DF)<-c("emo", "soz.stress", "ausbildung")
#' DF<- as.data.frame(DF)
#'
#' #-----------------------------------------------------
#'
#' Text("Y=emo
#'      X=soz.stress
#'      Mediator/Moderator = ausbildung ")
#' APA2( ~ emo + soz.stress + ausbildung, DF)
#' model1 <-  lm(emo ~ soz.stress , DF)
#' model2 <-  lm(emo ~ soz.stress + ausbildung , DF)
#' model3 <-  lm(ausbildung ~  soz.stress , DF)
#' model4 <-  lm(emo ~ soz.stress * ausbildung , DF)
#'
#' Mediation2(model1, model2, model3, model4)
#'
#'
#' DF<- dapply2(DF, function(x)  scale(x))
#' model1 <-  lm(emo ~ soz.stress , DF)
#' model2 <-  lm(emo ~ soz.stress + ausbildung , DF)
#' model3 <-  lm(ausbildung ~  soz.stress , DF)
#' model4 <-  lm(emo ~ soz.stress * ausbildung , DF)
#'
#' Mediation2(model1, model2, model3, model4)
#' windows(4, 4)
#' plot(
#'   allEffects(model4),
#'   multiline = TRUE,
#'   main = "", x.var="soz.stress",
#'   key.args = list(border = 0, columns = 2)
#' )
#' SaveData()
#'
#'
#'
#' End()

Mediation2<- function( model_y_x,
                       model_y_xm,
                       model_m_x,
                       model_y_inter_xm=NULL,
                       X=model_info(model_m_x)$x ,
                       M= model_info(model_m_x)$y,
                       caption="",
                       type="tex",
                       ...){

  if(!is.null(model_y_inter_xm))
  APA_Table (model_y_x, model_y_xm, model_y_inter_xm, model_m_x,
             caption=caption,
             type="tex",
             names=c("(1) Y~X", "(2) Y~X+M", "(3) Y~X*M" , "(4) M~X"))
  else
    APA_Table (model_y_x, model_y_xm, model_m_x,
               caption=caption,
               type="tex",
               names=c("(1) Y~X", "(2) Y~X+M", "(4) M~X"))


    Sobel_Test( model_y_x, model_y_xm, model_m_x, X=X, M=M) %>%
    fix_format() %>%
    Output(caption="Sobel's Test for Mediation")
    Text("X=",X," M=", M)

}


#-- Interne Funktion
Sobel_Test <- function(model1,
                  model2,
                  model3,
                  X=NULL,
                  M=NULL){
  mod1.out <- summary(model1)$coef
  mod2.out <- summary(model2)$coef
  mod3.out <- summary(model3)$coef
  pred<- which(rownames(mod3.out) == X)
  med<- which(rownames(mod2.out) == M)

  indir <- mod3.out[pred,1] * mod2.out[med,1]

  effvar <- (mod3.out[pred, 1]) ^ 2 * (mod2.out[med, 2]) ^ 2 +
    (mod2.out[med, 1]) ^ 2 * (mod3.out[pred, 2]) ^ 2
  serr <- sqrt(effvar)
  zvalue = indir / serr

  data.frame(
    Source="indir",
    B = (indir),
    SE = (serr),
    Z = (zvalue),
    p.value = (2 * pnorm(-abs(zvalue))),
    row.names = "Sobel"
  )

}

# @rdname Mediation
# @export
APA2.mediate.mer<- function (x,
                             caption = "Causal Mediation Analysis",
                             note="ACME (average causal mediation effect), ADE (average direct effect)",
                             ...)
{
  x <- mediation:::summary.mediate.mer(x)
  clp <- 100 * x$conf.level
  txt<- "Causal Mediation Analysis"
  if (x$boot) {
    if (x$boot.ci.type == "perc") {
      txt<- paste(txt, "(Nonparametric Bootstrap Confidence Intervals with the Percentile Method)")
    }
    else if (x$boot.ci.type == "bca") {
      txt<- paste(txt, "(Nonparametric Bootstrap Confidence Intervals with the BCa Method)")
    }
  }
  else {
    txt<- paste(txt, "(Quasi-Bayesian Confidence Intervals)")
  }
  if (!is.null(x$covariates)) {
    txt<- paste(txt, "
                (Inference Conditional on the Covariate Values Specified in `covariates') ")
  }


  txt<- paste(txt, "Mediator Groups:", x$group.m, "\\n\\n")
  txt<- paste(txt, "Outcome Groups:", x$group.y, "\\n\\n")
  txt<- paste(txt, "Output Based on Overall Averages Across Groups", "\\n\\n")
  isLinear.y <- ((class(x$model.y)[1] %in% c("lm", "rq")) ||
                   (inherits(x$model.y, "glm") && x$model.y$family$family ==
                      "gaussian" && x$model.y$family$link == "identity") ||
                   (inherits(x$model.y, "survreg") && x$model.y$dist ==
                      "gaussian") || (inherits(x$model.y, "merMod") &&
                                        x$model.y@call[[1]] == "lmer"))
  printone <- !x$INT && isLinear.y
  if (printone) {
    smat <- c(x$d1, x$d1.ci, x$d1.p)
    smat <- rbind(smat, c(x$z0, x$z0.ci, x$z0.p))
    smat <- rbind(smat, c(x$tau.coef, x$tau.ci, x$tau.p))
    smat <- rbind(smat, c(x$n0, x$n0.ci, x$n0.p))
    rownames(smat) <- c("ACME", "ADE", "Total Effect", "Prop. Mediated")
  }
  else {
    smat <- c(x$d0, x$d0.ci, x$d0.p)
    smat <- rbind(smat, c(x$d1, x$d1.ci, x$d1.p))
    smat <- rbind(smat, c(x$z0, x$z0.ci, x$z0.p))
    smat <- rbind(smat, c(x$z1, x$z1.ci, x$z1.p))
    smat <- rbind(smat, c(x$tau.coef, x$tau.ci, x$tau.p))
    smat <- rbind(smat, c(x$n0, x$n0.ci, x$n0.p))
    smat <- rbind(smat, c(x$n1, x$n1.ci, x$n1.p))
    smat <- rbind(smat, c(x$d.avg, x$d.avg.ci, x$d.avg.p))
    smat <- rbind(smat, c(x$z.avg, x$z.avg.ci, x$z.avg.p))
    smat <- rbind(smat, c(x$n.avg, x$n.avg.ci, x$n.avg.p))
    rownames(smat) <- c("ACME (control)", "ACME (treated)",
                        "ADE (control)", "ADE (treated)", "Total Effect",
                        "Prop. Mediated (control)", "Prop. Mediated (treated)",
                        "ACME (average)", "ADE (average)", "Prop. Mediated (average)")
  }
  colnames(smat) <- c("Estimate", paste(clp, "% CI Lower",
                                        sep = ""), paste(clp, "% CI Upper", sep = ""), "p.value")
  #printCoefmat(smat, digits = 3)

  txt<- paste(txt, "Sample Size Used:", x$nobs, "\\n\\n")

  txt<- paste(txt, "Simulations:", x$sims, "\\n\\n")


  smat <-   prepare_output(fix_to_data_frame(smat),
                           caption = caption ,
                           note=note)
  smat%>% fix_format %>% Output()
  invisible(smat)
  }






# APA_mediate <- function(l, ...) {
#   Print2_mediate <- function(fit,
#                              print = TRUE,
#                              caption = "Mediation",
#                              ...) {
#     print_summary_mediate2 <-
#       function(x,
#                print = print,
#                caption = caption,
#                ...) {
#         clp <- 100 * x$conf.level
#         cat("\n")
#         cat("Causal Mediation Analysis \n")
#         if (x$boot) {
#           cat("Confidence Intervals Based on Nonparametric Bootstrap\n\n")
#         } else {
#           cat("Quasi-Bayesian Confidence Intervals\n")
#         }
#         
#         if (!is.null(x$covariates)) {
#           cat(
#             "(Inference Conditional on the Covariate Values Specified in `covariates')\n\n"
#           )
#         }
#         
#         isLinear.y <-
#           (
#             (class(x$model.y)[1] %in% c("lm", "rq")) ||
#               (
#                 inherits(x$model.y, "glm") &&
#                   x$model.y$family$family == "gaussian" &&
#                   x$model.y$family$link == "identity"
#               ) ||
#               (
#                 inherits(x$model.y, "survreg") &&
#                   x$model.y$dist == "gaussian"
#               )
#           )
#         
#         printone <- !x$INT && isLinear.y
#         
#         if (printone) {
#           # Print only one set of values if lmY/quanY/linear gamY without interaction
#           smat <- c(x$d1, x$d1.ci, x$d1.p)
#           smat <-
#             rbind(smat, c(x$z0, x$z0.ci, x$z0.p))
#           smat <-
#             rbind(smat, c(x$tau.coef, x$tau.ci, x$tau.p))
#           smat <-
#             rbind(smat, c(x$n0, x$n0.ci, x$n0.p))
#           rownames(smat) <-
#             c(
#               "Mediation Effect",
#               "Direct Effect",
#               "Total Effect",
#               "Proportion via Mediation"
#             )
#         } else {
#           smat <- c(x$d0, x$d0.ci, x$d0.p)
#           smat <-
#             rbind(smat, c(x$d1, x$d1.ci, x$d1.p))
#           smat <-
#             rbind(smat, c(x$z0, x$z0.ci, x$z0.p))
#           smat <-
#             rbind(smat, c(x$z1, x$z1.ci, x$z1.p))
#           smat <-
#             rbind(smat, c(x$tau.coef, x$tau.ci, x$tau.p))
#           smat <-
#             rbind(smat, c(x$n0, x$n0.ci, x$n0.p))
#           smat <-
#             rbind(smat, c(x$n1, x$n1.ci, x$n1.p))
#           smat <-
#             rbind(smat, c(x$d.avg, x$d.avg.ci, x$d.avg.p))
#           smat <-
#             rbind(smat, c(x$z.avg, x$z.avg.ci, x$z.avg.p))
#           smat <-
#             rbind(smat, c(x$n.avg, x$n.avg.ci, x$n.avg.p))
#           rownames(smat) <-
#             c(
#               "Mediation Effect_0",
#               "Mediation Effect_1",
#               "Direct Effect_0",
#               "Direct Effect_1",
#               "Total Effect",
#               "Proportion via Mediation_0",
#               "Proportion via Mediation_1",
#               "Mediation Effect (Ave.)",
#               "Direct Effect (Ave.)",
#               "Proportion via Mediation (Ave.)"
#             )
#         }
#         colnames(smat) <-
#           c(
#             "Estimate",
#             paste(clp, "% CI Lower", sep = ""),
#             paste(clp, "% CI Upper", sep =
#                     ""),
#             "p-value"
#           )
#         
#         
#         
#         #cat("\n")
#         cat("Sample Size Used:", x$nobs, "\n")
#         # cat("\n")
#         cat("Simulations: ", x$sims, "\n\n")
#         
#         
#         #invisible(x)
#         if (print) {
#           if (options()$prompt[1] == "HTML> ")
#             HTML(
#               printCoefmat(smat, digits = 3) ,
#               caption = caption,
#               captionalign = "top",
#               Border = 0
#             )
#           else
#             printCoefmat(smat, digits = 3)
#         } else {
#           smat
#         }
#         
#         
#         
#       }
#     
#     med_summary <- summary(fit, ...)
#     print_summary_mediate2(med_summary , print = print, caption = caption)
#   }
#   Print2(l, caption = caption, ...)
# }
