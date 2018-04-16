# Hilfsfunctionen --------------------------------------




# Correlation -------------------------------------------------------------
# in Dokument APA2_Correlation.R
 
 




#' @rdname Tabelle
#' @description conTest Hilfsfunktion fuer Tabellen
conTest = function(fml,
                   data,
                   test_name = TRUE) {
#  cat("\nin conTest\n\n")

  # Default-Tests ------------------------------------
  spearmanTest2 <- function(fml, data) {
    st <- Hmisc::spearman2(fml, data)
    if (is.na(st[3]))
      return("Error")
    rndr_F(st[2], st[3], st[4], st[5])
  }
  # Wilkox
  WilkoxTest2 <- function(fml, data) {
    wlx <- stats::wilcox.test(fml, data, alternative =  "two.sided")
    rndr_U(wlx$statistic, wlx$p.value)
  }
  KruskalTest2 <- function(fml, data) {
    wlx <- stats::kruskal.test(fml, data)
    rndr_H(wlx$statistic, wlx$parameter, wlx$p.value)

  }
  # ANOVA
  Aov2 <- function(fml, data) {
    res <- aov(fml, data, alternative =  "two.sided")
    res <- car::Anova(res, type = 3)
    rndr_F(res[2, 3], res[2, 2], res[3, 2], res[2, 4])

  }
  # t-Test
  TTest2 <- function(fml, data) {
    res <- stats::t.test(fml, data, alternative =  "two.sided")
   
    rndr_T(res$statistic, res$parameter, res$p.value)
  }

# Begin Funktion ---------------------------

  if (is.logical(test_name)) {
    spearmanTest2(fml, data)
  } else{
    if (test_name == "SPSS") {
      if (res[1] == "Wilcoxon")
        WilkoxTest2(fml, data)
      else
        KruskalTest2(fml, data)
    }
    else if (test_name == "wilcox.test")
      WilkoxTest2(fml, data)
    else if (test_name == "u.test")
      WilkoxTest2(fml, data)
    else if (test_name == "h.test")
      KruskalTest2(fml, data)
    else if (test_name == "kruskal.test")
      KruskalTest2(fml, data)
    else if (test_name == "t.test")
      TTest2(fml, data)
    else if (test_name == "aov")
      Aov2(fml, data)
    else if (test_name == "anova")
      Aov2(fml, data)
    else if (test_name == "Hmisc")
      spearmanTest2(fml, data)
    else
      test_name
    
  }
}


#' @rdname Tabelle
#' @description conTest Hilfsfunktion fuer Tabellen
catTest = function(fml, data, include.test="chisq.test") {
  #Fehlende Factoren eliminieren , drop.unused.levels = TRUE
  res <- stats::chisq.test(
    xtabs(fml, data, drop.unused.levels = TRUE),
    correct = FALSE)
  res<- c(
    testname = "Chi-squared",
    stat = fftest(as.numeric(res$statistic)),
    df = Format2(res$parameter,0),
    p = ffpvalue(res$p.value)
  )
  #  paste0("X(df=", res[3], ")=", res[2], "; p=", res[4])
  rndr_X(res[2], res[3], NULL, res[4] )
}






# Con-Test -----------------------------------------------------------------
#
#
# conTest = function(fml,
#                    data,
#                    test_name) {
#
#   spearmanTest2 <- function(fml, data) {
#     st <- Hmisc::spearman2(fml, data)
#     if(is.na(st[3])) return("Error")
#
#     # res<- c(
#     #   testname = if (st[3] == 1)
#     #     "Wilcoxon"
#     #   else
#     #     "Kruskal-Wallis",
#     #   stat =  fftest(st[2]),
#     #   df1 = paste(st[3:4], collapse = ";"),
#     #   P =  ffpvalue(st[5])
#     # )
#     # paste0("F(df=", res[3], ")=", res[2], "; p=", res[4])
#
#     rndr_F(st[2], st[3], st[4],st[5])
#   }
#
#   # Wilkox ------------------------------------------------------------------
#
#   WilkoxTest2 <- function(fml, data) {
#     wlx <- stats::wilcox.test(fml, data, alternative =  "two.sided")
#
#     # wlx <- c(
#     #   testname =   "Mann-Whitney-U-Test"  ,
#     #   stat =  fftest(wlx$statistic),
#     #   df = 1,# paste(st[3:4], collapse = ";"),
#     #   P =  ffpvalue(wlx$p.value)
#     # )
#     # paste0("U=", wlx[2], "; p=", wlx[4])
#     #
#
#     rndr_U(wlx$statistic, wlx$p.value)
#   }
#
#   KruskalTest2 <- function(fml, data) {
#  #   print(fml)
#    # print(head(data))
#     wlx <- stats::kruskal.test(fml, data)
# #
# #     wlx<- c(
# #       testname =   "Kruskal-Wallis-Test"  ,
# #       stat =  fftest(wlx$statistic),
# #       df = Format2(wlx$parameter,0),
# #       P =  ffpvalue(wlx$p.value)
# #     )
# #     paste0("H(df=", wlx[3], ")=", wlx[2], "; p=", wlx[4])
# #
#     rndr_H(wlx$statistic,wlx$parameter,wlx$p.value)
#
#   }
#
#   # ANOVA -------------------------------------------------------------------
#
#
#   Aov2 <- function(fml, data) {
#     res <- aov(fml, data, alternative =  "two.sided")
#     res<-car::Anova(res, type=3)
#     # res <-c(
#     #   testname =   "ANOVA"  ,
#     #   stat =  fftest(res[2,3]),
#     #   df = ff(res[2,2],0),
#     #   P =  ffpvalue(res[2,4])
#     # )
#     # paste0("F(df=", res[3], ")=", res[2], "; p=", res[4])
#     #
#     rndr_F(res[2,3], res[2,2], res[3,2], res[2,4])
#
#   }
#
#   # t-Test ------------------------------------------------------------------
#
#
#   TTest2 <- function(fml, data) {
#     res <- stats::t.test(fml, data, alternative =  "two.sided")
#     # res<- c(
#     #   testname =   "T-Test"  ,
#     #   stat =  fftest(res$statistic),
#     #   df = ff(res$paramete,1),# paste(st[3:4], collapse = ";"),
#     #   P =  ffpvalue(res$p.value)
#     # )
#     # paste0("T(df=", res[3], ")=", res[2], "; p=", res[4])
#     #
#
#     rndr_T(res[2], res[3], res[4] )
#   }
#
#
#
#
#
#   res <-   spearmanTest2(fml, data) # testet-welchert Test
#   if (test_name == "SPSS")
#     if (res[1] == "Wilcoxon")       WilkoxTest2(fml, data)
#     else                            KruskalTest2(fml, data)
#   else if(test_name=="t.test")      TTest2(fml, data)
#   else if(test_name=="aov")         Aov2(fml, data)
#   else if(test_name=="wilcox.test") WilkoxTest2(fml, data)
#   else if(test_name=="kruskal.test")KruskalTest2(fml, data)
#   else res
# }
#
#
# # Cat-test ----------------------------------------------------------------
#
#
# catTest = function(fml, data) {
# #Fehlende Factoren eliminieren , drop.unused.levels = TRUE
#   res <- stats::chisq.test(
#                   xtabs(fml, data, drop.unused.levels = TRUE),
#                   correct = FALSE)
#   res<- c(
#     testname = "Chi-squared",
#     stat = fftest(as.numeric(res$statistic)),
#     df = Format2(res$parameter,0),
#     p = ffpvalue(res$p.value)
#   )
# #  paste0("X(df=", res[3], ")=", res[2], "; p=", res[4])
#   rndr_X(res[2], res[3], NULL, res[4] )
# }

ordTest = function(group, x) {
  #   x<- as.numeric(x) # ?noch nicht getestet
  #require(rms)
  f <- rms::lrm(x ~ group)$stats
  list(P = f["P"], stat = f["Model L.R."], df = f["d.f."],
       testname = "Proportional odds likelihood ratio",
       statname = "Chi-square", plotmathstat = "chi[df]^2")
}
