



# APA_Table_merModLmerTest <- function(fit,
#                                 #caption = "" ,
#                                 #note = "",
#
#                                 anova = TRUE,
#                                 random.effects=TRUE,
#                                 anova_type="F",   #  type = "III",
#                                 type=1,
#                                 #  F-werte (wie SPSS) oder Chi (car::Anova)
#                                 ...) {
#   cat("\nAPA2.merModLmerTest noch nicht getestet!\n")
#   #     he lmerTest package overloads the lmer function, so you can just
#   #     re-fit the model using exactly the same code, but the summary()
#   #     will now include approximate degrees of freedom and p-values. This
#   #     implementation is extremely easy to use, but can be a little maddening
#   #     if you forget whether your model is a an object of type lmerMod
#   #     or merModLmerTest.
#
#   # require2(lmerTest)
#   res <- lmerTest::summary(fit)
#   anovaTabel <- if(type==2)  as.data.frame(anova(fit))
#                  else NA
#   goodnes <- if(type==3) cbind(
#               Obs = res$devcomp$dims["N"],
#               round(r.squared.merMod(fit)[, 4:6], 2),
#               # BIC = round(res$BIC,2),
#               logLik = round(c(as.numeric(res$logLik)), 2),
#               REML =  round(res$devcomp$cmp["REML"], 2))
#               else NA
#
#   regssion<-fix_data_frame2(Source = rownames(res$coefficients),
#                          res$coefficients[, 1:4],
#                          p.value = res$coefficients[, 5]
#   )
#
#   if(random.effects) {
#     myranef<- ranef(fit)
#     for( i in  names(myranef)){
#       x <- myranef[[i]]
#       names(x)[1]<- "Intercept"
#       x <- fix_data_frame2(Source = rownames(x), x)
#       names(x)[1]<- i
#       Output(x, caption="Random Effect")
#
#       chiTest <-  rand(fit)$rand.table
#       Output(
#         fix_data_frame2(Source = rownames(chiTest), chiTest )
#         , caption="Likelihood Ratio Test")
#
#
#     }
#   }
#
#
#   if (anova)
#     Output( fix_data_frame2(
#       Source = rownames(anovaTabel),
#       anovaTabel[, c(1,3:6)]),
#       caption = paste("ANOVA:", caption),
#       note = "Analysis of Variance Table of type III")
#
#   Output(goodnes,
#          caption = paste("Goodness-of-fit", caption),
#          note = "R-Quadrat entspricht Marginal und Conditional"
#   )
#
# }
#







