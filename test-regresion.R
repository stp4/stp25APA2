 
require(stpvers)
 
 Projekt("html")

# ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
# trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
# group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
# weight <- c(ctl, trt)
# lm.D9 <- lm(weight ~ group)
# 
# 
# 
# APA_Table(lm.D9, type="long2", include.gof = FALSE)
# 


fit <-
  glm(I(sr < 10) ~ pop15 + pop75 + dpi,
      data = LifeCycleSavings,
      family = binomial)



# extract_param(fit,
#               include.odds = TRUE,
#               include.odds.ci = TRUE,
#               include.p = TRUE,
#               include.b = FALSE,
#               include.se = FALSE, include.statistic = F,
#               fix_format = T)
x<-APA_Table(
  fit,
  names = c("Leistung"),
  
  include.odds = TRUE,
  include.odds.ci = TRUE,
  include.p = TRUE,
  include.b = FALSE,
  include.se = FALSE,
  type = "long2", output=FALSE
)
names(x),
c("term" ,     "odds"  ,    "odds.conf", "p" )
End()