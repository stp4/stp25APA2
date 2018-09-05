

require(stpvers)


  

library(lmerTest)

fit1 <- lm(chol0 ~  ak + rrs0 + med + g, hyper)
fit2 <- glm(chol0 ~ med +   ak +   g + rrs0 , hyper, family = poisson())
fit3 <- lmerTest::lmer(chol0 ~ rrs0 + med +  ak  +  (1|g) , hyper )
fits <- list(fit1, fit2, fit3)

APA_Table(fit3,type="long")

x1<-APA_Table(fit1,
              type="long",
              include.b = TRUE,
              include.se = TRUE,
              include.beta = TRUE,
              include.t =TRUE,
              #  
              # # include.odds = TRUE,
              include.ci = TRUE,
              #  
              #  
              #  #Fehler abfangeb
              include.p = TRUE,
              include.stars =  TRUE,
              #  
              #  include.variance = FALSE,
              include.r = FALSE,
              #  include.pseudo = TRUE,
              #  # noch nicht fertig
              # # include.ftest = TRUE,
              # # include.loglik = TRUE,
              #  # noch nicht fertig
              #  
              #  include.custom = TRUE,
              #  
              include.aic = FALSE,
              include.bic = FALSE,

              include.gof = FALSE,
              
              output = FALSE
)
 

   
   x2<-APA_Table(fit1,
                 type="long",
                 include.b = TRUE,
                  include.se = TRUE,
                  include.beta = TRUE,
                  include.t =TRUE,

                 include.ci = TRUE,

                #  #Fehler abfangeb
                 include.p = TRUE,
                 include.stars =  TRUE,
                #  
                 #  include.variance = FALSE,
                  include.r = FALSE,
               
                #  
                   include.aic = FALSE,
                   include.bic = FALSE,
                 
             
                include.param = FALSE,
                
                 output = FALSE
   )
   
 #  x2
 
  #RMSE( fit3)
   
  
   
 #  Extract2(fit1) 
 
   stp25APA2:::Ordnen.lm(fit1)
   broom::tidy(fit1)
   broom::tidy(fit2)
 #  broom::tidy(fit3)
   
 
  # stp25APA2:::Extract2.default(fit1)
   
   
   stp25APA2:::test_regression(fit1)
   
   