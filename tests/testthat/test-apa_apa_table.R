context("test-APA_Table.R")

test_that("APA_Table works", {
  fit1 <- lm(chol0 ~  ak + rrs0 + med + g, hyper)
  fit2 <- glm(chol0 ~ med +   ak +   g + rrs0 , hyper, family = poisson())
  fit3 <- lmerTest::lmer(chol0 ~ rrs0 + med +  ak  +  (1|g) , hyper )
  fits <- list(fit1, fit2, fit3)
  
  x1<-stp25APA2:::APA2.list(fits,
                            custom.model.names=c("lm", "glm", "lmer"),
                            digits= list(c(1,2,3,4,5,6,7),
                                         c(1,2,3,4,5,6,7),
                                         c(1,2,3,4,5,6)),
                            include.custom=list(
                              Wald=c("F(1)=245", 
                                     "F(2)=241",
                                     "F(3)=242"),
                              Chi=c("X(4)=2.45", "X(5)=24.5","X(6)=24.5")),
                            include.pseudo = FALSE,
                             output = FALSE
  )
  
  
  
  x2<-APA_Table(fit1, fit2, fit3, type="long",
                 names=c("lm", "glm", "lmer"),
                digits= list(c(1,2,3,4,5,6,7),
                             c(1,2,3,4,5,6,7),
                             c(1,2,3,4,5,6)),
                include.custom=list(
                  Wald=c("F(1)=245", "F(2)=241","F(3)=242"),
                  Chi=c("X(4)=2.45", "X(5)=24.5","X(6)=24.5"))   
                ,output = FALSE
  )
  
  
  expect_equivalent(x1, x2)
  
  # Coefficients:
  #   (Intercept)  ak56-65 Jahre  ak66-75 Jahre   ak> 75 Jahre  
  # 56.5478        10.7481        11.3493        14.4797  
  # rrs0     medBetasan      gweiblich  
  # 0.9891        -6.3107         8.2281 
  # Coefficients:
  #   (Intercept)     medBetasan  ak56-65 Jahre  ak66-75 Jahre  
  # 4.712910      -0.026846       0.046692       0.049128  
  # ak> 75 Jahre      gweiblich           rrs0  
  # 0.062710       0.035105       0.004117  
  # Fixed Effects:
  #   (Intercept)           rrs0     medBetasan  ak56-65 Jahre  
  # 58.552          1.001         -6.237         12.256  
  # ak66-75 Jahre   ak> 75 Jahre  
  # 13.092         16.351 
  
  
 # x1[1:5,c(2,4,6)]
  
  # lm_b        glm_b    lmer_b
  # (Intercept)         56.5       4.7***      58.6
  # ak56-65 Jahre      10.75     0.047***   12.2562
  # ak66-75 Jahre     11.349    0.0491***  13.09179
  # ak> 75 Jahre     14.4797   0.06271*** 16.350860
  # rrs0          0.98913*** 0.0041170***   1.00***
  expect_equal(
  x1$lm_b,
  c(
    "56.5",
    "10.75",
    "11.349",
    "14.4797",
    "0.98913***",
    "-6.310742",
    "8.2280535" ,
    "1845.73",
    "1871.00",
    "0.11",
    "0.08" ,
    "174" ,
    "47.44",
    NA,
    NA,
    NA,
    NA,
    NA,
    "F(1)=245",
    "X(4)=2.45"
  ) )
  
  
})



test_that(
  "APA_Table works",
  {
    fit1 <- lm(chol0 ~  ak + rrs0 + med + g, hyper)
    x2 <- APA_Table(fit1,
                    type = "long",
                    include.p = TRUE,
                    output = FALSE)
    expect_equal(x2[, 4] ,
                 c(
                   ".219",
                   ".260",
                   ".250",
                   ".225",
                   "<.001",
                   ".385" ,
                   ".293",
                   NA, NA, NA, NA, NA, NA
                 ))
  
  
})


test_that(
  "APA2 estimate Output",
  {
    fit1 <- lm(chol0 ~  ak + rrs0 + med + g, hyper)
    estimate_lm <-  APA2(fit1, output = FALSE)[,2]
    
    expect_that(is.character(estimate_lm$estimate),
            is_true())
    
    
  # median( hyper$chol0)
    
    fit2 <- glm(chol0>230 ~  ak + rrs0 + med + g, hyper, family =  binomial)
    estimate_glm <-  APA2(fit2, output = FALSE)[,2]
    
    expect_that(is.character(estimate_glm$estimate),
                is_true())
    
    
    
  })



