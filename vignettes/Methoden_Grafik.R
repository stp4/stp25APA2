## ----setup, include=FALSE------------------------------------------------
library(stpvers)
library(lattice)
library(RColorBrewer)
library(effects)
require(gridExtra)


## ----fig.height=4, fig.width=6-------------------------------------------
# -------------------------------------------------------------------------------
 x1<-rnorm(100);  x2<-gl(2, 50, labels = c("Control", "Treat"))
 y<-(1.5-as.numeric(x2))*x1+rnorm(100)
#windows(7,4)
p1<- xyplot(y~x1|x2,
            xlab = expression(hat(mu)[0]),
            type=(c("p", "r")),
            # - auch mit fontsize=20
            ylab=list(label="Percent of Respondents", cex=2),
            par.strip.text=list(lines=2.5, col=6),
            strip=strip.custom(factor.levels=
                                 expression(
                                   sqrt(G^{1}), sqrt(italic(R)^{1}))))
 print(p1)


## ---- fig.height=4, fig.width=6------------------------------------------
MySet(knit=TRUE, axis.grid=TRUE)
xyplot(y~x1|x2,
            xlab = expression(hat(mu)[0]),
            type=(c("p", "r"))
            # par.settings = list( axis.line = list(col = NA),
            #                      reference.line = list(col = "grey"))
       )

## ----fig.height=3.4, fig.width=4-----------------------------------------
MySet()
bwplot2(tzell ~ factor(lai), hkarz, groups=gruppe, xlab="",
             box.width = 1/4,
             auto.key=list(columns=2)
)

## ----fig.height=4, fig.width=6-------------------------------------------

 bwplot(yield ~ site, barley, groups = year,
       panel = function(x, y, groups, subscripts, ...) {
          #  panel.grid(h = -1, v = 0)
           panel.stripplot(x, y, ..., jitter.data = TRUE,
                           groups = groups, subscripts = subscripts)
           panel.superpose(x, y, ..., panel.groups = panel.average,
                           groups = groups, subscripts = subscripts)

            panel.points(x, y, ..., panel.groups = panel.average,
                           groups = groups, subscripts = subscripts)
          #  panel.mean(x, y, ... )
       },
       auto.key = list(points = FALSE, lines = TRUE, columns = 2))


## ---- Effectplot_model---------------------------------------------------
 x1<- rnorm(10); x2<- rnorm(10); x3<-  rnorm(10)
 y1<- x1*2+x2 +rnorm(10); y2<- x1/2+x2 +rnorm(10)

 m1<- lm(y1 ~x1+x2+x3)
 m2<- lm(y2 ~x1+x2)

## ---- fig.height=3, fig.width=6------------------------------------------
 p1 <- plot(effect("x1", m1) )
 p2 <- plot(effect("x1", m2) )
 p3 <- plot(effect("x2", m2) )

 grid.arrange(p1, p2, p3, ncol=3)

## ------------------------------------------------------------------------
fit2<-lm(chol0 ~ rrs0 + ak*g + med*g, hyper)
#head(hyper)

## ---- fig.height=5, fig.width=7------------------------------------------
plot(allEffects(fit2), main="",
     factor.names=FALSE
     )


## ---- fig.height=6, fig.width=6------------------------------------------
fit2<-lm(chol0 ~ rrs0 + ak*g + med*g, hyper)

# Modifiziert geht nicht mehr plot.efflist <- stp25:::plot.efflist
assignInNamespace("plot.efflist", stp25plot:::plot.efflist, "effects")

plot(allEffects(fit2), main="",
     multiline=TRUE,
     ylab= "HDL-Cholesterin\n[mg/dl]",
     # das geht nicht
     xlab=c("syst. Blutdruck",  "Altersklassen",   "Medikament"),
     key.args=list(x=0.35,y=0.99,
                   corner=c(x=1, y=1),
                   border=0,
                   #geht nicht lines=FALSE Workaround
                   lines=list(col=0), between=-2.05,
                   cex=.7, title = NULL #, cex.title = 1.2

     )
)

## ---- include=FALSE------------------------------------------------------
library(HH)
library(effects)
data(hotdog, package="HH")

## ----fig.height=3.4, fig.width=6-----------------------------------------
CpT <- ancovaplot(Sodium ~ Calories + Type,
                  data=hotdog,
                  superpose.panel=TRUE)
CpT

anova(fit<-aov(Sodium ~ Calories*Type, data=hotdog))

plot(allEffects(fit))

## ----fig.height=3.4, fig.width=6-----------------------------------------
set.seed(1)
x <- runif(1000, 0, 100)
z <- cut(x, c(10,20,30))
table(z)
#windows(8,5)
par(mfrow=c(1,2))
boxplot(x~z, main="cut")
abline(h=c(10,20,30))
set.seed(1)
x <- runif(1000, 0, 100)
z <- Hmisc::cut2(x, c(10,20,30))
table(z)
boxplot(x~z, main="cut2")
abline(h=c(10,20,30))


