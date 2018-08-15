#' @rdname BlandAltman
#' @export
Kappa <- function(xtb,  ..., CI=FALSE)
  vcd:::Kappa(xtb, ...) %>% fix_vdc_kappa(CI=CI)


#' @rdname BlandAltman
#' @export
Kappa2<- function(xtb, ..., CI = FALSE,
                  caption="Cohen's Kappa-Koeffizient"#,
                  # digits = max(getOption("digits") - 3, 3), level = 0.95
){
  Output(
    fix_format(Kappa(xtb, ..., CI)),
    caption=caption)
}
#x<-vcd::Kappa(xtb, ...)    ####%>% stp25output:::Output.Kappa(CI=CI)
# #  @rdname Output
# #  @export
# Output.Kappa<- function (x
#                          CI = FALSE,
#
#                          caption="Cohen's Kappa-Koeffizient",
#                          ...){
#
#
# }



#-- Helper kapa Formatieren
fix_vdc_kappa<-   function (x, digits = max(getOption("digits") - 3, 3),
                            CI = FALSE,
                            level = 0.95,
                            ...)
{
  tab <- rbind(x$Unweighted, x$Weighted)
  
  
  z <- tab[, 1]/tab[, 2]
  tab <- cbind(tab, z, `Pr(>|z|)` = 2 * pnorm(-abs(z)))
  if (CI) {
    q <- qnorm((1 + level)/2)
    lower <- tab[, 1] - q * tab[, 2]
    upper <- tab[, 1] + q * tab[, 2]
    tab <- cbind(tab, lower, upper)
  }
  rownames(tab) <- names(x)[1:2]
  if(!CI) colnames(tab)<- c("Kappa", "SE", "z", "p.value")
  else colnames(tab)<- c("Kappa", "SE", "z", "p.value", "low.CI", "up.CI")
  
  #abweichung von vdc
  #if(tab[1,1] == tab[2,1]) tab <- tab[1,]
  
  data.frame(source=rownames(tab), tab)
  
}