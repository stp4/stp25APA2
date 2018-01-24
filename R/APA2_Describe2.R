Describe2<- function(fml, data, caption="", note="",
                     stat=c("n", "mean","sd","min","max"),
                     #  na.rm = TRUE,
                     ...){
  vars <- which(names(data) %in% all.vars(fml))
  data <- data[vars]
  result <-  as.data.frame(psych::describe(data))[stat]
  result[-1] <- Format2(result[-1] )
  prepare_output(
    cbind(Item=GetLabelOrName(data), result),
    caption, note,nrow(data)
  )
}
