

f <- function(text) {
  i1 <- grep(text, ppdvars$`Variable Name`, ignore.case=TRUE)
  i2 <- grep(text, ppdvars$`Variable Label`, ignore.case=TRUE)
  df <- ppdvars[unique(c(i1, i2)), ] %>% select(vname=`Variable Name`, vlabel=`Variable Label`) %>%
    mutate(vlabel=str_sub(str_trim(vlabel), 1, 75))
  return(df)
}

f("asset")
f("age")
f("ratio")
f("arc")
f("req")
f("payroll")


%>%
  select(year, variable, rpch) %>%
  spread(variable, rpch) %>%
  mutate(rgdp2=rgdp.a^2 * (-1)^(rgdp.a<0)) # 2nd term is -1 or 1


hstack <- function(pendf, valvar, classvar, binw=5) {
  # Stacked histogram
  pdata <- pendf %>% select_(varf=classvar, value=valvar)
  main <- paste0("Distribution of ", valvar)
  xlab <- paste0(valvar, "\nSource: CRR Public Plans Database")
  p <- ggplot(aes(x=value), data=pdata) +
    geom_histogram(fill="blue", binwidth=binw)
  
  # main title
  p <- p + labs(title=main) + theme(plot.title=element_text(size=16, face="bold"))
  
  # y axis
  p <- p+labs(y="Count") + theme(axis.text.y=element_text(size=12, face="bold", colour="black"))
  
  # x axis
  p <- p + scale_x_continuous(minor_breaks=NULL, limits=c(0, 80)) + # breaks=brks, 
    labs(x=xlab) + theme(axis.title.x=element_text(size=14, face="bold", colour="black")) +
    theme(axis.text.x=element_text(size=14, face="bold", colour="black"))
  
  # add vertial line for each group's median
  p <- p + geom_vline(data=group_by(pdata, varf) %>% summarize(mdn=median(value, na.rm=TRUE)), aes(xintercept=mdn), size=1.05)
  
  # now wrap (create multiple graphs)
  q <- p + facet_wrap(~varf, ncol=1)
  return(q)
}  
