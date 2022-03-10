# Analysis of results of the 100 datasets used as "observed data"

library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(xtable)

# Merge results with "true" parameter values
res <- readRDS("analyses/results/results_simuData.rds")
params <- readRDS("analyses/results/params_set_of_100.rds")
params$sample <- 1:100
paramslong <- pivot_longer(params,cols = !sample,names_to="param",values_to="true_value")

res <- merge(res,paramslong)

# Graphs true value vs posterior median
true_vs_median <- function(p){
  res %>% filter(!grepl("LocLH",type) & param==p) %>% ggplot(aes(x=true_value,y=median,col=type)) + geom_point() + facet_wrap(~type,scales="free",nrow=2) +
  geom_abline(slope=1,intercept = 0,col="black") + ggtitle("a") + scale_color_manual("Method",values=myColors) + guides(color = "none") + xlab("True value") + ylab("Posterior median")
}
true_vs_median("a")
# and so on for each parameter


# Compute bias
res$bias_mean <- res$mean - res$true_value
res$bias_median <- res$median - res$true_value
res$rae_mean <- abs(res$bias_mean)/abs(res$true_value)
res$rae_median <- abs(res$bias_median)/abs(res$true_value)
res$inCI <- (res$q2.5 <= res$true_value) & (res$true_value <= res$q97.5)

# Plot
res$type <- factor(res$type,levels=c("Rejection 2.5%","Rejection 5%","Linear adjustment 2.5%",
                                 "Linear adjustment 5%","Linear adjustment 5% with transformed parameters",
                                 "Nonlinear adjustment 2.5%","Nonlinear adjustment 5%",
                                 "Iterated nonlinear adjustment 2.5%","Iterated nonlinear adjustment 5%",
                                 "weighted random forest","unweighted random forest",
                                 "Gradient boosting with L1 loss","Gradient boosting with L2 loss"),
                 labels=c("Rej 2.5%","Rej 5%","LocLH 2.5%","LocLH 5%",
                          "LocLH-tr 5%","LocNLH 2.5%","LocNLH 5%","ANLH 2.5%","ANLH 5%","wqRF","uwqRF","qGBM L1","qGBM L2"))

myColors1 <- c("grey",brewer.pal(12,"Set3"))
myColors <- c("lightgrey","#005bc6","#7fa9ff","#be0018","#f76d22","#a27500","#d1ca58","#604781","#cf92ff","#00692a","#3CB371")

# plot
ggplot(data=res[abs(res$mean)<10000,],aes(x=type,y=mean,fill=type)) + geom_boxplot() + facet_wrap(~param, scales = "free") +
  scale_fill_manual("Method",values=myColors1) +  theme(axis.text.x=element_text(angle=45,vjust=0.75,hjust=0.75),legend.position="bottom",legend.justification = "center")

# no loc linear methods
res$param <- factor(res$param,levels=c("tau0","f0","a","b","beta","betaPer2","betaPer3","sigma2"),
                  labels=c("tau[0]","f[0]","a","b","beta[1]","beta[2]","beta[3]","sigma^2"))
dnoloc <- res %>% filter(!grepl("LocLH",type))
# format files for ggplot
dtv <- dnoloc %>% select(param,true_value)
dtv$type <- "True value"
names(dtv) <- c("param","mean","type")
dtv$median <- dtv$bias_mean <- dtv$bias_median <- dtv$mean
dnoloc <- bind_rows(dnoloc,dtv)

dnoloc$type <- factor(dnoloc$type,levels=c("True value","Rej 2.5%","Rej 5%","LocLH 2.5%","LocLH 5%",
                          "LocLH-tr 5%","LocNLH 2.5%","LocNLH 5%","ANLH 2.5%","ANLH 5%","wqRF","uwqRF","qGBM L1","qGBM L2"))

dnoloc %>% ggplot(aes(x=type,y=rae_median,fill=type)) + geom_boxplot() + facet_wrap(~param, scales = "free", labeller = label_parsed) +
  scale_fill_manual("Method",values=myColors)+ theme(axis.text.x=element_text(angle=45,vjust=0.75,hjust=0.75))

# avoid too extreme points to be shown on boxplots
maxPerParam <- aggregate(res$true_value,by=list(res$param),FUN="quantile",0.98)
names(maxPerParam) <- c("param","maxValues")

dnoloc <- merge(dnoloc,maxPerParam)
dnoloc[dnoloc$mean<d3$maxValues,] %>% ggplot(aes(x=type,y=rae_median,fill=type)) + geom_boxplot() + facet_wrap(~param, scales = "free", labeller = label_parsed) +
  scale_fill_manual("Method",values=myColors)+ theme(axis.text.x=element_text(angle=45,vjust=0.75,hjust=0.75))


# Credible interval coverage
covCI <- aggregate(res$inCI, by = list(res$param,res$type), FUN = "mean", na.rm = TRUE)
names(covCI) <- c("param","type","covCI")

# Number of valid values
notNA <- aggregate(res$mean, by = list(res$param,res$type), FUN = function(x){sum(is.finite(x))/100})
names(notNA) <- c("param","type","validValues")

# table coverages
tableau <- pivot_wider(covCI, values_from = covCI, names_from = param)
xt <- xtable(tableau,digits=3)
print(xt,include.rownames = F)

# table NA values
tableau <- pivot_wider(notNA, values_from = validValues, names_from = param)
xt <- xtable(tableau,digits=2)
print(xt,include.rownames = F)


