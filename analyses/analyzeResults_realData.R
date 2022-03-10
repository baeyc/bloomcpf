# Analysis of results on real data

library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(here)
library(EnvStats)
library(invgamma)
library(xtable)


# Import results
res <- readRDS("analyses/results/results_realdata.rds")

res$type <- factor(res$type,levels=c("Rejection 2.5%","Rejection 5%","Linear adjustment 2.5%",
                                                 "Linear adjustment 5%","Linear adjustment 5% with transformed parameters",
                                                 "Nonlinear adjustment 2.5%","Nonlinear adjustment 5%",
                                                 "Iterated nonlinear adjustment 2.5%","Iterated nonlinear adjustment 5%",
                                                 "weighted random forest","unweighted random forest",
                                                 "Gradient boosting with L1 loss","Gradient boosting with L2 loss"),
                         labels=c("Rej 2.5%","Rej 5%","LocLH 2.5%","LocLH 5%",
                                  "LocLH-tr 5%","LocNLH 2.5%","LocNLH 5%","ANLH 2.5%","ANLH 5%","wqRF","uwqRF","qGBM L1","qGBM L2"))

res$param <- factor(res$param,levels=c("tau0","f0","a","b","beta","betaPer2","betaPer3","sigma2"),
                          labels=c("tau[0]","f[0]","a","b","beta[1]","beta[2]","beta[3]","sigma^2"))

# Plot
myColors1 <- c("grey",brewer.pal(12,"Set3"))
myColors <- c("lightgrey","#005bc6","#7fa9ff","#be0018","#f76d22","#a27500","#d1ca58","#604781","#cf92ff","#00692a","#3CB371")

ggplot(data=res,aes(x=type,y=median,ymin=q2.5,ymax=q97.5,color=type)) + geom_pointrange() + facet_wrap(~param, nr=2, scales="free")  +
  xlab("") + ylab("") + scale_color_manual("Method",values=myColors1) + ggtitle("Comparaison of the 95% CI of the ABC posterior distributions") +
  theme(axis.text.x=element_text(angle=45,vjust=0.75),legend.position="bottom",legend.justification = "center")

# no local method
res %>% filter(!grepl("LocLH",type)) %>% ggplot(aes(x=type,y=median,ymin=q2.5,ymax=q97.5,color=type)) + geom_pointrange() + facet_wrap(~param, nr=2, scales="free", labeller = label_parsed)  +
  xlab("") + ylab("") + scale_color_manual("Method",values=myColors) + #scale_x_discrete(labels=c("Prior","Rej 5%","Rej 2.5%","NCH 5%","NCH 2.5%","ANCH 2.5%","SA 5%","SA 2.5%")) +
  ggtitle("Comparaison of the 95% CI of the ABC posterior distributions") +  theme(axis.text.x=element_text(angle=45,vjust=0.75),legend.position="bottom",legend.justification = "center")


# compare with prior
priors <- read.table(here("data","priors.txt"),sep=";")
qpriors <- data.frame()
for (i in 1:nrow(priors)){
  if (priors$V2[i] != 0){
    qdist <- as.character(priors$V2[i])
    x <- c(0.025,0.5,0.975)
    substr(qdist,1,2) <- "q"
    eval(parse(text=paste0("y<-",qdist)))
    y <- y[is.finite(y)]
    temp <- data.frame(q2.5=y[1],median=y[2],q97.5=y[3],param=priors$V1[i],type="Prior")
    qpriors <- rbind(qpriors,temp)
  }
}

qpriors <- qpriors %>% filter(param != "invsigma2")
qpriors$param <- factor(qpriors$param,levels=c("tau0","f0","a","b","beta","betaPer2","betaPer3","sigma2"),
                        labels=c("tau[0]","f[0]","a","b","beta[1]","beta[2]","beta[3]","sigma^2"))

res <- bind_rows(res,qpriors)

res$type <- factor(res$type,levels=c("Prior","Rej 2.5%","Rej 5%","LocLH 2.5%","LocLH 5%",
                                                 "LocLH-tr 5%","LocNLH 2.5%","LocNLH 5%","ANLH 2.5%","ANLH 5%","wqRF","uwqRF","qGBM L1","qGBM L2"))


res %>% filter(!grepl("LocLH",type)) %>% ggplot(aes(x=type,y=median,ymin=q2.5,ymax=q97.5,color=type)) + geom_pointrange() + facet_wrap(~param, nr=3, scales="free", labeller = label_parsed)  +
  xlab("") + ylab("") + scale_color_manual("Method",values=myColors) + #scale_x_discrete(labels=c("Prior","Rej 5%","Rej 2.5%","NCH 5%","NCH 2.5%","ANCH 2.5%","SA 5%","SA 2.5%")) +
  theme(axis.text.x=element_text(angle=45,vjust=0.75),legend.position="none",legend.justification = "center")


# mean and median of each method (no local linear)
d_noloclin <- res %>% filter(!grepl("LocLH",type))
d_noloclin$res <- paste0(format(d_noloclin$median,3)," [",format(d_noloclin$q2.5,3)," ; ",format(d_noloclin$q97.5,3),"]")
head(d_noloclin)
d_noloclin <- d_noloclin %>% select(type,param,res)
d_noloclin <- d_noloclin %>% pivot_wider(names_from = param, values_from = res)

t <- xtable(d_noloclin)
print(t,include.rownames = F)
