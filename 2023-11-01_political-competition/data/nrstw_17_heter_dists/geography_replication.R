########
### Replication code for "Geography, Uncertainty, and Polarization"
########

# Change the following to whatever your local path is
#setwd("Geography/Replication")

# Flag for greyscale
greyscale = T

# Load replication 
load(file="merged public and legislators.Rdata")
load(file="state polarization.Rdata")

# load required libraries

library(ggplot2)
library(reshape2)
library(stringr)
library(plyr)
library(AICcmodavg)
library(Matching)
library(xtable)
library(foreign)
library(stargazer)
library(arm)
# important functions

ggs <- function (filename,width=8,height=5,types=c("pdf"),save=TRUE,subfolder=FALSE,dropbox=NA) {
  
  if (save) {
    for (filetype in types) {
      if (subfolder) { folderpath = str_c(filetype,"/") } else { folderpath="" }
      filen = str_c("Plots/",folderpath,filename,".",filetype)
      print(filen)
      ggsave(file=filen, width=width, height=height) 
      if (!is.na(dropbox)) {
        filen = str_c("Plots/",folderpath,filename,".",filetype)
        print(filen)
        ggsave(file=filen, width=width, height=height)
      }
    }    
  }
  
}

plusminus <- function (x, n.sd = 0.5) {
  return(c((mean(x,na.rm=T)-n.sd*sd(x,na.rm=T)),(mean(x,na.rm=T)+n.sd*sd(x,na.rm=T))))
}



########################
######################
### Models for State Legislature
#######################
########################

###################
# Figure 1 and C.1: density plots

for (chamb in c("s","h")) {
  dist.measure = ifelse(chamb=="s","median_citizens","median_allsample")
  legis.melt = melt(legis.m[[chamb]],id=c("sld","year"),c("pred.np",dist.measure))
  levels(legis.melt$variable)=c("Legislators","District Medians")
  p=ggplot(legis.melt, aes(x=value)) + geom_density(alpha=.5, fill="gray") + guides(fill=FALSE) +
    theme_bw() +
    labs(list(x="Ideology",y=NULL)) + facet_wrap(~variable)
  p
  ggs(str_c("Density/legislators_individuals_",chamb),subfolder=T,width=8,height=4.5)
}

##################
# Figure 2 and C.2: Aggregates by state


st.avg = ddply(st.data,.(st),summarize,comp.diffs=mean(comp.diffs,na.rm=T))

for (chamb in c("s","h")) {
  
  st.het = ddply(subset(sld_chris[[chamb]],!is.na(abb)),.(abb),summarize,
                 within_citizens=mean(heterogeneity_allsample,na.rm=T),
                 between_citizen = sd(mrp_estimate,na.rm=T))
  
  print(st.het)
  
  st.het = plyr::rename(st.het,c("abb"="st"))
  
  within.melt = melt(st.het,id="st",c("within_citizens"))
  between.melt = melt(st.het,id="st",c("between_citizen"))
  
  st.m.within = merge(st.avg,within.melt,by="st")
  st.m.between = merge(st.avg,between.melt,by="st")
  
  st.m.within = subset(st.m.within,st!="HI")
  
  within.cor = cor.test(st.m.within$comp.diffs, st.m.within$value) 
  between.cor = cor.test(st.m.between$comp.diffs, st.m.between$value)
  
  print(within.cor)
  print(between.cor)
  
  data.labels <- data.frame(label = str_c("r = ",round(within.cor$estimate,2)))
  x.loc = ifelse(chamb=="s", 1.22, 1.14)
  
  
  # each one separately
  p=ggplot(st.m.within, aes(x=value,y=comp.diffs,label=st)) + geom_text(alpha=.9, size=4.5) +  geom_smooth(method=lm,se=F, colour="black") + theme_bw() +
    theme(axis.title.x = element_text(size=22), axis.text.x  = element_text(size=16), axis.title.y = element_text(size=22), axis.text.y  = element_text(size=16), title = element_text(size=18)) +
    geom_text(data=data.labels, aes(x = x.loc, y = 2.75, label = label),size=10,show.legend=F) +  
    labs(list(x="Average Within District Ideological Polarization",y="Average Legislative Polarization",fill="")) # title="Legislative Polarization and Within District Ideological Polarization",
  
  p
  
  ggs(str_c("Scatter/within_district_polarization_",chamb),subfolder=T,width=11,height=8.5)
  
  data.labels <- data.frame(label = str_c("r = ",round(between.cor$estimate,2)))
  x.loc = ifelse(chamb=="s", 0.11, 0.13)
  
  # between
  p=ggplot(st.m.between, aes(x=value,y=comp.diffs,label=st)) + geom_text(alpha=.9, size=4.5) +  geom_smooth(method=lm,se=F, colour="black") + theme_bw() +
    theme(axis.title.x = element_text(size=22), axis.text.x  = element_text(size=16), axis.title.y = element_text(size=22), axis.text.y  = element_text(size=16), title = element_text(size=18)) +    #geom_text(aes(x = x.loc, y = 2.75, label = str_c("r = ",round(between.cor$estimate,2))),size=10,show.legend=F) +
    geom_text(data=data.labels, aes(x = x.loc, y = 2.75, label = label),size=10,show.legend=F) +  
    labs(list(x="Average Between District Ideological Polarization",y="Average Legislative Polarization",fill="")) # ,title="Legislative Polarization and Between District Ideological Polarization"
  p
  ggs(str_c("Scatter/between_district_polarization_",chamb),subfolder=T,width=11,height=8.5)
}

##########
## Figure 3 and C.3: average district ideology and within-district polarization

for (chamb in c("s","h")) {
  if (chamb == "s") {
    het_var = "heterogeneity_citizens"
    legis.melt = melt(legis.m[[chamb]],id=c("sld","year"),c(het_var))    
  } else {
    het_var = "heterogeneity_allsample"
    legis.melt = melt(legis.m[[chamb]],id=c("sld","year"),c(het_var))        
  }
  legis.melt = merge(legis.melt,subset(legis.m[[chamb]],select=c(sld,year,pred.np,mrp_estimate,st, party)))
  
  p=ggplot(subset(legis.melt,variable==het_var), aes(x=mrp_estimate,y=value)) + 
    geom_point(alpha=.25) +  geom_smooth(method=loess,se=F,size=1.25,alpha=.25, colour="black") + theme_bw() +
    theme(axis.title.x = element_text(size=22), axis.text.x  = element_text(size=16), axis.title.y = element_text(size=22), axis.text.y  = element_text(size=16), title = element_text(size=18)) +
    labs(list(x="Median Citizen Ideology",y="Heterogeneity",fill=""))
  p
  ggs(str_c("Scatter/ideology_heterogeneity_",chamb),subfolder=T,width=11,height=8.5)
  
}

# Figure 5 and C.4: scatterplot

cols = c("blue4","red4")
if(greyscale) {cols = c("gray48","gray22")}
cuts = 3
shapes = c("triangle","circle")

for (chamb in c("s","h")) {
  
  cor.cat = cor.cat.rep = cor.cat.dem = list()
  
  # names conflict with arm package
  legis.m[[chamb]]$cat.het.cit = car::recode(as.integer(cut_number(legis.m[[chamb]]$het, cuts)),"1='First';2='Second';3='Third'")
  
  cor.cat[["het.cit"]] = ddply(subset(legis.m[[chamb]],!is.na(cat.het.cit)),"cat.het.cit",function(x) cor(x$pred.np,x$median_allsample))[,2]
  cor.cat.rep[["het.cit"]] = ddply(subset(legis.m[[chamb]],!is.na(cat.het.cit) & party == "R"),"cat.het.cit",function(x) cor(x$pred.np,x$median_allsample))[,2]
  cor.cat.dem[["het.cit"]] = ddply(subset(legis.m[[chamb]],!is.na(cat.het.cit) & party == "D"),"cat.het.cit",function(x) cor(x$pred.np,x$median_allsample))[,2]
  
  p=ggplot(subset(legis.m[[chamb]],!is.na(cat.het.cit)), aes(x=median_allsample,y=pred.np, color = party, shape=party)) + ylim (-2.25,2.25)  + stat_smooth(method = "loess", linetype=1, se=F, span=0.9) +  geom_point(alpha=.25) + scale_color_manual(values = cols) +
    theme_bw() + labs(list(y="Legislator Ideology",x="District Opinion",title="",fill="")) + facet_wrap(~cat.het.cit)  + theme(legend.position="none")
  p
  ggs(str_c("Scatter/opinion_ideology_",chamb),subfolder=T,width=9,height=4)
  
  cor.cat.df = round(sapply(cor.cat,t),2)
  cor.cat.rep.df = round(sapply(cor.cat.rep,t),2)
  cor.cat.dem.df = round(sapply(cor.cat.dem,t),2)
  
  rownames(cor.cat.df) = rownames(cor.cat.rep.df) = rownames(cor.cat.dem.df)  = c("First Tercile", "Second Tercile", "Third Tercile")
  
  print(chamb)
  print(cor.cat.df)
  print(cor.cat.rep.df)
  print(cor.cat.dem.df)
  
}

#####################
# Table 1 & Table C.1: main model - varying intercepts by division, party subsets

mlm.div = list()

mlm.div[["h.r"]]=lmer(pred.np~het+mrp_estimate+(1|division),data=subset(legis.m[["h"]],party=="R"))
mlm.div[["h.d"]]=lmer(pred.np~het+mrp_estimate+(1|division),data=subset(legis.m[["h"]],party=="D"))
mlm.div[["s.r"]]=lmer(pred.np~het+mrp_estimate+(1|division),data=subset(legis.m[["s"]],party=="R"))
mlm.div[["s.d"]]=lmer(pred.np~het+mrp_estimate+(1|division),data=subset(legis.m[["s"]],party=="D"))

lapply(mlm.div,summary)

for (chamb in c("s","h")) {
  
  paths = str_c("Tables/div_heterogeneity_party_mlm_",chamb,".tex")
  print(paths)
  
  chtype = ifelse(chamb=="s","Upper","Lower")
  
  stargazer(mlm.div[[str_c(chamb,".r")]],mlm.div[[str_c(chamb,".d")]],digits = 2,
            title = str_c("Heterogeneity - ",chtype," Chamber Score Models (Multilevel)"),
            covariate.labels = c("Heterogeneity","Citizen Ideology","Constant"),
            column.labels = c("R","D"),
            dep.var.labels=c("Legislator Score"),  label = str_c("div.het.mlm.models.",chamb),
            out = paths)
  
}

#########
# Effects (for table 1)


effects = list()

benchmark = mean(sd.st$V1)

for (chamb in c("s")) {
  
  print(chamb)
  cat("\n")
  
  pl.mi = plusminus(sld_chris[[chamb]]$het)
  cat("Range is",pl.mi,"\n")
  for (model in c("r","d")) {
    print(model)
    cat("\n")
    
    # sd
    pred = expand.grid(division=5,het=pl.mi,mrp_estimate=mean(sld_chris[[chamb]]$mrp_estimate,na.rm=T))
    pred
    pred$pred <- predict(mlm.div[[str_c(chamb,".",model)]], newdata = pred)
    print(round(pred,3))
    cat("\n")
    
    effect=pred$pred[2]-pred$pred[1]
    print(paste("Effect size of 1 SD shift:",round(effect,3)))
    print(paste("Effect size as a proportion of SD of legislator ideology:",round(effect/benchmark,3)))
    cat("\n")
    
    effects[str_c(chamb,".",model)]=effect
    
  }    
  cat("\n")
  effect.total = as.numeric(effects[str_c(chamb,".r")])-as.numeric(effects[str_c(chamb,".d")])
  print(paste("Total predicted effect:",round(effect.total,3)))
  print(paste("Total Effect size as a proportion of SD of legislator ideology:",round(effect.total/benchmark,3)))
  cat("\n\n")
  
}


#########
# Figure 6: Marginal effect plot

for (chamb in c("s")) {
  
  for (model in c("r","d")) {
    pred = expand.grid(division=5,het=seq(min(sld_chris[[chamb]]$het,na.rm=T),max(sld_chris[[chamb]]$het,na.rm=T),length.out=1000),mrp_estimate=mean(sld_chris[[chamb]]$mrp_estimate,na.rm=T))
    preds <- predictSE(mlm.div[[str_c(chamb,".",model)]], newdata = pred,se.fit=T)
    pred$pred = preds$fit
    pred$se = preds$se.fit
    
    if (chamb == "s") { xrange = c(0.8,1.6)} else { xrange = c(0,3)} 
    p=ggplot(pred, aes(x = het, y = pred)) + 
      geom_line() + theme_bw()  + xlim(xrange) +
      theme(axis.text.x = element_text(size=16), axis.text.y = element_text(size=16), axis.title.x = element_text(size=22), axis.title.y = element_text(size=22), title = element_text(size=18)) +
      theme(legend.position="none") +  labs(list(x="Heterogeneity",y="Predicted Score",fill=""))
    p
    
    ggs(str_c("Models/",str_c("predicted_",chamb,"_",model)),subfolder=T,width=11,height=8.5,save=T)
  }
}



#########
# Table 2 and B.3: matching


estimand = "ATE"
fit = fit.unc = list()

for (chamb in c("s","h")) { # chamber.list
  
  legis.m[[chamb]]$matching = legis.m[[chamb]]$mrp_estimate
  
  legis.sub = subset(legis.m[[chamb]],!is.na(pred.np) & !is.na(party) & !is.na(matching))
  
  match.fit = Match(Y=legis.sub$pred.np, Tr=legis.sub$pid, X=legis.sub$matching, estimand = estimand)
  summary(match.fit)
  
  fit[["overall"]] = fit.unc[["overall"]] = match.fit
  
  # heterogeneity
  legis.hi.het.cit = subset(legis.sub,het>median(het,na.rm=T))
  legis.low.het.cit = subset(legis.sub,het<=median(het,na.rm=T))    
  
  fit[["hi.het.cit"]] = Match(Y=legis.hi.het.cit$pred.np, Tr=legis.hi.het.cit$pid, X=legis.hi.het.cit$matching, estimand = estimand)
  summary(fit[["hi.het.cit"]])
  
  fit[["low.het.cit"]] = Match(Y=legis.low.het.cit$pred.np, Tr=legis.low.het.cit$pid, X=legis.low.het.cit$matching, estimand = estimand)
  summary(fit[["low.het.cit"]])
  
  AIDD = round(sapply(fit,function(x){ x $ est}),2)
  SE = round(sapply(fit,function(x){ x $ se}),2)
  N.Obs = sapply(fit,function(x){ x $ orig.nobs})
  N.Rep = sapply(fit,function(x){ x $ orig.treated.nobs})
  
  match.results =  data.frame(N.Obs,N.Rep,AIDD,SE)
  rownames(match.results) = c("Overall","High Heterogeneity","Low Heterogeneity")
  print(match.results)
  
  chtype = ifelse(chamb=="s","Upper","Lower")
  
  sink(str_c("Tables/match_results_",chamb,".tex"))
  print(xtable(match.results, caption = str_c("Matching Estimates of the AIDD (Average Treatment Effect) in the ",chtype," Chamber"), label=str_c("matching.ests",chamb)))
  sink()
  
  
  if (chamb=="s") {
    # uncertainty
    legis.hi.unc.cit = subset(legis.sub,unc>median(unc,na.rm=T))
    legis.low.unc.cit = subset(legis.sub,unc<=median(unc,na.rm=T))    
    
    fit.unc[["hi.unc.cit"]] = Match(Y=legis.hi.unc.cit$pred.np, Tr=legis.hi.unc.cit$pid, X=legis.hi.unc.cit$matching, estimand = estimand)
    summary(fit.unc[["hi.unc.cit"]])
    
    fit.unc[["low.unc.cit"]] = Match(Y=legis.low.unc.cit$pred.np, Tr=legis.low.unc.cit$pid, X=legis.low.unc.cit$matching, estimand = estimand)
    summary(fit.unc[["low.unc.cit"]])
    
    AIDD = round(sapply(fit.unc,function(x){ x $ est}),2)
    SE = round(sapply(fit.unc,function(x){ x $ se}),2)
    N.Obs = sapply(fit.unc,function(x){ x $ orig.nobs})
    N.Rep = sapply(fit.unc,function(x){ x $ orig.treated.nobs})
    
    match.results =  data.frame(N.Obs,N.Rep,AIDD,SE)
    rownames(match.results) = c("Overall","High Uncertainty","Low Uncertainty")
    print(match.results)
    
    sink(str_c("Tables/match_results_uncertainty_",chamb,".tex"))
    print(xtable(match.results, caption = str_c("Matching Estimates of the AIDD (Average Treatment Effect) in the ",chtype," Chamber"), label="matching.unc.ests"))
    sink()
    
  }
  
  
}


##########################
# Figure 7a and C.5a: within-district switches

for (chamb in c("s","h")) {
  
  nparty.sld = ddply(legis.m[[chamb]], .(sld), summarize, parties = length(unique(party)))
  sld.switch = subset(nparty.sld,parties == 2)$sld
  sld.switch.df = subset(nparty.sld,parties == 2)
  table(nparty.sld$parties)
  
  score.all = ddply(subset(legis.m[[chamb]],sld%in%sld.switch), .(sld,party), summarize, score = mean(pred.np), het = mean(het))
  score.r = ddply(subset(legis.m[[chamb]],sld%in%sld.switch & legis.m[[chamb]]$party == "R"), .(sld,party), summarize, score = mean(pred.np), het = mean(het))
  score.d = ddply(subset(legis.m[[chamb]],sld%in%sld.switch & legis.m[[chamb]]$party == "D"), .(sld,party), summarize, score = mean(pred.np), het = mean(het))
  
  score.sld = subset(score.r,select=c(sld,het))
  score.sld$diverge = score.r$score - score.d$score
  
  # het
  withindist.cor = with(score.sld,cor.test(diverge,het)) # 0.31 S # 0.15 H
  print(withindist.cor)
  
  data.labels <- data.frame(label = str_c("r = ",round(withindist.cor$estimate,2)))
  p=ggplot(score.sld, aes(x=het,y=diverge))  +  geom_point(alpha=.5, size = 3) +   geom_smooth(method=lm,se=F,size=1.25,alpha=.25, colour="black") + 
    theme_bw() + labs(list(y="District Divergence",x="Heterogeneity",fill=""))  + # ,title="Within-District Party Divergence",
    geom_text(data=data.labels, aes(x = min(score.sld$het,na.rm=T)+.04, y = 3.5, label = label),size=10,show.legend=F) +  
    theme(axis.text.x = element_text(size=16), axis.text.y = element_text(size=16), axis.title.x = element_text(size=22), axis.title.y = element_text(size=22), title = element_text(size=18)) +
    theme(legend.position="none")
  p
  ggs(str_c("Scatter/within_district_divergence_",chamb),subfolder=T,width=11,height=8.5)
  
}


##################
# Figure 7b and C.5b: within-district,  within-year

legis.m.all = list("s"=senate.m.all,"h"=house.m.all)
legis.m.all[["s"]]$sldy = str_c(legis.m.all[["s"]]$sld,"_",legis.m.all[["s"]]$year)
legis.m.all[["h"]]$sldy = str_c(legis.m.all[["h"]]$sld,"_",legis.m.all[["h"]]$year)

for (chamb in c("s","h")) {
  
  nparty.sldy = ddply(legis.m.all[[chamb]], .(sldy,st), summarize, parties = length(unique(party)))
  table(nparty.sldy$parties)
  
  sld.mmd = subset(nparty.sldy,parties == 2)$sldy
  sld.mmd.df = subset(nparty.sldy,parties == 2)
  
  score.all = ddply(subset(legis.m.all[[chamb]],sldy%in%sld.mmd), .(sldy,st,party), summarize, score = mean(pred.np), het = mean(het))
  score.r = ddply(subset(legis.m.all[[chamb]],sldy%in%sld.mmd & legis.m.all[[chamb]]$party == "R"), .(sldy,st,party), summarize, score.r = mean(pred.np), het = mean(het))
  score.d = ddply(subset(legis.m.all[[chamb]],sldy%in%sld.mmd & legis.m.all[[chamb]]$party == "D"), .(sldy,st,party), summarize, score.d = mean(pred.np))
  
  score.sldy = merge(score.r,score.d,by=c("sldy","st"))
  
  score.sldy$diverge = score.sldy$score.r - score.sldy$score.d
  
  withindist.cor = with(score.sldy,cor.test(diverge,het))
  print(withindist.cor)
  
  data.labels <- data.frame(label = str_c("r = ",round(withindist.cor$estimate,2)))
  
  p=ggplot(score.sldy, aes(x=het,y=diverge))  +  geom_point(alpha=.5, size = 3) +   geom_smooth(method=lm,se=F,size=1.25,alpha=.25, colour="black") + 
    theme_bw() + labs(list(y="District Divergence",x="Heterogeneity",fill=""))  + # ,title="Within-District, Within-Year Party Divergence",
    geom_text(data=data.labels, aes(x = min(score.sldy$het,na.rm=T)+.015, y = 3.5, label = label),size=10,show.legend=F) +  
    theme(axis.text.x = element_text(size=16), axis.text.y = element_text(size=16), axis.title.x = element_text(size=22), axis.title.y = element_text(size=22), title = element_text(size=18)) +
    theme(legend.position="none")
  p
  
  ggs(str_c("Scatter/within_district_year_divergence_",chamb),subfolder=T,width=11,height=8.5)
}


####################
# Appendix Only

# Table B.2: uncertainty

mlm.u.div = list()

mlm.u.div[["s.r"]]=lmer(pred.np~unc+mrp_estimate+(1|division),data=subset(legis.m[["s"]],party=="R"))
mlm.u.div[["s.d"]]=lmer(pred.np~unc+mrp_estimate+(1|division),data=subset(legis.m[["s"]],party=="D"))

lapply(mlm.u.div,summary)

for (chamb in c("s")) {
  
  paths = str_c("Tables/div_uncertainty_party_mlm_",chamb,".tex")
  print(paths)
  
  stargazer(mlm.u.div[[str_c(chamb,".r")]],mlm.u.div[[str_c(chamb,".d")]],digits = 2,
            title = "Uncertainty - Legislator Score Models (Multilevel)",
            covariate.labels = c("Uncertainty","Citizen Ideology","Constant"),
            column.labels = c("R","D"),
            dep.var.labels=c("Legislator Score"),  label = "div.unc.mlm.models",
            out=paths)
  
}

# Table B.4: Varying intercepts by state

mlm.st = list()

mlm.st[["s.r"]]=lmer(pred.np~het+mrp_estimate+(1|st),data=subset(legis.m[["s"]],party=="R"))
mlm.st[["s.d"]]=lmer(pred.np~het+mrp_estimate+(1|st),data=subset(legis.m[["s"]],party=="D"))

lapply(mlm.st,summary)


for (chamb in c("s")) {
  
  paths = str_c("Tables/st_heterogeneity_party_mlm_",chamb,".tex")
  print(paths)
  
  stargazer(mlm.st[[str_c(chamb,".r")]],mlm.st[[str_c(chamb,".d")]],digits = 2,
            title = "Heterogeneity - Legislator Score Models (Multilevel)",
            covariate.labels = c("Heterogeneity","Citizen Ideology","Constant"),
            column.labels = c("R","D"),
            dep.var.labels=c("Legislator Score"),  label = "st.het.mlm.models",
            out=paths)
  
}


# Table E.3: repeat for percentiles as proxy for primary

mlm.p.div = list()

mlm.p.div[["s.r"]]=lmer(pred.np~x80+x20+mrp_estimate+(1|division),data=subset(legis.m[["s"]],party=="R"))
mlm.p.div[["s.d"]]=lmer(pred.np~x80+x20+mrp_estimate+(1|division),data=subset(legis.m[["s"]],party=="D"))

lapply(mlm.p.div,summary)

for (chamb in c("s")) {
  
  paths = str_c("Tables/div_percentile_party_mlm_",chamb,".tex")
  print(paths)
  
  stargazer(mlm.p.div[[str_c(chamb,".r")]],mlm.p.div[[str_c(chamb,".d")]],digits = 2,
            title = "Percentiles - Legislator Score Models (Multilevel)",
            covariate.labels = c("80th Percentile","20th Percentile","Citizen Ideology","Constant"),
            column.labels = c("R","D"),
            dep.var.labels=c("Legislator Score"),  label = "div.perc.mlm.models",
            out=paths)
}


########################
######################
### Models for Congress
#######################
########################

chamb<-"congress"

# important functions



load(file=paste("cd_uncertainty_140415.RData", sep=""))

congress<-read.dta(paste("congress_ideology_collapsed.dta", sep=""))
#lp<-read.csv(paste(loc,"LP_District_Heterogeneity_Estimates2.csv", sep=""))
congress_ideology<-read.csv(paste("cd_mrp_estimates.csv", sep=""))

#cd_uncertainty2<-merge(cd_uncertainty, lp, by.x="cd",by.y="cd", all.x=T)
cd_uncertainty2<-merge(cd_uncertainty, congress, by.x="cd",by.y="fips", all.x=T)
cd_uncertainty2<-merge(cd_uncertainty2, congress_ideology, by.x="cd",by.y="fips", all.x=T)
cd_uncertainty2$republican<-NA
cd_uncertainty2$republican[cd_uncertainty2$pid3==1]<-0
cd_uncertainty2$republican[cd_uncertainty2$pid3==3]<-1
cd_uncertainty2$presdem_2008<-as.numeric(as.vector(cd_uncertainty2$presdem_2008))

fit.h.voters=lm(dwnom1~republican* heterogeneity_citizens+ mrp_estimate*republican,data=cd_uncertainty2)
fit.u.voters=lm(dwnom1~republican* heterogeneity_voters+ mrp_estimate*republican,data=cd_uncertainty2)
fit.h.citizens=lm(dwnom1~republican* uncertainty_citizens+ mrp_estimate*republican,data=cd_uncertainty2)
fit.u.citizens=lm(dwnom1~republican* uncertainty_voters+ mrp_estimate*republican,data=cd_uncertainty2)

#subsetted
fit.h.citizens.dem=lm(dwnom1~heterogeneity_citizens+ mrp_estimate,data=cd_uncertainty2[cd_uncertainty2$pid3==1,])
fit.u.citizens.dem=lm(dwnom1~uncertainty_citizens+ mrp_estimate,data=cd_uncertainty2[cd_uncertainty2$pid3==1,])
fit.h.citizens.rep=lm(dwnom1~heterogeneity_citizens+ mrp_estimate,data=cd_uncertainty2[cd_uncertainty2$pid3==3,])
fit.u.citizens.rep=lm(dwnom1~uncertainty_citizens+ mrp_estimate,data=cd_uncertainty2[cd_uncertainty2$pid3==3,])

fit.h.citizens.dem.pres=lm(dwnom1~heterogeneity_citizens+ presdem_2008,data=cd_uncertainty2[cd_uncertainty2$pid3==1,])
fit.h.citizens.rep.pres=lm(dwnom1~heterogeneity_citizens+ presdem_2008,data=cd_uncertainty2[cd_uncertainty2$pid3==3,])


#stargazer(fit.h.voters,fit.h.citizens,fit.u.voters,fit.u.citizens, digits = 2, font.size="footnotesize",
#          title = "Legislator Models for Members of Congress. Unlike previous models, this model does not include Presidential Vote Share as a covariate.",
##          covariate.labels = c("Republican","Heterogeneity Voters","Heterogeneity Citizens","Uncertainty Voters","Uncertainty Citizens","Median Citizen Ideology","Heterogeneity Voters * R","Heterogeneity Citizens * R","Uncertainty Voters * R","Uncertainty Citizens * R","Citizen Ideology * R"),
#         dep.var.labels=c("Legislator Score"),  omit=c("Constant"), omit.stat = "f", label = "models", out = paths)


#######
### Table D.1 Hetereogeneity - Congress Models (OLS)
######
paths = str_c("Tables/mlm_heterogeneity_",chamb,".tex")
print(paths)

stargazer(fit.h.citizens.rep,fit.h.citizens.rep.pres,fit.h.citizens.dem,fit.h.citizens.dem.pres, digits = 2, font.size="footnotesize",intercept.top=TRUE,intercept.bottom=FALSE,
          title = "Hetereogeneity - Congress Models (OLS)",
          covariate.labels = c("Intercept", "Hetereogeneity", "Mean Ideology"),
          dep.var.labels=c("Legislator Score"),  omit.stat = "f", label = "models",
          out = paths)


#########
# Congress matching
#########


estimand = "ATE"
fit = bal = list()

cd_uncertainty2$matching = cd_uncertainty2$mrp_estimate

legis.sub = subset(cd_uncertainty2,!is.na(dwnom1) & !is.na(republican)   & !is.na(matching) )


match.fit = Match(Y=legis.sub$dwnom1, Tr=legis.sub$republican, X=legis.sub$matching, estimand = estimand)
summary(match.fit)
#bal[["overall"]] = MatchBalance(pid ~ matching,  = legis.sub, match.out=match.fit, nboots = 10)

# heterogeneity

legis.hi.het.cit = subset(legis.sub,heterogeneity_citizens>median(heterogeneity_citizens,na.rm=T))
legis.low.het.cit = subset(legis.sub,heterogeneity_citizens<=median(heterogeneity_citizens,na.rm=T))

fit[["overall"]] = match.fit

fit[["hi.het.cit"]] = Match(Y=legis.hi.het.cit$dwnom1, Tr=legis.hi.het.cit$republican, X=legis.hi.het.cit$matching, estimand = estimand)
summary(fit[["hi.het.cit"]])
#bal[["hi.het.cit"]] = MatchBalance(republican ~ matching,  = legis.hi.het.cit, match.out=fit[["hi.het.cit"]], nboots = 10)

fit[["low.het.cit"]] = Match(Y=legis.low.het.cit$dwnom1, Tr=legis.low.het.cit$republican, X=legis.low.het.cit$matching, estimand = estimand)
summary(fit[["low.het.cit"]])
#bal[["low.het.cit"]] = MatchBalance(republican ~ matching,  = legis.low.het.cit, match.out=fit[["low.het.cit"]], nboots = 10)

AIDD = c(sapply(fit,function(x){ x $ est}))
SE = c(sapply(fit,function(x){ x $ se}))
N.Obs = sapply(fit,function(x){ x $ orig.nobs})
N.Rep = sapply(fit,function(x){ x $ orig.treated.nobs})

match.results =  data.frame(N.Obs,N.Rep,AIDD,SE)
rownames(match.results) = c("Overall","High Heterogeneity Citizens","Low Heterogeneity Citizens")
print(match.results)


#######
### Table D.2: Matching Estimates of the AIDD (Average Treatment E↵ect)
######
library(xtable)
sink(str_c("Tables/match_results_",chamb,".tex"))
xtable(match.results, caption = "Matching Estimates of the AIDD (Average Treatment Effect)", label="matching.ests")
sink()


#cd_uncertainty2[which(is.nan(cd_uncertainty2))] = NA
cd_uncertainty2[which(cd_uncertainty2==Inf)] = NA

# scatter by tercile

cd_uncertainty2$party<-NA
cd_uncertainty2$party[cd_uncertainty2$republican==0]<-"D"
cd_uncertainty2$party[cd_uncertainty2$republican==1]<-"R"


cols = c("blue4","red4")
if(greyscale) {cols = c("gray48","gray22")}
cuts = 3
shapes = c("triangle","circle")

cor.cat = cor.cat.rep = cor.cat.dem = list()

# names conflict with arm package
cd_uncertainty2$cat.het.cit = car::recode(as.integer(cut_number(cd_uncertainty2$heterogeneity_citizens, cuts)),"1='First';2='Second';3='Third'")

cor.cat[["het.cit"]] = ddply(subset(cd_uncertainty2,!is.na(cat.het.cit)),"cat.het.cit",function(x) cor(x$dwnom1,x$mrp_estimate))[,2]
cor.cat.rep[["het.cit"]] = ddply(subset(cd_uncertainty2,!is.na(cat.het.cit) & party == "R"),"cat.het.cit",function(x) cor(x$dwnom1,x$mrp_estimate))[,2]
cor.cat.dem[["het.cit"]] = ddply(subset(cd_uncertainty2,!is.na(cat.het.cit) & party == "D"),"cat.het.cit",function(x) cor(x$dwnom1,x$mrp_estimate))[,2]

####
### Figure D.1 Scatterplot of Representative Ideology and District Opinion, by Heterogeneity Tercile
####
p<-ggplot(subset(cd_uncertainty2,!is.na(cat.het.cit)), aes(x=mrp_estimate,y=dwnom1, color = party, shape=party))  
p<- p+ stat_smooth(method = "lm", formula = y ~ poly(x, 2), size = 1) 
p<- p+  geom_point(alpha=.25, size = 2) 
p<- p+ scale_color_manual(values = cols) 
p<- p+ theme_bw() 
p<- p+ labs(list(y="Legislator Ideology",x="District Opinion",title="Heterogeneity",fill="")) 
p<- p+ facet_wrap(~cat.het.cit) 
p<- p+ theme(legend.position="none")
p
ggs(str_c("Scatter/opinion_ideology_","congress"),subfolder=T,width=9,height=4,dropbox="state representation")


########################
######################
### Geographic Distribution of Preferences
#######################
########################


