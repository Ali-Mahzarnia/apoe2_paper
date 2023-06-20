
library(readxl)
library(dplyr)

path_wind='###/APOE_MWM_AB.csv'
data_wind=read.csv(path_wind )%>% filter(! Stage %in% c("Probe_D8", "Probe_D5" ), APOE=="E22") %>% select(Animal, APOE, Age.months,Stage,Sex, Trial, NormSWDist  ,Distance ,Winding)
data_wind=data_wind%>%mutate(agecat=case_when( Age.months >=median(Age.months) ~2,
                              Age.months <median(Age.months) ~1,
                              ))



data_wind$NormSWDist=as.numeric(data_wind$NormSWDist)
data_wind$Animal=gsub("_", "-", data_wind$Animal)


mean(data_wind$Age.months[data_wind$agecat==1])
sd(data_wind$Age.months[data_wind$agecat==1])

mean(data_wind$Age.months[data_wind$agecat==2])
sd(data_wind$Age.months[data_wind$agecat==2])

sum(data_wind$Sex=="M" & data_wind$agecat==1)
sum(data_wind$Sex=="F" & data_wind$agecat==1)

sum(data_wind$Sex=="M" & data_wind$agecat==2)
sum(data_wind$Sex=="F" & data_wind$agecat==2)




library(lmerTest)

# lm_winding_trials <- lmer(Winding ~ agecat*Stage*Trial+(1|Animal), data_wind, REML = TRUE)
# 
# anova(lm_winding_trials)
# summary(anova(lm_winding_trials))
# 
# 



agg = aggregate(data_wind,
                by = list(data_wind$Animal, data_wind$Stage, data_wind$Sex),
                FUN = mean)
agg=agg%>%select( Group.1, Group.2, Group.3, Age.months,agecat, agecat, NormSWDist  ,Distance ,Winding  )
agg=na.omit(agg)
colnames(agg)[1:3]=c("Animal", "Stage", "Sex")




library(ggplot2)
p1=ggplot(data = agg, aes(x = Stage, y = Winding, color = as.factor(agecat), fill=as.factor(agecat) , group=agecat  ) )
p1=p1 + geom_point()+
  geom_smooth(formula = y ~ x, method = "loess", alpha=0.5) +
  scale_color_manual(values=c( 'chartreuse1', 'blueviolet'))+
  scale_fill_manual(values=c( 'chartreuse1', 'blueviolet'))+
  theme_bw()
p1_wind=p1

p2=ggplot(data = agg, aes(x = Stage, y = Distance, color = as.factor(agecat) ,fill=as.factor(agecat) , group=agecat  ) )
p2=p2 + geom_point()+
  geom_smooth(formula = y ~ x, method = "loess", alpha=0.5) +
  scale_color_manual(values=c( 'chartreuse1', 'blueviolet'))+
  scale_fill_manual(values=c( 'chartreuse1', 'blueviolet'))+
  theme_bw()
p2_dist=p2

p3=ggplot(data = agg, aes(x = Stage, y = NormSWDist, color = as.factor(agecat) ,fill=as.factor(agecat)  , group=agecat  ) )
p3=p3 + geom_point()+
  geom_smooth(formula = y ~ x, method = "loess", alpha=0.5) +
  scale_color_manual(values=c( 'chartreuse1', 'blueviolet'))+
  scale_fill_manual(values=c( 'chartreuse1', 'blueviolet'))+
  theme_bw()
p3_norm=p3
# 

# agg = aggregate(data_wind,
#                 by = list( data_wind$Stage, data_wind$agecat),
#                 FUN = mean,na.rm = TRUE)
# agg=agg%>%select( Group.1,Group.2, NormSWDist ,Distance ,Winding  )
# agg=na.omit(agg)
# library(ggplot2)
# p=ggplot(data = agg, aes(x = Group.1, y = Winding, color = as.factor(Group.2) , group=Group.2  ) )
# p + geom_point()+
#   scale_color_manual(values=c( 'chartreuse1', 'blueviolet'))+
#  # geom_line( cex=2)+
#   geom_smooth(formula = y ~ x, method = "lm") +
#   theme_bw()
# 
# p=ggplot(data = agg, aes(x = Group.1, y = Distance, color = as.factor(Group.2) , group=Group.2  ) )
# p + geom_point()+
#   scale_color_manual(values=c( 'chartreuse1', 'blueviolet'))+
#   geom_line()+
#   theme_bw()
# 
# p=ggplot(data = agg, aes(x = Group.1, y = NormSWDist, color = as.factor(Group.2) , group=Group.2  ) )
# p + geom_point()+
#   geom_line()+
#   theme_bw()
# 
# 




dodge <- position_dodge(width = 1)

##############

data_wind=read.csv(path_wind )%>% filter( Stage %in% c("Probe_D5" ), APOE=="E22") %>% select(Animal, APOE, Age.months,Stage,Sex, Trial, NormSWDist  ,Distance ,Winding)
data_wind=data_wind%>%mutate(agecat=case_when( Age.months >=median(Age.months) ~2,
                                               Age.months <median(Age.months) ~1,
))
data_wind$NormSWDist=as.numeric(data_wind$NormSWDist)
data_wind$Animal=gsub("_", "-", data_wind$Animal)

library(lmerTest)
agg=data_wind

lm_winding_trials <- lm(Distance ~ agecat*Sex, agg)
a=anova(lm_winding_trials)
b=emmeans(lm_winding_trials , ~ agecat*Sex, contr="tukey")
c=summary(glht(lm_winding_trials, emm(pairwise ~ agecat|Sex)))
d=eta_squared(lm_winding_trials)
e=effectsize::cohens_f(lm_winding_trials, alternative='two.sided')
#list(a,d,e)

capture.output(list(a,d,e),file="D5Dist.doc")



library(emmeans)
library(multcomp)
library(rstatix)
# emmeans(lm_winding_trials , ~ agecat*Sex, contr="tukey")
# summary(glht(lm_winding_trials, emm(pairwise ~ agecat|Sex)))
# eta_squared(lm_winding_trials)
# effectsize::cohens_f(lm_winding_trials, alternative='two.sided')



#my_comparisons <- list(  c("1", "2") )

p_d5_dist= ggplot(data=agg, aes(x=as.factor(agecat), y=Distance, fill =Sex, color=Sex, alpha=0.3 ))+
  geom_violin(inherit.aes=TRUE, position=dodge, alpha=0.3) +
  scale_color_manual(values=c('blueviolet', 'chartreuse1', 'red'))+
  scale_fill_manual(values=c('blueviolet', 'chartreuse1', 'red'))+
  geom_dotplot(binaxis='y', stackdir='center', dotsize=2, alpha=0.95, position=dodge)+
  geom_boxplot(color="black", outlier.color="black", width=0.2, alpha=.8, position=dodge) +
  #stat_compare_means(comparisons = my_comparisons)+ # Add pairwise comparisons p-value
  #stat_compare_means(aes(group = Sex), label = "p.format")+
  theme_bw()
p_d5_dist




lm_winding_trials <- lm(Winding ~ agecat*Sex, agg)
a=anova(lm_winding_trials)
b=emmeans(lm_winding_trials , ~ agecat*Sex, contr="tukey")
c=summary(glht(lm_winding_trials, emm(pairwise ~ agecat|Sex)))
d=eta_squared(lm_winding_trials)
e=effectsize::cohens_f(lm_winding_trials, alternative='two.sided')
#list(a,d,e)

capture.output(list(a,d,e),file="D5Wind.doc")

p_d5_wind= ggplot(data=agg, aes(x=as.factor(agecat), y=Winding, fill =Sex, color=Sex, alpha=0.3 ))+
  geom_violin(inherit.aes=TRUE, position=dodge, alpha=0.3) +
  scale_color_manual(values=c('blueviolet', 'chartreuse1', 'red'))+
  scale_fill_manual(values=c('blueviolet', 'chartreuse1', 'red'))+
  geom_dotplot(binaxis='y', stackdir='center', dotsize=2, alpha=0.95, position=dodge)+
  geom_boxplot(color="black", outlier.color="black", width=0.2, alpha=.8, position=dodge) +
  theme_bw()
p_d5_wind





lm_winding_trials <- lm(NormSWDist ~ agecat*Sex, agg)
a=anova(lm_winding_trials)
#capture.output(a,file="D5Norm.doc")

b=emmeans(lm_winding_trials , ~ agecat*Sex, contr="tukey")
c=summary(glht(lm_winding_trials, emm(pairwise ~ agecat|Sex)))
d=eta_squared(lm_winding_trials)
e=effectsize::cohens_f(lm_winding_trials, alternative='two.sided')
capture.output(list(a,d,e),file="D5Norm.doc")


p_d5_Norm= ggplot(data=agg, aes(x=as.factor(agecat), y=as.numeric(NormSWDist), fill =Sex, color=Sex, alpha=0.3 ))+
  geom_violin(inherit.aes=TRUE, position=dodge, alpha=0.3) +
  scale_color_manual(values=c('blueviolet', 'chartreuse1', 'red'))+
  scale_fill_manual(values=c('blueviolet', 'chartreuse1', 'red'))+
  geom_dotplot(binaxis='y', stackdir='center', dotsize=2, alpha=0.95, position=dodge)+
  geom_boxplot(color="black", outlier.color="black", width=0.2, alpha=.8, position=dodge) +
  theme_bw()
p_d5_Norm




#######

data_wind=read.csv(path_wind )%>% filter( Stage %in% c( "Probe_D8" ), APOE=="E22") %>% select(Animal, APOE, Age.months,Stage,Sex, Trial, NormSWDist  ,Distance ,Winding)
data_wind=data_wind%>%mutate(agecat=case_when( Age.months >=median(Age.months) ~2,
                                               Age.months <median(Age.months) ~1,
))
data_wind$NormSWDist=as.numeric(data_wind$NormSWDist)
data_wind$Animal=gsub("_", "-", data_wind$Animal)

library(lmerTest)
agg=data_wind

lm_winding_trials <- lm(Distance ~ agecat*Sex, agg)
a=anova(lm_winding_trials)
#capture.output(a,file="D8Dist.doc")

b=emmeans(lm_winding_trials , ~ agecat*Sex, contr="tukey")
c=summary(glht(lm_winding_trials, emm(pairwise ~ agecat|Sex)))
d=eta_squared(lm_winding_trials)
e=effectsize::cohens_f(lm_winding_trials, alternative='two.sided')
capture.output(list(a,d,e),file="D8Dist.doc")


p_d8_dist= ggplot(data=agg, aes(x=as.factor(agecat), y=Distance, fill =Sex, color=Sex, alpha=0.3 ))+
  geom_violin(inherit.aes=TRUE, position=dodge, alpha=0.3) +
  scale_color_manual(values=c('blueviolet', 'chartreuse1', 'red'))+
  scale_fill_manual(values=c('blueviolet', 'chartreuse1', 'red'))+
  geom_dotplot(binaxis='y', stackdir='center', dotsize=2, alpha=0.95, position=dodge)+
  geom_boxplot(color="black", outlier.color="black", width=0.2, alpha=.8, position=dodge) +
  theme_bw()
p_d8_dist


lm_winding_trials <- lm(Winding ~ agecat*Sex, agg)
a=anova(lm_winding_trials)
# capture.output(a,file="D8Wind.doc")


b=emmeans(lm_winding_trials , ~ agecat*Sex, contr="tukey")
c=summary(glht(lm_winding_trials, emm(pairwise ~ agecat|Sex)))
d=eta_squared(lm_winding_trials)
e=effectsize::cohens_f(lm_winding_trials, alternative='two.sided')
capture.output(list(a,d,e),file="D8Wind.doc")


p_d8_wind= ggplot(data=agg, aes(x=as.factor(agecat), y=Winding, fill =Sex, color=Sex, alpha=0.3 ))+
  geom_violin(inherit.aes=TRUE, position=dodge, alpha=0.3) +
  scale_color_manual(values=c('blueviolet', 'chartreuse1', 'red'))+
  scale_fill_manual(values=c('blueviolet', 'chartreuse1', 'red'))+
  geom_dotplot(binaxis='y', stackdir='center', dotsize=2, alpha=0.95, position=dodge)+
  geom_boxplot(color="black", outlier.color="black", width=0.2, alpha=.8, position=dodge) +
  theme_bw()
p_d8_wind




lm_winding_trials <- lm(NormSWDist ~ agecat*Sex, agg)
a=anova(lm_winding_trials)
# capture.output(a,file="D8Norm.doc")

b=emmeans(lm_winding_trials , ~ agecat*Sex, contr="tukey")
c=summary(glht(lm_winding_trials, emm(pairwise ~ agecat|Sex)))
d=eta_squared(lm_winding_trials)
e=effectsize::cohens_f(lm_winding_trials, alternative='two.sided')
capture.output(list(a,d,e),file="D8Norm.doc")

p_d8_Norm= ggplot(data=agg, aes(x=as.factor(agecat), y=as.numeric(NormSWDist), fill =Sex, color=Sex, alpha=0.3 ))+
  geom_violin(inherit.aes=TRUE, position=dodge, alpha=0.3) +
  scale_color_manual(values=c('blueviolet', 'chartreuse1', 'red'))+
  scale_fill_manual(values=c('blueviolet', 'chartreuse1', 'red'))+
  geom_dotplot(binaxis='y', stackdir='center', dotsize=2, alpha=0.95, position=dodge)+
  geom_boxplot(color="black", outlier.color="black", width=0.2, alpha=.8, position=dodge) +
  theme_bw()
p_d8_Norm

library(ggpubr)
ggarrange(p1_wind, p_d5_wind, p_d8_wind,
          p2_dist, p_d5_dist, p_d8_dist,
          p3_norm, p_d5_Norm, p_d8_Norm,
          #labels = c("A: Day d5", "B: Day d5"),
          ncol = 3 , nrow = 3)
outpath='/Users/ali/Desktop/aug/winding_multi_cca/'

ggsave(paste(outpath,'winding_anova.pdf',sep=''), plot = last_plot(), device='pdf', scale=1, width=20, height=20, unit=c("in"), dpi=200)
