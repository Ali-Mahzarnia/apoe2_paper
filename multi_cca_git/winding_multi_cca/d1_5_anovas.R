path_wind='/Users/ali/Desktop/aug/winding_multi_cca/APOE_MWM_AB.csv'
data_wind=read.csv(path_wind )%>% filter( !(Stage %in% c( "Probe_D5","Probe_D8" )), APOE=="E22") %>% select(Animal, APOE, Age.months,Stage,Sex, Trial, NormSWDist  ,Distance ,Winding)
data_wind=data_wind%>%mutate(agecat=case_when( Age.months >=median(Age.months) ~2,
                                               Age.months <median(Age.months) ~1,
))


data_wind$NormSWDist=as.numeric(data_wind$NormSWDist)
data_wind$Animal=gsub("_", "-", data_wind$Animal)

library(lmerTest)
agg=data_wind

lm_winding_trials <- lmer(Distance ~ agecat*Sex*Stage+(1|Animal), data=agg, REML = TRUE)
a=anova(lm_winding_trials)
b=emmeans(lm_winding_trials , ~ agecat|Stage, contr="tukey")
c=summary(glht(lm_winding_trials, emm(pairwise ~ agecat|Stage)))
d=eta_squared(lm_winding_trials)
e=effectsize::cohens_f(lm_winding_trials, alternative='two.sided')

capture.output(list(a,d,e),file="D1to5_Dist.doc")




lm_winding_trials <- lmer(Winding ~ agecat*Sex*Stage+(1|Animal), data=agg, REML = TRUE)
a=anova(lm_winding_trials)
b=emmeans(lm_winding_trials , ~ agecat*Sex, contr="tukey")
c=summary(glht(lm_winding_trials, emm(pairwise ~ agecat|Sex)))
d=eta_squared(lm_winding_trials)
e=effectsize::cohens_f(lm_winding_trials, alternative='two.sided')

capture.output(list(a,d,e),file="D1to5_Wind.doc")





lm_winding_trials <- lmer(NormSWDist ~ agecat*Sex*Stage+(1|Animal), data=agg, REML = TRUE)
a=anova(lm_winding_trials)
b=emmeans(lm_winding_trials , ~ agecat*Sex, contr="tukey")
c=summary(glht(lm_winding_trials, emm(pairwise ~ agecat|Sex)))
d=eta_squared(lm_winding_trials)
e=effectsize::cohens_f(lm_winding_trials, alternative='two.sided')

capture.output(list(a,d,e),file="D1to5_Norm.doc")