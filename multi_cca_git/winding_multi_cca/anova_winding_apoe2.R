

lm <- lm( Winding ~ Age_Months*as.factor(Sex),data=response )
 anova(lm)
 library(emmeans)
 emmeans(lm, ~ Age_Months, contr="tukey") 
 
 lm <- lm( Winding ~ as.factor(age_cat)*as.factor(Sex),data=response )
 anova(lm)
 emmeans(lm, ~Sex|age_cat , contr="tukey") 

 library(ggplot2)
 dodge <- position_dodge(width = 1)
 p= ggplot(data=response, aes(x=as.factor(age_cat), y=Winding, fill =Sex, color=Sex, alpha=0.3 ))+
   geom_violin(inherit.aes=TRUE, position=dodge, alpha=0.3) +
   scale_color_manual(values=c('blueviolet', 'chartreuse1', 'red'))+
   scale_fill_manual(values=c('blueviolet', 'chartreuse1', 'red'))+
   
   geom_dotplot(binaxis='y', stackdir='center', dotsize=2, alpha=0.95, position=dodge)+
   geom_boxplot(color="black", outlier.color="black", width=0.2, alpha=.8, position=dodge) +
   theme_bw()
plot(p) 
#dev.off()
outpath='###/winding_multi_cca/'
ggsave(paste(outpath,'apoe22_winding.png',sep=''), plot = last_plot(), device='png', scale=1, width=4, height=4, unit=c("in"), dpi=200)



##############3



lm <- lm( Distance ~ as.factor(Age_Months)*as.factor(Sex),data=response )
anova(lm)
library(emmeans)
emmeans(lm, ~ Age_Months, contr="tukey") 

lm <- lm( Distance ~ as.factor(age_cat)*as.factor(Sex),data=response )
anova(lm)
emmeans(lm, ~Sex|age_cat , contr="tukey") 

library(ggplot2)
dodge <- position_dodge(width = 1)
p= ggplot(data=response, aes(x=as.factor(age_cat), y=Distance, fill =Sex, color=Sex, alpha=0.3 ))+
  geom_violin(inherit.aes=TRUE, position=dodge, alpha=0.3) +
  scale_color_manual(values=c('blueviolet', 'chartreuse1', 'red'))+
  scale_fill_manual(values=c('blueviolet', 'chartreuse1', 'red'))+
  
  geom_dotplot(binaxis='y', stackdir='center', dotsize=2, alpha=0.95, position=dodge)+
  geom_boxplot(color="black", outlier.color="black", width=0.2, alpha=.8, position=dodge) +
  theme_bw()
plot(p) 
#dev.off()
outpath='/Users/ali/Desktop/aug/winding_multi_cca/'
ggsave(paste(outpath,'apoe22_distance.png',sep=''), plot = last_plot(), device='png', scale=1, width=4, height=4, unit=c("in"), dpi=200)





##############3



lm <- lm( NormSWDist ~ as.factor(Age_Months)*as.factor(Sex),data=response )
anova(lm)
library(emmeans)
emmeans(lm, ~ Age_Months, contr="tukey") 

lm <- lm( NormSWDist ~ as.factor(age_cat)*as.factor(Sex),data=response )
anova(lm)
emmeans(lm, ~Sex|age_cat , contr="tukey") 

library(ggplot2)
dodge <- position_dodge(width = 1)
p= ggplot(data=response, aes(x=as.factor(age_cat), y=as.numeric(NormSWDist), fill =Sex, color=Sex, alpha=0.3 ))+
  geom_violin(inherit.aes=TRUE, position=dodge, alpha=0.3) +
  scale_color_manual(values=c('blueviolet', 'chartreuse1', 'red'))+
  scale_fill_manual(values=c('blueviolet', 'chartreuse1', 'red'))+
  
  geom_dotplot(binaxis='y', stackdir='center', dotsize=2, alpha=0.95, position=dodge)+
  geom_boxplot(color="black", outlier.color="black", width=0.2, alpha=.8, position=dodge) +
  theme_bw()
plot(p) 
#dev.off()
outpath='##/winding_multi_cca/'
ggsave(paste(outpath,'apoe22_NormDS.png',sep=''), plot = last_plot(), device='png', scale=1, width=4, height=4, unit=c("in"), dpi=200)
