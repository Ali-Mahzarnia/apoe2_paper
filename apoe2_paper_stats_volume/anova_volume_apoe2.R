path_vol="***/individual_label_statistics/"
file_list=list.files(path_vol)

whole_volume=matrix(NA,length(file_list),2)

for (i in 1:length(file_list)) {
#print(file)
    temp=read.delim( paste0(path_vol,file_list[i]) )
  #print(sum(temp$volume_mm3))
    whole_volume[i,2]=sum(temp$volume_mm3)
    whole_volume[i,1]=substr( file_list[i] , 1, 6)
  }
whole_volume=as.data.frame(whole_volume);
whole_volume$V2=as.numeric(whole_volume$V2)
whole_volume$V1=as.numeric(substr(whole_volume$V1,2,6)) # make dwi numeric



path_master='/Users/ali/Desktop/aug/MasterSheet_Experiments2021.xlsx'
data=read_xlsx(path_master, sheet = '18ABB11_readable02.22.22_BJ_Cor' )
datatemp=data%>%dplyr::select(DWI,Genotype,Weight, Sex, Diet, Age_Months)#subselect
#nchar(datatemp[111,1])
datatemp=na.omit(datatemp)
datatemp[nchar(datatemp$DWI)==1,]=matrix(NA,1,dim(datatemp)[2])
datatemp=na.omit(datatemp)
datatemp[substr(datatemp$DWI,1,1)!="N",]=matrix(NA,1,dim(datatemp)[2])
datatemp=na.omit(datatemp) ## ommit all na and zero character dwi and died durring
datatemp$DWI=as.numeric(substr(datatemp$DWI,2,6)) # make dwi numeric
datatemp=datatemp[datatemp$Genotype=="APOE22",]

datatemp$DWI%in%whole_volume$V1
datatemp$DWI[1]==whole_volume$V1
indeces_of_whole=match(datatemp$DWI, whole_volume$V1)
temp_bind=cbind(whole_volume[indeces_of_whole,],datatemp)
temp_bind=as.data.frame(temp_bind)
temp_bind=na.omit(temp_bind)

temp_bind=temp_bind%>%mutate( age_cat=case_when(  Age_Months<median(Age_Months)~1 ,
  Age_Months>=median(Age_Months)~2            ) )

lm <- lm( V2 ~ as.factor(Age_Months)*as.factor(Sex),data=temp_bind )
 anova(lm)
 library(emmeans)
 emmeans(lm, ~ Age_Months, contr="tukey") 
 
 lm <- lm( V2 ~ as.factor(age_cat)*as.factor(Sex),data=temp_bind )
 anova(lm)
 emmeans(lm, ~Sex|age_cat , contr="tukey") 
 
 mean(temp_bind$V2[  temp_bind$age_cat==1  ])
 median(temp_bind$V2[  temp_bind$age_cat==1  ])
 mean(temp_bind$V2[  temp_bind$age_cat==2  ])
 median(temp_bind$V2[  temp_bind$age_cat==2 ])
 
 
 dodge <- position_dodge(width = 1)
 p= ggplot(data=temp_bind, aes(x=as.factor(age_cat), y=V2, fill =Sex, color=Sex, alpha=0.3 ))+
   geom_violin(inherit.aes=TRUE, position=dodge, alpha=0.3) +
   scale_color_manual(values=c('blueviolet', 'chartreuse1', 'red'))+
   scale_fill_manual(values=c('blueviolet', 'chartreuse1', 'red'))+
   
   geom_dotplot(binaxis='y', stackdir='center', dotsize=2, alpha=0.95, position=dodge)+
   geom_boxplot(color="black", outlier.color="black", width=0.2, alpha=.8, position=dodge) +
   theme_bw()
plot(p) 
#dev.off()
outpath='***/apoe2_paper_stats_volume'
ggsave(paste(outpath,'apoe22brainvol.png',sep=''), plot = last_plot(), device='png', scale=1, width=4, height=4, unit=c("in"), dpi=200)
