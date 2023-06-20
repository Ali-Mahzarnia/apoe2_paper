
library(readxl)
library(dplyr)
library(xlsx)
path_wind='/Users/ali/Desktop/Jun23/apoe2_paper/divide_by_sum/multi_cca/winding_multi_cca/APOE_MWM_AB.csv'
data_wind=read.csv(path_wind )%>% filter(Stage=="Probe_D8", APOE=="E22") %>% select(Animal, APOE, Age.months,NormSWDist  ,Distance ,Winding)
data_wind$Animal=gsub("_", "-", data_wind$Animal)


path_master='/Users/ali/Desktop/Jun23/apoe2_paper/divide_by_sum/multi_cca/MasterSheet_Experiments2021.xlsx'
data=read_xlsx(path_master, sheet = '18ABB11_readable02.22.22_BJ_Cor' )
datatemp=data%>%dplyr::select(DWI,Genotype,Weight, Sex, Diet, Age_Months, CIVM_ID, BadeaID)#subselect
datatemp$CIVM_ID=gsub(":.*","",datatemp$CIVM_ID)
datatemp$CIVM_ID=gsub("_", "-", datatemp$CIVM_ID)
datatemp$BadeaID=gsub("_", "-", datatemp$BadeaID)

sum(data_wind$Animal %in% datatemp$CIVM_ID | data_wind$Animal %in% datatemp$BadeaID)
ind1=match( data_wind$Animal ,  datatemp$CIVM_ID   ) 
ind2=match( data_wind$Animal ,  datatemp$BadeaID  )


indec_data=rowMeans(cbind(ind1,ind2), na.rm=TRUE)

datatemp=datatemp[indec_data,]
datatemp=cbind(datatemp,data_wind)

datatemp=na.omit(datatemp)

datatemp[substr(datatemp$DWI,1,1)!="N",]=matrix(NA,1,dim(datatemp)[2])
datatemp$DWI=as.numeric(substr(datatemp$DWI,2,6)) # make dwi numeric


####
path_connec="/Users/ali/Desktop/Jun23/apoe2_paper/divide_by_sum/connectome/"
file_list=list.files(path_connec)
temp_conn= read.csv( paste0(path_connec,file_list[1]) , header = F)
#temp_conn=temp_conn[,2: dim(temp_conn)[2]]
connectivity=array( NA ,dim=c(dim(temp_conn)[1],dim(temp_conn)[2],dim(datatemp)[1]))
dim(connectivity)




notfound=0
##read connec
for (i in 1:dim(connectivity)[3]) {
  
  temp_index=which(datatemp$DWI[i]==as.numeric(substr(file_list,2,6)))
  if (length(temp_index)>0) 
  {
    temp_connec=read.csv( paste0(path_connec,file_list[temp_index]) , header = F)
    #temp_connec=temp_connec[,2:dim(temp_connec)[2]]
    colnames(temp_connec)=NA
    temp_connec=as.matrix(temp_connec) ##
    diag(temp_connec)=0### diag is 0
    #temp_connec=temp_connec/sum(temp_connec) ### divide by sum of connectviity
    #temp_connec=100*temp_connec
    connectivity[,,i]=as.matrix(temp_connec)
  }
  else
    notfound=c(notfound, datatemp$DWI[i])
  
}

# notfound=notfound[2:length(notfound)]
# not_found_index=which( datatemp$DWI  %in%  notfound )

# datatemp=datatemp[-not_found_index,]
# connectivity=connectivity[,,-not_found_index]
sum(is.na(connectivity))

response=datatemp
response=response%>%mutate(age_cat=case_when( Age_Months>=median(Age_Months)~1, 
                                              Age_Months<median(Age_Months)~0    ))  







save(response, file="response.rda")
save(connectivity, file="connectivity.rda")





