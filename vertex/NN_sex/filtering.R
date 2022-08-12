

temp=connectivity[,,1]
indexlower=lower.tri(temp, diag=FALSE)
indexlowertrue=which(indexlower==TRUE)
temp=temp[indexlower]
len=sum(indexlower)



riskfactors=as.factor(response$Sex)


image=matrix(NA,  dim(connectivity)[3], len)

for (i in 1:dim(connectivity)[3]) {
  temp=connectivity[,,i]
  indexlower=lower.tri(temp, diag=FALSE)
  temp=temp[indexlower]
  image[i,]=temp
}


#recordzerocols
indd=0
for (i in 1:dim(image)[2]) if(sd(image[,i])==0 ) {indd=rbind(indd,i);  cat ( i , sd(image[,i]), "\n" );}
indd=indd[2:dim(indd)[1]]
image=image[,-indd]




#############################

image_all=scale(image)
save(image_all, file="image_all.rda")


#########filter
V=read.csv('/Users/ali/Desktop/Jul/apoe2_paper/vertex/Vertices.csv', header = F)
V=unlist(V)

temp=connectivity[V,V,1]
indexlower=lower.tri(temp, diag=FALSE)
indexlowertrue=which(indexlower==TRUE)
temp=temp[indexlower]
len=sum(indexlower)



riskfactors=as.factor(response$Sex)


image=matrix(NA,  dim(connectivity)[3], len)

for (i in 1:dim(connectivity)[3]) {
  temp=connectivity[V,V,i]
  indexlower=lower.tri(temp, diag=FALSE)
  temp=temp[indexlower]
  image[i,]=temp
}


#recordzerocols
indd=0
for (i in 1:dim(image)[2]) if(sd(image[,i])==0 ) {indd=rbind(indd,i);  cat ( i , sd(image[,i]), "\n" );}
indd=indd[2:dim(indd)[1]]
image=image[,-indd]
dim(image)


image_filter=image
image_filter=scale(image_filter)
save(image_filter, file="image_filter.rda")
