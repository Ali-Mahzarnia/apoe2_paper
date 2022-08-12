con332=connectivity[,,1]
con332[con332!=0]=0
dim(con332)

for (i in 1:dim(t)[1]) {
temp=t[i,]
  row=temp[1]; col=temp[2];
  con332[row, col]=con332[col,row]=temp[3]
}


con332[293,112]
con332[112,293]

library(R.matlab)
writeMat("con332.mat", con332=con332 )
