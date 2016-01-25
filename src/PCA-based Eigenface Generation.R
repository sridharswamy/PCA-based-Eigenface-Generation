install.packages("pixmap")
library(pixmap)

#set working directory to the folder containing the images
#setwd('Set path')
setwd("D:/1-NCSU/Projects/PCA-based Eigenface Generation/faces-corrected")
#reading the file list
file_list <- list.files()

#reading the column names of each file
for(file in file_list){
  file_name=file_list[file]
  if (exists("tempnames")){
    tempnames=cbind(tempnames,file)
    rm(file_name)
  }
  
  if (!exists("tempnames")){
    tempnames=file;
  }
}

#read the pixels of each image
for (file in file_list){
  dataset <- read.pnm(file)
  y<-getChannels(dataset)
  file=as.vector(y)
  if (exists("temp")){
    temp=cbind(temp,file)
    rm(dataset)
    rm(y)
  }
  if (!exists("temp")){
    temp=file;
  }
}

#create a data frame of (M*N)*K where M*N is the image resolution and K is the number of images
imagesdataframe=as.data.frame(temp)

#assign the file names as the column names for the data frame
colnames(imagesdataframe)<-tempnames

#calculate the mean of each row
meanval<-rowMeans(imagesdataframe)

#commands to plot the image consisting of mean values
img=matrix(meanval,nrow=231)
plot(pixmapGrey(img))

B=imagesdataframe

#subtracting mean values from all pixels
B=B-meanval
Bmatrix=as.matrix(B)

#multiplying the normalized matrix and its transpose to obtain the covariance matrix
C_small=t(Bmatrix)%*%(Bmatrix)

#applying the eigen function to the covariance matrix
eigenCsmall=eigen(C_small)

#applying the svd function to the covariance matrix(alternative approach)
#svdCsmall=svd(C_small)


#eigenValues=svdCsmall$d
#eigenVectors=svdCsmall$v

#obtaining eigen values and vectors
eigenValues=eigenCsmall$values
eigenVectors=eigenCsmall$vectors

#obtaining the top 10 eigen vectors
top10EigenVectors=eigenVectors[,1:10]

#multiplying the top 10 eigen vectors with the normalized matrix to obtain the top 10 eigen faces
top10images=Bmatrix%*%top10EigenVectors

#partitioning the plot area into 2 rows and 5 columns to plot the top 10 images
par(mfrow=c(2,5))

#plotting the top 10 eigen faces

for(i in 1:10){
  imagex1=matrix(top10images[,i],nrow=231)
  plot(pixmapGrey(imagex1), main=paste("Eigenface",i))
}
?plot