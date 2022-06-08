library(cluster)
library(Rtsne)
library(ggplot2)
library(dplyr)
library(clustrd)
library(caret)
library(tree)
library(rpart)
library(rpart.plot)
library(glmnet)
library(fossil)
library(gbm)
library(ipred)
library(earth)
library(car)


##Preparation
#convert 'obamacare' to factor
medical=MedicareData2
str(medical)

care=medical[,3]
care_yn=as.factor(ifelse(care==1,'Yes','No'))

medical.m=medical[,c(-1, -3)]
data=cbind(medical.m,care_yn)
class(data) #data.frame
str(data)


###EDA
plot(data)
transformed.medical=daisy(data,metric = "gower")
summary(transformed.medical)

##Gower+PAM:
#cluster=2 is the best number of clusters: big separation
silhouette <- c()
silhouette = c(silhouette, NA)
for(i in 2:10){
  pam_clusters = pam(as.matrix(transformed.medical),
                     diss = TRUE,
                     k = i)
  silhouette = c(silhouette ,pam_clusters$silinfo$avg.width)
}
plot(1:10, silhouette,
     xlab = "Clusters",
     ylab = "Silhouette Width")
lines(1:10, silhouette)

pam_medical=pam(transformed.medical, diss = TRUE, k = 2)
pam_medical$clusinfo
gowerPAMclustering=pam_medical$clustering
data[pam_medical$medoids, ]

tsne_object <- Rtsne(transformed.medical, is_distance = TRUE, perplexity = floor((nrow(data) - 1) / 3))

tsne_df <- tsne_object$Y %>% data.frame() %>% setNames(c("X", "Y")) %>% mutate(cluster = factor(pam_medical$clustering))

ggplot(aes(x = X, y = Y), data = tsne_df) +geom_text(aes(label=seq(1,nrow(tsne_df),1)),size=3)+
  geom_point(aes(color = cluster))

##MCA+KMeans
#--So I'm wondering what could be my best dimension+cluster combination---#
best.combination=tuneclus(data, nclusrange = 3:7, ndimrange = 2:5,
                          method = c("clusCA","iFCB","MCAk"),
                          criterion = "asw", dst = "full", alpha = 1, alphak = 1,
                          center = TRUE, scale = TRUE, rotation = "none", nstart = 100,
                          smartStart = NULL, seed = NULL)

#the result shows that 3-cluster in 2-dimension are the best
best.combination

#################################
#--library(clustrd)---#
mix32=clusmca(data, nclus=3, ndim=2, method=c("clusCA","MCAk"),
              alphak = .5, nstart = 100, smartStart = NULL, gamma = TRUE,
              inboot = FALSE, seed = NULL)

mix32
options(ggrepel.max.overlaps = Inf)
plot(mix32)
mix32.clustering=mix32$cluster

mix22=clusmca(data, nclus=2, ndim=2, method=c("clusCA","MCAk"),
              alphak = 1, nstart = 100, smartStart = NULL, gamma = TRUE,
              inboot = FALSE, seed = NULL)
mix22
plot(mix22)
mix22.clustering=mix22$cluster


################
##Hybrid Approach: PCA followed by KMEANS
medical.data=medical[,c(-1,-3)] #remove qualitative variables
medical.data
scaled.medical.data=scale(medical.data)

#--So I'm wondering what could be my best dimension+cluster combination---#
#6-cluster in 4-dimension is the best combination
best.combination2=tuneclus(scaled.medical.data, nclusrange = 3:7, ndimrange = 2:5,
                          method = c("RKM","FKM","mixedRKM","mixedFKM","clusCA","iFCB","MCAk"),
                          criterion = "asw", dst = "full", alpha = 1, alphak = NULL,
                          center = TRUE, scale = TRUE, rotation = "none", nstart = 100,
                          smartStart = NULL, seed = NULL)

best.combination2

#---Okay, let's then check a 6 cluster, 4-dimensional choice---#
mixt64=cluspca(scaled.medical.data,nclus = 6,ndim = 4, alpha = 1,center = TRUE, scale = TRUE, rotation = "varimax", nstart = 100)
mixt64
summary(mixt64)
plot(mixt64, cludesc = TRUE)
plot(mixt64, dims = c(1,2), cludesc = FALSE, what = c(TRUE,TRUE))
mixt64.clustering=mixt64$cluster

##Comparison
rand.index(gowerPAMclustering,as.vector(mix32.clustering)) #0.52
rand.index(gowerPAMclustering,as.vector(mix22.clustering)) #0.49
rand.index(as.vector(mix32.clustering),as.vector(mix22.clustering)) #0.50
rand.index(as.vector(mix32.clustering),as.vector(mixt64.clustering)) #0.61

##Tree plot for clustering methods
Cluster.Assignment=data.frame(as.vector(gowerPAMclustering),as.vector(mix32.clustering),as.vector(mix22.clustering), as.vector(mixt64.clustering))
colnames(Cluster.Assignment)<-c("Gower+PAM","MIX32","MIX22","MIXT64")
rownames(Cluster.Assignment)<-as.factor(medical[,1])

#--library(fossil)---#
#rand.index(Cluster.Assignment[,1],Cluster.Assignment[,6])
similarity.matrix=matrix(0,ncol(Cluster.Assignment),ncol(Cluster.Assignment))
for(i in 1:ncol(Cluster.Assignment))
{
  for(j in 1:ncol(Cluster.Assignment))
  {
    similarity.matrix[i,j]=rand.index(Cluster.Assignment[,i],Cluster.Assignment[,j])
  }
}
similarity.matrix
rownames(similarity.matrix)=colnames(Cluster.Assignment)
colnames(similarity.matrix)=colnames(Cluster.Assignment)
similarity.matrix
plot(hclust(as.dist(1-similarity.matrix),method = "complete"))


###Modeling
data
predictors=data[,-1]
predictors
response.df=data.frame(data$MSPB)

set.seed(1)
tr=createDataPartition(data$MSPB,p=3/4,list = F) #---in a regression setting, we are preserving quartiles--#

tr.pred=predictors[tr,]
test.pred=predictors[-tr,]
tr.response=response.df[tr,]
test.response=response.df[-tr,]

#---Some data transformation (scaling, centering, etc.), notice how the categoricals are untouched---#
trans=preProcess(tr.pred, method = c('knnImpute','center','scale'))
trans.tr.pred=predict(trans,tr.pred)
trans.test.pred=predict(trans,test.pred)

##############
ctrl=trainControl(method = "repeatedcv", number = 10, repeats=5, savePredictions = T)
#################


##########
#--One pruned tree--#
##########
cv.tree=train(x=trans.tr.pred,y=tr.response,method='rpart',trControl=ctrl)
cv.tree
plot(cv.tree)
pred.tree=predict(cv.tree,newdata = trans.test.pred)
pred.tree

#head(predict(cv.tree,newdata = trans.test.pred, type = "prob"))
#confusionMatrix(data=pred.tree,test.response,positive = "yes")
cv.tree$results
Acc.tree=data.frame(
  RMSE = RMSE(pred.tree, test.response),
  Rsquare = R2(pred.tree, test.response)
)
Acc.tree
system.time(train(x=trans.tr.pred,y=tr.response,method='rpart',trControl=ctrl))
varImp(cv.tree)
plot(varImp(cv.tree))

# advanced Tree plot
cont=rpart.control(minsplit=3, cp = 0.03508263,
                   maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, xval = 10,
                   surrogatestyle = 0, maxdepth = 30)

modelTree2<- rpart(MSPB~., data = data, method = 'anova', control = cont)
rpart.plot(modelTree2, extra = "auto")
modelTree2

###################
#--Next, a bagged tree---#
###################
cv.bagged.tree=train(x=trans.tr.pred,y=tr.response,method='treebag',trControl=ctrl)
cv.bagged.tree
#plot(cv.bagged.tree)
pred.bagged.tree=predict(cv.bagged.tree,newdata = trans.test.pred)
pred.bagged.tree

#head(predict(cv.bagged.tree,newdata = trans.test.pred, type = "prob"))
cv.bagged.tree$results
Acc.TreeBagged=data.frame(
  RMSE = RMSE(pred.bagged.tree, test.response),
  Rsquare = R2(pred.bagged.tree, test.response)
)
Acc.TreeBagged
system.time(train(x=trans.tr.pred,y=tr.response,method='treebag',trControl=ctrl))
varImp(cv.bagged.tree)
plot(varImp(cv.bagged.tree))


###################
#--Next, a random forest---#
###################
cv.randomforest=train(x=trans.tr.pred,y=tr.response,method='cforest',trControl=ctrl)
cv.randomforest
plot(cv.randomforest)
pred.randomforest=predict(cv.randomforest,newdata = trans.test.pred)
pred.randomforest
cv.randomforest$results
Acc.RF=data.frame(
  RMSE = RMSE(pred.randomforest, test.response),
  Rsquare = R2(pred.randomforest, test.response)
)
Acc.RF
system.time(train(x=trans.tr.pred,y=tr.response,method='cforest',trControl=ctrl))
varImp(cv.randomforest)
plot(varImp(cv.randomforest))


##################
#---A simple straight line---#
##################
cv.linearreg=train(x=trans.tr.pred,y=tr.response,method='lm',trControl=ctrl)
cv.linearreg
summary(cv.linearreg$finalModel)

pred.linearreg=predict(cv.linearreg,newdata = trans.test.pred)
pred.linearreg
cv.linearreg$results

Acc.Line=data.frame(
  RMSE = RMSE(pred.linearreg, test.response),
  Rsquare = R2(pred.linearreg, test.response)
)
Acc.Line
system.time(train(x=trans.tr.pred,y=tr.response,method='lm',trControl=ctrl))
varImp(cv.linearreg)
plot(varImp(cv.linearreg))


##Line Assumption check
crPlots(cv.linearreg$finalModel)
qqnorm(resid(cv.linearreg$finalModel))
dwt(cv.linearreg$finalModel)


