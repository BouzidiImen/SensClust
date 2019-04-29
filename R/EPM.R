

#########################EPM###################
#' External preference mapping
#' @description This function is dedicated for hedonic data segmentation
#' @param Y :a numeric matrix or a data frame with all numeric columns (Ex:consumers scores).
#' @param X :a data frame with n rows (individuals) and p columns (numeric variables)
#' @param ModelType :Type of regression's model can be 'Vector' ,'Circular','Quadratic' or 'Eliptic'
#' @param nbpoints :Number of points to create the dicrete space
#' @param Graphpred :TRUE if you want to view the Prediction scores of one consumer heatmap,FALSE otherwise
#' @param Graph2D :TRUE if you want to view preferences of one consumer,FaLSE otherwise
#' @param Graph3D :TRUE if you want to view the external preference Map in 3D,FALSE otherwise
#' @param statistic.Value.Type :Extract the 'rsquared' , 'fstatistic' or 'AIC' of every regression
#' @import FactoMineR
#' @import plotly
#' @import fields
#' @return pred,pref,Regression,Statistic.values,Graphpred,Graph2D,Graph3D
#' @export
#'
#' @examples
#' library(ClusteringR)
#'
#' E=EPM(Y=hedo,X=senso,ModelType='Quadratic',
#' nbpoints=50,Graphpred=FALSE,Graph2D=FALSE,
#' Graph3D=FALSE,statistic.Value.Type='rsquared')
#' consumer.preferences=E$prefc
#'
EPM=function(Y,X,ModelType='Quadratic',nbpoints=50,Graphpred=FALSE,Graph2D=FALSE,Graph3D=FALSE,statistic.Value.Type='rsquared'){

  ###### PCA on the senso Dataset X : contains marks given by professional jugdes
    S= stats::aggregate(X[,4:ncol(X)], list(X[,3]), mean,na.rm=T)
    respca=PCA(S[,2:ncol(S)],graph = FALSE)
    Dim1=respca$ind$coord[,1]
    Dim2=respca$ind$coord[,2]
  ##### Make the discrete space
    xmin=floor(min(Dim1))
    xmax=ceiling(max(Dim1))
    ymin=floor(min(Dim2))
    ymax=ceiling(max(Dim2))
    x <- seq(xmin, xmax,length.out=nbpoints)
    y <- seq(ymin, ymax,length.out=nbpoints)
    DiSp <- expand.grid(x = x, y = y)
    colnames(DiSp)[1]='Dim1'
    colnames(DiSp)[2]='Dim2'

  #####Regression of every conso with the notes of judges
    pred=matrix(0,nrow = nrow(DiSp),ncol = ncol(Y)) #Declaration of prediction matrix
    pref=matrix(0,nrow = nrow(DiSp),ncol = ncol(Y))
    res.reg=vector('list',ncol(Y))
    switch(ModelType,
           Vector={model1=stats::as.formula(paste('conso','~Dim1+Dim2'))},#Setting the formula of the model depending on the type
           Circular={model1=stats::as.formula(paste('conso','~Dim1+Dim2+I(Dim1*Dim1+Dim2*Dim2)'))  },
           Quadratic={ model1=stats::as.formula(paste('conso','~I(Dim1*Dim1)+I(Dim2*Dim2)+Dim1*Dim2'))},
           Eliptic={model1=stats::as.formula(paste('conso','~I(Dim1+Dim1)+I(Dim2+Dim2)')) },
           stop("ModelType must be 'Vector' ,'Circular','Quadratic' or 'Eliptic'")
    )

    for (i in 1:ncol(Y)){
      mapreg=cbind.data.frame(Y[,i],Dim1,Dim2)#CONCA Database of every consumer with the PCA two first Dim
      colnames(mapreg)[1]='conso'
      res.reg[[i]]=stats::lm(formula =model1 ,data=mapreg)
      pred[,i]=stats::predict(res.reg[[i]], newdata=DiSp)
      pref[,i]=(as.numeric(pred[,i])>as.numeric(mean(Y[,i])))}




  z=rowMeans(pred)
  p=rowMeans(pref)*100
  imgpred=as.image(Z=z,x=DiSp,ncol = nbpoints,nrow = nbpoints)
  imgpref=as.image(Z=p,x=DiSp,ncol = nbpoints,nrow = nbpoints)

  p1=plot_ly(x= imgpred$x,y= imgpred$y,z= imgpred$z,type='contour', contours = list(showlabels = TRUE)) %>%
    colorbar(title ='Score')%>%
    layout(title = "Prediction of one consumer's score ")
  p2=plot_ly(x=imgpref$x,y=imgpref$y,z=imgpref$z,type='contour', contours = list(showlabels = TRUE)) %>%
    colorbar(title ='Score')%>%
    layout(title = "Preferences of one consumer")

  p3=plot_ly(x=imgpref$x,y=imgpref$y,z=imgpref$z,type='surface')%>%
    colorbar(title ='Score')%>%
    layout(title = "Preference Map ")
  if(Graphpred)show(p1)
  if(Graph2D) show(p2)
  if(Graph3D) show(p3)


  ###### values available are rsquared , fstatistic and AIC
  switch (statistic.Value.Type,
          rsquared= {
            A=unlist(res.reg,lapply(res.reg$regression, function(x)summary(x)$r.squared))
          }, #Extract the rsquared of every regression
          fstatistic={
            A=unlist(res.reg,lapply(res.reg$regression, function(x)summary(x)$fstatictic[1]))
          },
          AIC={
            A=unlist(res.reg,lapply(res.reg$regression, function(x)summary(x)$extractAIC[2]))
          },
          stop("type must be 'rsquared' , 'fstatistic' or 'AIC'" ))


  return(list(pred=pred,pref=pref,Regression=res.reg,Statistic.values=A,Graphpred=p1,Graph2D=p2,Graph3D=p3))
}
