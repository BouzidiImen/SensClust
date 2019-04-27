

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
#' @return PCA,Regression,Statistic.values
#' @export
#'
#'
EPM=function(Y,X,ModelType='Quadratic',nbpoints=50,Graphpred=FALSE,Graph2D=FALSE,Graph3D=FALSE,statistic.Value.Type='rsquared'){

  ###### PCA on the senso Dataset X : contains marks given by professional jugdes
  PCAm=function(X){
    X = stats::aggregate(X[,4:ncol(X)], list(X[,3]), mean,na.rm=T)
    respca=PCA(X[,2:ncol(X)],graph = FALSE)
    Dim1=respca$ind$coord[,1]
    Dim2=respca$ind$coord[,2]
    return(data.frame(Dim1,Dim2))
  }
  Dim1=PCAm(X)$Dim1
  Dim2=PCAm(X)$Dim2
  ##### Make the discrete space
  DiscreteSpace=function(Dim1,Dim2,nbpoints=50){
    xmin=floor(10*min(Dim1))/10
    xmax=ceiling(10*max(Dim1))/10
    ymin=floor(2*min(Dim2))/2
    ymax=ceiling(2*max(Dim2))/2
    x <- seq(xmin, xmax,length.out=nbpoints)
    y <- seq(ymin, ymax,length.out=nbpoints)
    d <- expand.grid(x = x, y = y)
    colnames(d)[1]='Dim1'
    colnames(d)[2]='Dim2'

    return(d)

  }
  DiSp=DiscreteSpace(Dim1,Dim2,nbpoints)
  #####Regression of every conso with the notes of judges
  Regression.lm = function(Y,Dim1,Dim2,ModelType){
    pred=matrix(0,nrow = nrow(DiSp),ncol = ncol(Y)) #Declaration of prediction matrix
    pref=matrix(0,nrow = nrow(DiSp),ncol = ncol(Y))
    regs=vector('list',ncol(Y))
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
      regs[[i]]=stats::lm(formula =model1 ,data=mapreg)
      pred[,i]=stats::predict(regs[[i]], newdata=DiSp)
      pref[,i]=(as.numeric(pred[,i])>as.numeric(mean(Y[,i])))


      return(list(Regression=regs, Prediction=pred,Preference=pref))}

  }
  ###### values available are rsquared , fstatistic and AIC
  res.reg=Regression.lm(Y,Dim1,Dim2,ModelType)
  statistic.values=function(res.reg,statistic.Value.Type='rsquared'){
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
            stop("type must be 'rsquared' , 'fstatistic' or 'AIC'" )
    )

    return(list(A))
  }
  z=rowMeans(res.reg$Prediction)
  p=rowMeans(res.reg$Preference)*100
  imgpred= as.image(Z=z,x=DiSp,ncol = nbpoints,nrow = nbpoints)
  imgpref  = as.image(Z=p,x=DiSp,ncol = nbpoints,nrow = nbpoints)

    p=plot_ly(x= imgpred$x,y= imgpred$y,z= imgpred$z,type='contour', contours = list(showlabels = TRUE)) %>%
    colorbar(title ='')%>%
    layout(title = "Prediction scores of one consumer")

  pc=plot_ly(x=imgpref$x,y=imgpref$y,z=imgpref$z,type='contour', contours = list(showlabels = TRUE)) %>%
    colorbar(title ='')%>%
    layout(title = "Preferences of one consumer")

  p3d=plot_ly(x=imgpref$x,y=imgpref$y,z=imgpref$z,type='surface')

  if(Graphpred) show(p)
  if(Graph2D) show(pc)
  if(Graph3D) show(p3d)


  return(list(Gpredc=p,Gpref=pc,G3D=p3d,PCA=PCAm(X),pred=res.reg$Prediction,pref=res.reg$Preference,Regression=Regression.lm(Y,Dim1,Dim2,ModelType),
              Statistic.values=statistic.values(res.reg,statistic.Value.Type)))
}
