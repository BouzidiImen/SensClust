
#' Clustering
#'
#' @param Y a numeric matrix or a data frame with all numeric columns (Ex:consumers scores)
#' @param ClustMeth Clustering method that must be 'Hierarchical','Diana','Kmeans','Clara','Pam','Sota' or 'Som'
#' @param k integer, the number of clusters. It is required that 0<k<n where n is the number of observations (i.e., n = nrow(x))
#' @param Sotadismethod character string specifying the metric to be used for calculating dissimilarities between observations for Sota method.It could be "euclidean" or "correlation"
#' @param Pdismethod  character string specifying the metric to be used for calculating dissimilarities between observations for PAM method.It could be "euclidean" or "manhattan"
#' @param Cdismethod character string specifying the metric to be used for calculating dissimilarities between observations for Clara method.It could be "euclidean","manhattan" or "jaccard"
#' @param Ddismethod character string specifying the metric to be used for calculating dissimilarities between observations for Diana method.It could be "euclidean" or "manhattan"
#' @param Hdismethod  The method to calculate a dissimilarity structure as produced by dist for hierarchical method.It could be :"aitchison", "euclidean", "maximum", "manhattan", "canberra","binary" or "minkowski"
#' @param Hmethod the agglomeration method to be used ,should be "single", "complete", "average", "mcquitty", "ward.D", "ward.D2", "centroid" or "median"
#' @param Graph  TRUE if you want to visualize the dendrogram (only for Hierarchical and Diana methods )
#' @param VarCart  TRUE if you want to visualize Variables's representation
#' @param IndCart  TRUE if you want to visualize Distribution of consumers
#' @param ElbowP TRUE if you want to visualize The plot of the elbow method for Hierarchical method
#'
#' @return list
#' @export
#' @import FactoMineR
#' @import factoextra
#' @import cluster
#' @import clValid
#' @import kohonen
#'
Clustering=function(Y,ClustMeth='Hierarchical',k=3,Sotadismethod='euclidean',Pdismethod='euclidean',Cdismethod='euclidean',Ddismethod='euclidean',Hdismethod='euclidean',Hmethod="ward.D2",
                    Graph=T,VarCart=F,IndCart=F,ElbowP=F ){
repPCA=function(class,Y){

      classif=cbind.data.frame(class,Y)

      res.pca=PCA(classif,quali.sup =1,graph = F )
      p=fviz_pca_var(res.pca, col.var = "cos2",
                 gradient.cols ="jco",
                 repel = T )
      p2=fviz_pca_ind(res.pca,
                  geom.ind = "point",
                  col.ind = classif$class, # colorer by groups
                  palette = 'jco',
                  addEllipses = T,
                  legend.title = "Groups")
  return(list(graphvar=p,graphind=p2))
}

if(ClustMeth=='Hierarchical'||ClustMeth=='Diana'||ClustMeth=='Kmeans'||ClustMeth=='Clara'||ClustMeth=='Pam'||
   ClustMeth=='Sota'||ClustMeth=='Som'){
########### hierarchical ########
switch (ClustMeth,
        Hierarchical = {
    if (Hdismethod=='euclidean'||Hdismethod=='aitchison'||Hdismethod=='maximum'||Hdismethod=='manhattan'||
        Hdismethod=='canberra'||Hdismethod=='minkowski'||Hdismethod=='binary') d=dist(Y,method = Hdismethod)
    else stop('type must be "aitchison", "euclidean", "maximum", "manhattan", "canberra","binary" or "minkowski"')

    if(Hmethod=="single"||Hmethod=="complete"||Hmethod=="average"||
       Hmethod=="mcquitty"||Hmethod=="ward.D"
       ||Hmethod=="ward.D2"||Hmethod=="centroid" ||Hmethod=="median") hc=hclust(d,method = Hmethod)
    else stop('type must be "single", "complete", "average", "mcquitty", "ward.D", "ward.D2", "centroid" or "median"')

  classes=cutree(hc,k=k)
  class=as.factor(classes)
  #C=catdes(hedo.class,num.var = 1)
  dend=as.dendrogram(hc)
  dend_plot=fviz_dend(hc, cex = 0.5, k=k, main = "Dendrogram ", xlab = "Objects", ylab = "Distance", # Cut in four groups
                      k_colors = "jco",rect = TRUE, # Add rectangle around groups
                      rect_border = 'jco', rect_fill =F)

  if(Graph==T) show(dend_plot)
  res=repPCA(class,Y)
  p=res$graphvar
  if(VarCart==T) show(p)
  p2=res$graphind
  if(IndCart==T) show(p2)
  p3=fviz_nbclust(Y, hcut, method = "wss") +
    geom_vline(xintercept = 4, linetype = 2)+
    labs(subtitle = "Elbow method")
  if(ElbowP) show(p3)

  return(list(Distance=d,Hclust=hc,dendrogram=dend_plot,Pvar=p,Pind=p2,ElbowP=p3))
  },
  #############DIANA##############
  Diana={

            if(Ddismethod=="euclidean" ||Ddismethod=="manhattan") D=diana(Y,metric=Ddismethod,diss=FALSE)
            else stop('type must be "euclidean" or "manhattan"')
            dend_plot=fviz_dend(D, cex = 0.5, k=3, main = "Dendrogram ", xlab = "Objects", ylab = "Distance", # Cut in four groups
                                k_colors = "jco",rect = TRUE, # Add rectangle around groups
                                rect_border = 'jco', rect_fill = F)

            if(Graph) show(dend_plot)
            classes=cutree(D,k=k)
            class=as.factor(classes)
            res=repPCA(class,Y)
            p=res$graphvar
            if(VarCart) show(p)
            p2=res$graphind
            if(IndCart) show(p2)


            return(list(Dianaclust=D,dendro= dend_plot,Pvar=p,Pind=p2))
  },
  ################Kmeans########################
  Kmeans={
      km.res1 <- kmeans(Y,k)
      f=fviz_cluster(list(data = Y, cluster = km.res1$cluster),
                     ellipse.type = "norm", geom = "point", stand = FALSE, palette = "jco",
                     ggtheme = theme_classic())
      if(Graph) show(f)
      class=as.factor(km.res1$cluster)
      res=repPCA(class,Y)
      p=res$graphvar
      if(VarCart==T) show(p)
      p2=res$graphind
      if(IndCart==T) show(p2)
      return(list(Km=km.res1,graph=f,IndCart=p2,VarCart=p))

  }
  ##################CLARA##############
  ,Clara={

      if (Cdismethod=='euclidean'||Cdismethod=='manhattan'||Cdismethod=="jaccard") cl=clara(Y,k,metric = Cdismethod) #it's recomended to fix samples(default=5)
      else stop('Type must be "euclidean","manhattan" or "jaccard"')
      f=fviz_cluster(cl,palette ="jco",# color paletteellipse.type ="t",# Concentration
                     ellipsegeom ="point",pointsize =1,ggtheme=theme_classic())
      if(Graph==T) show(f)
      class=as.factor(cl$clustering)
      res=repPCA(class,Y)
      p=res$graphvar
      if(VarCart==T) show(p)
      p2=res$graphind
      if(IndCart==T) show(p2)
      return(list(Claracl=cl,Graph=f,IndCart=p2,VarCart=p))
  },
  #################PAM#############
  #pam may need too much memory or too much computation time since both are O(n^2). Then, clara() is preferable, see its documentation.
  Pam={
      if (Pdismethod=='euclidean'||Pdismethod=='manhattan') p=pam(Y,k,metric = Pdismethod) #it's recomended to fix samples(default=5)
      else stop('Type must be "euclidean"or "manhattan"')

      f=fviz_cluster(p,palette ="jco",# color paletteellipse.type ="t",# Concentration
                     ellipsegeom ="point",pointsize =1,ggtheme=theme_classic())
      if(Graph==T) show(f)
      class=as.factor(p$clustering)
      res=repPCA(class,Y)
      p=res$graphvar
      if(VarCart==T) show(p)
      p2=res$graphind
      if(IndCart==T) show(p2)
      return(list(Pamcl=p,Graph=f,IndCart=p2,VarCart=p))



  },
  ###################SOTA################
  Sota={
      if(Sotadismethod=='euclidean'||Sotadismethod=='correlation') s=sota(as.matrix(Y),maxCycles=k-1,distance=Sotadismethod)
      else stop('Type must be "euclidean" or"correlation"')
      class=as.factor(s$clust)
      res=repPCA(class,Y)
      p=res$graphvar
      if(VarCart==T) show(p)
      p2=res$graphind
      if(IndCart==T) show(p2)
      return(list(sotaCl=s,IndCart=p2,VarCart=p))

  },
  Som={
      s = som(Y,somgrid(k, k, "hexagonal")) # use of a cart

      class=as.factor(s$unit.classif)
      res=repPCA(class,Y)
      f=res$graphvar
      if(VarCart) show(p)
      p2=res$graphind
      if(IndCart) show(p2)
      return(list(SomCl=s,Graph=f,IndCart=p2,VarCart=p))

    }



)

}

else stop("ClustMeth must be 'Hierarchical','Diana','Kmeans','Clara','Pam','Sota' or 'Som'")



}
