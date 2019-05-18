
ClusteringR
===========

Its functionality includes exploratory data analysis, data segmentation and data visualization.It is designed to handle realistic data sets : hedonic data set and sensory data set. It makes use of several clustering methods as well as the implementation of partition-validity approach.

Finally, a graphical user interface is implemented with R shiny in order to propose a user friendly package.

Installation
------------

You can install the development version from [GitHub](https://github.com/) with :

``` r
install.packages("devtools")
devtools::install_github("BouzidiImen/ClusteringR")
```

Usage
-----

You can find below an overall look at how ClusteringR can be useful for your sensory analysis.

### Clustering :

Diverse methods of clustering are available in the ClusteringR package :

'hierarchical', 'diana', 'kmeans', 'clara', 'pam', 'sota' and 'som'

``` r
library(ClusteringR)
# Create a clustering object  -------------------------------------------------
cl <- Clustering(t(hedo),ClustMeth='hierarchical',k=3,Hdismethod='euclidean',Hmethod="ward.D2",
                    Graph=F,VarCart=F,IndCart=F )
# get clusters
clusters=cl$classes

#Plot of  dendrogram 

plot(cl$dendrogram)
```

<img src="man/figures/Dendo.png" align="center" />

``` r
help("Clustering") # to see more information about the function of clustering 
```

### Sensory Analysis :

Based on the sensory map, this package make it easier to know consumers's behaviour, their likes and dislikes.

``` r
library(ClusteringR)
# Create an EPM object  -------------------------------------------------
E <- EPM(hedo,senso,ModelType='Quadratic',respt=FALSE,nbpoints=50,Graphpred=FALSE,Graph2D=TRUE,Graph3D=FALSE,statistic.Value.Type='rsquared')
```

<img src="man/figures/fig.png" align="center" />

``` r
help("EPM") # to see more information about the function of external preferences mapping 
```

Data available in the package
-----------------------------

-   senso : sensory data from a professional trial of 8 biscuits by 12 judges.
-   hedo : hedonic data from a trial of 8 biscuits by 294 customers.

``` r
#Usage 
library(ClusteringR)
S=senso # sensory data 
H=hedo # hedonic data
```

A User Friendly Package
-----------------------

Within the package you find a shiny application that demonstrate what the package does and make its use easier.

``` r
ClustShiny() #run shiny application
```

<img src="man/figures/Shiny.PNG" align="center" />

<img src="man/figures/shiny1.PNG" align="center" />

<img src="man/figures/shiny2.PNG" align="center" />

P.S : You can visit the following link to get a sneak peek on the package functionalities.

[Shiny application for the package](https://imenbouzidi.shinyapps.io/InterfaceForThepackage/)

Acknowledgement :
-----------------

In preparation of my package, I had to take the help and guidance of my professor Ibtihel Rebhi, who deserves my deepest gratitude.
