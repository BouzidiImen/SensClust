

shinyServer(function(input, output) {
############### Hedonic Data #############
  hedo <- reactive({
    if(is.null(input$file)){return()}
    read.table(file=input$file$datapath,sep=input$sep, header = input$header,row.names = 1)

  })

  output$table <- DT::renderDataTable({
    if(is.null(hedo())){return ()}
    DT::datatable(hedo(),options = list(scrollX = TRUE))
    })
  output$sum <-renderPrint({
    if(is.null(hedo())){return ()}
    summary(hedo())
  })
  output$tb <- renderUI({
    if(is.null(hedo())){return ()}
    else
      tabsetPanel(
        tabPanel("Dataset", br(),DT::dataTableOutput("table")),
        tabPanel("Summary",br(), verbatimTextOutput("sum"))
        )
    }
     )

############Sensory data ##############
  senso <- reactive({
    if(is.null(input$fileS)){return()}
    read.table(file=input$fileS$datapath,sep=input$sepS, header = input$headerS)
  })
output$table2 <- DT::renderDataTable({
  if(is.null(senso())){return ()}
  DT::datatable(senso(),options = list(scrollX = TRUE))
  })

output$sum2 <-renderPrint({
  if(is.null(senso())){return ()}
  summary(senso())
})
output$tb2 <- renderUI({
  if(is.null(senso())){return ()}
  else
    tabsetPanel(

      tabPanel("Dataset",br(), DT::dataTableOutput("table2")),

      tabPanel("Summary",br(),verbatimTextOutput("sum2"))) })


##################Pclust##################
###########kmeans############
K=reactive({
  if(is.null(hedo())) return ()
  K=Clustering(t(hedo()),ClustMeth = 'Kmeans',k=input$clusts,Graph = F,VarCart = F,IndCart = F)

  return(K)
})
output$gk=renderPlot({
  K()$graph
})
output$down <- downloadHandler(
  filename =  function() {
    paste("Clusterskmeans.pdf")
  },
  # content is a function with argument file. content writes the plot to the device
  content = function(file) {
    pdf(file) # open the pdf device
    print(K()$graph)
    dev.off()  # turn the device off
  }
)
output$vk=renderPlot({
  K()$VarCart
})
output$down1 <- downloadHandler(
  filename =  function() {
    paste("Varkmeans.pdf")
  },
  # content is a function with argument file. content writes the plot to the device
  content = function(file) {
    pdf(file) # open the pdf device
    print(K()$VarCart)
    dev.off()  # turn the device off
  }
)
output$ik=renderPlot({
  K()$IndCart
})

output$down2 <- downloadHandler(
  filename =  function() {
    paste("Indkmeans.pdf")
  },
  # content is a function with argument file. content writes the plot to the device
  content = function(file) {
    pdf(file) # open the pdf device
    print(K()$IndCart)
    dev.off()  # turn the device off
  }
)

############Clara ############
C=reactive({
  if(is.null(hedo())){return()}
  C=Clustering(t(hedo()),ClustMeth = 'Clara',Cdismethod = input$MetC,k=input$clusts,Graph = F,VarCart = F,IndCart = F)
  return(C)
})
output$gc=renderPlot({
  C()$Graph
})
output$down3 <- downloadHandler(
  filename =  function() {
    paste("clustersDiana.pdf")
  },
  # content is a function with argument file. content writes the plot to the device
  content = function(file) {
    pdf(file) # open the pdf device
    print(C()$Graph)
    dev.off()  # turn the device off
  }
)
output$vc=renderPlot({
  C()$VarCart
})
output$down4 <- downloadHandler(
  filename =  function() {
    paste("VarDiana.pdf")
  },
  # content is a function with argument file. content writes the plot to the device
  content = function(file) {
    pdf(file) # open the pdf device
    print(C()$VarCart)
    dev.off()  # turn the device off
  }
)
output$ic=renderPlot({
  C()$IndCart
})
output$down5 <- downloadHandler(
  filename =  function() {
    paste("IndDiana.pdf")
  },
  # content is a function with argument file. content writes the plot to the device
  content = function(file) {
    pdf(file) # open the pdf device
    print(C()$IndCart)
    dev.off()  # turn the device off
  }
)
################PAM#############
P=reactive({
  if(is.null(hedo()))return ()
  P=Clustering(t(hedo()),ClustMeth = 'Pam',Pdismethod =input$MetPa ,k=input$clusts,Graph = F,VarCart = F,IndCart = F)

  return(P)
})
output$gp=renderPlot({
  P()$Graph
})
output$down6 <- downloadHandler(
  filename =  function() {
    paste("clustersPam.pdf")
  },
  # content is a function with argument file. content writes the plot to the device
  content = function(file) {
    pdf(file) # open the pdf device
    print(P()$Graph)
    dev.off()  # turn the device off
  }
)
output$vp=renderPlot({
  P()$VarCart
})
output$down7 <- downloadHandler(
  filename =  function() {
    paste("VarPam.pdf")
  },
  # content is a function with argument file. content writes the plot to the device
  content = function(file) {
    pdf(file) # open the pdf device
    print( P()$VarCart)
    dev.off()  # turn the device off
  }
)
output$ip=renderPlot({
  P()$IndCart
})
output$down8 <- downloadHandler(
  filename =  function() {
    paste("IndPam.pdf")
  },
  # content is a function with argument file. content writes the plot to the device
  content = function(file) {
    pdf(file) # open the pdf device
    print(  P()$IndCart)
    dev.off()  # turn the device off
  }
)





###############Sota ##################
S=reactive({
  if(is.null(hedo())){return ()}
  S=Clustering(t(hedo()),ClustMeth = 'Sota',Sotadismethod =input$MetSo ,k=input$clusts,Graph = F,VarCart = F,IndCart = F)

  return(S)
})
output$gS=renderPlot({
  plot(S()$sotaCl)
})
output$down15 <- downloadHandler(
  filename =  function() {
    paste("clustersSota.pdf")
  },
  # content is a function with argument file. content writes the plot to the device
  content = function(file) {
    pdf(file) # open the pdf device
    print(plot(S()$sotaCl))
    dev.off()  # turn the device off
  }
)
output$vS=renderPlot({
  S()$VarCart
})
output$down16 <- downloadHandler(
  filename =  function() {
    paste("VarSota.pdf")
  },
  # content is a function with argument file. content writes the plot to the device
  content = function(file) {
    pdf(file) # open the pdf device
    print( S()$VarCart)
    dev.off()  # turn the device off
  }
)
output$iS=renderPlot({
  S()$IndCart
})
output$down17 <- downloadHandler(
  filename =  function() {
    paste("IndSota.pdf")
  },
  # content is a function with argument file. content writes the plot to the device
  content = function(file) {
    pdf(file) # open the pdf device
    print( S()$IndCart)
    dev.off()  # turn the device off
  }
)







###############SOM ##################
SOM=reactive({
  if(is.null(hedo())){return ()}
  SOM=Clustering(t(hedo()),ClustMeth = 'Som',k=input$clusts,Graph = F,VarCart = F,IndCart = F)
  return(SOM)
})
output$gSom=renderPlot({
  co2<-c("#FFFFCC","#C7E9B4","#7FCDBB","#40B6C4","#2C7FB8" ,"#253494")
  co2<-colorRampPalette(co2)
  plot(SOM()$SomCl,palette.name = co2)
})
output$downSO1 <- downloadHandler(
  filename =  function() {
    paste("clustersSOM.pdf")
  },
  # content is a function with argument file. content writes the plot to the device
  content = function(file) {
    pdf(file) # open the pdf device
    print(plot(SOM()$sotaCl))
    dev.off()  # turn the device off
  }
)
output$vSom=renderPlot({
  SOM()$VarCart
})
output$downSO2 <- downloadHandler(
  filename =  function() {
    paste("VarSOM.pdf")
  },
  # content is a function with argument file. content writes the plot to the device
  content = function(file) {
    pdf(file) # open the pdf device
    print( SOM()$VarCart)
    dev.off()  # turn the device off
  }
)
output$iSom=renderPlot({
  SOM()$IndCart
})
output$downSO3<- downloadHandler(
  filename =  function() {
    paste("IndSOM.pdf")
  },
  # content is a function with argument file. content writes the plot to the device
  content = function(file) {
    pdf(file) # open the pdf device
    print( SOM()$IndCart)
    dev.off()  # turn the device off
  }
)







################Hclustering#################

#############hierch##########
H=reactive({
  if(is.null(hedo())){return()}
  H=Clustering(t(hedo()),ClustMeth='Hierarchical',k=input$clustsH,Hdismethod=input$MetH,Hmethod=input$MetricH,Graph=F,VarCart=F,IndCart=F,ElbowP=F )
  return(H)
})
output$dh=renderPlot({

  withProgress(message = 'Making plot', value = 0.1, {
    # Number of times we'll go through the loop
    n <- 10

    for (i in 1:n) {
      # Each time through the loop, add another row of data. This is
      # a stand-in for a long-running computation.
      p=H()$dendrogram

      # Increment the progress bar, and update the detail text.
      incProgress(1/n, detail = paste("Doing part", i))

      # Pause for 0.1 seconds to simulate a long computation.
      Sys.sleep(0.1)
    }})
  p

})
output$down9 <- downloadHandler(
  filename =  function() {
    paste("dendroHier.pdf")
  },
  # content is a function with argument file. content writes the plot to the device
  content = function(file) {
    pdf(file) # open the pdf device
    print(H()$dendrogram)
    dev.off()  # turn the device off
  }
)
output$vh=renderPlot({
  H()$Pvar
})
output$down10 <- downloadHandler(
  filename =  function() {
    paste("VarHier.pdf")
  },
  # content is a function with argument file. content writes the plot to the device
  content = function(file) {
    pdf(file) # open the pdf device
    print(H()$Pvar)
    dev.off()  # turn the device off
  }
)
output$ih=renderPlot({
  H()$Pind
})
output$down11 <- downloadHandler(
  filename =  function() {
    paste("IndHier.pdf")
  },
  # content is a function with argument file. content writes the plot to the device
  content = function(file) {
    pdf(file) # open the pdf device
    print(H()$Pind)
    dev.off()  # turn the device off
  }
)

output$ELH=renderPlot({
  show(H()$ElbowP)
})
output$down111 <- downloadHandler(
  filename =  function() {
    paste("Elbowplot.pdf")
  },
  # content is a function with argument file. content writes the plot to the device
  content = function(file) {
    pdf(file) # open the pdf device
    print( H()$ElbowP)
    dev.off()  # turn the device off
  }
)


############Dina#####################
D=reactive({
  if(is.null(hedo())){return()}
  D=Clustering(t(hedo()),ClustMeth='Diana',k=input$clustsH,Ddismethod = input$MetD,Graph=F,VarCart=F,IndCart=F,ElbowP=F )

  return(D)
})
output$dd=renderPlot({
  withProgress(message = 'Making plot', value = 0.1, {
    # Number of times we'll go through the loop
    n <- 10

    for (i in 1:n) {
      # Each time through the loop, add another row of data. This is
      # a stand-in for a long-running computation.
      p=D()$dendro

      # Increment the progress bar, and update the detail text.
      incProgress(1/n, detail = paste("Doing part", i))

      # Pause for 0.1 seconds to simulate a long computation.
      Sys.sleep(0.001)
    }})
  p

})
output$down12 <- downloadHandler(
  filename =  function() {
    paste("dendroDiana.pdf")
  },
  # content is a function with argument file. content writes the plot to the device
  content = function(file) {
    pdf(file) # open the pdf device
    print(D()$dendro)
    dev.off()  # turn the device off
  }
)
output$vd=renderPlot({
  D()$Pvar
})
output$down13 <- downloadHandler(
  filename =  function() {
    paste("VarDiana.pdf")
  },
  # content is a function with argument file. content writes the plot to the device
  content = function(file) {
    pdf(file) # open the pdf device
    print(D()$Pvar)
    dev.off()  # turn the device off
  }
)
output$id=renderPlot({
  D()$Pind
})
output$down14 <- downloadHandler(
  filename =  function() {
    paste("IndDiana.pdf")
  },
  # content is a function with argument file. content writes the plot to the device
  content = function(file) {
    pdf(file) # open the pdf device
    print( D()$Pind)
    dev.off()  # turn the device off
  }
)




})


