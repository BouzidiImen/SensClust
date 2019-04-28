

shinyServer(function(input, output) {
  ############ Hedonic Data #############
  hedo <- reactive({
    if(is.null(input$file)) return()
    else{
      if (tolower(tools::file_ext(input$file$datapath)) != "csv") return()
      else read.table(file=input$file$datapath,sep=input$sep, header = input$header,row.names = 1)}

  })
  output$msg1<-renderText({
    if(is.null(input$file)) return(NULL)
    else{
      if (tolower(tools::file_ext(input$file$datapath)) != "csv") return( "Please upload a csv file")
      else return(NULL) }
  })
  output$table <- DT::renderDataTable({
    if(is.null(hedo())) return ()
    DT::datatable(hedo(),options = list(scrollX = TRUE))
  })
  output$sum <-renderPrint({
    if(is.null(hedo())) return ()
    summary(hedo())
  })
  output$tb <- renderUI({
    if(is.null(hedo())) return ()
    else
      tabsetPanel(
        tabPanel("Dataset", br(),DT::dataTableOutput("table")),
        tabPanel("Summary",br(), verbatimTextOutput("sum"))
      )
  })

  ############ Sensory data ##############
  senso <- reactive({
    if(is.null(input$fileS)) return()

    else{
      if (tolower(tools::file_ext(input$fileS$datapath)) != "csv") return()
      else read.table(file=input$fileS$datapath,sep=input$sepS, header = input$headerS)}
  })
  output$msg2<-renderText({
    if(is.null(input$fileS)) return(NULL)
    else{
      if (tolower(tools::file_ext(input$fileS$datapath)) != "csv") return("Please upload a csv file")
      else return(NULL) }
  })

  output$table2 <- DT::renderDataTable({
    if(is.null(senso())) return ()
    DT::datatable(senso(),options = list(scrollX = TRUE))
  })

  output$sum2 <-renderPrint({
    if(is.null(senso())) return ()
    summary(senso())
  })
  output$tb2 <- renderUI({
    if(is.null(senso())) return ()
    else
      tabsetPanel(

        tabPanel("Dataset",br(), DT::dataTableOutput("table2")),

        tabPanel("Summary",br(),verbatimTextOutput("sum2"))) })


  ############ Pclust ##################
  ############ kmeans ############
  K=reactive({
    if(is.null(hedo())) return ()
    return(Clustering(t(hedo()),ClustMeth = 'Kmeans',k=input$clusts,Graph = F,VarCart = F,IndCart = F))


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

  ############ Clara ############
  C=reactive({
    if(is.null(hedo())) return()
    return(Clustering(t(hedo()),ClustMeth = 'Clara',Cdismethod = input$MetC,k=input$clusts,Graph = F,VarCart = F,IndCart = F))
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
  ############ PAM #############
  P=reactive({
    if(is.null(hedo()))return ()
    return(Clustering(t(hedo()),ClustMeth = 'Pam',Pdismethod =input$MetPa ,k=input$clusts,Graph = F,VarCart = F,IndCart = F))
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

  ############ Sota ##################
  S=reactive({
    if(is.null(hedo())){return ()}
    return(Clustering(t(hedo()),ClustMeth = 'Sota',Sotadismethod =input$MetSo ,k=input$clusts,Graph = F,VarCart = F,IndCart = F))
  })
  output$gS=renderPlot({
    plot(S()$sotaCl)
  })
  output$down15 <- downloadHandler(
    filename =  function() {
      paste("clustersSota.pdf")
    },
    content = function(file) {
      pdf(file)
      print(plot(S()$sotaCl))
      dev.off()
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







  ############ SOM ##################
  SOM=reactive({
    if(is.null(hedo())) return ()
    return(Clustering(t(hedo()),ClustMeth = 'Som',k=input$clusts,Graph = F,VarCart = F,IndCart = F))
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







  ############ Hclustering #################

  ############ Hierch ##########
  H=reactive({
    if(is.null(hedo())) return()
    return(Clustering(t(hedo()),ClustMeth='Hierarchical',k=input$clustsH,Hdismethod=input$MetH,Hmethod=input$MetricH,Graph=F,VarCart=F,IndCart=F,ElbowP=F ))
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
    H()$VarCart
  })
  output$down10 <- downloadHandler(
    filename =  function() {
      paste("VarHier.pdf")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      pdf(file) # open the pdf device
      print(H()$VarCart)
      dev.off()  # turn the device off
    }
  )

  output$ih=renderPlot({
    H()$IndCart
  })
  output$down11 <- downloadHandler(
    filename =  function() {
      paste("IndHier.pdf")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      pdf(file) # open the pdf device
      print(H()$IndCart)
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


  ############ Dina #####################
  D=reactive({
    if(is.null(hedo())) return()
    return(Clustering(t(hedo()),ClustMeth='Diana',k=input$clustsH,Ddismethod = input$MetD,Graph=F,VarCart=F,IndCart=F,ElbowP=F ))
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
    D()$VarCart
  })
  output$down13 <- downloadHandler(
    filename =  function() {
      paste("VarDiana.pdf")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      pdf(file) # open the pdf device
      print(D()$VarCart)
      dev.off()  # turn the device off
    }
  )
  output$id=renderPlot({
    D()$IndCart
  })
  output$down14 <- downloadHandler(
    filename =  function() {
      paste("IndDiana.pdf")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      pdf(file) # open the pdf device
      print( D()$IndCart)
      dev.off()  # turn the device off
    }
  )





  ############ CLvalid ############
  validation <- reactive({
    if(is.null(hedo())) return ()
    return(clValid( t(hedo()),input$min:input$max,clMethods =input$MethodCLV ,validation = input$MetCLV))

  })
  output$sumval <-renderPrint({
    if(is.null(hedo())) return ()
    summary(validation())
  })
  output$optsc <- DT::renderDataTable({
    if(is.null(hedo())) return ()
    DT::datatable(optimalScores(validation()),options = list(scrollX = TRUE))
  })
  output$x3 = downloadHandler('OptimalScores.csv' , content = function(file) {
    write.table(optimalScores(validation()), file,sep=";",row.names = F)
  })

  #output$progressBox <- renderInfoBox({
   # if(is.null(validation())) infoBox('',paste0('Waiting for inputing the dataset'),icon = icon("window-close"),
    #                                  color = "black",fill = T)
    #else infoBox(paste0('Best Method is ',as.character(optimalScores(validation())[1,2])),
     #            paste0('number of optimal clusters is ',as.character(optimalScores(validation())[1,3])),
      #           icon = icon("check-square"),
       #          color = "black",fill = T)
  #})






})


