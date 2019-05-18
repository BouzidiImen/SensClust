

shinyUI(dashboardPage(


  dashboardHeader(title = 'Sensory Analysis'),
  ########## sider panel ####
  dashboardSidebar( width = 250,
                    sidebarMenu(
                      menuItem("Input Datasets", tabName = "Home", icon = icon("window-restore")),


                      menuItem("Clustering", tabName = "CLus", icon = icon("connectdevelop"),
                               startExpanded = T,
                               menuSubItem("Partitional Clustering", tabName = "PCLus", icon = icon("boxes")),
                               menuSubItem("Hierarchical Clustering", tabName = "HCLus", icon = icon("megaport"))
                      ),
                      menuItem("Clustering's Validation", tabName = "ClV", icon = icon("check-square"), startExpanded = F
                      )
                      ,
                      menuItem("External preference mapping", tabName = "EPM", icon = icon("slideshare"))
                      #,
                      #menuItem("About", tabName = "Home", icon = icon("info-circle"))


                    )),
  dashboardBody(


    shinyDashboardThemes( ### changing theme
      theme ='poor_mans_flatly'
    ),
    tabItems(
      ########## Home ######
      tabItem(
        tabName = "Home",
        fluidRow(
          column(
            width=12,
            box( title = " Hedonic data's inputs",status = "primary", solidHeader = T,
                 collapsible = T,
                 fileInput("file","Upload the file in csv", accept = ".csv", multiple = F),
                 tags$head(tags$style("#msg1{color: #8B0000;
                                      font-size: 17px;}
                                      #msg2{color: #8B0000;
                                      font-size: 17px;}
                                      #msg3{color: #087558;
                                      font-size: 17px;}
                                      #msg4{color: #0A926E;
                                      font-size: 17px;}
                                      #cite {
                                      color: #949793;
                                      position: absolute;
                                      bottom: 10px;
                                      right: 10px;
                                      font-size: 12px;
                                      }
                                      #msg5{color: #8B0000;
                                      font-size: 17px;}

                                      ")),
                 textOutput('msg1'),
                 helpText("Default max. file size is 5MB"),
                 checkboxInput(inputId = 'header', label = 'Header', value = T),
                 radioButtons(inputId = 'sep', label = 'Separator',
                              choices = c(Semicolon=';',Comma=',',Tab='\t', Space=''), selected = ';')),
            box( title = " Sensory data's inputs",status = "primary", solidHeader = T,
                 collapsible = T,
                 fileInput("fileS","Upload the file in csv",accept = ".csv", multiple = F),
                 # fileinput() function is used to get the file upload contorl option
                 textOutput('msg2'),
                 helpText("Default max. file size is 5MB"),
                 checkboxInput(inputId = 'headerS', label = 'Header', value = T),
                 radioButtons(inputId = 'sepS', label = 'Separator',
                              choices = c(Semicolon=';',Comma=',',Tab='\t', Space=''), selected = ';'))

                 ),
          br()
            ),
        fluidRow(

          column(
            width=12,
            box(title = "Basic statistics: Hedonic data ",status = "primary", solidHeader = T,
                collapsible = T,uiOutput('tb')),


            box(title = "Basic statistics: Sensory data",status = "primary", solidHeader = T,
                collapsible = T,uiOutput('tb2'))
          )
        ),
        tags$div(id="cite",
                 '\U00A9 Imen Bouzidi'
        )),
      ########## Partional clustering ########
      tabItem(
        tabName = "PCLus",
        fluidRow(
          column(
            width=12,
            box( status = "primary",width = NULL,h3('Partitional Clustering : ', align = "center")
                 ,h4('a division of the set of data objects into non-overlapping subsets
                     (clusters) such that each data object is in exactly one subset.',align='center')
                 ))
            ),
        fluidRow(


          box( title = "Choices of the method",status = "primary", solidHeader = T,
               collapsible = F,  width = 4  ,
               selectInput('MethodP', 'Choose the method of clustering:',
                           choices=c('K-means','CLARA','PAM','SOTA','SOM'),
                           selected='K-means'),
               conditionalPanel(
                 condition = "input.MethodP == 'CLARA'",
                 selectInput("MetC", "Metric:",c("euclidean", "manhattan", "jaccard"))),
               conditionalPanel(
                 condition = "input.MethodP == 'PAM'",
                 selectInput("MetPa", "Metric:",c("euclidean", "manhattan"))),
               conditionalPanel(
                 condition = "input.MethodP == 'SOTA'",
                 selectInput("MetSo", "Metric:",c("euclidean","correlation"))),

               numericInput('clusts', 'Cluster count:' , 2, min = 2, max = 9)
          ),
          box( status = "primary",  width = 8 ,
               tabsetPanel(
                 selected = "Optimal Number Of Clusters",
                 tabPanel("Optimal Number Of Clusters",br()
                          ,plotOutput('nbcluster'),
                          downloadButton(outputId = "downpc", label = "Download the plot")

                 ),
                 tabPanel("Cluster Plot",
                          conditionalPanel(
                            condition= "input.MethodP == 'K-means'",br(), plotOutput('gk'),
                            downloadButton(outputId = "down", label = "Download the plot")
                          ),
                          conditionalPanel(
                            condition= "input.MethodP == 'CLARA'",br(), plotOutput('gc'),
                            downloadButton(outputId = "down3", label = "Download the plot")
                          ) ,
                          conditionalPanel(
                            condition = "input.MethodP == 'PAM'", br(),plotOutput('gp'),
                            downloadButton(outputId = "down6", label = "Download the plot")
                          ),
                          conditionalPanel(
                            condition = "input.MethodP == 'SOTA'", br(),plotOutput('gS'),
                            downloadButton(outputId = "down15", label = "Download the plot")
                          ),
                          conditionalPanel(
                            condition = "input.MethodP == 'SOM'", br(),plotOutput('gSom'),
                            downloadButton(outputId = "downSO1", label = "Download the plot")
                          )
                 )#,
                 # tabPanel("Variables Distribution ",
                 #         conditionalPanel(
                 #          condition= "input.MethodP == 'K-means'",br(), plotOutput('vk'),
                 #        downloadButton(outputId = "down1", label = "Download the plot")
                 #       ),
                 #      conditionalPanel(
                 #       condition= "input.MethodP == 'CLARA'",br(), plotOutput('vc'),
                 #       downloadButton(outputId = "down4", label = "Download the plot")
                 #     ),
                 #        conditionalPanel(
                 #         condition = "input.MethodP == 'PAM'", br(),plotOutput('vp'),
                 #          downloadButton(outputId = "down7", label = "Download the plot")
                 #        ),conditionalPanel(
                 #         condition = "input.MethodP == 'SOTA'", br(),plotOutput('vS'),
                 #        downloadButton(outputId = "down16", label = "Download the plot")
                 #      ),
                 #     conditionalPanel(
                 #      condition = "input.MethodP == 'SOM'", br(),plotOutput('vSom'),
                 #     downloadButton(outputId = "downSO2", label = "Download the plot")
                 #  )

                 #),
                 #tabPanel("Individuals Plot",
                 #        conditionalPanel(
                 #         condition= "input.MethodP == 'K-means'",br(), plotOutput('ik'),
                 #        downloadButton(outputId = "down2", label = "Download the plot")
                 #     ),
                 #    conditionalPanel(
                 #     condition= "input.MethodP == 'CLARA'",br(), plotOutput('ic'),
                 #    downloadButton(outputId = "down5", label = "Download the plot")
                 # ),
                 #  conditionalPanel(
                 #   condition = "input.MethodP == 'PAM'", br(),plotOutput('ip'),
                 #  downloadButton(outputId = "down8", label = "Download the plot")
                 #),
                 #conditionalPanel(
                 # condition = "input.MethodP == 'SOTA'", br(),plotOutput('iS'),
                 #downloadButton(outputId = "down17", label = "Download the plot")
                 #    ),
                 #   conditionalPanel(
                 #    condition = "input.MethodP == 'SOM'", br(),plotOutput('iSom'),
                 #      downloadButton(outputId = "downSO3", label = "Download the plot")
                 #    )


                 #)#,
                 #tabPanel("Further Information")
               ))
        )
        ,
        tags$div(id="cite",
                 '\U00A9 Imen Bouzidi'
        )


        ),
      ########## Hier clust #####################
      tabItem(
        tabName = "HCLus",
        fluidRow(
          column(
            width=12,
            box( status = "primary",width = NULL,h3('Hierarchical Clustering: ', align = "center")
                 ,h4('is an algorithm that groups similar objects into groups called clusters.
                     The endpoint is a set of clusters, where each cluster is distinct from each other cluster,
                     and the objects within each cluster are broadly similar to each other.',align='center')
                 ))
                 ),
        fluidRow(




          box( title = "Choices of the method",status = "primary", solidHeader = T,
               collapsible = F,  width = 4  ,
               selectInput('M', 'Select a cluster analysis method: ',
                           choices=c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid"),
                           selected='ward.D'),
               selectInput('MethodH', 'Choose the method of clustering:',
                           choices=c('Hierarchical','Diana'),
                           selected='Hierarchical'),
               conditionalPanel(
                 condition = "input.MethodH == 'Hierarchical'",
                 selectInput("MetH", "Method for calculating dissimilarities",c("euclidean",
                                                                                "maximum", "manhattan", "canberra","binary" , "minkowski"))),
               conditionalPanel(
                 condition = "input.MethodH == 'Diana'",
                 selectInput("MetD", "Metric:",c("euclidean", "manhattan"))),
               conditionalPanel(
                 condition = "input.MethodH == 'Hierarchical'",
                 selectInput("MetricH", "Metric:",c("ward.D", "ward.D2","single", "complete", "average",
                                                    "mcquitty",  "centroid", "median"))),


               numericInput('clustsH', 'Cluster count:' , 2, min = 2, max = 9)
          ),
          box( status = "primary",  width = 8 ,
               tabsetPanel(
                 selected = "Optimal Number Of Clusters",
                 tabPanel("Optimal Number Of Clusters",plotOutput('nbclusterH'),downloadButton(outputId = "downhc", label = "Download the plot")),
                 tabPanel("Dendrogram",
                          conditionalPanel(
                            condition= "input.MethodH == 'Hierarchical'",br(), plotOutput('dh'),
                            downloadButton(outputId = "down9", label = "Download the plot")
                          ),
                          conditionalPanel(
                            condition= "input.MethodH == 'Diana'",br(), plotOutput('dd'),
                            downloadButton(outputId = "down12", label = "Download the plot")
                          )
                 )#,
                 #tabPanel("Variables Distribution ",
                 #        conditionalPanel(
                 #          condition= "input.MethodH == 'Hierarchical'",br(), plotOutput('vh'),
                 #           downloadButton(outputId = "down10", label = "Download the plot")
                 #          ),
                 #            conditionalPanel(
                 #             condition= "input.MethodH == 'Diana'",br(), plotOutput('vd'),
                 #            downloadButton(outputId = "down13", label = "Download the plot")
                 #         )

                 #),
                 # tabPanel("Individuals Plot",
                 #         conditionalPanel(
                 #          condition= "input.MethodH == 'Hierarchical'",br(), plotOutput('ih'),
                 #          downloadButton(outputId = "down11", label = "Download the plot")
                 #       ),
                 #       conditionalPanel(
                 #       condition= "input.MethodH == 'Diana'",br(), plotOutput('id'),
                 # #       downloadButton(outputId = "down14", label = "Download the plot")
                 #                         )
                 #

                 #               )
                 #tabPanel("Elbow method",
                 #        conditionalPanel(
                 #         condition= "input.MethodH == 'Hierarchical'",br(), plotOutput('ELH'),
                 #        downloadButton(outputId = "down111", label = "Download the plot")
                 #     ),
                 #    conditionalPanel(
                 #     condition= "input.MethodH == 'Diana'",br()
                 #  )


                 #)
               )
          )
        ),
        tags$div(id="cite",
                 '\U00A9 Imen Bouzidi'
        )



            ),

      ########## Cluster validation ##################
      tabItem(
        tabName = "ClV",
        fluidRow(
          box( title = "Validate the choice of the Clustering method",status = "primary", solidHeader = T,
               collapsible = F,width = 4 ,
               selectInput('Methodvalid', 'Choose the methods of clustering to evaluate :',multiple = T,
                           choices=c("hierarchical", "kmeans", "diana", "sota", "pam", "clara"),
                           selected='hierarchical'),
               selectInput("MethValid", "Clustering validation measures's type ",
                           c("internal","stability"),selected = 'internal'),

               numericInput("min1", "Input minimum number of cluster ", 2, min = 2, max = 9),
               numericInput("max1", "Input mmaximum number of cluster ", 10, min = 10, max = 20)
          )
          , box( status = "primary",  width = 8 ,
                 tabsetPanel(
                   selected = "Summary",
                   tabPanel("Summary",br(),verbatimTextOutput('sumval')),
                   tabPanel("Optimal Scores",br(),DT::dataTableOutput("optsc"), br(),downloadButton('x3', 'Download Data'))
                 ))


        ),tags$div(id="cite",
                   '\U00A9 Imen Bouzidi'
        )

        #,
        #fluidRow(
        # infoBoxOutput("progressBox",width = 6)
        #)
      ),


      ########## EPM #####################
      tabItem(
        tabName = "EPM",
        fluidRow( width=12,
                  box(title = "Clustering's parameters ",status = "primary", solidHeader = T,
                      collapsible = F,


                      selectInput("MethValidEPM", "Clustering validation measures's type ",
                                  c("internal","stability"),selected = 'internal'),
                      conditionalPanel(
                        condition = "input.MethValidEPM == 'internal'",
                        selectInput("Measurei", "Choose a measure:",c("Connectivity","Dunn","Silhouette"))),
                      conditionalPanel(
                        condition = "input.MethValidEPM == 'stability'",
                        selectInput("Measures", "Choose a measure:",c("APN","AD","ADM",'FOM'))),
                      textOutput('msg3'),textOutput('msg4')),
                  box(title = "Parameters for external preference mapping ",status = "primary", solidHeader = T,
                      collapsible = F,


                      #Type of regression's model
                      selectInput("mapping", "Type of regression's model for mapping",
                                  c('Quadratic','Vector' ,'Circular' , 'Eliptic'),selected = 'Quadratic'),
                      numericInput("ncl", "Input the cluster you want to visualize ", 1, min = 1, max ='output.nbcl')
                      #To fix
                      ,textOutput('msg5')



                  )),
        fluidRow(
          box( status = "primary",width=12,
               tabsetPanel(
                 selected = "Sensory variables distribution"
                 ,
                 tabPanel("Sensory variables distribution",br(),plotOutput("pca"),downloadButton(outputId = "downDIST", label = "Download the plot")),
                 tabPanel("Prediction Plot",br(),plotlyOutput("pred")),
                 tabPanel("Preference Plot",br(),plotOutput("pref"),downloadButton(outputId = "downPM", label = "Download the plot"))
                 ,
                 tabPanel("Preference Map",br(),plotlyOutput("map"))

               ))


        ),tags$div(id="cite",
                   '\U00A9 Imen Bouzidi'
        )
      )
      )



    )





  )


  )




