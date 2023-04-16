#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(ggplot2)
library(rpart)
library(caret)

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme=shinytheme('flatly'),
  includeCSS('./style.css'),
    # Application title
   titlePanel(tags$h1(tags$p("Female cycle prediction"))),
    
    tabsetPanel(
      tabPanel('Home',
               tags$div(
                 wellPanel(
                 tags$h2('Women Mentrual Cycle Prediction App')
                 ),
               ),
               tags$div(
                 class='image',
                tags$img(class='imgC', src="cycle.jpg",alt='image') ,
               tags$i(tags$h4(tags$p('Everthing to put any woman in a state of tranquility to the coming of her next period')))
                 
               ),
               
               tags$footer(class='footer1',
                           tags$div(class='flex  space-around flex-row',
                                    tags$p('Copyright 2023', class='mb'),
                                    tags$p('Lyse Ndifo', class='mb')
                           ))
      ),
      
    tabPanel('View prediction',
             sidebarLayout(
               sidebarPanel(tags$h4('Predictive Value Form'),
                            wellPanel(
                            numericInput('id1','Length of Cycle',value = 28),
                            numericInput('id2','Length of Luteal Phase',value = 11),
                            numericInput('id3','First Day of High',value = 25),
                            numericInput('id4','Length of Menses',value = 4),
                            selectInput('choice','intended prediction',choices = c('Ovulation days','Mentrual cycle')),
                            submitButton(text = 'view',width = '200px',icon('th'))
                            )
                            ),
               mainPanel(wellPanel(tags$h3('Presentation of the result')),
                 
                   conditionalPanel(condition="input.choice=='Ovulation days'",
                                          plotOutput('prediction'),
                                          tableOutput("restable")
                         )  ,     
                 conditionalPanel(condition="input.choice=='Menstrual cycle'",
                     tags$p('bonjour')             
                 )
                
               )
             ),
             tags$footer(class='footer',
                         tags$div(class='flex  space-around flex-row',
                                  tags$p('Copyright 2023', class='mb'),
                                  tags$p('Lyse Ndifo', class='mb')
                         ))
             
          ), 
    
    
    tabPanel('Generality',
             sidebarLayout(
               sidebarPanel(
             wellPanel(
 selectInput('histogr','visualization of Histogram',choices=c('Length of Cycle','Estimated Day of Ovulation','Length of Luteal Phase','First Day of High','Length of Menses'))
               ),
             
              submitButton(text = 'view',width = '200px',icon('th'))
             ),
          
             mainPanel(
               wellPanel(
                 tags$h3('In the world,On average we observe')
               ),
                tags$div(class='div',
               conditionalPanel(condition="input.histogr=='Length of Cycle'",
                                plotOutput('hist1'),
                               tags$strong(tags$p('It is common ground that the majority of women have a Length of Cycle between 26 and 28 days')),
          
                                ),
               conditionalPanel(condition="input.histogr=='Estimated Day of Ovulation'",
                                plotOutput('hist2'),
                               tags$strong(tags$p('It is found that the majority of women have an ovulation date between the 11th and 14th day of the cycle' ))
               ),
               conditionalPanel(condition="input.histogr=='Length of Luteal Phase'",
                                plotOutput('hist3'),
                               tags$strong(tags$p('It is found that the majority of women have a luteal phase between the 10th and 15th day of the cycle'))
               ),
               conditionalPanel(condition="input.histogr=='First Day of High'",
                                plotOutput('hist4'),
                                tags$strong(tags$p('It is found that the majority of women have for First Day of High between the 10th and 12th'))
               ),
               conditionalPanel(condition="input.histogr=='Length of Menses'",
                                plotOutput('hist5'),
                               tags$strong(tags$p('It is found that the majority of women have a Length of Menses of between 4 and 5 days'))
               )

               )
            
               )
             ),

             tags$footer(class='footer',
                         tags$div(class='flex  space-around flex-row',
                                  tags$p('Copyright 2023', class='mb'),
                                  tags$p('Lyse Ndifo', class='mb')
                         ))
    ),
    tabPanel('Preprocessing',
       tabsetPanel(
           tabPanel('gestion des outliers',  
             sidebarLayout(
               sidebarPanel(
                 wellPanel(
                                selectInput('choice1','Data visualization',choices =c('Boxplot','Scatter plot')),
                               conditionalPanel(condition = "input.choice1=='Boxplot' ",
                                               selectInput('boxp','Boxplot(detection univariee)',choices=c('LengthofCycle','EstimatedDayofOvulation','LengthofLutealPhase','FirstDayofHigh','LengthofMenses'))
                             ),
                            conditionalPanel(condition = "input.choice1=='Scatter plot' ",
                                            selectInput('scatter1','First attribute(detection multivariee)',choices=c('LengthofCycle','EstimatedDayofOvulation','LengthofLutealPhase','FirstDayofHigh','LengthofMenses')),
                                            selectInput('scatter2','Second attribute(detection multivariee)',choices=c('LengthofCycle','EstimatedDayofOvulation','LengthofLutealPhase','FirstDayofHigh','LengthofMenses'))
                   ), 
                   submitButton(text = 'view',width = '200px',icon('th'))
                 # actionButton('run','RENDU')
                 )
               ),
               mainPanel(
                 wellPanel(h3('Detection des outliers')),
                 tags$div(
                            conditionalPanel(condition="input.boxp=='LengthofCycle'",
                                             plotOutput('box1'),
                                             tags$strong(tags$p('min:22,max:37,5,mediane:28,outliers sont compris entre:38 a 53')),
                                             
                            ),
                            conditionalPanel(condition="input.boxp=='EstimatedDayofOvulation'",
                                             plotOutput('box2'),
                                             tags$strong(tags$p('min:8,max:24,mediane:15,outliers:25 a 30' ))
                            ),
                            conditionalPanel(condition="input.boxp=='LengthofLutealPhase'",
                                             plotOutput('box3'),
                                             tags$strong(tags$p('min:8,max:18,mediane:14,outliers:0 a 7 et 19 a 40'))
                            ),
                            conditionalPanel(condition="input.boxp=='FirstDayofHigh'",
                                             plotOutput('box4'),
                                             tags$strong(tags$p('min:6,max:17,mediane:12,outliers:5 et 18 a 25'))
                            ),
                            conditionalPanel(condition="input.boxp=='LengthofMenses'",
                                             plotOutput('box5'),
                                             tags$strong(tags$p('min:4,max:7,mediane:5,outliers:2 a 3 et 8 a 15'))
                            ),
                            
                   
                   conditionalPanel(
                     condition = "input.choice1=='Scatter plot' ",
                     plotOutput('scatter')
                   )
                 )
               )
             ),
             tags$footer(class='footer',
                         tags$div(class='flex  space-around flex-row',
                                  tags$p('Copyright 2023', class='mb'),
                                  tags$p('Lyse Ndifo', class='mb')
                         ))
              ),
           tabPanel('Clean Data',
                        wellPanel(h3('Clean Data')),
                        tags$div( class='datascrol',
                        dataTableOutput('clean')
                        ),
                        downloadButton('save','save to csv'),

                    tags$footer(class='footer',
                                tags$div(class='flex  space-around flex-row',
                                         tags$p('Copyright 2023', class='mb'),
                                         tags$p('Lyse Ndifo', class='mb')
                                ))
           ),
           tabPanel('Others',
                    tags$div(class='others',
                             tags$div(class='oth1',
                                      wellPanel(h3('Summary of Data')),
                                      verbatimTextOutput('resume')),
                                      tags$div(class='oth2',)
                             ),
             tags$footer(class='footer',
                         tags$div(class='flex  space-around flex-row',
                                  tags$p('Copyright 2023', class='mb'),
                                  tags$p('Lyse Ndifo', class='mb')
                         ))   
           )
 
 
            
            ),#fin du sous tabsetpanel
           
             
             ),
   tabPanel('Clustering',
            tabsetPanel(
              tabPanel('Kmeans',
                       sidebarLayout(
                         
                         sidebarPanel(
                           numericInput('clust','Select the number of clusters',min=2,value=2,step=1),
                           selectInput('X','Select the X variable',choices=c('LengthofCycle','EstimatedDayofOvulation','LengthofLutealPhase','FirstDayofHigh','LengthofMenses' )),
                           selectInput('Y','Select the Y variable',choices=c('LengthofCycle','EstimatedDayofOvulation','LengthofLutealPhase','FirstDayofHigh','LengthofMenses' )),
                           submitButton(text = 'Apply',width = '200px',icon('th'))
                           
                         ),
                         mainPanel(
                           
                           tags$div(class='divv',
                                   # tags$div(class=' div1 ',verbatimTextOutput('print')),
                                    tags$div(class='divv3',h3('clusters visualization'),plotOutput('plot1')),
                                    tags$div(class='divv2',tableOutput('kmean'))
                                    
                           )
                           
                         )
                         
                       ),
                
                tags$footer(class='footer',
                            tags$div(class='flex  space-around flex-row',
                                     tags$p('Copyright 2023', class='mb'),
                                     tags$p('Lyse Ndifo', class='mb')
                            ))
              ),
              tabPanel('Hierachic/CAH',
                       sidebarLayout(
                         sidebarPanel( numericInput('den','Select the number of clusters',min=2,value=2,step=1 ),
                                       submitButton(text = 'View',width = '200px',icon('th'))
                                       
                         ),
                         mainPanel(
                           tags$div(class='div8',
                                    tags$div(class='div9',plotOutput('hier4')),
                                    tags$div(class='div10',tableOutput('hier5'))
                           ),
                           tags$div(class='div11',tableOutput('hier6'))
                         )
                       ),
                
                tags$footer(class='footer',
                            tags$div(class='flex  space-around flex-row',
                                     tags$p('Copyright 2023', class='mb'),
                                     tags$p('Lyse Ndifo', class='mb')
                            ))
              )
            )
            
            ),
   tabPanel('Precision',
            #matrice de correlation
            
            tags$footer(class='footer',
                        tags$div(class='flex  space-around flex-row',
                                 tags$p('Copyright 2023', class='mb'),
                                 tags$p('Lyse Ndifo', class='mb')
                        ))
     
   ),

    
    tabPanel('About',
             sidebarLayout(
               sidebarPanel(tags$strong('Why this project??'),
                            wellPanel(
                            tags$p('Menstruation usually arrives every month in a women life.But the often tend to forget it,either do not know how to count 
                                   them because it is irregular or not.It is therefore  a question here of helping women to better predict their future period
                                   and ovulation dates')
                            ),
                            tags$strong('Description des attributs'),
                            wellPanel(
                              tags$p(' LengthofCycle:represents the length of time the woman remains without seeing her menses') ,
                              tags$p('EstimatedDayofOvulation: represents the date on which an ovum is released in women and is waiting to be feconder'),
                              tags$p('LengthofLutealPhase :the luteal phase lasts from the day after ovulation until the day before your period starts'),
                              tags$p('FirstDayofHigh:represents the first day that the woman see her menses'),
                              tags$p('LengthofMenses:represents the number of days that blood flow occurs in women')
                              )
                            ),
               mainPanel(
                         wellPanel(
                         tags$p(tags$strong('About Me')),
                         tags$p('I am a third-year computer science student at the University of Yaounde I in Cameroon.'),
                         tags$p('Passionate about forecasts,i specialized in data science at the said university'),
                         tags$p('Contact:+237 654960541/+237 699753500'),
                         tags$a('write me here',href='mailto:lysendifo8@gmail.com')
                         )
                         )
             ),
             
             tags$footer(class='footer',
                         tags$div(class='flex  space-around flex-row',
                                  tags$p('Copyright 2023', class='mb'),
                                  tags$p('Lyse Ndifo', class='mb')
                         ))
             )
    
   
      
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ###----CHARGEMENT DU JEU DE DONNEES----###
  
  data = read.csv('./data/FedCycleData071012 (2).csv')
#data1=data[,c()]
  dat1=data[,c(-2,-3,-5,-7,-11,-12,-13,-14,-15,-18,-19,-20,-21,-22,-23,
               -24,-25,-26,-27,-28,-29,-30,-31,-32,-34,-35,-36,-37,
               -38,-39,-41,-42,-43,-44,-45,-46,-47,-48,-49,-50,-51,-52,-53,-54,-55,-56,-57,
               -58,-59,-60,-61,-62,-63,-64,-65,-66,-67,-68,-69,-70,
               -71,-72,-73,-74,-75,-76,-77,-78,-79,-80)]
  dat2=dat1[,c(-1,-2,-8,-9,-10)]
  
   output$dat=renderDataTable({dat2})
   
   ###----CLEAN DATA---###
   cleandata=na.omit(dat2)
   output$clean=renderDataTable({cleandata})
   
   ###--RESUME DES DONNEES--###
   output$resume=renderPrint({summary(cleandata)})
   
   ###---SAUVEGARDE AU FORMAT CSV DES DONNEES NETOYEES---###
   output$save=downloadHandler(filename = function(){
     paste('data_',Sys.Date(),'.csv',sep = '') #Sys.Date() met la date du jour
   },
   content=function(file){
     write.csv(cleandata,file)#enregistre sous forme de csv
   }
   )
   
   ###---AFFICHAGE DES HISTOGRAMMES---###
  
   output$hist1=renderPlot({hist(cleandata$LengthofCycle,main='Length of Cycle',xlab='')})
   output$hist2=renderPlot({hist(cleandata$EstimatedDayofOvulation,main='Day of Ovulation',xlab='')})
   output$hist3=renderPlot({hist(cleandata$LengthofLutealPhase,main='Length of Luteal Phase',xlab='')})
   output$hist4=renderPlot({hist(cleandata$FirstDayofHigh,main='First Day of High',xlab='')})
   output$hist5=renderPlot({hist(cleandata$LengthofMenses,main='Length of Menses',xlab='')})
   
   ###NUAGE DE POINTS ###
   output$scatter=renderPlot({plot(cleandata[,input$scatter1],cleandata[,input$scatter2],xlab=input$scatter1,ylab=input$scatter2)})
   
   ###--BOITE A MOUSTACHE---###
   output$box1=renderPlot({boxplot(cleandata$LengthofCycle,main='Length of Cycle',xlab='')})
   output$box2=renderPlot({boxplot(cleandata$EstimatedDayofOvulation,main='Day of Ovulation',xlab='')})
   output$box3=renderPlot({boxplot(cleandata$LengthofLutealPhase,main='Length of Luteal Phase',xlab='')})
   output$box4=renderPlot({boxplot(cleandata$FirstDayofHigh,main='First Day of High',xlab='')})
   output$box5=renderPlot({boxplot(cleandata$LengthofMenses,main='Length of Menses',xlab='')})
   
   ###---HIERACHIQUE/CAH---###
   #centrer et reduire les donnees
   cendata=scale(as.matrix(cleandata,center=TRUE,scale=TRUE))
   #calcul des distances(euclidean,maximum,manhattan,canberra,binary,minkowski) pour la matrice de distance entre les individu
   cendata=as.matrix(cleandata) 
   distdata1=dist(cendata,method='euclidean')
   
   output$hier2=renderPrint({distdata1})
   
   #classification ascendante hierarchique 'single','complete','average','ward','ward.D','ward.D2'
   cahdata=hclust(distdata1,method = 'ward.D2')
   output$hier3=renderPrint({cahdata})
   #cahdata$labelnom des regions
   #affichage du dendogramme avec materialisation des classes border=1 a 5 pour la couleur du cardre
   output$hier4=renderPlot({
     plot(cahdata)
     rect.hclust(cahdata,k=input$den,border=2) #mets les bordures
   })
   
   #decoupage en k groupes ou classes
   output$hier5=renderTable({
     kcahdata=cutree(cahdata,k=input$den)
     table(kcahdata)
   })
   
   # #ajout au fichier d'origine la nouvelle variable kcahdata
   
   output$hier6=renderTable({
     kcahdata=cutree(cahdata,k=input$den)
     ajout=cbind(cleandata,kcahdata)
     head(ajout)
     
   })
   
   
   ###--KMEANS--###
   data_norm2=scale(as.matrix(cleandata,center=TRUE,scale=TRUE)) #normalisation z-score
   data_norm2=as.matrix(cleandata) 
   distdata2=dist(data_norm2,method='euclidean') #matrice de distance
   
  # output$print=renderPrint({kmoy=kmeans(distdata,input$clust)
   # kmoy$cluster}) 
   
   output$kmean=renderTable({ kmoy=kmeans(distdata2,input$clust) #tableau de frquence
   attributes(kmoy)
   kmoy$cluster
   table(kmoy$cluster)})
   
   selectedData <- reactive({
     data_norm2[, c(input$X, input$Y)]
   })
   
   clusters <- reactive({
     kmeans(selectedData(), input$clust)
   })
   
   output$plot1 <- renderPlot({
     
     palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
               "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
     
     
     # par(mar = c(5.1, 4.1, 0, 1))
     plot(selectedData(),
          col = clusters()$cluster,
          pch = 20, cex = 3)
     points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
   })
   
   ###--PREDICTION--###
   
  # predict_Acc = predict(ad,tests[,-1],type=c("class"))
  # matc = table(predict_Acc,tests[,1])
  # confusionMatrix(matc)
   output$prediction=renderPlot({
     
      LengthofCycle = as.numeric(input$id1)
      LengthofLutealPhase = as.numeric(input$id2)
      FirstDayofHigh =as.numeric(input$id3) 
      LengthofMenses = as.numeric(input$id4)
      
     
     nt = sample(1:nrow(cleandata),0.7*nrow(cleandata))
     trains=cleandata[nt,]
     tests = cleandata[-nt,-2] 
    # ad = rpart(trains$EstimatedDayofOvulation~trains$LengthofCycle+trains$LengthofLutealPhase+trains$FirstDayofHigh+trains$LengthofMenses,data = trains)
     #ad=rpart('EstimatedDayofOvulation'~'LengthofCycle'+'LengthofLutealPhase'+'FirstDayofHigh'+'LengthofMenses',data=trains)
     # text(ad,use.n=TRUE)
     ad=rpart(EstimatedDayofOvulation~.,data=trains)
   #  prediction=predict(ad,data=tests)
      plot(ad)
   })
   
   output$restable = renderTable({
     
     LengthofCycle = as.numeric(input$id1)
     LengthofLutealPhase = as.numeric(input$id2)
     FirstDayofHigh =as.numeric(input$id3) 
     LengthofMenses = as.numeric(input$id4)
     
     new_data=data.frame(LengthofCycle,LengthofLutealPhase,FirstDayofHigh,LengthofMenses)
     
     nt = sample(1:nrow(cleandata),0.7*nrow(cleandata))
     trains=cleandata[nt,]
     tests = cleandata[-nt,-2] 
    
     ad=rpart(EstimatedDayofOvulation~.,data=trains)
       prediction= predict(ad,new_data)
       floor(round(prediction,00))
    
     })
   
 

   
   
}

# Run the application 
shinyApp(ui = ui, server = server)
