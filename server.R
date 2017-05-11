#
# Calculate cost and rank species

library(shiny)
library(maps)
library(mapdata)
library(ggplot2)



# Define server logic required to draw a histogram
shinyServer(function(input, output) {
     
        #Simulate data
        n<-20
        set.seed(36)
        
        #Translating number of observation into area
        ha_per_Obs<-7
        
        #Making up costs for two economic models
        marketCostM1<-10
        nonmarketCostM1<-30
        
        marketCostM2<-30
        nonmarketCostM2<-5
        
        ymax<-n*2*ha_per_Obs*(max(marketCostM1,marketCostM2)+max(nonmarketCostM1,nonmarketCostM2))
        
        data<-data.frame(latitude = c(rnorm(n,-31,2),rnorm(n*2,-30,3),rnorm(n*2,-29,2),rnorm(n,-30,1)),
                         longitude = c(rnorm(n,135,5),rnorm(n*2,140,5),rnorm(n*2,141,5),rnorm(n,127,5)))
        data$species<-c(rep("A",n),rep("B",n*2),rep("C",n*2),rep("D",n))
        data$colour<-c(rep("#1f77b4",n),rep("#ff7f0e",n*2),rep("#2ca02c",n*2),rep("#9467bd",n))
        data$damage<-c(rep(ymax*0.4,n),rep(ymax*0.3,n*2),rep(ymax*0.2,n*2),rep(ymax*0.1,n))
        
        
        #Map species 
        output$map <- renderPlot({
        
        #Select species 
        typeSp<-reactive(input$species)  
        dataMap<-subset(data, data$species== typeSp())
                
          map("worldHires","Australia", xlim=c(105,155), ylim=c(-45,-10), col="gray90", fill=TRUE)
          with(dataMap,points(longitude,latitude,pch=21,col=colour[1],bg=colour[1],cex=3))
    })
  
  model1pred <- reactive({
          costUnit<-0+marketCostM1*input$market+nonmarketCostM1*input$nonmarket
          
          typeSp<-reactive(input$species)
          dataMap<-subset(data, data$species== typeSp())
          cost<-costUnit*length(dataMap[,1])*ha_per_Obs+dataMap$damage[1]
         })
  
  model2pred <- reactive({
          costUnit<-0+marketCostM2*input$market+nonmarketCostM2*input$nonmarket
          
          typeSp<-reactive(input$species)
          dataMap<-subset(data, data$species== typeSp())
          cost<-costUnit*length(dataMap[,1])*ha_per_Obs+dataMap$damage[1]
          })
  
  output$area<-renderText({
          #Select species 
          typeSp<-reactive(input$species)
          
          dataMap<-subset(data, data$species== typeSp())
          length(dataMap[,1])*ha_per_Obs
          })
  
  output$costModel1 <- renderText({
          model1pred()
  })
  
  output$costModel2 <- renderText({
          model2pred()
  })
  
  
  output$rank1 <- renderPlot({
          
          df<-data.frame(species= c("A","B","C","D"), costM1 = c(1:4),costM2 = c(1:4))
          
          costUnit1 <- reactive({
                  costUnit<-0+marketCostM1*input$market+nonmarketCostM1*input$nonmarket
          })
          
          costUnit2 <- reactive({
                  costUnit<-0+marketCostM2*input$market+nonmarketCostM2*input$nonmarket
          })
          
          
          df$costM1[1]<-costUnit1()*sum(data$species== "A")*ha_per_Obs+data$damage[data$species== "A"][1]
          df$costM1[2]<-costUnit1()*sum(data$species== "B")*ha_per_Obs+data$damage[data$species== "B"][1]
          df$costM1[3]<-costUnit1()*sum(data$species== "C")*ha_per_Obs+data$damage[data$species== "C"][1]
          df$costM1[4]<-costUnit1()*sum(data$species== "D")*ha_per_Obs+data$damage[data$species== "D"][1]
          
          df$costM2[1]<-costUnit2()*sum(data$species== "A")*ha_per_Obs+data$damage[data$species== "A"][1]
          df$costM2[2]<-costUnit2()*sum(data$species== "B")*ha_per_Obs+data$damage[data$species== "B"][1]
          df$costM2[3]<-costUnit2()*sum(data$species== "C")*ha_per_Obs+data$damage[data$species== "C"][1]
          df$costM2[4]<-costUnit2()*sum(data$species== "D")*ha_per_Obs+data$damage[data$species== "D"][1]
          
          
         p<-ggplot(df, aes(species, costM1)) + geom_bar(stat = "identity", fill = c("#1f77b4","#ff7f0e","#2ca02c","#9467bd"))+
                  ggtitle("Economic cost") +
                  ylab("Millions")+ ylim(0,2*ymax)+
                  xlab("Species")+theme(legend.position="bottom")+
                  scale_fill_manual(values=c("#1f77b4","#ff7f0e","#2ca02c","#9467bd"), name="Species")
          
          print(p)
          
          
  })
  
  output$rank2 <- renderPlot({
          
          df<-data.frame(species= c("A","B","C","D"), costM1 = c(1:4),costM2 = c(1:4))
          
          costUnit1 <- reactive({
                  costUnit<-0+marketCostM1*input$market+nonmarketCostM1*input$nonmarket
          })
          
          costUnit2 <- reactive({
                  costUnit<-0+marketCostM2*input$market+nonmarketCostM2*input$nonmarket
          })
          
          
          df$costM1[1]<-costUnit1()*sum(data$species== "A")*ha_per_Obs+data$damage[data$species== "A"][1]
          df$costM1[2]<-costUnit1()*sum(data$species== "B")*ha_per_Obs+data$damage[data$species== "B"][1]
          df$costM1[3]<-costUnit1()*sum(data$species== "C")*ha_per_Obs+data$damage[data$species== "C"][1]
          df$costM1[4]<-costUnit1()*sum(data$species== "D")*ha_per_Obs+data$damage[data$species== "D"][1]
          
          df$costM2[1]<-costUnit2()*sum(data$species== "A")*ha_per_Obs+data$damage[data$species== "A"][1]
          df$costM2[2]<-costUnit2()*sum(data$species== "B")*ha_per_Obs+data$damage[data$species== "B"][1]
          df$costM2[3]<-costUnit2()*sum(data$species== "C")*ha_per_Obs+data$damage[data$species== "C"][1]
          df$costM2[4]<-costUnit2()*sum(data$species== "D")*ha_per_Obs+data$damage[data$species== "D"][1]
          
          
          p<-ggplot(df, aes(species, costM2)) + geom_bar(stat = "identity", fill = c("#1f77b4","#ff7f0e","#2ca02c","#9467bd"))+
                  ggtitle("Economic cost") +ylim(0,2*ymax)+
                  ylab("Millions")+ 
                  xlab("Species")+theme(legend.position="bottom")+
                  scale_fill_manual(values=c("#1f77b4","#ff7f0e","#2ca02c","#9467bd"), name="Species")
          
          print(p)
          
          
  })
  
})
