#CEBRA
#Calculate costs and rank species

################################################
#Code executed once
library(shiny)
library(maps)
library(mapdata)
library(ggplot2)

library(plyr)
require(MASS)
library(scales)
library(gtable)
library(grid)
library(googlesheets)

theme_set(theme_classic())
theme_update(axis.text.y = element_text(face = "bold", size = 14))

table <- "Cebra"

saveDataG <- function(data) {
        # Grab the Google Sheet
        sheet <- gs_title(table)
        # Add the data as a new row
        gs_add_row(sheet, ws = 1, input = data, verbose = TRUE)
        #gs_add_row(sheet, input = data)
}

loadDataG <- function() {
        # Grab the Google Sheet
        sheet <- gs_title(table)
        # Read the data
        gs_read_csv(sheet)
}

#Example of use
#x<-loadDataG()
#print(x$Name)

humanTime <- function() {
        format(Sys.time(), "%Y%m%d-%H%M%OS")
}

#Load data containing predefined beta distribution and 
#a 5x25 matrix that controls how these are combined
load("JH.Rdata") 

#Change scores from 1 to 5 scale to "Very Good" to "Poor"
beta$SelScore[beta$SelScore==1]<-"Very Good"
beta$SelScore[beta$SelScore==2]<-"Good"
beta$SelScore[beta$SelScore==3]<-"Medium"
beta$SelScore[beta$SelScore==4]<-"Poor"
beta$SelScore[beta$SelScore==5]<-"Very Poor"

#Change column names for uncertainty dimension "VL" to "Very Low", etc
names(beta)<-c("Score","SelScore","Very Low","Low","Medium","High")

#Choose traffic light colours suitable for colour blind people
colours<-c("#008837","#a6dba0","#f7f7f7","#c2a5cf","#7b3294")

#Read in data 
input_app<-read.csv("data/Input_App.csv", stringsAsFactors = FALSE)

#Create columns for user inputs
input_app<-mutate(input_app, Environment_User=Environment)
input_app<-mutate(input_app, Amenity_User=Amenity)
input_app<-mutate(input_app, Monetary_Damage_User=Monetary_Damage)
input_app<-mutate(input_app, Time_User=Time)
input_app<-mutate(input_app, Community_User=Community)
input_app<-mutate(input_app, Likelihood_User=Likelihood)
input_app<-mutate(input_app, Man_Utility_User=Man_Utility)

#Colours
library(RColorBrewer)
set.seed(36)
names_obs<-array()
lat_obs<-array()
long_obs<-array()
col_obs<-array()

col_sp<-colorRampPalette(brewer.pal(7,"Dark2"))(length(input_app$Species))
col_sp_ranked<-colorRampPalette(brewer.pal(9,"YlOrRd"))(length(input_app$Species))


####################################################################

#Server function, code below is ran for every app user
shinyServer(function(session,input, output) {

        ####################################################################        
        #Display the picture of the species of interest     
        output$species_photo <- renderImage({
                                typeSp<-reactive(input$species)  
                                src_image<-input_app[input_app$Species==typeSp(),"Image_Name"]
                                srcI<-paste("www/", src_image,sep="")
                                return(list(src = srcI, filetype = "image/jpeg", 
                                            width = 300,
                                            height = 220,
                                            alt = "Pest"))
                                }, deleteFile = FALSE) #end select picture
        
        ####################################################################        
        #Link species image to the website with the biological assumptions    
        output$species_bio_assump <- renderUI({
                                typeSp<-reactive(input$species)  
                                url<-tags$a(" click here", href=input_app[input_app$Species==typeSp(),"WebLink"],target="_blank")
                                tagList("Biological assumptions: ", url)
                        }) #end select web link
        
      
        ####################################################################         
        #Map showing maximum range of invasive species 
        output$map <- renderImage({
                        typeSp<-reactive(input$species)
                        srcI<-paste("www/",typeSp(),"_map.png",sep="")
                        return(list(src = srcI, filetype = "image/png", 
                                    width = 600,
                                    height = 375,
                            alt = srcI))
                        }, deleteFile = FALSE) #end select picture
        
        ####################################################################         
        #Message that goes with the map
        observeEvent(input$show_map, {
                showModal(modalDialog(
                                title = "Illustration Only",
                                p("These areas are similuted for demonstration purposes only. In the future, 
                                models like CLIMEX will provided estimates of the unticipated 
                                maximum spread of invasive species in the absence of management, 
                                after a length of time.",  align= "center"),
                                easyClose = TRUE,
                                footer = NULL
                        ))
                })
        #################################################################### 
        #Message that goes with the impact scores
        observeEvent(input$show_impact, {
                showModal(modalDialog(
                        title = "Scales: score of 1 = top row (best), 5 = bottom row (worst)",
                        h2(img( src = "impact_scale_def.png",
                                height = 600, width = 500, 
                                inlign = TRUE)),
                        easyClose = TRUE,
                        footer = NULL
                        ))
        })
        
        #################################################################### 
        output$species_name<-renderText({
                #Select species 
                typeSp<-reactive(input$species)
                typeSp()
        })  
        #################################################################### 
        #Calculate the total value of agricultural production at risk
        output$agri_value<-renderText({
                typeSp<-reactive(input$species)
                input_app[input_app$Species==typeSp(),"Econ_Flowers"]+
                input_app[input_app$Species==typeSp(),"Econ_Fruit"]+
                input_app[input_app$Species==typeSp(),"Econ_Veg"]+
                input_app[input_app$Species==typeSp(),"Econ_Hay"]+
                input_app[input_app$Species==typeSp(),"Econ_Livestock"]+
                input_app[input_app$Species==typeSp(),"Econ_Crops"]
                
                }) 
        #################################################################### 
        #Set slider values to species defaults
        observe({
                typeSp<-input$species
                updateSliderInput(session,"broadacre_percent_damage", 
                                  value = input_app[input_app$Species==typeSp,"Impact_Crops"],
                                  min = 0, max = 1, step = 0.01)
                updateSliderInput(session,"hay_percent_damage", 
                                  value = input_app[input_app$Species==typeSp,"Impact_Hay"],
                                  min = 0, max = 1, step = 0.01)
                updateSliderInput(session,"fruit_percent_damage", 
                                  value = input_app[input_app$Species==typeSp,"Impact_Fruit"],
                                  min = 0, max = 1, step = 0.01)
                updateSliderInput(session,"flower_percent_damage", 
                                  value = input_app[input_app$Species==typeSp,"Impact_Flowers"],
                                  min = 0, max = 1, step = 0.01)
                updateSliderInput(session,"veg_percent_damage", 
                                  value = input_app[input_app$Species==typeSp,"Impact_Veg"],
                                  min = 0, max = 1, step = 0.01)
                updateSliderInput(session,"livestock_percent_damage", 
                                  value = input_app[input_app$Species==typeSp,"Impact_Livestock"],
                                  min = 0, max = 1, step = 0.01)
                updateSliderInput(session,"amenity", 
                                  value = input_app[input_app$Species==typeSp,"Amenity"],
                                  min = 0, max = 5, step = 1)
                updateSliderInput(session,"environment", 
                                  value = input_app[input_app$Species==typeSp,"Environment"],
                                  min = 0, max = 5, step = 1)
                updateSliderInput(session,"likelihood", 
                                  value = input_app[input_app$Species==typeSp,"Likelihood"],
                                  min = 0, max = 1, step = 0.1)
                updateSliderInput(session,"community", 
                                  value = input_app[input_app$Species==typeSp,"Community"],
                                  min = 0, max = 5, step = 1)
                updateSliderInput(session, "timescale", 
                                  min = 0, max = 100, step = 5,
                                  value = input_app[input_app$Species==typeSp,"Time"])
        })
        

        #################################################################### 
        #Calculate the total damage to agricultural production for each species
        total_agri_damage<-reactive(
                        
                input_app[input_app$Species==input$species,"Econ_Crops"]*input$broadacre_impact*input$broadacre_percent_damage+
                input_app[input_app$Species==input$species,"Econ_Hay"]*input$hay_impact*input$hay_percent_damage+
                input_app[input_app$Species==input$species,"Econ_Flowers"]*input$flower_impact*input$flower_percent_damage+
                input_app[input_app$Species==input$species,"Econ_Fruit"]*input$fruit_impact*input$fruit_percent_damage+
                input_app[input_app$Species==input$species,"Econ_Veg"]*input$veg_impact*input$veg_percent_damage+
                input_app[input_app$Species==input$species,"Econ_Livestock"]
                *input$livestock_impact*input$livestock_percent_damage
                )
        ####################################################################
        #Stores user choices for each species
        amenity_user_temp<-reactive(input$amenity)
        environment_user_temp<-reactive(input$environment)
        time_user_temp<-reactive(as.numeric(input$timescale))
        community_user_temp<-reactive(input$community)
        likelihood_user_temp<-reactive(input$likelihood)
        
        #################################################################### 
        #Monetised damage by species 
        output$agri_value_damage<-renderText({
                signif(total_agri_damage(), digits = 3)
        }) 

        ####################################################################        
        #Record responses for each time 'submit' is clicked 
        observe({
                if(input$submit==0) return(NULL)
                isolate({
                        showNotification(paste("Thank you! Your input for ",input$species, "has been recorded. 
                                         Please proceed to the next species."), duration = 5)
                        
                        data<-data.frame(t(c(input$name,humanTime(),input$species,
                                             input$flower_impact*input$flower_percent_damage,
                                             input$fruit_impact*input$fruit_percent_damage,
                                             input$veg_impact*input$veg_percent_damage,
                                             input$hay_impact*input$hay_percent_damage,
                                             input$broadacre_impact*input$broadacre_percent_damage,
                                             input$livestock_impact*input$livestock_percent_damage, 
                                             input$amenity,input$environment, input$timescale,
                                             input$score_1,
                                             input$unc_1,
                                             input$score_2,
                                             input$unc_2,
                                             input$likelihood,
                                             input$community
                                        )))
                        
                        colnames(data)<-c("Name","Time_Of_Response", "Species",	
                                        "Impact_Flowers",	
                                        "Impact_Fruit",	
                                        "Impact_Veg",	
                                        "Impact_Hay",	
                                        "Impact_Crops",	
                                        "Impact_Livestock",	
                                        "Amenity", "Environment", "Time", 
                                        "Mananagement_Impact", 
                                        "Management_Impact_Unc", 
                                        "Implementability",
                                        "Implementability_Unc", "Likelihood","Community")											
                        
                        saveDataG(data[1,])
                        
                        #Record user values for ranking
                        input_app[input_app$Species==input$species,"Monetary_Damage_User"]<<-total_agri_damage()
                        input_app[input_app$Species==input$species,"Amenity_User"]<<-amenity_user_temp()
                        input_app[input_app$Species==input$species,"Environment_User"]<<-environment_user_temp()
                        input_app[input_app$Species==input$species,"Time_User"]<<-time_user_temp()
                        input_app[input_app$Species==input$species,"Community_User"]<<-community_user_temp()
                        input_app[input_app$Species==input$species,"Likelihood_User"]<<-likelihood_user_temp()
                        input_app[input_app$Species==input$species,"Man_Utility_User"]<<-exp_utility_user_temp()
                        
                        
                        output$rank_default <- renderPlot({
                        
                                #Calculates weighted utility relative to default       
                                temp <- reactive({
                                                (input$aspect_environment*input$weight_env*input_app$Environment/
                                                         max(input_app$Environment)+
                                                input$aspect_economy*(input$weight_econ)*input_app$Monetary_Damage/
                                                        max(input_app$Monetary_Damage)+
                                                input$aspect_amenity*input$weight_amen*input_app$Amenity/
                                                        max(input_app$Amenity)+
                                                input$aspect_time*(1-input_app$Time/
                                                        max(input_app$Time))+
                                                input$aspect_community*input$weight_local*input_app$Community/
                                                        max(input_app$Community)+
                                                input$aspect_measures*(1-input_app$Man_Utility/5
                                                        ))*input_app$Likelihood
                                    
                                })
                                
                                
                                
                                max_Y <- reactive({
                                             max((input$aspect_environment*input$weight_env+
                                              input$aspect_economy*input$weight_econ+
                                              input$aspect_amenity*input$weight_amen+
                                              input$aspect_time+
                                              input$aspect_community*input$weight_local+
                                              input$aspect_measures),1)
                                    })
                                
                                input_app<-mutate(input_app, Measure=temp())
                               
                                input_app <- transform(input_app, Species = reorder(Species, Measure))
                                
                                p1<-ggplot(input_app, aes(Species, Measure)) + geom_bar(stat = "identity",fill = col_sp_ranked)+
                                                scale_y_continuous(limits = c(0, max_Y()+0.1))+
                                                ylab("Risk score")+ theme(axis.title.y=element_blank())+
                                                scale_fill_manual(values=col_sp_ranked, name="Species", 
                                                                  labels=input_app$Species)+
                                                coord_flip() 
                                
                                print(p1)
                        }) #end ranking default
                        
                        output$rank_user <- renderPlot({
                                
                        #Calculates weighted utility relative to default       
                                temp <- reactive({
                                        (input$aspect_environment*input$weight_env*input_app$Environment_User/
                                                 max(input_app$Environment)+
                                        input$aspect_economy*input$weight_econ*input_app$Monetary_Damage_User/
                                                max(input_app$Monetary_Damage)+
                                        input$aspect_amenity*input$weight_amen*input_app$Amenity_User/
                                                max(input_app$Amenity)+
                                        input$aspect_time*(1-input_app$Time_User/
                                                max(input_app$Time))+
                                        input$aspect_community*input$weight_local*input_app$Community_User/
                                                max(input_app$Community)+
                                        input$aspect_measures*(1-input_app$Man_Utility_User/5
                                                ))*input_app$Likelihood_User
                                })
                          
                          max_Y <- reactive({
                            max((input$aspect_environment*input$weight_env+
                                   input$aspect_economy*input$weight_econ+
                                   input$aspect_amenity*input$weight_amen+
                                   input$aspect_time+
                                   input$aspect_community*input$weight_local+
                                   input$aspect_measures),1)
                          })
                          
                          input_app<-mutate(input_app, Measure=temp())
                          
                          input_app <- transform(input_app, Species = reorder(Species, Measure))
                          
                          p2<-ggplot(input_app, aes(Species, Measure)) + geom_bar(stat = "identity", fill = col_sp_ranked,
                                                            position = position_stack(reverse = TRUE)) +
                                                  scale_y_continuous(limits = c(0, max_Y()+0.1))+
                                                  ylab("Risk score")+ theme(axis.title.y=element_blank())+
                                                  scale_fill_manual(values=col_sp_ranked, name="Species", 
                                                                    labels=input_app$Species)+
                                                  coord_flip() 
                                               
                        print(p2)
                        }) #end ranking user
                        
                }) #end isolate      
                
                
        })

                
#################################################################### 
# Allow user to download responses
output$downloadResponses <- downloadHandler(
                filename = function() { 
                        paste("Responses_", humanTime(), ".csv", sep="")
                        
                },
                content = function(file) {
                        write.csv(loadDataG(), file, row.names = TRUE)
                }
        )            
        
        

 ####################################################################   
 output$image1 <- renderPlot({
          score1<-reactive(input$score_1) 
          uncert1<-reactive(input$unc_1) 
          in1<-subset(beta, beta$SelScore==score1())[[uncert1()]]
          p<-data.frame(status=c("VG","G","M","P","VP"), dens=in1)
          p$status<-factor(p$status,levels=p$status)
          
          g<-ggplot(p, aes(status,dens))+geom_col(aes(fill=status))+
                  scale_fill_manual(values =colours, name = "Rating",
                                    labels = c("Very Good","Good","Medium",
                                               "Poor", "Very Poor"))+
                  theme(panel.background = element_rect(fill='grey87', colour = 'grey87'))+
                  theme(legend.background = element_rect(fill="gray87"))+
                  theme(axis.title.x=element_blank())
          
          print(g)
}) #end  
  
 output$image2 <- renderPlot({
         score2<-reactive(input$score_2) 
         uncert2<-reactive(input$unc_2) 
          
         in2<-subset(beta, beta$SelScore==score2())[[uncert2()]]
          
         p<-data.frame(status=c("VG","G","M","P","VP"), dens=in2)
         p$status<-factor(p$status,levels=p$status)
         g<-ggplot(p, aes(status,dens))+geom_col(aes(fill=status))+ 
                  scale_fill_manual(values =colours, name = "Rating",
                                    labels = c("Very Good","Good","Medium",
                                               "Poor", "Very Poor"))+
                  theme(panel.background = element_rect(fill='grey87', colour = 'grey87'))+
                  theme(legend.background = element_rect(fill="gray87"))+
                  theme(axis.title.x=element_blank())
          
          print(g)
 }) #end 
 
 
 #Calculate expected utility of eradication measures
 exp_utility_user_temp<-reactive({
                score1<-input$score_1 
                uncert1<-input$unc_1 
                score2<-input$score_2 
                uncert2<-input$unc_2 
                in1<-subset(beta, beta$SelScore==score1)[[uncert1]]
                in2<-subset(beta, beta$SelScore==score2)[[uncert2]]
                y<-rep(in2,5)
                x<-c(rep(in1[1],5),rep(in1[2],5),rep(in1[3],5),rep(in1[4],5),rep(in1[5],5))
                prob<-c(sum(x*y*AveM[1,]),sum(x*y*AveM[2,]),sum(x*y*AveM[3,]),sum(x*y*AveM[4,]),sum(x*y*AveM[5,]))
                p<-data.frame(status=c("VG","G","M","P","VP"), dens=prob)
                #expected utility with 5 corresponding to "VG"
                sum(c(5,4,3,2,1)*p$dens)
         })

 
 
 
 
 output$image3 <- renderPlot({
          score1<-reactive(input$score_1) 
          uncert1<-reactive(input$unc_1) 
          score2<-reactive(input$score_2) 
          uncert2<-reactive(input$unc_2) 
          in1<-subset(beta, beta$SelScore==score1())[[uncert1()]]
          in2<-subset(beta, beta$SelScore==score2())[[uncert2()]]
          y<-rep(in2,5)
          x<-c(rep(in1[1],5),rep(in1[2],5),rep(in1[3],5),rep(in1[4],5),rep(in1[5],5))
          prob<-c(sum(x*y*AveM[1,]),sum(x*y*AveM[2,]),sum(x*y*AveM[3,]),sum(x*y*AveM[4,]),sum(x*y*AveM[5,]))
          p<-data.frame(status=c("VG","G","M","P","VP"), dens=prob)

          p$status<-factor(p$status,levels=p$status)
          g<-ggplot(p, aes(status,dens))+geom_col(aes(fill=status))+ 
                  scale_fill_manual(values =colours, name = "Rating",
                                    labels = c("Very Good","Good","Medium",
                                               "Poor", "Very Poor"))+
                  theme(panel.background = element_rect(fill='grey87', colour = 'grey87'))+
                  theme(legend.background = element_rect(fill="gray87"))+
                  theme(axis.title.x=element_blank())
print(g)
})#end  


})#end server function
