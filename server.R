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
# Load the pre-sampled random variables (sampling them on the fly slows the app down substantially)
load("data/sims.df.RData")

#Load data containing predefined beta distribution and 
#a 5x25 matrix that controls how these are combined
load("JH.Rdata") 

#Change scores from 1 to 5 scale to "Very Good" to "Poor"
beta$SelScore[beta$SelScore==1]<-"Very Good"
beta$SelScore[beta$SelScore==2]<-"Good"
beta$SelScore[beta$SelScore==3]<-"Medium"
beta$SelScore[beta$SelScore==4]<-"Poor"
beta$SelScore[beta$SelScore==5]<-"Very Poor"

#Choose traffic light colours suitable for colour blind people
colours<-c("#008837","#a6dba0","#f7f7f7","#c2a5cf","#7b3294")

#Read in data with pest names
pests_ref<-read.csv("data/pests.csv", colClasses=c(rep("character",3),rep("numeric",3)))

#Simulate spatial data
#Simulate data
library(RColorBrewer)
set.seed(36)
names_obs<-array()
lat_obs<-array()
long_obs<-array()
col_obs<-array()

col_sp<-colorRampPalette(brewer.pal(7,"Dark2"))(length(pests_ref$Name))
col_sp_ranked<-colorRampPalette(brewer.pal(9,"YlOrRd"))(length(pests_ref$Name))

nobs<-round(runif(20,1,100))
for (i in 1:length(pests_ref$Name))
{
        names_obs<-c(names_obs,rep(pests_ref$Name[i],nobs[i]))
        #Simulate latitude
        mu_lat<-rnorm(1,-26,2.5)
        sd_lat<-rnorm(1,1.5,0.5)
        lat_obs<-c(lat_obs,rnorm(nobs[i],mu_lat,sd_lat))
        #Simulate longitude
        mu_long<-rnorm(1,136,6)
        sd_long<-rnorm(1,3,0.5)
        long_obs<-c(long_obs,rnorm(nobs[i],mu_long,sd_long))
        
        col_obs<-c(col_obs, rep(col_sp[i],nobs[i]))
        
}
names_obs<-names_obs[-1]
long_obs<-long_obs[-1]
lat_obs<-lat_obs[-1]
col_obs<-col_obs[-1]

data<-data.frame(latitude = lat_obs,longitude = long_obs)
data$species<-names_obs
data$colour<-col_obs
data$damage<-c(rep(100, length(data$latitude)))

#Translating number of observation into area (mil ha)
#Based on Australia being roughly 700 mil ha 
#And the maximum number of observations being set at a 100
ha_per_Obs<-7

#Agricultural Production in millions of AUD
Broadacre<-16134
Hay<-1403
Flower<-1252
Fruit<-3512
Veg<-3350
Livestock<-8127

##!!!!TEMP
#Making up costs for two economic models
marketCostM1<-10
nonmarketCostM1<-20
marketCostM2<-15
nonmarketCostM2<-5
envCostM1<-7
envCostM2<-9

n<-20
ymax<-n*2*ha_per_Obs*(max(marketCostM1,marketCostM2)+max(nonmarketCostM1,nonmarketCostM2))
df<-data.frame(species= pests_ref$Name, costM1 = c(1:length(pests_ref$Name)),
               costM2 = c(1:length(pests_ref$Name)))




####################################################################

#Server function, code below is ran for every app user
shinyServer(function(input, output) {

        ####################################################################        
        #Display the picture of the species of interest     
        output$species_photo <- renderImage({
                                typeSp<-reactive(input$species)  
                                src_image<-subset(pests_ref, pests_ref$Name==typeSp())[[3]]
                                srcI<-paste("www/", src_image,sep="")
                                return(list(src = srcI, filetype = "image/jpeg",alt = "Pest"))
                                 }, deleteFile = FALSE) #end select picture
        
        

        ####################################################################         
        #Map showing maximum range of invasive species 
        output$map <- renderPlot({
                                #Select species 
                                typeSp<-reactive(input$species)  
                                dataMap<-subset(data, data$species== typeSp())
                                map("worldHires","Australia", xlim=c(105,155), ylim=c(-45,-10), col="gray90", fill=TRUE)
                                x<-col2rgb(dataMap$colour[1], alpha = FALSE)
                                with(dataMap,points(longitude,latitude,pch=22,
                                    col=rgb(x[1,],x[2,],x[3,], 100, maxColorValue = 255),
                                    bg=rgb(x[1,],x[2,],x[3,],100, maxColorValue = 255),cex=4))
                                }) #end map
        ####################################################################         
        #Message that goes with the map
        observeEvent(input$show, {
                showModal(modalDialog(
                                title = "Illustration Only",
                                p("Observation points indicate sectors invaded, so that the area affected can be calculated based on these simulated values.
                                These values were similuted for demonstration purposes only. In the future, models like CLIMEX will provided estimates of the
                                unticipated maximum spread of invasive species in the absence of management, after a length of time.",  align= "center"),
                                easyClose = TRUE,
                                footer = NULL
                        ))
                })
        #################################################################### 
        #Calculate the area invaded
        output$area<-renderText({
                                #Select species 
                                typeSp<-reactive(input$species)
                                dataMap<-subset(data, data$species== typeSp())
                                length(dataMap[,1])*ha_per_Obs
                                })  
        
        #################################################################### 
        #Calculate the total value of agricultural production at risk
        output$agri_value<-renderText({signif(Broadacre+
                                Hay+Flower+Fruit+Veg+Livestock, digits = 3)
                                }) 
        #################################################################### 
        #Calculate the total damage to agricultural production 
        total_agri_damage<-reactive(
                        Broadacre*input$broadacre_impact*input$broadacre_percent_damage+
                        Hay*input$hay_impact*input$hay_percent_damage+
                        Flower*input$flower_impact*input$flower_percent_damage+
                        Fruit*input$fruit_impact*input$fruit_percent_damage+
                        Veg*input$veg_impact*input$veg_percent_damage+
                        Livestock*input$livestock_impact*input$livestock_percent_damage)
        
        #################################################################### 
        #Calculate the total value of agricultural production at risk
        output$agri_value_damage<-renderText({
                                signif(total_agri_damage(), digits = 3)
                                }) 

        ####################################################################        
        #Record responses for each time 'submit' is clicked 
        observe({
                if(input$submit==0) return(NULL)
                isolate({
                        data<-data.frame(t(c(input$name,input$species, total_agri_damage(), 
                                             input$amenity,input$environment, input$timescale)))
                        colnames(data)<-c("Name","Species", "Agri_Damage", "Amenity", "Environment", "Timescale")
                        saveDataG(data[1,])
                        
                })
        })
        
#################################################################### 
 ##!!!!TEMP 
 output$rank1 <- renderPlot({
          
          costUnit1 <- reactive({
                  costUnit<-input$weight_env*envCostM1*input$native_species+
                          input$weight_econ*marketCostM1*input$market+
                          input$weight_amen*nonmarketCostM1*input$nonmarket
          })
          
          costUnit2 <- reactive({
                  costUnit<-input$weight_env*envCostM2*input$native_species+
                          input$weight_econ*marketCostM2*input$market+
                          input$weight_amen*nonmarketCostM2*input$nonmarket
          })
          
          for (i in 1:length(pests_ref$Name))
          {
                  df$costM1[i]<-costUnit1()*nobs[i]*ha_per_Obs 
                  df$costM2[i]<-costUnit2()*max(1,rnorm(1,nobs[i],5))*ha_per_Obs     
          }
         
                 
          df <- transform(df, species = reorder(species, costM1))
          
          p1<-ggplot(df, aes(species, costM1)) + geom_bar(stat = "identity", fill = col_sp_ranked)+
                  ggtitle("Prioritisation of invasive species") +
                  scale_y_continuous(limits = c(0, max(df$costM1,df$costM2)))+
                  ylab("Utility")+ theme(axis.title.x=element_blank())+
                  theme(axis.text.x = element_text(angle = 90, hjust = 0.5))+
                  scale_fill_manual(values=col_sp_ranked, name="Species", labels=pests_ref$Name)
          
          print(p1)
}) #end ranking model 1
  
 output$rank2 <- renderPlot({
          

         
         costUnit1 <- reactive({
                 costUnit<-input$weight_env*envCostM1*input$native_species+
                         input$weight_econ*marketCostM1*input$market+
                         input$weight_amen*nonmarketCostM1*input$nonmarket
         })
         
         costUnit2 <- reactive({
                 costUnit<-input$weight_env*envCostM2*input$native_species+
                         input$weight_econ*marketCostM2*input$market+
                         input$weight_amen*nonmarketCostM2*input$nonmarket
         })
         
         for (i in 1:length(pests_ref$Name))
         {
                 df$costM1[i]<-costUnit1()*nobs[i]*ha_per_Obs 
                 df$costM2[i]<-costUnit2()*max(1,rnorm(1,nobs[i],5))*ha_per_Obs     
         }
         
          df <- transform(df, species = reorder(species, costM1))
          p2<-ggplot(df, aes(species, costM2)) + geom_bar(stat = "identity", fill = col_sp_ranked)+
                  ggtitle("Prioritisation of invasive species") +
                  scale_y_continuous(limits = c(0, max(df$costM1,df$costM2)))+
                  ylab("Utility")+ theme(axis.title.x=element_blank())+
                  theme(axis.text.x = element_text(angle = 90, hjust = 0.5))+
                  scale_fill_manual(values=col_sp_ranked, name="Species", labels=pests_ref$Name)
          
          print(p2)
 }) #end ranking model 2          
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
}) #end image 1 
  
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
 }) #end image 2 
  
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
})#end image 2 

####################################################################
##Aaron's code the only change is currently to z1

dataInput <- reactive({
         
         # Eradication Feasibility Model Variables
         #z1 <- input$z1        # Nett Infested Area (ha)
         typeSp<-input$species
         dataMap<-subset(data, data$species== typeSp)
         z1<-length(dataMap[,1])*ha_per_Obs
         
         z2 <- 3               # CLIMATE match (0-5)
         z3 <- input$z3        # Detectability Period (months)
         z4 <- input$z4        # Detection Distance (metres)
         z5 <- input$z5*1000   # Distance to manager (metres)
         z6 <- input$z6        # Propagule Longevity (years)
         z7 <- input$z7        # Monitoring Rate  (count (pa))
         z8 <- 1               # Pre-productive period (years)
         z9 <- 1               # Eradicated Elsewhere (Y/N)
         z10 <- 18             # Species Random Effect
         
         sims <- 100000        # number of simulations
         
         m_dp <- 8.42          # mean detectability period
         m_pl <- 8.12          # mean propagule longevity
         
         # Species random effects
         re.df <- data.frame(
                 re1 = c(2.309, 1.108),      # Acacia karroo
                 re2 = c(-0.6908, 1.631),    # Alhagi maurorum
                 re3 = c(-0.5519, 1.776),    # Carduus nutans
                 re4 = c(3.217, 2.159),      # Centaurea nigra
                 re5 = c(0.9708, 2.028),     # Eichhornia crassipes
                 re6 = c(-1.379, 1.81),      # Equisetum spp.
                 re7 = c(1.229, 1.983),      # Fallopia japonica
                 re8 = c(-0.578, 2.376),     # Fallopia sachalinensis
                 re9 = c(-1.281, 2.176),     # Hieracium aurantiacum ssp. carpathicola
                 re10 = c(-0.5984, 2.444),   # Hieracium praealtum subsp. bauhinii
                 re11 = c(-0.4942, 2.424),   # Hieracium spp.
                 re12 = c(-0.819, 2.334),    # Iva axillaris ssp. robustior
                 re13 = c(-1.061, 1.936),    # Malvella leprosa
                 re14 = c(-0.7713, 2.353),   # Nassella charruana
                 re15 = c(-0.7565, 2.301),   # Nassella tenuissima
                 re16 = c(0.5824, 2.092),    # Prosopis spp.
                 re17 = c(0.2995, 2.159),    # Salvinia molesta
                 re18 = c(0.000, 2.000))     # Random Species
         
         # Costing Model Variables
         # Economic parameters
         #v_u <- input$z11         # Search effort (h ha-1) # * calculated below by reformulating the coverage
         v_L <- input$z12          # Labour costs ($ h-1)
         v_A <- z1                 # Area initially searched (ha)
         v_Z <- input$z13          # Control costs ($ ha-1)
         v_Ca <- input$z14         # Annual administration costs ($)
         v_Cr <- input$z15         # Annual research and communication costs ($)
         v_d <- input$z16          # Discount rate
         
         # Search parameters
         v_Pka <- input$z17        # Proportion of detected plants killed in the active area
         v_Pkm <- input$z18        # Proportion of detected plants killed in the monitored area
         v_S <- input$z19          # Search speed (m h-1)
         v_Wa <- z4                # Detectability in active stage (m)                      
         v_Wm <- v_Wa/input$z20    # Detectability in the monitoring stage (m)
         
         # Demographic parameters
         v_J <- z6 + input$z21     # Minimum mumber of years monitoring required before local extinction (years)
         #v_HL <-                  # Seed half-life (years) # * calculated below by reformulating the prop long
         #v_Da <- 250              # Average weed density in the active area (ha-1)
         #v_Dm <- 25               # Average weed density in the monitored area (ha-1)
         v_DH <- input$z22         # Decision horizon (years)
         
         # Add the species random effect
         sims.df <- mutate(sims.df, re = rnorm(n=sims, mean=re.df[1,z10], sd=re.df[2,z10]))    
         
         # Calculate the eradication rate (equation 6 in Dodd et al 2015)
         sims.df <- mutate(sims.df, mpred = exp(a + b1*log(z1) + b2*z2 + b3*(z3-m_dp) + b4*log(z4) + b5*log(z5/1000) +
                                                        b6*(z6-m_pl) + b7*z7 + b8*z8 + b9*z9 + re))
         sims.df <- mutate(sims.df, lambda = gamma(1+1/v)*mpred)
         
         # Calculate the static parameters used in Hester et al 2013
         # Germination rate
         v_HL <- v_J*log(2)                  # * new function to calculate halflife from proplong.
         v_G <- (-log(0.5))/v_HL             # eqn. 5
         
         # Coverage
         #v_ca <- (v_S * v_u * v_Wa)/10000   # eqn 9 | 10 (active)
         #v_cm <- (v_S * v_u * v_Wm)/10000   # eqn 9 | 10 (monitoring)
         v_cm <- ((1/input$z20)*v_Wm)        # * adjusted to reflect the likley (rather than theoretical) search effort
         v_ca <- v_cm*input$z20              # * adjusted to follow the above change
         
         # Proportion found
         v_Pfa <- 1-(exp(-v_ca)^z7)          # eqn. 8 (active) *adjusted by raising to the power of n
         v_Pfm <- 1-(exp(-v_cm)^z7)          # eqn. 8 (monitoring) *adjusted by raising to the power of n
         
         # calculate the amount of time spent at each site given coverage and width *req due to change above
         #v_u <- ((10000 * v_ca)/(v_S*v_Wa))*z7            # uses the estimated coverage (based on search theory) 
         v_u <- ((10000 * 2)/(v_S*v_Wa))*z7                # uses the assumed coverage (double the adult search distance)
         
         # Create a monitoring stage matrix and summary vectors
         mat_Mt <- matrix(nrow=v_J, ncol=500)
         vec_At <- vector(mode="numeric", length=500)
         vec_Mt <- vector(mode="numeric", length=500)
         vec_Rt <- vector(mode="numeric", length=500)
         vec_Ht <- vector(mode="numeric", length=500)
         
         # Temporal parameters used in Hester et al 2013
         for(i in 1:500){
                 if(i==1){
                         mat_Mt[1,i] <- (v_Pfa*v_Pka)*v_A                                          # eqn. 2
                         mat_Mt[2:v_J,i] <- (1-v_G + v_Pfm*v_Pkm*v_G)*0                            # eqn. 3*
                         vec_Mt[i] <- sum(mat_Mt[,i], na.rm=TRUE)                                  # eqn. 1
                         
                         vec_Rt[i] <- (1-(v_Pfm*v_Pkm))*(v_G * vec_Mt[i])                          # eqn. 4
                         
                         vec_At[i] <- v_A-mat_Mt[1,i]+0                                            # eqn. 6*
                         
                         vec_Ht[i] <- vec_At[i] + vec_Mt[i]                                        # eqn. 7 
                         
                 } else {
                         mat_Mt[1,i] <- (v_Pfa*v_Pka)*vec_At[(i-1)]                                # eqn. 2
                         mat_Mt[2:v_J,i] <- (1-v_G + v_Pfm*v_Pkm*v_G)*mat_Mt[1:(v_J-1),(i-1)]      # eqn. 3*
                         vec_Mt[i] <- sum(mat_Mt[,i], na.rm=TRUE)                                  # eqn. 1
                         
                         vec_Rt[i] <- (1-(v_Pfm*v_Pkm))*(v_G * vec_Mt[i])                          # eqn. 4
                         
                         vec_At[i] <- vec_At[(i-1)]-mat_Mt[1,i]+vec_Rt[(i-1)]                      # eqn. 6*
                         
                         vec_Ht[i] <- vec_At[i] + vec_Mt[i]                                        # eqn. 7 
                         
                 }
         }
         
         # calculate the time to eradication (sensu Hester 2013)
         v_T <- min(which(vec_Ht<1))
         
         output$time <- renderText({comma(v_T)})
         
         # add 'year zero' to the vectors to account for treatment costs
         vec_At <- c(v_A, vec_At)
         vec_Mt <- c(0, vec_Mt)
         vec_Rt <- c(0, vec_Rt)
         vec_Ht <- c(v_A, vec_Ht)
         
         # trim vectors
         vec_At <- c(vec_At[1:v_T], rep(0, times=(ifelse(v_DH>v_T, v_DH-v_T, 0))))
         vec_Mt <- c(vec_Mt[1:v_T], rep(0, times=(ifelse(v_DH>v_T, v_DH-v_T, 0))))
         vec_Rt <- c(vec_Rt[1:v_T], rep(0, times=(ifelse(v_DH>v_T, v_DH-v_T, 0))))
         vec_Ht <- c(vec_Ht[1:v_T], rep(0, times=(ifelse(v_DH>v_T, v_DH-v_T, 0))))
         
         # calculate the probabilities of eradication over the time estimated by Hester (uses eqns 2 and 5 from Dodd et al 2015)
         CDF <- matrix(nrow=ifelse(v_DH>=v_T, v_DH, v_T), ncol=2)
         for(i in 0:ifelse(v_DH>=v_T, v_DH, v_T)){
                 CDF[i,1] = 1-exp(-median(sims.df$lambda)*(i^median(sims.df$v)))
                 CDF[i,2] = i
         }
         
         # calculate costs (notation per Hester)
         #v_Cs <- sum((v_u*v_L*vec_Ht) * (1+v_d)^-(0:(v_T-1)))                   # eqn. 12                      
         vec_Cs <- (v_u*v_L*v_A) * (1+v_d)^-(0:(v_T-1))                          # eqn. 12* amended to match the workbook
         vec_Cs <- c(vec_Cs, rep(0, times=(ifelse(v_DH>v_T, v_DH-v_T, 0))))
         v_Cs <- sum(vec_Cs)
         
         #v_Cc <- sum(((v_Z*v_Pfa*vec_At)+(v_Z*v_Pfm*vec_Mt))*(1+v_d)^-(0:(v_T-1))) # eqn. 13
         vec_Cc <- ((v_Z*v_Pfa*vec_At)+(v_Z*v_Pfm*v_G*vec_Mt))*(1+v_d)^-(0:(length(vec_At)-1)) # eqn. 13* amended to include germination rate
         v_Cc <- sum(vec_Cc)
         
         vec_CA <- v_Ca * (1+v_d)^-(0:(v_T-1))   # part eqn. 11
         vec_CA <- c(vec_CA, rep(0, times=(ifelse(v_DH>v_T, v_DH-v_T, 0))))
         v_CA <- sum(vec_CA)                                
         
         vec_CR <- v_Cr * (1+v_d)^-(0:(v_T-1))   # part eqn. 11
         vec_CR <- c(vec_CR, rep(0, times=(ifelse(v_DH>v_T, v_DH-v_T, 0))))
         v_CR <- sum(vec_CR)
         
         vec_TC <- vec_Cs + vec_Cc + vec_CA + vec_CR
         v_TC <- v_Cs + v_Cc + v_CA + v_CR                                       # eqn. 11
         
         # build a table of outputs
         Activity <- c("Surveillance", "Treatment", "Admin", "Research/Comms")
         Cost <- c(v_Cs, v_Cc, v_CA, v_CR)
         Percentage <- Cost/sum(Cost)
         
         # render the cost outputs for the ui
         output$cost_tab <- renderTable(
                 cbind(Activity, Cost=signif(Cost, digits = 3), Percentage=percent(Percentage)))
         
         output$cost <- renderText({signif(v_TC/10^6, digits = 3)})
         
         # construct a results table
         res_df <- data.frame("Prob"=CDF[,1], "Infested"=vec_Ht, "Active"=vec_At, "Cost"=vec_TC, "time"=c(0:(length(vec_TC)-1)))
         
         # Calculate summary statistics and render them to the ui
         Cost_Erad <- sum(res_df$Cost[1:v_DH])     # Cost of Eradication | time
         output$costtime <- renderText({signif(Cost_Erad, digits = 3)})
         
         Pr_Erad <- res_df$Prob[v_DH]              # Probability of Eradication | Time
         output$prob <- renderText({signif(Pr_Erad, digits = 3)})
         
         Cost_Eff <- (Pr_Erad*1000000) / Cost_Erad # Cost-Effectiveness
         output$costeff <- renderText({signif(Cost_Eff, digits =3)})
         
         output$horizon <- renderText({signif(v_DH,digits=3)})      # Time horizon
         
         output$PFA <- renderText({v_Pfa})         # PFA (used for debugging)
         output$PFM <- renderText({v_Pfm})         # PFM (used for debugging) 
         
         # Writes the table for the download button
         output$downloadData <- downloadHandler(
                 filename = function() { 
                         paste("results", '.csv', sep='') 
                 },
                 content = function(file) {
                         write.csv(res_df, file)
                 }
         )
         
         # write the main plot
         p1 <- ggplot(res_df) + 
                 geom_line(aes(x=time, y=Infested), color="Black", size=1) +
                 geom_line(aes(x=time, y=Active), color="Black", size=1, linetype="dashed") +
                 annotate("text", label = "Infested", x=(0.1*v_T), y=(vec_Ht[2]*0.9), size = 6, colour = "Black") +
                 annotate("text", label = "Active", x=(0.1*v_T), y=(vec_At[4]*2), size = 6, colour = "Black") +
                 xlab("Time (Years)") +
                 ylab("Area (ha)") +  
                 theme_bw(base_size=20) +
                 scale_y_continuous(expand = c(0,(0.1*v_A)), limits=c(0,(1.01*v_A))) +
                 scale_x_continuous(expand = c(0,0), limits=c(0,v_T)) +
                 theme(panel.background = element_rect(fill = NA),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),  
                       plot.background = element_rect(fill = NA),
                       axis.text.x = element_text(angle = 45, hjust = 1, size=18),
                       axis.text.y = element_text(angle = 0, hjust = 1, size=18),
                       axis.title.x = element_text(face="bold", vjust=0.3),
                       axis.title.y = element_text(face="bold", vjust=0.8))
         
         p2 <- ggplot(res_df) + 
                 geom_line(aes(x=time, y=Prob), color="Red", size=1) +                      
                 xlab("Time (Years)") +
                 ylab("Pr(Eradication | Time)") +  
                 theme_bw(base_size=20) +
                 scale_y_continuous(expand = c(0,0.1), limits=c(0,1), breaks=c(0.2,0.4,0.6,0.8,1.0)) +
                 scale_x_continuous(expand = c(0,0), limits=c(0,v_T)) +
                 theme(panel.background = element_rect(fill = NA),
                       #panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),  
                       plot.background = element_rect(fill = NA),axis.text.x = element_text(angle = 45, hjust = 1, size=18),
                       axis.text.y = element_text(angle = 0, hjust = 1, size=18),
                       axis.title.x = element_text(face="bold", vjust=0.3),
                       axis.title.y = element_text(face="bold", vjust=0.8))    
         
         
         g1 <- ggplot_gtable(ggplot_build(p1))
         g2 <- ggplot_gtable(ggplot_build(p2))
         
         # overlap the panel of 2nd plot on that of 1st plot
         pp <- c(subset(g1$layout, name == "panel", se = t:r))
         g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, 
                              pp$l, pp$b, pp$l)
         
         # axis tweaks
         ia <- which(g2$layout$name == "axis-l")
         ga <- g2$grobs[[ia]]
         ax <- ga$children[[2]]
         ax$widths <- rev(ax$widths)
         ax$grobs <- rev(ax$grobs)
         ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
         g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
         g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)
         
         # label tweaks
         ia <- which(g$layout$name == "ylab-l")
         ylab <- g2$grobs[[ia]]
         g <- gtable_add_cols(g, g$widths[g$layout[ia, ]$l], length(g$widths) - 1)
         g <-  gtable_add_grob(g, ylab, pp$t, length(g$widths) - 1, pp$b)
         
         # draw it
         grid.newpage()
         grid.draw(g)                
         
         
 })
 # print the plot to the ui 
 output$distPlot <- renderPlot({                  
         print(dataInput())
         
 })
 # handler to download the plot (not currently in use)
 output$downloadPlot <- downloadHandler(
         filename = function() { paste('magic_graph', '.png', sep='') },
         content = function(file) {
                 device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 300, units = "in")
                 ggsave(file, plot = dataInput(), device = device)
         }
 )
 # handler to download the results file
 output$downloadData <- downloadHandler(
         filename = function() { 
                 paste("results", '.csv', sep='') 
         },
         content = function(file) {
                 write.csv(res_df, file)
         }
 )
#end aaron' code
 
})#end server function
