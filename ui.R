# CEBRA project
# This is a simulation model for
# calculating costs of invasive species in Australia

library(shiny)

####################################################################
# Define UI 
shinyUI(navbarPage(

#Include css file    
tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css")), #end css style set up 

title = "CEBRA", 

####################################################################                        
tabPanel("Introduction",
        # Application desciption
        h1("Economic cost of invasive species Australia"),
        
        tags$p("This application is designed to illustrate alternative models for calculating 
                economic cost of invasive species to Australia. Alternative models will 
                account for market and non-market values. 
                Impacts on environment, economy, culture and human health
                will be considered. It will combine elements of Dodd's framework, ACERA 1002 and Holt et al 2013; it will utilise statistics 
                related to potentially affected sectors of the economy and vulnerable native plats."),
        br(),        
        h3("Dodd's framework for prioritising is based on the following equation and linear programing (LP):"),
        
        tags$p("(Risk [total expected damage]*Effectiveness [Proportion of damage that could be reduced]*
                Probability that management succeeds )/(Cost of management)"),
        br(),
        h4("Notes:"),
        tags$p("I am not sure if LP contributes much value, the order is either determined by benefit/cost ratio or
                not robust to uncertainty if distributions for benefit/cost variables are overlapping for different species."),
        tags$p("For the 'effectiveness*prob of success' part of Dodd's equation, I suggest we use John Holt's method for elicitation."),
        tags$p("And for the 'management cost' part, a simplified version of Hester et al 2013."),
        tags$p("To calculate 'total damage ($)', I suggest we list possibly affected industries and elicit severity with which these
                       can be impacted."),
        br(),
        h3("Moving beyond money, ACERA 1002:"),
        tags$p("Total expected damage is a combinations of: harm to property,  health, and business($), amenity impacts (zero= benign
                to 100 (extreme severity), enviromental impacts (number of susceptible native species out of some pre-agreed list))"),
        br(),
        h3("Rank 20 species as a demo"),
        tags$p("Use the app to demostrate how ranking will change depending on the weights given to business, amenity and environment,
                as well as methodology chosen for combining these effects."),
        br(),        
        h3("Some of the key assumptions (all criticised in the literature):"),
        tags$ul("Ignore climate change"),
        tags$ul("Ignore dynamics of invasion (economists seem to especially unhappy about this)"),
        tags$ul("Assume maximum possible range (based on CLIMEX model)"),
        tags$ul("Focus on single species at a time"),
        tags$ul("Model management/damage (market) costs as linear in space and time")
),#end introduction panel

####################################################################                         
tabPanel("Model inputs: biology", 
        h3("Select one of the 20 species from Australia's priority list"), 
        selectInput("species", "Select species", c("Xylella","Khapra beetle",
                "Exotic fruit fly","Karnal bunt","Huanglongbing","Gypsy moths",
                "Tramp ants","Bees mites","Giant African snail","Stink bug",
                "Zebra chip","Ug99","Russian wheat aphid","Citrus canker",
                "Guava rust","Airborne phytophthora","Exototic bees","Panama disease tropical race 4",
                "Potato cyst nematode","Leaf miner")),

        imageOutput("species_photo"),
                
        h3("Map indicating simulated maximum range of selected species:"),
        h4("Observation points indicate sectors invaded, so that the area affected can be calculated based on these simulated values.
           These values were similuted for demonstration purposes only. In the future, models like CLIMEX will provided estimates of the
           unticipated maximum spread of invasive species in the absence of management, after a length of time."),  
        plotOutput("map",width = "500px", height = "400px"),

        h3("Predicted area maximum invasion (millions of ha):"),
        textOutput("area")
),#end biology inputs and map

#################################################################### 
tabPanel("Eradication Costs",
         # Application title
         titlePanel("Theoretical cost estimation for empirical plant eradication scenarios"),
         p("Evidence-based cost-efficiency estimates for plant eradication programs based on the models of Hester et. al. (2013) and Dodd et. al. (2015)"),
         
         # Add the first row of objects. Plot in the first column and summary stats in the second.
         fluidRow(
                 column(8,
                        plotOutput("distPlot")
                 ),
                 column(4,
                        h4("Time to eradication: ", strong(textOutput("time", inline=TRUE)), "years"),
                        h4("Cost ($NPV):", strong(textOutput("cost", inline=TRUE))),
                        h4("Pr(Erad|horizon):", textOutput("prob", inline=TRUE)),
                        h4("Cost(Erad|horizon):", textOutput("costtime", inline=TRUE)),
                        h4("Efficiency(Erad|horizon):", textOutput("costeff", inline=TRUE)),
                        #h4("CvA: ", strong(textOutput("costtime", inline=TRUE)), "CvM: ", strong(textOutput("costeff", inline=TRUE))),           
                        #h4("PFA: ", strong(textOutput("PFA", inline=TRUE)), "PFM: ", strong(textOutput("PFM", inline=TRUE))),
                        downloadButton('downloadData', 'Download Results'),
                        
                        hr(),
                        
                        #h4("Cost Breakdown"),
                        tableOutput('cost_tab')
                        
                 )
         ),
         
         # adds a line separating the rows
         hr(),
         
         # adds a second row for the headings
         fluidRow(
                 column(4,
                        h4("Infestation Details")#,
                        #p("Select using slider")
                 ),
                 column(4,
                        h4("Management Strategy")#,
                        #p("Select using slider")
                 ),
                 column(4,
                        h4("Economic Inputs")
                 )
         ),
         
         # adds a third row with the input sliders enclosed in a box
         fluidRow(
                 column(4,
                        wellPanel(
                                #sliderInput("z1", "Infested Area (ha):", min = 1, max = 1000, value = 100),
                                sliderInput("z6", "Propagule Longevity (years):", min = 1, max = 15, value = 7),         
                                sliderInput("z3", "Detectability Period (months):", min = 1, max = 12, value = 12),
                                sliderInput("z4", "Detection distance - adult (m):", min = 1, max = 40, value = 20),
                                sliderInput("z20", "A/J detection distance ratio:", min = 3, max = 6, value = 5, step = 0.25),          
                                sliderInput("z19", "Effective search speed (m h-1):", min = 1, max = 5000, value = 1000)
                        )),
                 column(4,
                        wellPanel(
                                sliderInput("z5", "Distance to manager (km):", min = 1, max = 100, value = 30),
                                #sliderInput("z11", "Search effort (h ha-1):", min = 0.5, max = 10, value = 2),
                                sliderInput("z7", "Monitoring Rate  (count (pa)):", min = 1, max = 4, value = 1),
                                sliderInput("z21", "Additional monitoring period (years):", min = 0, max = 4, value = 0),          
                                sliderInput("z17", "Control effectiveness - adults:", min = 0, max = 1, value = 0.98),
                                sliderInput("z18", "Control effectiveness - juveniles:", min = 0, max = 1, value = 0.99)           
                        )),
                 column(4,
                        wellPanel(
                                sliderInput("z22", "Decision horizon (years):", min = 10, max = 50, value = 20, step = 5), 
                                sliderInput("z12", "Labour costs ($ h-1):", min = 10, max = 50, value = 25),      
                                sliderInput("z13", "Control costs ($ ha-1):", min = 1, max = 500, value = 272.40),
                                numericInput("z14", "Administration costs ($ year-1):", value = 143231),
                                numericInput("z15", "Research / Comms. costs ($ year-1):", value = 10000),
                                sliderInput("z16", "Discount rate:", min = 0, max = 0.1, value = 0.06)
                        ))
         )
),#end aaron 
####################################################################
tabPanel("Monetised damages", 
        ##!!!!TEMP
        h3("Enter values"), 
        numericInput("tree", "The cost of removing and replacing trees, (AUD/ha/year) ", 0,
                                   0, 1000, 10),
        numericInput("health", "Health costs, (AUD/ha/year) ", 0,
                             0, 1000, 10),
        sliderInput("house", "Reduction in house prices, %", 0, 1,
                                     0.1, 0.05),
        sliderInput("agri", "Reduction in agricultural revenues, %", 0, 1,
                            0.1, 0.05),
        h3("Total value of agricultural production at risk (mil AUD)"),
        textOutput("agri_value"),
        radioButtons("type_industry", "Type affected", 
                     c("Broadacre cereal and non-cereal crops","Hay and silage","Flower and nurseries","Fruit and nuts", 
                       "Vegatables for human consumption", "Livestock products")),
        sliderInput("tourism", "Reduction in tourism revenues, %", 0, 1,
                            0.1, 0.05)
                
),#end model inputs market

####################################################################
tabPanel("Non-monetised damages",
         ##!!!!TEMP
         h3("ACERA 1002")
         

),#end model inputs non-market 

####################################################################         
tabPanel("Model inputs: weighting",
         ##!!!!TEMP
         h3("Select which aspects to include in calculating potential cost of invasion:"), 
         checkboxInput("market", "Economy", value = TRUE),
         checkboxInput("nonmarket", "Amenity", value = TRUE), 
         checkboxInput("native_species", "Environment", value = TRUE),
         br(),
         h3("Preferences on a scale from 0 (low) to 10 (high):"), 
         numericInput("weight_business", "Weight, economy", 0,
                      0, 10, 1),
         numericInput("weight_exist", "Weight, amenity", 0,
                      0, 10, 1),
         numericInput("weight_env", "Weight, environment", 0,
                      0, 10, 1)
),#end model inputs weighting     

####################################################################
tabPanel("Management Utility",
         titlePanel("Predicted effectiveness combined with a chance of successful implementation"),
         h1("A template for expert elicitations"),
         br(),
         h3("Rate the potential impact of the management measure
            on the success of achieving management objectives
            assuming full implementation"), 
         selectInput("score_1", "Management Impact", c("Very Good",
                                                       "Good","Medium",
                                                       "Poor","Very Poor")),
         h3("Express your uncertainty in such rating"), 
         selectInput("unc_1", "Uncertainty", c("VL","L","M","H")),
         imageOutput("image1", width = "50%"),
         
         h3("Rate the chances of the full implementation of 
            the management measure bearing in mind technical, 
            economic, labour constraints, etc"), 
         selectInput("score_2", "Implementation Feasibility", c("Very Good",
                                                                "Good","Medium",
                                                                "Poor","Very Poor")),
         h3("Express your uncertainty about this rating"), 
         selectInput("unc_2", "Uncertainty", c("VL","L","M","H")),
         imageOutput("image2", width = "50%"),
         
         br(),
         h1("Calculating management measure utility"), 
         br(),
         h3("Management measure utility is calculated using the 'Matrix Method' of combining
            distributions, described in "),
         h3(tags$a(href= "http://onlinelibrary.wiley.com/doi/10.1111/risa.12089/abstract", 
                   "Holt et al. 2013")),
         br(),
         h3("The distribution below represents the utility of a proposed measure based on its
            believed impact on the objectives and a possibility of succesful implementation"),
         br(),   
         imageOutput("image3", width = "50%")
),#end management utility 

####################################################################        
tabPanel("Ranking",
         ##!!!!TEMP
        titlePanel("Compare ranking for all species under different methods"),
        h4("Currently the two models differ in unit (AUD/ha) costs for market and non-market variables."),
        h4("The total cost also includes a species-specific constant."),
        h3("Compare all costs model 1:"),
        plotOutput("rank1", width= "50%", height = "400px"),
        br(),
        h3("Compare all costs model 2:"),
        plotOutput("rank2", width= "50%",height = "400px")
)#end overall ranking panel


)#end navbar page 
)#end shiny      
        
        
       
  
 
