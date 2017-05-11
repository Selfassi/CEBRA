# CEBRA project
# This is a simulation model for
# calculating costs of invasive species in Australia

library(shiny)

# Define UI for application that draws a histogram
shinyUI(navbarPage(
                tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css")
                ), #end css style set up 
                title = "CEBRA", 
                        
        tabPanel("Introduction",
                # Application desciption
                h1("Economic cost of invasive species Australia"),
                tags$p("This application is designed to illustrate alternative models for calculating 
                        economic cost of invasive species to Australia. Alternative models will 
                        account for market and non-market values. 
                        Impacts on environment, economy, culture and human health
                        will be considered."),
                br(),
                h3("Dodd's framework, should we apply it?:"),
                tags$p("(Risk [total expected damage]*Effectiveness [Proportion of damage that could be reduced]*Probability that management succeeds )/(Cost of management)
                "),
                br(),
                tags$p("Some of the key assumptions (all criticised in the literature):"),
                tags$ul("Ignore climate change"),
                tags$ul("Ignore dynamics of invasion (economists seem to especially unhappy about this)"),
                tags$ul("Assume maximum possible range (based on CLIMEX model)"),
                tags$ul("Focus on single species at a time"),
                tags$ul("Model management/damage (market) costs as linear in space and time"),
                tags$ul("Model non-market costs using benefit transfer"),
                br(),
                tags$p("Outstanding questions:"),
                tags$ul("What are the implicit management objectives (need to assume something in order to 
                        calculate management costs)?"),
                tags$ul("What about externalities of control methods?"),
                tags$ul("What spatial resolution?"),
                tags$ul("How to deal with cultural values? Weights? Legal precedents (payouts)?"),
                tags$ul("What kind of invasive species/what ecosystems?"),
                tags$ul("Discounting and other time related issues"),
                tags$ul("If using both expert opinion and data, how to make existing data more useful?"),
                tags$ul("Collect data on values at risk: agriculture, housing, tourism in Australia"),
                tags$ul("Build a case study to illustrate the model: fire ant"),
                tags$ul("Include in the app: transfering of non-market values"),
                tags$ul("Is there a way to account for generic biological characteristics: Allee effects, 
                        long-distance dispersal mechanism, etc. Or will this be 'double counting' wrt CLIMEX model?"),
                tags$ul("What is this app really for? To alert to non-robustness of dif methods, or
                        to be a prototype of an assessment tool?")
                         
                         
        ),#end introduction panel
                         
        tabPanel("Model inputs", 
                # Inputs desciption
                titlePanel("Expert based inputs"),
                h3("There are 4 simulated potentially invasive species named: A,B,C and D."), 
                selectInput("species", "Select species", c("A","B","C","D")),
                br(),
                h3("Select which variables to include in calculating potential cost of invasion"), 
                checkboxInput("market", "Market values", value = TRUE),
                checkboxInput("nonmarket", "Non-market values", value = TRUE),
                
                br(),
                h4("Choose specific market inputs"), 
                numericInput("tree", "The cost of removing and replacing trees, (AUD/ha/year) ", 0,
                                      0, 1000, 10),
                numericInput("health", "Health costs, (AUD/ha/year) ", 0,
                             0, 1000, 10),
                numericInput("control", "Control costs, (AUD/ha/year) ", 0,
                             0, 1000, 10),
                sliderInput("house", "Reduction in house prices, %", 0, 1,
                                     0.1, 0.05),
                sliderInput("agri", "Reduction in agricultural revenues, %", 0, 1,
                            0.1, 0.05),
                sliderInput("tourism", "Reduction in tourism revenues, %", 0, 1,
                            0.1, 0.05),
                br(),
                h4("Choose specific non-market inputs"), 
                radioButtons("valuations", "Non-Market values", c("Biodiversity","Recreational values", 
                                                                  "Ecosystem services", "Aesthetic", "Cultural values"))
        ),#end model inputs tab
        
        tabPanel("Model outputs", 
                #Results desciption
                titlePanel("Simulation of economic costs"),
                h3("Map indicating simulated maximum range of selected species:"),
                h4("Observation points indicate sectors invaded,"),
                h4("so that the area equals the number of points times X ha per obs."),  
                plotOutput("map",width = "500px", height = "400px"),
                h3("Predicted area maximum invasion (ha):"),
                textOutput("area"),
                br(),
                h3("Predicted cost from Model 1:"),
                textOutput("costModel1"),
                h3("Predicted cost from Model 2:"),
                textOutput("costModel2")
        ),#end model outputs panel
        
        tabPanel("Overall ranking",
                titlePanel("Compare ranking for all species under different models"),
                h4("Currently the two models differ in unit (AUD/ha) costs for market and non-market variables."),
                h4("The total cost also includes a species-specific constant."),
                h3("Compare all costs model 1:"),
                plotOutput("rank1",width = "500px", height = "400px"),
                br(),
                h3("Compare all costs model 2:"),
                plotOutput("rank2",width = "500px", height = "400px")
        ),#end overall ranking panel
        
        tabPanel("Case Study",
                 titlePanel("Simple examples of cost modelling"),
                 
                 h3("Fusarium circinatum (Cook and Matheson 2008)"),
                 tags$p("Some of the assumptions:"),
                 tags$ul("Cost of removing trees $12000 -15000 per ha (does this seem high?) "),
                 tags$ul("Maximum area 740000 ha (relevant forest area?) "),
                 tags$ul("At maximum range of the invasive, species 15% of trees 
                         can be affected and would need to be removed"),
                 tags$ul("Calculates only direct costs; 
                         their MCMC model gives $0.3 - 12.4 mil per year"),
                 tags$p("It seems that the back of the envelop calc should be
                        15000 (per ha cost)*0.15 (area to be treated)*740000 (area)/1000000"),
                 print(15000*0.15*740000/1000000), print ("million of Australian dollars."),
                 tags$p("which seems a lot higher than their answer, not sure why. 
                        Because it does not consider probability of entry and establishment?
                        And does not discount over 30 years until the maximum impact could occur?")
                 
                
        )#end case study
        
                
)#end navbar page 
)#end shiny      
        
        
       
  
 
