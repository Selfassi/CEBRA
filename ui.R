# CEBRA project
# This is a simulation model for
# calculating costs of invasive species in Australia

library(shiny)

####################################################################
# Define UI 
shinyUI(navbarPage(

####################################################################        
#CSS  
tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css")), #end css style set up 

tags$head(tags$style("
      .head_row{height:70px;background-color: #1B9E77; color: white;}"
)
),

tags$head(tags$style("
      .time_box{background-color:  #7570B3; color: white;}"
)
),

tags$head(tags$style("
      .mid_row{height:35px;background-color: #666666;}"
)
),

css <- "#large .selectize-input { line-height: 40px; }
        #large .selectize-dropdown { line-height: 30px; }",

####################################################################
title = "Economic Cost of Invasive Species in Australia",

#The main panel which will appear in every tab
h1(class = "head_row","Elicitation tool", align = "center"),

fluidRow(
         column(4,  
                h3("Select one of the 20 species from Australia's priority list"), 
                div(id = "large", selectInput("species", h4("Select species"), c("Xylella","Khapra beetle",
                                "Exotic fruit fly","Karnal bunt","Huanglongbing","Gypsy moths",
                                "Tramp ants","Bees mites","Giant African snail","Stink bug",
                                "Zebra chip","Ug99","Russian wheat aphid","Citrus canker",
                                "Guava rust","Airborne phytophthora","Exotic bees",
                                "Panama disease tropical race 4","Potato cyst nematode","Leaf miner"))),
                
                imageOutput("species_photo")
        ),# end first column 
        column(5,
                h3("Map indicating simulated maximum range of selected species:"),
               
                h3("Simulated area of maximum invasion is ", 
                   strong(textOutput("area", inline = TRUE)), "in millions of ha." ),
                
                plotOutput("map"),
                actionButton("show", "Exlanatory note")
        ),# end second column
        column(2, br(),br(), br(), br(),br(), br(),
                wellPanel(class= "time_box",
                        h3("How quickly can the maximum range be reached?"),
                        selectInput("timescale", h4("Time horizon"), c("< 10 years","< 30 years", "< 100 years"))
                )
                
      
       ) # end third column
),# end first row        
h1(class = "mid_row","", align = "center"),
#################################################################### 
tabPanel("Introduction",
# Application description
h2("Synopsis", align = "center"),
br(),
tags$p("This application is designed to illustrate alternative models for calculating the economic cost of invasive species to Australia. 
       The models will account for market and non-market values. 
       Impacts on environment, economy, culture and human health will be considered. 
       This web application will combine elements of Dodd's framework, ACERA 1002, Holt et al 2013, and possibly other approaches; 
       it will utilise statistics related to potentially affected sectors of the economy and vulnerable native plants.",
       align = "center"),
tags$p("This implementation is a ‘story-board’ version. It illustrates broadly how the system will work and what data and 
       assumptions are needed to drive it. Not all the elements are currently linked as they should be and several of the functions are missing. 
       As a result, changing one of the parameters or assumptions will not change all of the relevant others, as will happen once implementation is complete. 
       At this stage, it is designed to elicit feedback on the broad approach and the type of product that will emerge from this work.
       ",
       align = "center"),
br(),   

fluidRow(column(6, 
                h3("Dodd's framework for prioritising is based on the following equation and linear programming:"),
                tags$p("(RISK [total expected damage]*EFFECTIVENESS [proportion of damage that could be reduced]*
                       IMPLEMENTABILITY*[probability that management succeeds])/(COSTS OF ERADICATION)"),
                
                h5("Notes:"),
                tags$p("I am not sure if linear programming contributes much value - 
                        the results of linear programming are either determined by benefit/cost ratios or are not robust 
                        to uncertainty if the distributions for benefit/cost ratios overlap for different species."),
                tags$p("For the 'EFFECTIVENESS*IMPLEMENTABILITY' part of Dodd's equation, we could use Holt et al. method 
                        for elicitation which accounts for uncertainty in judgements."),
                tags$p("And for the 'management cost' part, use Aaron's code. It may be possible to simplify it, 
                        especially if we don’t use the linear programming implementation. 
                        This will also simplify explanation considerably."),
                tags$p("To calculate 'total damage ($)', we could list affected industries and elicit the severity with which these
                        could be impacted."),
                br(),
                h3("Moving beyond money, ACERA 1002:"),
                tags$p("Total expected damage is a combination of: harm to property,  health, and business($), amenity impacts (0 = benign
                        to 100 = extreme severity), environmental impacts (number of susceptible native species out of some pre-agreed list))")
                
                ),# end first column
         column(6,        
                h3("Additional ideas to consider:"),
                tags$li("Use corrective coefficients to adjust values-at-risk as an alternative approach to account for amenity,
                        or other difficult to monetise values. An example of this approach can be found in Chapter 2 of ", 
                        strong(tags$a(href= "http://www.springer.com/gp/book/9783319468969", 
                        "Practical Tools for Plant and Food Biosecurity"))),
                tags$li("Potential economic impact could be worse if the species is used or suspected in agro-terror attack: 
                        should we include such considerations?
                        "),
                br(),        
                h3("Some of the key assumptions (all criticised at least somewhere in the literature):"),
                tags$li("Ignore climate change"),
                tags$li("Ignore dynamics of invasion (economists seem to be especially unhappy about this)"),
                tags$li("Assume maximum possible range (based on CLIMEX model)"),
                tags$li("Focus on single species at a time"),
                tags$li("Model management/damage (market) costs as linear in space and time"),
                br(),
                h3("Rank 20 species as a demo"),
                tags$p("Use the app to demonstrate how ranking will change depending on the weights given to business, amenity and environment,
                       as well as methodology chosen for combining these effects.")
                )# end second column
)# end first row
),# end intro tab

#################################################################### 
tabPanel("Eradication costs",
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
                        h4("Cost (NPV):", strong(textOutput("cost", inline=TRUE)), "mil AUD"),
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
                                sliderInput("z19", "Effective search speed (m per hour):", min = 1, max = 5000, value = 1000)
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
                                sliderInput("z12", "Labour costs ($ per hour):", min = 10, max = 50, value = 25),      
                                sliderInput("z13", "Control costs ($ per ha):", min = 1, max = 500, value = 272.40),
                                numericInput("z14", "Administration costs ($ per year):", value = 143000),
                                numericInput("z15", "Research / Comms. costs ($ per year):", value = 10000),
                                sliderInput("z16", "Discount rate:", min = 0, max = 0.1, value = 0.06)
                        ))
         )
),#end aaron 
####################################################################
tabPanel("Monetised damages", 
        fluidRow(column(4, 
                                      
                        h3("Enter values"), 
                        numericInput("tree", h4("The cost of removing and replacing trees, ($ per ha per year) "), 0,
                                0, 1000, 10),
                        numericInput("health", h4("Health costs, ($ per ha per year) "), 0,
                                0, 1000, 10),
                        sliderInput("house", h4("Reduction in house prices, %"), 0, 1,
                                0.1, 0.05),
                        sliderInput("tourism", h4("Reduction in tourism revenues, %"), 0, 1,
                                0.1, 0.05)
                ),# end first column 
                column(8,
                     
                        h3("Select types of agricultural production at risk from selected pest and state your belief about possible proportional damage:"),
                        checkboxInput("broadacre_impact", h4("Broadacre cereal and non-cereal crops"), value = TRUE),
                        conditionalPanel(
                                        condition = "input.broadacre_impact == true",
                                        sliderInput("broadacre_percent_damage", "Reduction in cereal and non-cereal crops, %", 0, 1,
                                                0.1, 0.05)
                                ),
                       
                        checkboxInput("hay_impact", h4("Hay and silage"), value = TRUE),
                        conditionalPanel(
                                        condition = "input.hay_impact == true",
                                        sliderInput("hay_percent_damage", "Reduction in hay and silage, %", 0, 1,
                                                0.1, 0.05)
                                ),                        
                        checkboxInput("flower_impact", h4("Flower and nurseries"), value = TRUE),
                        conditionalPanel(
                                        condition = "input.flower_impact == true",
                                        sliderInput("flower_percent_damage", "Reduction in flower and nurseries, %", 0, 1,
                                                0.1, 0.05)
                       ), 
                        checkboxInput("fruit_impact", h4("Fruit and nuts"), value = TRUE),
                        conditionalPanel(
                                        condition = "input.fruit_impact == true",
                                        sliderInput("fruit_percent_damage", "Reduction in fruit and nuts, %", 0, 1,
                                                0.1, 0.05)
                       ), 
                        checkboxInput("veg_impact", h4("Vegetables for human consumption"), value = TRUE),
                        conditionalPanel(
                                        condition = "input.veg_impact == true",
                                        sliderInput("veg_percent_damage", "Reduction in vegetables production, %", 0, 1,
                                                0.1, 0.05)
                       ), 
                        checkboxInput("livestock_impact", h4("Livestock products"), value = TRUE),
                        conditionalPanel(
                                        condition = "input.livestock_impact == true",
                                        sliderInput("livestock_percent_damage", "Reduction in livestock products, %", 0, 1,
                                                0.1, 0.05)
                       ), 

                       
                        h3("Total possible damage is", strong(textOutput("agri_value_damage", inline = TRUE)),
                        " - out of the maximum value of agricultural production at risk", strong(textOutput("agri_value",inline = TRUE)),"in millions of Australian                            dollars.")
                )# end second column
        )# end first row        
       
),#end model inputs market

####################################################################
tabPanel("Non-monetised damages",
 
         h3("Amenity, based on ACERA 1002, consider the following: "),
         br(),
         tags$li ("Community Stability (includes employment/displacement effects)"),
         tags$li ("Spiritual Values"), 
         tags$li ("Aesthetics (landscapes, views, waterways)"),
         tags$li ("Recreational, leisure, cultural activities"), 
         tags$li ("Personal loss of freedom, impacts on mobility, choices, usually from management actions"),  
         tags$li ("Fear and worry, perceptions of risk, not necessarily supported by evidence"),
         br(),
         sliderInput("amenity", h4("Negative impact on amenity, higher score values represent worse damage"), 
                0, 100, 30, 5),
         br(),
         h3("A proxy for environmental damage measured in terms of key native species, based on ACERA 1002"),
         sliderInput("environment", h4("Proportion of listed native species affected"),
                0, 100,30, 5)
         

),#end model inputs non-market 

####################################################################
tabPanel("Management utility",
         h1("Predicted effectiveness of measures combined with a chance of successful implementation", 
         align = "left"),
 
        fluidRow(column(6,        
                
                        h3("Rate the potential impact of the management measure
                        on the success of achieving management objectives
                        assuming full implementation"), 
                        selectInput("score_1", "Management Impact", c("Very Good",
                                                "Good","Medium",
                                                "Poor","Very Poor")),
                        h3("Express your uncertainty in such rating"), 
                        selectInput("unc_1", "Uncertainty", c("VL","L","M","H")),
                        imageOutput("image1", width = "60%")
                ),
                 column(6,         
                        h3("Rate the chances of the full implementation of 
                        the management measure bearing in mind technical, 
                        economic, labour constraints, etc", offset = -1), 
                        selectInput("score_2", "Implementation Feasibility", c("Very Good",
                                                "Good","Medium",
                                                "Poor","Very Poor")),
                        h3("Express your uncertainty about this rating"), 
                        selectInput("unc_2", "Uncertainty", c("VL","L","M","H")),
                        imageOutput("image2", width = "60%")
                )
        ),
        fluidRow(column(6,          
                        h1("Calculating management measure utility", align = "left"), 
                        br(),
                        h3("Management measure utility is calculated using the 'Matrix Method' of combining
                        distributions, described in ", strong(tags$a(href= "http://onlinelibrary.wiley.com/doi/10.1111/risa.12089/abstract", 
                                                "Holt et al. 2013")),align = "left")

                ),
                column(6,

                        h3("The distribution below represents the utility of a proposed measure based on its
                        believed impact on the objectives and a possibility of succesful implementation", align = "left"),
                        br(),   
                        imageOutput("image3", width = "60%")
                )
        )         
),#end management utility 

####################################################################        
tabPanel("Ranking",
         ##!!!!TEMP
        titlePanel("Prioritising species under different methods"),
        h4("Currently the two models differ in unit costs for 'economy', 'amenity', and 'environment' variables. 
           This demo is only related to the 'Model inputs: biology' tab, using same species and area."),
        h4("Economic model 2 has random variation in its linear relationship to the area of maximum invasion."),
        
        fluidRow(column(6,        
                        h3("Select aspects to include:"), 
                        checkboxInput("market", "Economy", value = TRUE),
                        checkboxInput("nonmarket", "Amenity", value = TRUE), 
                        checkboxInput("native_species", "Environment", value = TRUE)
                ),
       
                column(6,##weights
                        h3("Preferences on a scale from 1 (low) to 10 (high):"), 
                        numericInput("weight_econ", "Weight, economy", 1,
                              1, 10, 1),
                        numericInput("weight_amen", "Weight, amenity", 1,
                              1, 10, 1),
                        numericInput("weight_env", "Weight, environment", 1,
                              1, 10, 1)
                )
        ),
        br(),
        fluidRow(column(6, 
                        h3("Rank species using model 1:"),
                        plotOutput("rank1", height = "400px")
                ),
                column(6,
                        h3("Rank species using model 2:"),
                        plotOutput("rank2", height = "400px")
                )
        )
)#end overall ranking panel


)#end navbar page 
)#end shiny      
        
        
       
  
 
