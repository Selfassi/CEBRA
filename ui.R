# CEBRA project
# This is a simulation model for
# calculating costs of invasive species in Australia

library(shiny)

####################################################################
# Define UI 
shinyUI(fluidPage(
                           
#Title and css stuff
####################################################################           
tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css")), #end css style set up 
        
tags$head(tags$style("
                .head_row{height:100px;background-color: #1B9E77; color: white;}
                .species_row{height:500px;}
                .time_box{background-color:  #7570B3; color: white;}
                .mid_row{height:35px;background-color: #666666;}
                .black_font{color: black;}

                #large .selectize-input { line-height: 40px; }
                #large .selectize-dropdown { line-height: 30px; }

                ")),
                                           
#The main title
h1(class = "head_row", "This elicitation tool is designed to illustrate alternative ways of calculating the economic cost 
of invasive species to Australia." , align = "center"),
                                           
#Row 1 consists of species, distribution and time perios selection
#################################################################### 
fluidRow(
        column(4,class = "species_row",   
                h3("Select one of the 20 species from Australia's priority list"), 
                div(id = "large", selectInput("species", h4("Select species"), c("Xylella","Khapra beetle",
                                "Exotic fruit fly","Karnal bunt","Huanglongbing","Gypsy moths",
                                "Tramp ants","Bees mites","Giant African snail","Stink bug",
                                "Zebra chip","Ug99","Russian wheat aphid","Citrus canker",
                                "Guava rust","Airborne phytophthora","Exotic bees",
                                "Panama disease tropical race 4","Potato cyst nematode","Leaf miner"))),
                
                imageOutput("species_photo")
        ),# end first column 
        column(4,class = "species_row", 
                h3("Map indicating simulated maximum range of a species selected"),
                        #h4("Simulated area of maximum invasion is ", 
                        #strong(textOutput("area", inline = TRUE)), "in millions of ha."),
                actionButton("show", "Exlanatory note"),
                imageOutput("map")
                
        ),# end second column
        column(1, class = "species_row"), # end 3rd column
        column(3, class = "species_row", br(), 
                wellPanel(class= "time_box",
                        h4("How quickly can the maximum range be reached?"),
                        selectInput("timescale", h4("In years"), c(10, 30, 100)),
                        textInput("name", h4("Please enter your name or initials")),
                        br(),
                        h4("Record all of your answers for each species after visiting the tabs below"), 
                        actionButton("submit", strong("Click to submit once per species"), align = "center")
                )  # end wellpanel
        ) # end fourth column
),# end second row  

#Row 2 
#################################################################### 
fluidRow(column(12, 
#grey banner
h1(class = "mid_row","", align = "center"))),

#Row 3 consists of main elicitations
#################################################################### 
fluidRow(column(3, 
                
                h1(class = "head_row","Instructions", align = "center"),
                br(),
                h4("Once you finished submitting your responses from visiting the following tabs: "),
                br(),
                tags$li (strong("Monetised damages")),
                tags$li (strong("Non-monetised damages")), 
                tags$li (strong("Management utility")),
                br(),
                h4("for each of the species that you chose to do, proceed to the",strong("'Ranking'"),"tab to explore 
                alternative prioritisations.")
        ), 
        
        column(9, 
               tabsetPanel(
                        
####################################################################
tabPanel("Monetised damages",
         
        fluidRow(column(12,h3("Select types of agricultural production at risk from ",textOutput("species_name", inline = TRUE),
                              "and state your beliefs about possible proportional damage to each of the industries:"))), 
        fluidRow(column(6, 
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
                        ) 
                ),# end first column 
                column(6,
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
                       ))), 

        fluidRow(column(12, class = "time_box", 
                        h4("Given these impacts, the total possible annual damage from this pest is", 
                                strong(textOutput("agri_value_damage", inline = TRUE)),
                                "in millions of Australian dollars.")))     
       
),#end model inputs market

####################################################################
tabPanel("Non-monetised damages",
 
         h3("Amenity, consider the following: "),
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
         h3("Environment, consider damage to biodiversity, protected native species, and habitats."),
         sliderInput("environment", h4("A proxy for environmental damage, higher score values represent worse damage"),
                0, 100,30, 5)
         

),#end model inputs non-market 

####################################################################
tabPanel("Management utility",
         h1("Predicted effectiveness of measures combined with a chance of successful implementation", 
         align = "left"),
         br(),
         h4("Management measure utility is calculated using the 'Matrix Method' of combining
                        distributions, described in ", strong(tags$a(href= "http://onlinelibrary.wiley.com/doi/10.1111/risa.12089/abstract", 
                                                                     "Holt et al. 2013")),align = "left"),
         
 
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
        fluidRow(column(4), column(4, 
                                HTML('<img src="arrow.jpg" width="20%">'),
                                h3("Combine Distributions", align = "center"),
                                imageOutput("image3", width = "90%"),
                                h4("The distribution above represents the utility of a proposed measure based on its
                                believed impact on the objectives and a possibility of succesful implementation", align = "left")
                                )
                )         
),#end management utility 

####################################################################        
tabPanel("Ranking",
        
        titlePanel("Prioritising species under different methods"),
       
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
                        h3("Ranking using default values:"),
                        plotOutput("rank_default", height = "400px")
                ),
                column(6,
                        h3("Ranking using elicited values:"),
                        plotOutput("rank_user", height = "400px")
                )
        )
)#end overall ranking panel

)))#ends

)#end fluid page 
)#end shiny      
        
        
       
  
 
