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

tags$style(HTML("
        .tabbable > .nav > li > a  {background-color: #823c47;  color:white}
                ")), 

tags$head(tags$style("
                .head_row{height:150px;background-color: #857d6f; color: white;} 
                .species_row{height:400px;}

                body {background-color: #d5d2ca;
                        font-family: Calibri, arial, sans-serif;
                        font-size: 16px;
                        overflow: auto;}
                .instructions_tab {background-color:  #857d6f; color: white;}
                .time_box{background-color:  #284e36; color: white;}
                .tab_box{background-color:  #165788; color: white;}
                .erad_box{background-color:  #00505c; color: white;}
                .mid_row{height:35px;background-color: #343434;}
                .black_font{color: black;}

                #large .selectize-input { line-height: 40px; }
                #large .selectize-dropdown { line-height: 30px; }

                ")),
                                           
#The main title
fluidRow(class = "head_row", 
         column(3,
                h2(img( src = "cep-logo.jpeg", 
                        height = 110, width = 160, 
                       inlign = TRUE))),
                
         column(6,br(),h2(strong(" PRIORITISING INVASIVE SPECIES IN AUSTRALIA", 
                   align = "left"))),
         column(2,
                h2(img( src = "aust-logo.png", 
                        height = 110, 
                        inlign = TRUE),align = "right"))),
                                           
#Row 1 consists of species, distribution and time perios selection
#################################################################### 
fluidRow(
        column(3,  
                
                div(id = "large", selectInput("species", h3("Select species"), c("Xylella","Khapra beetle",
                                "Exotic fruit fly","Karnal bunt","Huanglongbing","Gypsy moths",
                                "Tramp ants","Bees mites","Giant African snail","Stink bug",
                                "Zebra chip","Ug99","Russian wheat aphid","Citrus canker",
                                "Guava rust","Airborne phytophthora","Exotic bees",
                                "Panama disease tropical race 4","Potato cyst nematode","Leaf miner"))),
                sliderInput("likelihood", h4("Probability of entry, establishment and spread"), 
                           0, 1, 1, 0.01)
        ),# end first column 
        column(5, 
                        h3("Maximum range"), 
                        #h4("Simulated area of maximum invasion is ", 
                        #strong(textOutput("area", inline = TRUE)), "in millions of ha."),
                        actionButton("show_map", "Exlanatory note")
        ),# end second column
        column(1),
        column(3,
               selectInput("timescale", h3("How quickly can the maximum range be reached, in years"), c(10, 30, 100)))
), 
#################################################################### 
fluidRow(
        column(3,  
                imageOutput("species_photo")
               
               
        ),# end first column 
        column(5, 
               imageOutput("map", inline = TRUE, width = "90%")
        ),# end second column
        column(1), # end 3rd column
        column(2, 
               wellPanel(class= "time_box",
                        textInput("name", h4("Please enter your name or initials")),
                         br(), br(),
                         h4(em("You must submit at least one answer to be able to compare rankings.")),
                         br(),
                         actionButton("submit", strong("Submit"), align = "center")
               )  # end wellpanel
        ) # end fourth column
),# 
#Row 2 
#################################################################### 
fluidRow(column(12, 
#grey banner
h1(class = "mid_row","", align = "center"))),

#Row 3 consists of main elicitations
#################################################################### 
fluidRow(column(3, 
                wellPanel(class = "instructions_tab",
                h2("Instructions", align = "center"),
                br(),
                h4("Once you finished submitting your responses for each species by visiting tabs: "),
                br(),
                tags$li (em("Monetised damages")),
                tags$li (em("Non-monetised damages")), 
                tags$li (em("Eradication measures' utility")),
                br(),
                h4("proceed to 'Ranking' to explore 
                alternative prioritisations."),
                br(),
                h4("Download all responses as a csv file:"), 
                downloadButton("downloadResponses", strong("Download"), align = "right"))
        ), 
        
        column(9, 
               tabsetPanel(
                        
####################################################################
tabPanel(h5("Monetised /damages"), 
         
        fluidRow(column(12,h3("Select types of agricultural production at risk from ",textOutput("species_name", inline = TRUE),
                                "and state your beliefs about possible proportional damage to each of the industries, 
                                in the areas of overlap with the pest
                                :"))), 
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

        fluidRow(column(9, class = "tab_box", 
                        
                       
                        h4("On a national scale, the annual damage from this pest is", 
                           strong(textOutput("agri_value_damage", inline = TRUE)),
                           "in millions of Australian dollars.") 
                        
                        ))     
       
),#end model inputs market

####################################################################
tabPanel(h5("Non-monetised damages"),
        fluidRow(column(5, 
                        h3("Localised social impacts, consider the following: "), 
                        tags$li ("Community Stability (includes employment/displacement effects)"),
                        sliderInput("community", h4("Social impacts"), 
                                    0, 5, 0, 1),
                        actionButton("show_impact", "Impact Scores Explained"),
                        h3("To access environmental damage consider impacts on:"),
                        tags$li ("Biodiversity"),
                        tags$li ("Protected native species"),
                        tags$li ("Habitats"),
                        sliderInput("environment", 
                                    h4("Environmental damage"),
                                    0, 5, 0, 1)
                       
                        ),
                 column(1),
                 column(5,
                        h3("Amenity, consider the following: "),
                        tags$li ("Spiritual Values"), 
                        tags$li ("Aesthetics (landscapes, views, waterways)"),
                        tags$li ("Recreational, leisure, cultural activities"), 
                        tags$li ("Personal loss of freedom, impacts on mobility, choices, usually from management actions"),  
                        tags$li ("Fear and worry, perceptions of risk, not necessarily supported by evidence"),
                        br(),
                        sliderInput("amenity", h4("Negative impacts on amenity, higher score values represent worse damage"), 
                                    0, 5, 0, 1)
                        
                 )
        )
         

),#end model inputs non-market 

####################################################################
tabPanel(h5("Eradication measures' utility"),
         h3("Eradication measures' utility is a combination of effectiveness of measures and 
            a chance of successful implementation. Uncertainty in both is considered.", 
         align = "left"),
         br(),
         h4("Probability distributions are combined using the 'Matrix Method', described in ", strong(tags$a(href= 
                        "http://onlinelibrary.wiley.com/doi/10.1111/risa.12089/abstract", 
                        "Holt et al. 2013",".")),align = "left"),
         
        fluidRow(column(6,  
                        h4(em("Rate the potential for successful eradication
                        assuming full implementation of measures bearing in mind 
                        biological and ecological behaviour of the pest."))),
                column(6,  
                        h4(em("Rate the chances of the full implementation of 
                        eradication measures bearing in mind technical, 
                        economic, labour constraints, etc.", offset = -1)))), 
                         
 
        fluidRow(column(6,        

                        selectInput("score_1", h3("Effectiveness of eradication measures"), c("Very Good",
                                                "Good","Medium",
                                                "Poor","Very Poor")),
                        
                        selectInput("unc_1", h3("Express your uncertainty in such rating"), c("VL","L","M","H")),
                        
                        imageOutput("image1", width = "90%"), br()
                ),
                 column(6,         
                        selectInput("score_2", h3("Implementation Feasibility"), c("Very Good",
                                                "Good","Medium",
                                                "Poor","Very Poor")),

                        selectInput("unc_2", h3("Express your uncertainty about this rating"), c("VL","L","M","H")),
                        imageOutput("image2", width = "90%"),
                        br()
                )
        ),
        
        fluidRow(column(5), column(6, HTML('<img src="arrow.jpg" width="20%">')) ),
        
        fluidRow(column(12, class = "erad_box", h3("Eradication measures' utility: "), 
                               align = "center")),
        fluidRow(column(3), column(6, br(),
                        imageOutput("image3", width = "100%")
                                )
                )         
),#end management utility 

####################################################################        
tabPanel(h5("Ranking"),
        
        titlePanel("Prioritising species under different methods"),
        
        fluidRow(column(12, h4(em("The species are ranked by adding normalised values for selected aspects, 
                                using specified weights.")))), 
       
        fluidRow(column(6,        
                        h3("Select aspects to include:"), 
                        checkboxInput("aspect_economy", "National economy", value = TRUE),
                        checkboxInput("aspect_amenity", "Amenity", value = TRUE), 
                        checkboxInput("aspect_environment", "Environment", value = TRUE),
                        checkboxInput("aspect_community", "Social impacts", value = TRUE),
                        checkboxInput("aspect_time", "Speed of Invasion", value = TRUE),
                        checkboxInput("aspect_measures", "Utility of eradication measures", value = TRUE)
                        
                ),
       
                column(6,##weights
                        h3("Preferences on a scale from 1 (low) to 5 (high):"), 
                        numericInput("weight_econ", "Weight, national economy", 3, 1, 5, 1),
                        numericInput("weight_amen", "Weight, amenity", 3,
                              1, 5, 1),
                        numericInput("weight_env", "Weight, environment", 3,
                              1, 5, 1),
                        numericInput("weight_local", "Weight, social impacts", 3,
                              1, 5, 1)
                )
        ),
        br(),
        fluidRow(column(6, 
                        h3("Ranking using default values:"),
                        plotOutput("rank_default", width = "80%")
                ),
                column(6,
                        h3("Ranking using elicited values:"),
                        plotOutput("rank_user", width = "80%")
                )
        )
)#end overall ranking panel

)))#ends

)#end fluid page 
)#end shiny      
        
        
       
  
 
