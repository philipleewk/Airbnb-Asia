################################################################
#  Tokyo Airbnb version 6 as of 3 May 2020                     #
#  MSBA 2020 Capstone project - Team Asia Airbnb               # 
################################################################
library(shinythemes)
library(plotly)
library(leaflet)


neighvars <- c(
"Adachi Ku" =  "Adachi Ku",           
"Akiruno Shi" = "Akiruno Shi",        
"Akishima Shi" =  "Akishima Shi",      
"Arakawa Ku" = "Arakawa Ku",        
"Bunkyo Ku"  = "Bunkyo Ku",
"Chiyoda Ku" = "Chiyoda Ku",      
"Chofu Shi"  = "Chofu Shi",         
"Chuo Ku"    =    "Chuo Ku",       
"Edogawa Ku" =   "Edogawa Ku",       
"Fuchu Shi"  =   "Fuchu Shi",       
"Fussa Shi"  =   "Fussa Shi",       
"Hachijo Machi" =  "Hachijo Machi",    
"Hachioji Shi"  =   "Hachioji Shi",     
"Hamura Shi"    =   "Hamura Shi",   
"Higashikurume Shi" =  "Higashikurume Shi",
"Higashimurayama Shi" = "Higashimurayama Shi",
"Higashiyamato Shi" = "Higashiyamato Shi",
"Hino Shi"  =   "Hino Shi",       
"Hinohara Mura"  =  "Hinohara Mura",   
"Inagi Shi"   =   "Inagi Shi",     
"Itabashi Ku"  =   "Itabashi Ku",   
"Katsushika Ku"  =   "Katsushika Ku", 
"Kita Ku"   =   "Kita Ku",       
"Kodaira Shi"    =   "Kodaira Shi",  
"Koganei Shi"    =   "Koganei Shi" ,
"Kokubunji Shi"  =   "Kokubunji Shi",
"Komae Shi"      =   "Komae Shi",
"Koto Ku"        =   "Koto Ku",   
"Kozushima Mura" =   "Kozushima Mura",   
"Kunitachi Shi"  =   "Kunitachi Shi" ,
"Machida Shi"   =    "Machida Shi", 
"Meguro Ku"     =    "Meguro Ku" ,
"Minato Ku"     =    "Minato Ku",  
"Mitaka Shi"    =    "Mitaka Shi",  
"Miyake Mura"   =    "Miyake Mura",
"Musashimurayama Shi" = "Musashimurayama Shi",
"Musashino Shi"       = "Musashino Shi",
"Nakano Ku"           = "Nakano Ku",
"Nerima Ku"           = "Nerima Ku",
"Niijima Mura"        = "Niijima Mura",
"Nishitokyo Shi"      = "Nishitokyo Shi",
"Ogasawara Mura"      = "Ogasawara Mura",
"Okutama Machi"       =  "Okutama Machi",
"Ome Shi"             = "Ome Shi",
"Oshima Machi"        = "Oshima Machi",
"Ota Ku"              = "Ota Ku",
"Setagaya Ku"         = "Setagaya Ku",
"Shibuya Ku"          = "Shibuya Ku",
"Shinagawa Ku"        = "Shinagawa Ku",
"Shinjuku Ku"         = "Shinjuku Ku",
"Suginami Ku"         = "Suginami Ku",
"Sumida Ku"           = "Sumida Ku",
"Tachikawa Shi"       = "Tachikawa Shi",
"Taito Ku"            = "Taito Ku",
"Tama Shi"            = "Tama Shi", 
"Toshima Ku"          = "Toshima Ku")

propvars <- c(
"Aparthotel" = "Aparthotel",         
"Apartment" = "Apartment",        
"Bed and breakfast"  = "Bed and breakfast",
"Boat"           =   "Boat",
"Boutique hotel" =   "Boutique hotel", 
"Bungalow"  = "Bungalow",
"Cabin"  =    "Cabin",         
"Camper/RV"   =  "Camper/RV",    
"Condominium" =  "Condominium",
"Dorm"        =  "Dorm",
"Earth house" =  "Earth house",
"Guest suite" =   "Guest suite",    
"Guesthouse"  =    "Guesthouse",    
"Hostel"      =    "Hostel",   
"Hotel"       =    "Hotel",   
"House"       =    "House",    
"Hut"         =   "Hut",     
"Loft"        =   "Loft",     
"Other"       =   "Other",   
"Resort"      =   "Resort",
"Ryokan (Japan)"  =  "Ryokan (Japan)",
"Serviced apartment" = "Serviced apartment",
"Tent"         =   "Tent",
"Tiny house"   =   "Tiny house",  
"Townhouse"    =    "Townhouse",    
"Villa" = "Villa")   

roomvars <- c("Entire home/apt", "Private room", "Shared room", "Hotel room")

room_type <- c("Entire home/apt", "Private room", "Shared room")

bedvars <- c("Airbed", "Couch", "Futon", "Pull-out Sofa", "Real Bed")

shinyUI(
  navbarPage(title = "Airbnb in Tokyo", 
             #id ="nav",
             
             theme = shinytheme("cerulean"), #https://rstudio.github.io/shinythemes/
             
             ##### Map ########      
             tabPanel("Map",
                      div(class="outer",
                          tags$head(
                            includeCSS("styles.css")),
                          
                          leafletOutput(outputId = "map", width = "100%", height = "100%"),
                          
                          # Panel options: neighborhood, Room Type, Price, Rating, Reviews
                          absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE, draggable = FALSE, 
                                        top = 70, left = "auto", right = 20, bottom = 200,
                                        width = 300, height = 800,
                                        h2("Airbnb in Tokyo"),
                                        
                                        selectInput("neigh", "Neigbourhood", neighvars),
                                        textOutput("summary"),
                                        plotOutput("hist_prop_type", height = 300),
                                        plotOutput("hist_room_type", height = 300)  
                                        
                                        ),
                                        
                          absolutePanel(id = "controls", class = "panel panel-default", fixed = FALSE, draggable = TRUE, 
                                        top = 20, left = 20, right = "auto" , bottom = 200,
                                        width = 250, height = 350,
                                        checkboxGroupInput(inputId = "select_room", label = h4("Room Type"), 
                                                           choices = room_type, selected = room_type),
                                        sliderInput(inputId = "slider_price", label = h4("Price"), min = 0, max = 110000, step = 1000,
                                                    pre ="¥", sep = ",", value = c(0, 110000)),
                                        h6("The map dataset is generated on 02 October, 2019 from"),
                                        h6(a("http://insideairbnb.com", href = "http://insideairbnb.com/get-the-data.html", target="_blank"))
                          )
                          
                      )),


             tabPanel("Predict Rental Price",
                      fluidPage(
                               
                               fluidRow(
                                 
                                 column(3,  # col 1
                                 h5("Enter the attributes of your Airbnb property"),     
                                 br(),
                                 selectInput("neigh1", label = h5("Neigbourhood"), neighvars),   
                                 sliderInput("hostListings", label = h5("Listings that Host has"), min = 0, 
                                             max = 10, value = 1),
                                 sliderInput("minimumNights", label = h5("Minimum nights"), min = 1,
                                             max = 10, value = 1),
                                 radioButtons("hostResponse", label = h5("Host Response"),
                                              c("A few days or more"="a few days or more",
                                                "Within a day"="within a day",
                                                "Within a few hours"="within a few hours",
                                                "Within an hour"="within an hour")),
                                 br(),
                         
                                 sliderInput("cleaningFees", label = h5("Cleaning Fees ¥"), min = 0, max = 50000, value = 5000, step = 1000,
                                             pre ="¥", sep = ","),
                                 radioButtons("instantBook", label = h5("Can book instantly?"),
                                              c("True"="TRUE",
                                                "False"="FALSE")),
                                 actionButton("predictrent", "Predict now!")
                                 
                                 ),
                                 column(3,  # col 2
                                 br(),     
                                 br(),
                                 br(),
                                 selectInput("property", label = h5("Property type"), propvars),  
                                 sliderInput("accommo", label = h5("Accommodates"), min = 1, 
                                             max = 10, value = 1),
                                 sliderInput("maximumNights", label = h5("Maximum Nights"), min = 1,
                                             max = 365, value = 1),
                                 radioButtons("cancelPolicy", label = h5("Cancellation Policy"),
                                              c("Flexible"="flexible",
                                                "Moderate"="moderate",
                                                "Strict with grace period" = "strict_14_with_grace_period",
                                                "Super strict" = "super_strict_30")),
                                 br(),
                            
                          
                                 sliderInput("extraGuests", label = h5("Extra Guests ¥"), min = 0, max = 11500, value = 1500, step = 500,
                                             pre ="¥", sep = ",")
                                 ),
                                 column(3, # col 3
                                 br(),        
                                 br(),
                                 br(),
                                 selectInput("roomType", label = h5("Room Type"), roomvars), 
                                 sliderInput("numBedrooms", label = h5("Bedrooms"), min = 0, 
                                             max = 10, value = 1),
                                 sliderInput("numAmenities", label = h5("Num of amenities"), min = 0,
                                             max = 10, value = 1),
                                 radioButtons("hostGender", label = h5("Host Gender"),
                                              c("Male"="male", 
                                                "Female"="female", 
                                                "Both genders"="mixed",
                                                "Not defined"="undefined")),
                                 br(),
                                 br(),
                 
                                 sliderInput("securityDeposit", label = h5("Security Deposit ¥"), min = 0, max = 550000, value = 12000, step = 500,
                                             pre ="¥", sep = ",")
                                       
                                
                                 ),
                                 column(3, # col 4
                                 br(),        
                                 br(),
                                 br(),
                                 selectInput("bedType", label = h5("Bed Type"), bedvars),   
                                 sliderInput("numBathrooms", label = h5("Bathrooms"), min = 0, 
                                             max = 10, value = 1),
                                 checkboxGroupInput("amenitiesGroup", label = h5("Amenities"), 
                                                    choices = list("Aircon" = 1, "Cooking" = 2, "Dryer" = 3,
                                                                   "Internet" = 4, "Safety" = 5, "TV" = 6,
                                                                   "Washer" = 7, "Not applicable" = 8),
                                                    selected = 1)
                                 
                                 ),
                                 column(9, # col 4
                                 br(),        
                                 h5(textOutput("predict_XGB_Summary")),
                                 dataTableOutput("predictedXGBPricesTable"),
                                 plotOutput("predictXGBChart")
                                 )
                                 )
                         )   
                      ),
             
         
         
             tabPanel("Predict Rentability", 
                      fluidPage(
                        fluidRow(
                          column(4,
                                 numericInput("prob_price", label = h5("Rental price per night ¥"), value = 10000),
                                 actionButton("predictprob", "Predict now!")
                                 
                          ),
       
                        column(9, 
                        h4('_______________________________________________________________________________________________________'),
                        h4('Your predicted probability of your property getting rented'),
                        h5(textOutput("predict_rentability")),    
                        plotOutput("predict_Rentability_Chart"),
                        dataTableOutput("Predict_Rentability_Table")
                    
                        )
                       ))),
             
              tabPanel("Market Insights",
                    fluidPage(  
                        
                        fluidRow(
                          column(6, 
                             br(),
                             h4("Market Insights"),
                             h4("______________________________________________________________________________________________________"),
                             br(),
                             h4(strong(htmlOutput("insightNeigh"))),
                             br(),
                             strong("Hosts in your neighbourhood"),
                             br(),
                             h5(htmlOutput("insightHost")),
                             br(),
                             h5(htmlOutput("insightprop")),
                             br(),
                             h5(htmlOutput("insightbed")),
                             br(),
                             h5(htmlOutput("insightrent")),
                             br(),
                             h5(htmlOutput("insightnights")),
                             br(),
                             plotOutput("insight_hist_prop_type")
                             ),
                          column(6, 
                             br(),
                             br(),
                             br(),
                             br(),
                             br(),
                             br(),
                             br(),
                             br(),
                             br(),
                             strong("Super Hosts in your neighbourhood"),
                             br(),
                             h5(htmlOutput("insightsuperhost")),
                             br(),
                             h5(htmlOutput("insighttopamenities")),
                             br(),
                             h5(htmlOutput("insightamenities")),
                             br(),
                             h5(htmlOutput("insightresponse")),
                             br(),
                             h5(htmlOutput("insightsuperHprice")),
                             br(),
                             plotOutput("insight_hist_response_time_superH")
                                 
                            )
                       )
                      
                         
                       
                    )
              ),
             
             tabPanel("Recommendation", 
                      fluidRow(
                        column(12, 
                               h4(strong("Recommendations")),
                               br(),
                               h4("Here are few tips for you to increase your listing price and chance to rent your your listing!"),
                               br(),
                               h4(htmlOutput("bedrecommend")),
                               br(),
                               h4(htmlOutput("internetrecommend")),
                               br(),
                               h4(htmlOutput("washerrecommend")),
                               br(),
                               h4(htmlOutput("tvrecommend")),
                               br(),
                               h4(htmlOutput("responserecommend")),
                               br(),
                               h4(htmlOutput("cancelrecommend")),
                               br(),
                               h4(htmlOutput("cleaningrecommend")),
                               br(),
                               h4(htmlOutput("securityrecommend")),
                               br(),
                               h4(htmlOutput("extraGrecommend")),
                               br(),
                               textAreaInput("listingname", label = h5("Name of property listing"), width = '200%', rows = 1),
                               br(),
                               actionButton("analysenow", "Check your sentimental score"),
                               br(),
                               h4(textOutput("analysisName")),
                               br(),
                               br(),
                               h5(textOutput("accomText")),
                               br()
                               )
                      ),
                      fluidRow(
                        column(9,
                               plotOutput("OccupancyChart"))


                      )
                      )
             
     
             
  ))
