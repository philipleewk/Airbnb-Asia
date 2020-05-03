################################################################
#  Tokyo Airbnb version 6 as of 3 May 2020                     #
#  MSBA 2020 Capstone project - Team Asia Airbnb               # 
################################################################
library(readr)
library(shiny)
library(leaflet)
library(dplyr)
library(ggplot2)
library(mlr)
library(xgboost)
library(cowplot)
library(e1071)
library(sjmisc)
library(SentimentAnalysis)
library(randomForest)
library(ngram)
library(readxl)
library(SnowballC)


listings <- readRDS("Master_listings_walk_hotel_airbnb_cleaned_amenities_reviewscore_gender_201903.rda")
tokyo_neighbourhood <- read_csv("listings_neighbourhood_walk_hotel_airbnb_new.csv")

shinyServer(function(input, output, session) {

  room_type <- c("Entire home/apt", "Private room", "Shared room")
  
  groupColors <- colorFactor(c("red", "blue","black"),
  domain = c("Entire home/apt", "Private room","Shared room"))  

#############################################################
#  Create prediction list function                          #  
#############################################################
  
  create_prediction_list <- function(predictlist, testinglist) {
    predictlist[1, 'id'] <- 999999999
  
    tokyo_neigh <- read_excel("neighbourhood_district_mapping.xlsx", sheet = "Mapping")
    district_row <- match(input$neigh1, tokyo_neigh$neighbourhood)
    district_select <- tokyo_neigh[district_row, 'district']
    
    predictlist[1, 'district'] <- district_select
    
    hotel_property <- c('Aparthotel','Boutique hotel','Hotel')
    hostel_property <- c('Hostel','Guesthouse')
    common_property <- c('Hostel','Hotel','Apartment','House')
    
  
    if (input$property %in% hotel_property) {
      predictlist[1, 'property_type'] <- 'Hotel' 
    } else if (input$property %in% hostel_property) {
      predictlist[1, 'property_type'] <- 'Hostel'
    } else if (!(input$property %in% common_property)) {
      predictlist[1, 'property_type'] <- 'Others'
    }
    
    if (input$roomType == 'Hotel room') {
      predictlist[1, 'room_type'] <- 'Private room'
    } else {
      predictlist[1, 'room_type'] <- input$roomType
    }
    
    if (input$bedType == "Couch") {
      predictlist[1, 'bed_type'] <- 'Pull-out Sofa'
    } else {
      predictlist[1, 'bed_type'] <- input$bedType
    }
    
    if (input$cancelPolicy == 'super_strict_30' | 
        input$cancelPolicy == 'super_strict_60' |
        input$cancelPolicy == 'strict_14_with_grace_period') {
      predictlist[1, 'cancellation_policy'] <-  'strict'
    } else {  
      predictlist[1, 'cancellation_policy'] <- input$cancelPolicy
    }
    
    
    predictlist[1, 'roomtype.factor'] <- input$roomType
    predictlist[1, 'accommodates'] <- input$accommo
    predictlist[1, 'bedrooms'] <- input$numBedrooms
    predictlist[1, 'bathrooms'] <- input$numBathrooms
    predictlist[1, 'host_listings_count'] <- input$hostListings
    predictlist[1, 'host_response_time'] <- input$hostResponse
    predictlist[1, 'instant_bookable'] <- input$instantBook
    predictlist[1, 'Security_deposit'] <- input$securityDeposit
    predictlist[1, 'Extra_people'] <- input$extraGuests
    predictlist[1, 'Cleaning_fee'] <- input$cleaningFees
    predictlist[1, 'host_gender.factor'] <- input$hostGender
    predictlist[1, 'minimum_nights'] <- input$minimumNights
    predictlist[1, 'maximum_nights'] <- input$maximumNights
    predictlist[1, 'count_amenities'] <- input$numAmenities
    
    predictlist[1, 'flag_aircon.factor'] <- 0
    predictlist[1, 'flag_cooking.factor'] <- 0
    predictlist[1, 'flag_dryer.factor'] <- 0
    predictlist[1, 'flag_internet.factor'] <- 0
    predictlist[1, 'flag_safety.factor'] <- 0
    predictlist[1, 'flag_tv.factor'] <- 0
    predictlist[1, 'flag_washer.factor'] <- 0
    
    amenities_list <- input$amenitiesGroup
    key_amenities <- 0 
    if (str_contains(amenities_list,"1")) {
      predictlist[1, 'flag_aircon.factor'] <- 1
      key_amenities = key_amenities + 1
    }  
    if (str_contains(amenities_list,"2")) {
      predictlist[1, 'flag_cooking.factor'] <- 1
      key_amenities = key_amenities + 1
    } 
    if (str_contains(amenities_list,"3")) {
      predictlist[1, 'flag_dryer.factor'] <- 1
      key_amenities = key_amenities + 1
    } 
    if (str_contains(amenities_list,"4")) {
      predictlist[1, 'flag_internet.factor'] <- 1
      key_amenities = key_amenities + 1
    } 
    if (str_contains(amenities_list,"5")) {
      predictlist[1, 'flag_safety.factor'] <- 1
      key_amenities = key_amenities + 1
    } 
    if (str_contains(amenities_list,"6")) {
      predictlist[1, 'flag_tv.factor'] <- 1
      key_amenities = key_amenities + 1
    } 
    if (str_contains(amenities_list,"7")) {
      predictlist[1, 'flag_washer.factor'] <- 1
      key_amenities = key_amenities + 1
    } 
    
    predictlist[1, 'count_key_amenities'] <- key_amenities
    
    input_neighbourhood <- input$neigh1
    input_neighbourhood <- strsplit(input_neighbourhood, " ")
    inputneigh <- unlist(input_neighbourhood)
    inputneigh[1]
    
    for (neigh in 1:nrow(tokyo_neighbourhood)) {
      tokyo_neigh <- as.character(tokyo_neighbourhood[neigh, 'Neighbourhood'])
      toyko_neigh <- strsplit(tokyo_neigh, " ")
      tokyo_neigh_split <- unlist(toyko_neigh)
      
      if (inputneigh[1] == tokyo_neigh_split[1]) {
        predictlist[1, 'Nearest_tourist_dist'] <- tokyo_neighbourhood[neigh,"Nearest_tourist_dist"]
        predictlist[1, 'Nearest_metro_dist'] <- tokyo_neighbourhood[neigh,"Nearest_metro_dist"]
        predictlist[1, 'walking_tourist_dist'] <- tokyo_neighbourhood[neigh,"walking_tourist_dist"]
        predictlist[1, 'walking_metro_dist'] <- tokyo_neighbourhood[neigh,"walking_metro_dist"] 
        predictlist[1, 'average_price'] <- tokyo_neighbourhood[neigh,"average_price"] 
        predictlist[1, 'average_stars'] <- tokyo_neighbourhood[neigh,"average_stars"] 
        predictlist[1, 'average_airbnb_price'] <- tokyo_neighbourhood[neigh,"average_airbnb_price"] 
        predictlist[1, 'number_hotels'] <- tokyo_neighbourhood[neigh,"number_hotels"] 
        predictlist[1, 'number_airbnb_listing'] <- tokyo_neighbourhood[neigh,"number_airbnb_listing"]
      }  
    }
   
    #mean for review length to review subjectivity
    predictlist[1,60:62] <- apply(testinglist[,60:62], 2, FUN=mean, na.rm=TRUE)
    #mean of the length 
    predictlist[1,63:70] <- apply(testinglist[,63:70], 2, FUN=mean, na.rm=TRUE)
    #mean of the polarity and subjectivity
    predictlist[1,71:80] <- apply(testinglist[,71:80], 2, FUN=mean, na.rm=TRUE)
    predictlist[1,83:86] <- apply(testinglist[,83:86], 2, FUN=mean, na.rm=TRUE)
    
    predictlist[1, 'reviews_per_month'] <- mean(listings$reviews_per_month, na.rm=TRUE)
    predictlist[1, 'number_of_reviews'] <- mean(listings$number_of_reviews, na.rm=TRUE)
    predictlist[1, 'host_days'] <- 0  
    return(predictlist)
  }
  
  plot_graph <- function(predicted_Price_df, min_s, max_s, model) {
    predictedprice_chart <- ggplot(predicted_Price_df, aes(x = Month, y = Predicted_Price,group=1)) +
      geom_point()+geom_line(color="red") +
      xlab("Month") +
      ylab("Prices") +
      ggtitle(paste0("Tokyo Airbnb predicted rental price by month ", model)) +
      ylim(min_s, max_s) + 
      theme_bw() +
      theme(panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(color = "lightgray"),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank()) 
    return(predictedprice_chart)
  } 
  
  # create the map
  output$map <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles("Esri.WorldStreetMap") %>%
      addLegend(position = "bottomleft", pal = groupColors, values = room_type, opacity = 1, title = "Room Type") %>% 
      setView(lng = 139.7121, lat = 35.6884, zoom = 12)
  })

  #  Markets insights 
  output$insightNeigh <- renderText({
    paste0("Neighborhood ", "<font color=\"#FF0000\"><b>", input$neigh1, "</b></font>")
  })
  
  output$insightHost <- renderText({
    hostlistings <- subset(listings, neighbourhood_cleansed == input$neigh1)  
    numbhost <- length(unique(unlist(hostlistings$host_id)))
    paste0("* Number of hosts in your neighbourhood : ", "<font color=\"#FF0000\"><b>", numbhost, "</b></font>")
  })
  
  output$insightprop <- renderText({
    hostlistings <- subset(listings, neighbourhood_cleansed == input$neigh1)  
    propertytype <- tail(names(sort(table(hostlistings$property_type))), 1)
    paste0("* Most common property type : ", "<font color=\"#FF0000\"><b>", propertytype, "</b></font>")
  })
  
  output$insightbed <- renderText({
    hostlistings <- subset(listings, neighbourhood_cleansed == input$neigh1)  
    bednum <- tail(names(sort(table(hostlistings$bedrooms))), 1)
    paste0("* Most common number of bedrooms rented : ", "<font color=\"#FF0000\"><b>", bednum, "</b></font>")
  })
  
  output$insightrent <- renderText({
    hostlistings <- subset(listings, neighbourhood_cleansed == input$neigh1)  
    rent <- mean(hostlistings$Price)
    paste0("* Average price per night : ", "<font color=\"#FF0000\"><b>", round(rent,0), " ¥", "</b></font>")
  })
  
  output$insightnights <- renderText({
    hostlistings <- subset(listings, neighbourhood_cleansed == input$neigh1)  
    min_nights <- mean(hostlistings$minimum_nights, na.rm=TRUE)
    paste0("* Average minimum nights stay requirement : ", "<font color=\"#FF0000\"><b>", round(min_nights,0), " days", "</b></font>")
  })
  
  output$insight_hist_prop_type <- renderPlot ({
    hostlistings <- subset(listings, neighbourhood_cleansed == input$neigh1) 
    group_prop <- hostlistings %>% 
      group_by(proptype.factor) %>% 
      summarise(countprop = n())
    
    group_prop <- group_prop[with(group_prop,order(-countprop)),]
    
    group_prop <- group_prop[1:3,]
    
    ggplot(group_prop) +
      geom_col(aes(x=reorder(proptype.factor, -countprop), y=countprop),fill="deepskyblue",width = 0.7) +
      labs(y="Number of listing", x = "Property type") +
      ggtitle("Top 3 Property types") +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position="none") +
     # theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme(panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(color = "lightgray"),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank()) 
  })   
  
 
  output$insightamenities <- renderText({
    hostlistings <- subset(listings, neighbourhood_cleansed ==  input$neigh1 & host_is_superhost == 'TRUE') 
    avg_amenities <- mean(hostlistings$count_amenities)
    paste0("* Average number of amenities provided : ", "<font color=\"#FF0000\"><b>", round(avg_amenities,0), "</b></font>")
  })
  
  output$insightresponse <- renderText({
    hostlistings <- subset(listings, neighbourhood_cleansed ==  input$neigh1 & host_is_superhost == 'TRUE') 
    Host_response <- tail(names(sort(table(hostlistings$host_response_time))), 1)
    paste0("* Most common response time : ", "<font color=\"#FF0000\"><b>", Host_response, "</b></font>")
  })
  
  output$insightsuperhost <- renderText({
    hostlistings <- subset(listings, neighbourhood_cleansed == input$neigh1 & host_is_superhost == 'TRUE') 
    superhost <- length(unique(unlist(hostlistings$host_id)))
    paste0("* Number of superhost in your neighbourhood : ", "<font color=\"#FF0000\"><b>", superhost, "</b></font>")
  })
  
  output$insightsuperHprice <- renderText({
    hostlistings <- subset(listings, neighbourhood_cleansed ==  input$neigh1 & host_is_superhost == 'TRUE') 
    superHrent <- mean(hostlistings$Price)
    paste0("* Average price per night : ", "<font color=\"#FF0000\"><b>", round(superHrent,0), " ¥", "</b></font>")
  })
  
  
  output$insighttopamenities <- renderText({
    hostlistings <- subset(listings, neighbourhood_cleansed ==  input$neigh1 & host_is_superhost == 'TRUE')
    
    columns_label <- c("Amenities", "Number")
    amenities_df <- data.frame(matrix(ncol = 2, nrow = 7))
    colnames(amenities_df) <- columns_label
    amenities_df[1, 'Amenities'] <- 'Internet'
    amenities_df[1, 'Number'] <- sum(hostlistings$flag_internet)
    amenities_df[2, 'Amenities'] <- 'aircon'
    amenities_df[2, 'Number'] <- sum(hostlistings$flag_aircon)
    amenities_df[3, 'Amenities'] <- 'tv'
    amenities_df[3, 'Number'] <- sum(hostlistings$flag_tv)
    amenities_df[4, 'Amenities'] <- 'dryer'
    amenities_df[4, 'Number'] <- sum(hostlistings$flag_dryer)
    amenities_df[5, 'Amenities'] <- 'cooking'
    amenities_df[5, 'Number'] <- sum(hostlistings$flag_cooking)
    amenities_df[6, 'Amenities'] <- 'washer'
    amenities_df[6, 'Number'] <- sum(hostlistings$flag_washer)
    amenities_df[7, 'Amenities'] <- 'safety'
    amenities_df[7, 'Number'] <- sum(hostlistings$flag_safety)
    sorted_amenities <- amenities_df[order(-amenities_df$Number),]
    
    top_amenities <- head(sorted_amenities[,'Amenities'],1)
    sec_amenities <- head(sorted_amenities[,'Amenities'],3)[2]
    third_amenities <- head(sorted_amenities[,'Amenities'],3)[3]
    paste0("* Most common amenities provided : ", "<font color=\"#FF0000\"><b>", top_amenities, ", ", sec_amenities, ", ", third_amenities, "</b></font>")
  })
  
  output$insight_hist_response_time_superH <- renderPlot ({
    hostlistings1 <- subset(listings, !(is.na(host_response_time)))
    hostlistings <- subset(hostlistings1, neighbourhood_cleansed ==  input$neigh1 & host_is_superhost == 'TRUE')
    
    unique_rows <- !duplicated(hostlistings[c("host_id")])
    
    hostlistings <- hostlistings[unique_rows,]
    
    group_response <- hostlistings %>% 
      group_by(host_response_time) %>% 
      summarise(countresponse = n())
    
    ggplot(group_response) +
      geom_col(aes(x=reorder(host_response_time, -countresponse), y=countresponse),fill="deepskyblue",width = 0.7) +
      labs(y="Number of listings", x = "Response Time") +
      ggtitle("Distribution of Response Time") +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position="none") +
      theme(panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(color = "lightgray"),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank()) 
  })   
  
  
  output$bedrecommend <- renderText({
    if (input$bedType != 'Real Bed') {
       outstatement <- paste0("* Change your bed to <font color=\"#FF0000\"><b> real bed ", "</b></font>", 
                              "! Customers are willing to pay <font color=\"#FF0000\"><b> 23% ",  "</b></font>", "more per night for a comfy real bed.")
    } else {
       outstatement <- paste0("* Congratulations! You are providing <font color=\"#FF0000\"><b> real bed ", "</b></font>", "! Your predicted price is 23% more.")
     }
       
    paste0(outstatement)
  })
  
  output$internetrecommend <- renderText({
    amenities_recommend <- input$amenitiesGroup
    if (str_contains(amenities_recommend,"4")) {
      outinternetstatement <- paste0("* Congratulations! You are providing <font color=\"#FF0000\"><b> Internet connection ", "</b></font>", "! 
                                     Guests love to stay connected during their vacation!")
    } else {
      outinternetstatement <- paste0("* Provide your guest with great <font color=\"#FF0000\"><b> Internet Connection ",  "</b></font>", "! Guests love to stay connected during their vacation!")
    } 
    paste0(outinternetstatement)
  })
  
  output$washerrecommend <- renderText({
    amenities_recommend <- input$amenitiesGroup
    if (str_contains(amenities_recommend,"3") & str_contains(amenities_recommend, "7")) {
      outwasherstatement <- paste0("* Congratulations! You are providing <font color=\"#FF0000\"><b> Washer and Dryer ", "</b></font>", "! Guests with plenty of laundry would appreciate it!")
    } else {
      outwasherstatement <- paste0("* Provide your guest with a <font color=\"#FF0000\"><b> Washer and Dryer ", "</b></font>", "! Guests with plenty of laundry would appreciate it!")
    } 
    paste0(outwasherstatement)
  })
  
  output$tvrecommend <- renderText({
    amenities_recommend <- input$amenitiesGroup
    if (str_contains(amenities_recommend,"6")) {
      outtvstatement <- paste0("* Congratulations! You are providing <font color=\"#FF0000\"><b> TV ", "</b></font>", "! Netflix on vacation is every guest’s dream come true!")
    } else {
      outtvstatement <- paste0("* Provide your guest with a <font color=\"#FF0000\"><b> TV ", "</b></font>", "! Netflix on vacation is every guest’s dream come true!")
    } 
    paste0(outtvstatement)
  })
  
  output$responserecommend <- renderText({
    if (input$hostResponse != "within an hour") {
      outresponsestatement <- paste0("* Get back to your guest with <font color=\"#FF0000\"><b> Quick Response ", "</b></font>", "! They love having their questions answered within an hour!")
    } else {
      outresponsestatement <- paste0("* Congratulations! You are providing <font color=\"#FF0000\"><b> Quick Response ", "</b></font>", "! They love having their questions answered within an hour!")
    }
    paste0(outresponsestatement)  
  })
  
 
  output$cancelrecommend <- renderText({
    if (input$cancelPolicy != "flexible") {
      outcancelstatement <- paste0("* Consider a more <font color=\"#FF0000\"><b> Flexible Cancellation Policy ", "</b></font>", "! Guests appreciate some date flexibility during their reservation!")
    } else {
      outcancelstatement <- paste0("* Congratulations! You are providing <font color=\"#FF0000\"><b> Flexible Cancellation Policy ", "</b></font>", "! Guests appreciate some date flexibility during their reservation!")
    }
    paste0(outcancelstatement)  
  })
  
  output$cleaningrecommend <- renderText({
    hostlistings <- subset(listings, neighbourhood_cleansed ==  input$neigh1) 
    avg_cleaning <- mean(hostlistings$Cleaning_fee, na.rm=TRUE)
    if (input$cleaningFees > avg_cleaning) {
       paste0("* Try a lower Cleaning Fee! Your <font color=\"#FF0000\"><b> Cleaning Fee ", "</b></font>", 
              "are above the average of ", "<font color=\"#FF0000\"><b>", round(avg_cleaning,0), " ¥", "</b></font>", " per night! ")
    } else {
       paste0("* Congratulations! Your <font color=\"#FF0000\"><b> Cleaning Fee ",  "</b></font>", 
              "is below the average of ", "<font color=\"#FF0000\"><b>", round(avg_cleaning,0), " ¥", "</b></font>", " per night! ")
    }  
  })
  
  output$securityrecommend <- renderText({
    hostlistings <- subset(listings, neighbourhood_cleansed ==  input$neigh1) 
    avg_security <- mean(hostlistings$Security_deposit, na.rm=TRUE)
    if (input$securityDeposit > avg_security) {
      paste0("* Try a lower <font color=\"#FF0000\"><b> Security Deposit ", "</b></font>", 
             "! Your charges are above the average of ", "<font color=\"#FF0000\"><b>", round(avg_security,0), " ¥", "</b></font>")
    } else {
      paste0("* Congratulations! Your <font color=\"#FF0000\"><b> Security Deposit ", "</b></font>", 
             "is below the average of ", "<font color=\"#FF0000\"><b>", round(avg_security,0), " ¥", "</b></font>")
    }  
  })
  
  output$extraGrecommend <- renderText({
    hostlistings <- subset(listings, neighbourhood_cleansed ==  input$neigh1) 
    avg_extraG <- mean(hostlistings$Extra_people, na.rm=TRUE)
    if (input$extraGuests > avg_extraG) {
      paste0("* Try a lower <font color=\"#FF0000\"><b> Extra Guest Fee ", "</b></font>", 
             "! Your charges are above the average of ", "<font color=\"#FF0000\"><b>", round(avg_extraG,0), " ¥", "</b></font>", " per night")
    } else {
      paste0("* Congratulations! Your <font color=\"#FF0000\"><b> Extra Guest Fee ",  "</b></font>", 
             "is below the average of ", "<font color=\"#FF0000\"><b>", round(avg_extraG,0), " ¥", "</b></font>", " per night")
    }  
  })
  
  observe({ #require a trigger to call the observe function
    neighBy <- input$neigh
    neighlistings <- subset(listings, neighbourhood_cleansed == neighBy &
                                      room_type %in% input$select_room & 
                                      Price >= input$slider_price[1] &
                                      Price <= input$slider_price[2])
    
    numbunits <- nrow(neighlistings)
    meanprice <- mean(neighlistings$Price, 2)
    
    proxy <- leafletProxy("map",data = neighlistings) %>%
      clearMarkerClusters() %>% 
      clearMarkers() %>%
      # circle
      addCircleMarkers(lng = ~longitude, lat = ~latitude, radius = 2, color = ~groupColors(room_type),
                       group = "CIRCLE",
                       popup = ~paste('<b><font color="Black">','Listing Information','</font></b><br/>',
                                      'Room Type:', room_type,'<br/>',
                                      'Price:', Price, '¥', '<br/>',
                                      'Rating Score:', review_scores_rating, '<br/>',
                                      'Number of Reviews:', number_of_reviews,'<br/>')) %>% 
      # cluster
      addCircleMarkers(lng = ~longitude, lat = ~latitude, clusterOptions = markerClusterOptions(),
                       group = "CLUSTER",
                       popup = ~paste('<b><font color="Black">','Listing Information','</font></b><br/>',
                                      'Room Type: ', room_type, '<br/>',
                                      'Price:', Price, '¥', '<br/>',
                                      'Rating Score:', review_scores_rating, '<br/>',
                                      'Number of Reviews:', number_of_reviews,'<br/>')) %>% 
      # circle/ cluster panel
      addLayersControl(
        baseGroups = c("CIRCLE","CLUSTER"),
        options = layersControlOptions(collapsed = FALSE)
      ) 
    
    output$summary <- renderText ({
      paste0("Number of units ", numbunits,  " Mean price ", meanprice, " ¥") 
      
    }) 
    
    output$hist_prop_type <- renderPlot ({
      group_prop <- neighlistings %>% 
        group_by(proptype.factor) %>% 
        summarise(countprop = n())
      
      group_prop <- group_prop[with(group_prop,order(-countprop)),]
      
      group_prop <- group_prop[1:5,]
      
      ggplot(group_prop) +
        geom_col(aes(x=reorder(proptype.factor, -countprop), y=countprop),fill="deepskyblue",width = 0.7) +
        labs(y="Number of listing", x = "Property type") +
        ggtitle("Top 5 Property types") +
        theme(plot.title = element_text(hjust = 0.5),
              legend.position="none") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        theme(panel.border = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line = element_line(color = "lightgray"),
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank()) 
     })   
    
    output$hist_room_type <- renderPlot ({
      group_prop <- neighlistings %>% 
        group_by(roomtype.factor) %>% 
        summarise(countroom = n())
      
      ggplot(group_prop) +
        geom_col(aes(x=reorder(roomtype.factor, -countroom), y=countroom),fill="deepskyblue",width = 0.7) +
        labs(y="Number of listing", x = "Room type") +
        ggtitle("Number of listings by Room type") +
        theme(plot.title = element_text(hjust = 0.5),
              legend.position="none") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        theme(panel.border = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line = element_line(color = "white"),
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank()) 
    })    
    
    
  })
     

  observeEvent(input$predictrent, {
    ####################################################
    #  xgboost                                         #
    ####################################################
    listing_file_list <- c("201903", "201904", "201905", "201906", 
                            "201907", "201908", "201909", "201910", "201911")
    
    columns_label <- c("Month", "Predicted_Price")
    xgb_Predicted_Price_df <- data.frame(matrix(ncol = 2, nrow = 0))
    colnames(xgb_Predicted_Price_df) <- columns_label
    
    columns_label_mean <- c("Month", "Mean_Price")
    meanprice_df <- data.frame(matrix(ncol = 2, nrow =0 ))
    colnames(meanprice_df) <- columns_label_mean
    j = 1
    
    for (listing_file_name in listing_file_list) {
      tokyo_neigh <- read_excel("neighbourhood_district_mapping.xlsx", sheet = "Mapping")
      model_name <- paste0("Tokyo_rent_xgb_model_", listing_file_name,".rda")

      Tokyo.xgb.model <-readRDS(model_name)
      
      xgb_listing_name <- paste0("Tokyo_xgb_listings_", listing_file_name, ".rda")
      tokyolisting <- readRDS(xgb_listing_name) 

      predictlist <- tokyolisting[1,]
      predictlist[1, 'id'] <- 999999999

      district_row <- match(input$neigh1, tokyo_neigh$neighbourhood)
      district_select <- tokyo_neigh[district_row, 'district']
      
      predictlist[1, 'district'] <- district_select
      xgb_neigh_subset <- subset(tokyolisting, district == predictlist[1, 'district'])
      
      meanprice_df[j, "Mean_Price"] <- mean(xgb_neigh_subset$Price, na.rm=TRUE)

      hotel_property <- c('Aparthotel','Boutique hotel','Hotel')
      hostel_property <- c('Hostel','Guesthouse')
      common_property <- c('Hostel','Hotel','Apartment','House')
      
      if (input$property %in% hotel_property) {
        predictlist[1, 'property_type'] <- 'Hotel' 
      } else if (input$property %in% hostel_property) {
        predictlist[1, 'property_type'] <- 'Hostel'
      } else if (!(input$property %in% common_property)) {
        predictlist[1, 'property_type'] <- 'Others'
      }

      if (input$roomType == 'Hotel room') {
        predictlist[1, 'room_type'] <- 'Private room'
      } else {
        predictlist[1, 'room_type'] <- input$roomType
      }
      
      if (input$bedType == "Couch") {
        predictlist[1, 'bed_type'] <- 'Pull-out Sofa'
      } else {
        predictlist[1, 'bed_type'] <- input$bedType
      }
      
      if (input$cancelPolicy == 'super_strict_30' | 
          input$cancelPolicy == 'super_strict_60' |
          input$cancelPolicy == 'strict_14_with_grace_period') {
        predictlist[1, 'cancellation_policy'] <-  'strict'
      } else {  
        predictlist[1, 'cancellation_policy'] <- input$cancelPolicy
      }
      
      predictlist[1, 'accommodates'] <- input$accommo
      predictlist[1, 'bedrooms'] <- input$numBedrooms
      predictlist[1, 'bathrooms'] <- input$numBathrooms
      predictlist[1, 'host_listings_count'] <- input$hostListings
      predictlist[1, 'host_response_time'] <- input$hostResponse
      predictlist[1, 'instant_bookable'] <- input$instantBook
      predictlist[1, 'Security_deposit'] <- input$securityDeposit
      predictlist[1, 'Extra_people'] <- input$extraGuests
      predictlist[1, 'Cleaning_fee'] <- input$cleaningFees
      predictlist[1, 'host_gender'] <- input$hostGender
      predictlist[1, 'minimum_nights'] <- input$minimumNights
      predictlist[1, 'maximum_nights'] <- input$maximumNights
      predictlist[1, 'count_amenities'] <- input$numAmenities
      
      predictlist[1, 'flag_aircon'] <- 0
      predictlist[1, 'flag_cooking'] <- 0
      predictlist[1, 'flag_dryer'] <- 0
      predictlist[1, 'flag_internet'] <- 0
      predictlist[1, 'flag_safety'] <- 0
      predictlist[1, 'flag_tv'] <- 0
      predictlist[1, 'flag_washer'] <- 0
      
      amenities_list <- input$amenitiesGroup
      key_amenities <- 0 
      if (str_contains(amenities_list,"1")) {
        predictlist[1, 'flag_aircon'] <- 1
        key_amenities = key_amenities + 1
      }  
      if (str_contains(amenities_list,"2")) {
        predictlist[1, 'flag_cooking'] <- 1
        key_amenities = key_amenities + 1
      } 
      if (str_contains(amenities_list,"3")) {
        predictlist[1, 'flag_dryer'] <- 1
        key_amenities = key_amenities + 1
      } 
      if (str_contains(amenities_list,"4")) {
        predictlist[1, 'flag_internet'] <- 1
        key_amenities = key_amenities + 1
      } 
      if (str_contains(amenities_list,"5")) {
        predictlist[1, 'flag_safety'] <- 1
        key_amenities = key_amenities + 1
      } 
      if (str_contains(amenities_list,"6")) {
        predictlist[1, 'flag_tv'] <- 1
        key_amenities = key_amenities + 1
      } 
      if (str_contains(amenities_list,"7")) {
        predictlist[1, 'flag_washer'] <- 1
        key_amenities = key_amenities + 1
      } 
      
      input_neighbourhood <- input$neigh1
      input_neighbourhood <- strsplit(input_neighbourhood, " ")
      inputneigh <- unlist(input_neighbourhood)
      inputneigh[1]
      
      for (neigh in 1:nrow(tokyo_neighbourhood)) {
        tokyo_neigh <- as.character(tokyo_neighbourhood[neigh, 'Neighbourhood'])
        toyko_neigh <- strsplit(tokyo_neigh, " ")
        tokyo_neigh_split <- unlist(toyko_neigh)
        
        if (inputneigh[1] == tokyo_neigh_split[1]) {
          predictlist[1, 'walking_tourist_dist'] <- tokyo_neighbourhood[neigh,"walking_tourist_dist"]
          predictlist[1, 'walking_metro_dist'] <- tokyo_neighbourhood[neigh,"walking_metro_dist"] 
          predictlist[1, 'average_price'] <- tokyo_neighbourhood[neigh,"average_price"] 
          predictlist[1, 'average_stars'] <- tokyo_neighbourhood[neigh,"average_stars"] 
          predictlist[1, 'average_airbnb_price'] <- tokyo_neighbourhood[neigh,"average_airbnb_price"] 
          predictlist[1, 'number_hotels'] <- tokyo_neighbourhood[neigh,"number_hotels"] 
          predictlist[1, 'number_airbnb_listing'] <- tokyo_neighbourhood[neigh,"number_airbnb_listing"]
          break
        }  
      }
      
      predictlist[1,'review_scores_rating'] <- mean(listings$review_scores_rating, na.rm=TRUE)
      predictlist[1,'review_scores_cleanliness'] <- mean(listings$review_scores_cleanliness, na.rm=TRUE)
      predictlist[1,'review_scores_location'] <- mean(listings$review_scores_location, na.rm=TRUE)
      predictlist[1,'review_scores_communication'] <- mean(listings$review_scores_communication, na.rm=TRUE)
      predictlist[1,'review_scores_value'] <- mean(listings$review_scores_value, na.rm=TRUE)
      predictlist[1,'review_scores_accuracy'] <- mean(listings$review_scores_accuracy, na.rm=TRUE)
      predictlist[1,'availability_90'] <- mean(listings$availability_90, na.rm=TRUE)
      predictlist[1,'Review_Length'] <- mean(listings$Review_Length, na.rm=TRUE)
      predictlist[1,'review_polarity'] <- mean(listings$review_polarity, na.rm=TRUE)
      predictlist[1,'review_subjectivity'] <- mean(listings$review_subjectivity, na.rm=TRUE)
      predictlist[1, 'name_length'] <- mean(listings$name_length, na.rm=TRUE)
      predictlist[1, 'summary_length'] <- mean(listings$summary_length, na.rm=TRUE)   
      predictlist[1, 'space_length'] <- mean(listings$space_length, na.rm=TRUE) 
      predictlist[1, 'description_length'] <- mean(listings$description_length, na.rm=TRUE) 
      predictlist[1, 'neighborhood_overview_length'] <- mean(listings$neighborhood_overview_length, na.rm=TRUE) 
      predictlist[1, 'notes_length'] <- mean(listings$notes_length, na.rm=TRUE)         
      predictlist[1, 'house_rules_length'] <- mean(listings$house_rules_length, na.rm=TRUE) 
      predictlist[1, 'host_about_length'] <- mean(listings$host_about_length, na.rm=TRUE) 
      predictlist[1, 'name_polarity'] <- mean(listings$name_polarity, na.rm=TRUE)
      predictlist[1, 'name_subjectivity'] <- mean(listings$name_subjectivity, na.rm=TRUE)  
      predictlist[1, 'summary_polarity'] <- mean(listings$summary_polarity, na.rm=TRUE)
      predictlist[1, 'summary_subjectivity'] <- mean(listings$summary_subjectivity, na.rm=TRUE)
      
      predictlist[1, 'space_polarity'] <- mean(listings$space_polarity, na.rm=TRUE)
      predictlist[1, 'space_subjectivity'] <- mean(listings$space_subjectivity, na.rm=TRUE)
      predictlist[1, 'description_polarity'] <- mean(listings$description_polarity, na.rm=TRUE)
      predictlist[1, 'description_subjectivity'] <- mean(listings$description_subjectivity, na.rm=TRUE)  
      predictlist[1, 'neigoverview_polarity'] <- mean(listings$neigoverview_polarity, na.rm=TRUE)
      predictlist[1, 'neigoverview_subjectivity'] <- mean(listings$neigoverview_subjectivity, na.rm=TRUE)    
      predictlist[1, 'notes_polarity'] <- mean(listings$notes_polarity, na.rm=TRUE)  
      predictlist[1, 'notes_subjectivity'] <- mean(listings$notes_subjectivity, na.rm=TRUE)   
      predictlist[1, 'hserule_polarity'] <- mean(listings$hserule_polarity, na.rm=TRUE) 
      predictlist[1, 'hserule_subjectivity'] <- mean(listings$hserule_subjectivity, na.rm=TRUE) 
      predictlist[1, 'host_about_polarity'] <- mean(listings$host_about_polarity, na.rm=TRUE) 
      predictlist[1, 'host_about_subjectivity'] <- mean(listings$host_about_subjectivity, na.rm=TRUE)  
      predictlist[1, 'reviews_per_month'] <- mean(listings$reviews_per_month, na.rm=TRUE)
      predictlist[1, 'number_of_reviews'] <- mean(listings$number_of_reviews, na.rm=TRUE)
      predictlist[1, 'listingage'] <- 0 

      tokyolisting <- rbind(tokyolisting, predictlist)
      
      new_tokyolisting <- model.matrix(~.+0,data = tokyolisting)
      df_tokyolisting <- as.data.frame(new_tokyolisting)
      
      set.seed(1234)
      samplelisting <- sample(1:nrow(df_tokyolisting), 4000)
      traininglist <- df_tokyolisting[samplelisting,]
      testinglist <- df_tokyolisting[-samplelisting,]
      
      test_label <- testinglist$Price 
      new_testing <- model.matrix(~.+0,data = testinglist)
      new_testing <- as.data.frame(new_testing)
      new_testing <- subset(new_testing, select=-c(id,Price))
      new_testing <- model.matrix(~.+0,data = new_testing)
      dtesting <- xgb.DMatrix(data = new_testing,label= test_label)
      
      xgb.pred <- predict(Tokyo.xgb.model, dtesting)
      
      predict_label <- df_tokyolisting$Price
      new_prediction <- model.matrix(~.+0,data = df_tokyolisting)
      new_prediction <- as.data.frame(new_prediction)
      new_prediction <- subset(new_prediction, select=-c(id,Price))
      new_prediction <- model.matrix(~.+0,data = new_prediction)
      dtesting <- xgb.DMatrix(data = new_prediction,label= predict_label)
      xgb.prediction <- predict(Tokyo.xgb.model, dtesting)
      xgb_Predicted_Price_df[j, "Predicted_Price"] <- round(tail(xgb.prediction,1),0)
      j = j + 1
    }  
    
    meanprice_df$Month[1] <- 'Mar'
    meanprice_df$Month[2] <- 'Apr'
    meanprice_df$Month[3] <- 'May'
    meanprice_df$Month[4] <- 'Jun'
    meanprice_df$Month[5] <- 'Jul'
    meanprice_df$Month[6] <- 'Aug'
    meanprice_df$Month[7] <- 'Sep'
    meanprice_df$Month[8] <- 'Oct'
    meanprice_df$Month[9] <- 'Nov'
    meanprice_df$Month <- factor(meanprice_df$Month, levels=c("Mar", "Apr","May", "Jun", 
                                                               "Jul", "Aug", "Sep",
                                                               "Oct", "Nov"))
    
    xgb_Predicted_Price_df$Month[1] <- 'Mar'
    xgb_Predicted_Price_df$Month[2] <- 'Apr'
    xgb_Predicted_Price_df$Month[3] <- 'May'
    xgb_Predicted_Price_df$Month[4] <- 'Jun'
    xgb_Predicted_Price_df$Month[5] <- 'Jul'
    xgb_Predicted_Price_df$Month[6] <- 'Aug'
    xgb_Predicted_Price_df$Month[7] <- 'Sep'
    xgb_Predicted_Price_df$Month[8] <- 'Oct'
    xgb_Predicted_Price_df$Month[9] <- 'Nov'
    
    xgb_Predicted_Price_df$Month <- factor(xgb_Predicted_Price_df$Month, levels=c("Mar", "Apr","May", "Jun", 
                                                                                  "Jul", "Aug", "Sep",
                                                                                  "Oct", "Nov"))

    
    min_mean <- min(meanprice_df$Mean_Price)
    max_mean <- max(meanprice_df$Mean_Price)
    min_predicted <- min(xgb_Predicted_Price_df$Predicted_Price)
    max_predicted <- max(xgb_Predicted_Price_df$Predicted_Price)
    
    if (min_mean < min_predicted) {
      min_scale <- min_mean
    } else {
      min_scale <- min_predicted    
    }  
    
    if (max_mean > max_predicted) {
      max_scale <- max_mean
    } else {
      max_scale <- max_predicted    
    }  
    
    xgb_predicted_price_chart <- plot_graph(xgb_Predicted_Price_df, min_scale, max_scale, "XGB") 
    
    meanprice_chart <- ggplot(meanprice_df, aes(x = Month, y = Mean_Price,group=1)) +
      geom_point()+geom_line(color="blue") +
      xlab("Month") +
      ylab("Prices") +
      ggtitle(paste0("Tokyo Airbnb mean rental price by month for ", district_select)) +
      ylim(min_scale, max_scale) +
      theme_bw() +
      theme(panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(color = "lightgray"),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank()) 
    
    xgb_meanpredicted_price <- mean(xgb_Predicted_Price_df$Predicted_Price, na.rm=TRUE)
    saveRDS(xgb_meanpredicted_price, file = "xgb_rental_price.rda")

    output$predict_XGB_Summary <- renderText ({
      paste0("Predicted mean price ", round(xgb_meanpredicted_price, 0), " ¥") 
    })
    
    output$predictXGBChart <- renderPlot ({
      plot_grid(meanprice_chart, xgb_predicted_price_chart, labels = "AUTO")
    })
    
    output$predictedXGBPricesTable <- renderDataTable({
      columns_label <- c("Month", "Predicted Price ¥ (XGB)")
      colnames(xgb_Predicted_Price_df) <- columns_label
      xgb_Predicted_Price_df
    })
    
    
  })   

  observeEvent(input$predictprob, {
    columns_label <- c("Month", "Prob", "Prob_full")
    probit_prob_df <- data.frame(matrix(ncol = 3, nrow = 0))
    colnames(probit_prob_df) <- columns_label
    j = 1
    
    listing_file_list <- c("201903", "201904", "201905", "201906", 
                           "201907", "201908", "201909", "201910", "201911")
    
    testinglist <- readRDS("prob_testing_201903.rda")
    for (listing_file_name in listing_file_list) {
      
      model_name <- paste0("Tokyo_probability_rf_model_", listing_file_name,".rda")
   
      tokyoprobit <-readRDS(model_name)
      predictlist <- testinglist[1,]
      predictionlist <- create_prediction_list(predictlist, testinglist)
      
      predictionlist[1, "Price"] <-  input$prob_price
      
      probit.pred <- predict(tokyoprobit, predictionlist, type = 'prob')
    
      probit_prob_df[j,"Month"] <- listing_file_name
      probit_prob_df[j,"Prob"] <- round(probit.pred, 2) 
      
      model_name <- paste0("Tokyo_probability_tb_model_", listing_file_name,".rda")
      tokyoprobit_full <-readRDS(model_name)
      probit.pred_full <- predict(tokyoprobit_full, predictionlist, type = 'prob')
      probit_prob_df[j,"Prob_full"] <- round(probit.pred_full, 2) 

      j = j + 1
    }  
    
    rentability_meanprob <- mean(probit_prob_df$Prob, na.rm=TRUE)
    full_rentability_meanprob <- mean(probit_prob_df$Prob_full, na.rm=TRUE)
    output$predict_rentability <- renderText ({
       paste0("On average, your  property has a ", round(rentability_meanprob,2) * 100, "% chances being rented out for at least one day, and ",
              round(full_rentability_meanprob,2) * 100, "% chances to be rented for whole month!")
    })
      
    output$Predict_Rentability_Table <- renderDataTable ({
      columns_label <- c("Month", "Chance being rented out at least a day", "Chances being rented out for whole month")
      colnames(probit_prob_df) <- columns_label
      probit_prob_df
    })
    
    probit_prob_df$Month[1] <- 'Mar'
    probit_prob_df$Month[2] <- 'Apr'
    probit_prob_df$Month[3] <- 'May'
    probit_prob_df$Month[4] <- 'Jun'
    probit_prob_df$Month[5] <- 'Jul'
    probit_prob_df$Month[6] <- 'Aug'
    probit_prob_df$Month[7] <- 'Sep'
    probit_prob_df$Month[8] <- 'Oct'
    probit_prob_df$Month[9] <- 'Nov'
    
    probit_prob_df$Month <- factor(probit_prob_df$Month, levels=c("Mar", "Apr","May", "Jun", 
                                                                  "Jul", "Aug", "Sep",
                                                                  "Oct", "Nov"))
    
    prob_chart <- ggplot(probit_prob_df, aes(x = Month, y = Prob,group=1)) +
      geom_point() +
      geom_line(color="blue") +
      xlab("Month") +
      ylab("Probability") +
      ggtitle("Chances being rented out at least a day") +
      ylim(0,1) + 
      theme_bw() +
      theme(panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(color = "lightgray"),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank()) 
    
    prob_full_chart <- ggplot(probit_prob_df, aes(x = Month, y = Prob_full,group=1)) +
      geom_point() +
      geom_line(color="purple") +
      xlab("Month") +
      ylab("Probability") +
      ggtitle("Chances being rented out for whole month") +
      ylim(0,1) + 
      theme_bw() +
      theme(panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(color = "lightgray"),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank()) 
    
    output$predict_Rentability_Chart <- renderPlot ({
      plot_grid(prob_chart, prob_full_chart, labels = "AUTO")
    })
    
  })   
  
  observeEvent(input$analysenow, {
    proplistingname <- input$listingname
    name_sentiment <- analyzeSentiment(proplistingname)
    name_analysisscore <- name_sentiment$SentimentQDAP
    
    recommend_listings <- subset(listings, neighbourhood_cleansed ==  input$neigh1)
    mean_name_length <- mean(recommend_listings$name_length)
    mean_name_polarity <- mean(recommend_listings$name_polarity)
    
    output$analysisName <- renderText ({
      paste0("Your listing name is ", wordcount(proplistingname), " words long with a sentimental score of ", 
             round(name_analysisscore,2), ". Your competitor in your neighbourhood usually has a listing name length of ", 
             round(mean_name_length, 0), " words and sentimental score of ", round(mean_name_polarity,2))

    })

    columns_label <- c("Month", "Mean_Occupancy")
    Occupancy_df <- data.frame(matrix(ncol = 2, nrow = 0))
    colnames(Occupancy_df) <- columns_label
    neigh <- input$neigh1
    
    i = 1
    
    listing_file_list <- c("201904", "201905", "201906", 
                           "201907", "201908", "201909", "201910", "201911")
    
    total_meanprice <- 0 
    total_accom <- 0 
    
    for (listing_file_name in listing_file_list) {
      file_name <- paste0("Master_listings_walk_hotel_airbnb_cleaned_amenities_reviewscore_gender_occupancy_",
                          listing_file_name, ".rda")
      
      tokyolisting <- readRDS(file_name)
      
      listing_subset <- subset(tokyolisting, neighbourhood_cleansed == neigh)
      
      meanprice <- mean(listing_subset$Price, na.rm=TRUE)
      meanaccom <- mean(listing_subset$accommodates)
      total_meanprice <- total_meanprice + meanprice 
      total_accom <- total_accom + meanaccom
      mean_rental <- total_meanprice/8
      mean_accom <- total_accom/8 

      occupancyrate <- mean(listing_subset$occupancy_rate, na.rm=TRUE)
  
      Occupancy_df[i, "Month"] <- listing_file_name
      Occupancy_df[i, "Mean_Occupancy"] <- occupancyrate
      i = i + 1
    }

    Occupancy_df$Month[1] <- 'Apr'
    Occupancy_df$Month[2] <- 'May'
    Occupancy_df$Month[3] <- 'Jun'
    Occupancy_df$Month[4] <- 'Jul'
    Occupancy_df$Month[5] <- 'Aug'
    Occupancy_df$Month[6] <- 'Sep'
    Occupancy_df$Month[7] <- 'Oct'
    Occupancy_df$Month[8] <- 'Nov'
    
    Occupancy_df$Month <- factor(Occupancy_df$Month, levels=c("Apr","May", "Jun", 
                                                              "Jul", "Aug", "Sep",
                                                              "Oct", "Nov"))
    Occupancy_chart <- ggplot(Occupancy_df, aes(x = Month, y = Mean_Occupancy,group=1)) +
      geom_point()+geom_line(color="purple") +
      xlab("Month") +
      ylab("Occupancy Rate") +
      ggtitle(paste0("Tokyo Airbnb occupancy rate for ", neigh)) +
      theme_bw() +
      theme(panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(color = "lightgray"),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank()) 
    
    output$OccupancyChart <- renderPlot ({
      Occupancy_chart
    })
    
    xgb_meanpredicted_price <- readRDS("xgb_rental_price.rda")
    average_costperguest <- xgb_meanpredicted_price/input$accommo
    
    output$accomText <- renderText ({
      paste0("Your property is able to accommodate ", input$accommo, " guest.", " Cost per guest is ", round(average_costperguest,0) , " ¥.",
             " The properties in ", neigh, " is able to accommodate on average ", round(mean_accom,0), " guests.",
             " Cost per guest in ", neigh, " is ", round(mean_rental/mean_accom,0), " ¥.")
    })
    
  })  

})