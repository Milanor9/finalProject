#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# graph by month:


library(shiny)
library(dplyr)
library(DT)
library(ggplot2)
library(leaflet)

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()
trains <- read.csv("full_trains.csv", sep = ",")
flights <- read.csv("flights.csv", sep=",", nrows=10000)
airports2 <- read.csv("airports.csv", sep=",")

trains$num_of_trains_carried_out <- with(trains, total_num_trips-num_of_canceled_trains)


# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("Trains and Flights Dashboard"),
    sidebarLayout(
        sidebarPanel(
            uiOutput("type"),
            uiOutput("aggregation"),
            uiOutput("station"),
            uiOutput("vy"),
            uiOutput("map"),
        ),
        mainPanel(
            plotOutput("p"),
            leafletOutput("mymap")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    #Make the input with all stations and distinct departure station
    all<-data.frame("ALL STATIONS")
    names(all)<-c("departure_station")
    stations <- trains %>%
        select(departure_station) %>%
        distinct()
    stations <- rbind(all, stations)
    
    all<-data.frame("ALL AIRPORTS")
    names(all)<-c("ORIGIN_AIRPORT")
    airports <- flights %>%
        select(ORIGIN_AIRPORT) %>%
        distinct()
    airports <- rbind(all, airports)
    
    
    
    output$type <- renderUI({
        selectInput("type", "Choose the transportation", choices = c("Trains", "Planes"))
    })
    
    type<-reactive({input$type})
    
    output$aggregation <- renderUI({
        if (type() == "Trains"){
            selectInput("aggregation", "Select the value aggregated", choices = c("Per year", "Per station"))
        }
        else {
            selectInput("aggregation", "Select the value aggregated", choices = c("Per airline", "Per airport"))
        }
    })
    
    aggregation<-reactive({input$aggregation})
    
    output$station <- renderUI({
        if (type() == "Trains" & aggregation()== "Per year"){
            selectInput("station", "Select the departure station", choices = stations)
        } 
        else if (aggregation()== "Per airline"){
            selectInput("station", "Select the origin airport", choices = airports)
        }
    })
    
    output$vy <- renderUI({
        if (type() == "Trains"){
            selectInput("variabley", "Select the Y variable", choices = c("num_of_trains_carried_out","num_late_at_departure", "num_arriving_late", "percentage_delayed_at_departure", "percentage_delayed_at_arrival", "avg_delay_departure_of_all_trains", "avg_delay_arrival_of_all_trains", "num_of_canceled_trains", "percentage_of_canceled_trains"))
        }
        else if (type() == "Planes"){
            selectInput("variabley", "Select the Y variable", choices = c("Number of flights","Number of delayed flights", "Average flight duration", "Total distance covered by airline flights", "Average departure delay", "Average arrival delay"))
        }
    })
    
    output$map <- renderUI({
        if (type() == "Planes"){
            checkboxInput("checkbox", "Map", FALSE)
        }
    })
    
    x<-reactive({input$station})
    
    
    y<-reactive({input$variabley})
    
    output$p<-renderPlot({
        if (type() == "Trains"){
            if (x() == "ALL STATIONS"){
                train_data <- trains
            } 
            else{
                train_data <- trains[trains[, "departure_station"] == input$station,]
            }
            
            if (aggregation()=="Per year"){
            
                if(y() == "num_of_trains_carried_out"){
                    train_data %>% 
                        group_by(year) %>% 
                        mutate(num_of_trains_carried_out = (sum(num_of_trains_carried_out, na.rm = T))) %>% 
                        ggplot(aes(x=year, y=num_of_trains_carried_out))+ geom_point(aes(group=year))+stat_summary(geom = "line")
                }
                else if(y() == "num_late_at_departure"){
                    train_data %>% 
                        group_by(year) %>% 
                        mutate(num_late_at_departure = (sum(num_late_at_departure, na.rm = T))) %>% 
                        ggplot(aes(x=year, y=num_late_at_departure))+ geom_point(aes(group=year))+stat_summary(geom = "line")
                }
                else if(y() == "num_arriving_late"){
                    train_data %>% 
                        group_by(year) %>% 
                        mutate(num_arriving_late = (sum(num_arriving_late, na.rm = T))) %>% 
                        ggplot(aes(x=year, y=num_arriving_late))+ geom_point(aes(group=year))+stat_summary(geom = "line")
                }
                else if(y() == "percentage_delayed_at_departure"){
                    train_data %>% 
                        group_by(year) %>% 
                        mutate(percentage_delayed_at_departure = (sum(num_late_at_departure, na.rm = T)/sum(num_of_trains_carried_out, na.rm = T))*100) %>% 
                        ggplot(aes(x=year, y=percentage_delayed_at_departure))+ geom_point(aes(group=year))+stat_summary(geom = "line")
                }
                else if(y() == "percentage_delayed_at_arrival"){
                    train_data %>% 
                        group_by(year) %>% 
                        mutate(percentage_delayed_at_arrival = (sum(num_arriving_late, na.rm = T)/sum(num_of_trains_carried_out, na.rm = T))*100) %>% 
                        ggplot(aes(x=year, y=percentage_delayed_at_arrival))+
                        geom_point(aes(group=year))+stat_summary(geom = "line")
                }
                else if(y() == "avg_delay_departure_of_all_trains"){
                    train_data %>% 
                        group_by(year) %>% 
                        mutate(avg_delay_departure_of_all_trains = sum(avg_delay_all_departing * num_of_trains_carried_out , na.rm = T)/sum(num_of_trains_carried_out, na.rm = T)) %>% 
                        ggplot(aes(x=year, y=avg_delay_departure_of_all_trains))+
                        geom_point(aes(group=year))+stat_summary(geom = "line")
                }
                else if(y() == "avg_delay_arrival_of_all_trains"){
                    train_data %>% 
                        group_by(year) %>% 
                        mutate(avg_delay_arrival_of_all_trains = sum(avg_delay_all_arriving * num_of_trains_carried_out , na.rm = T)/sum(num_of_trains_carried_out, na.rm = T)) %>% 
                        ggplot(aes(x=year, y=avg_delay_arrival_of_all_trains))+
                        geom_point(aes(group=year))+stat_summary(geom = "line")
                }
                else if(y() == "num_of_canceled_trains"){
                    train_data %>% 
                        group_by(year) %>% 
                        mutate(num_of_canceled_trains = (sum(num_of_canceled_trains, na.rm = T))) %>% 
                        ggplot(aes(x=year, y=num_of_canceled_trains))+ geom_point(aes(group=year))+stat_summary(geom = "line")
                }
                else if(y() == "percentage_of_canceled_trains"){
                    train_data %>% 
                        group_by(year) %>% 
                        mutate(percentage_of_canceled_trains = (sum(num_of_canceled_trains, na.rm = T)/sum(total_num_trips, na.rm = T))*100) %>% 
                        ggplot(aes(x=year, y=percentage_of_canceled_trains))+
                        geom_point(aes(group=year))+stat_summary(geom = "line")
                }
            }
            else{
                if(y() == "num_of_trains_carried_out"){
                    trains %>% 
                        group_by(departure_station) %>% 
                        mutate(num_of_trains_carried_out = (sum(num_of_trains_carried_out, na.rm = T))) %>% 
                        ggplot(aes(x=departure_station, y=num_of_trains_carried_out))+
                        geom_bar(stat = "identity") +theme(axis.text.x=element_text(angle = -90, hjust = 0))
                }
                else if(y() == "num_late_at_departure"){
                    trains %>% 
                        group_by(departure_station) %>% 
                        mutate(num_late_at_departure = (sum(num_late_at_departure, na.rm = T))) %>% 
                        ggplot(aes(x=departure_station, y=num_late_at_departure))+
                        geom_bar(stat = "identity") +theme(axis.text.x=element_text(angle = -90, hjust = 0))
                }
                else if(y() == "num_arriving_late"){
                    trains %>% 
                        group_by(departure_station) %>% 
                        mutate(num_arriving_late = (sum(num_arriving_late, na.rm = T))) %>% 
                        ggplot(aes(x=departure_station, y=num_arriving_late))+
                        geom_bar(stat = "identity") +theme(axis.text.x=element_text(angle = -90, hjust = 0))
                }
                else if(y() == "percentage_delayed_at_departure"){
                    trains %>% 
                        group_by(departure_station) %>% 
                        mutate(percentage_delayed_at_departure = (sum(num_late_at_departure, na.rm = T)/sum(num_of_trains_carried_out, na.rm = T))*100) %>% 
                        ggplot(aes(x=departure_station, y=percentage_delayed_at_departure))+
                        geom_bar(stat = "identity") +theme(axis.text.x=element_text(angle = -90, hjust = 0))
                }
                else if(y() == "percentage_delayed_at_arrival"){
                    trains %>% 
                        group_by(departure_station) %>% 
                        mutate(percentage_delayed_at_arrival = (sum(num_arriving_late, na.rm = T)/sum(num_of_trains_carried_out, na.rm = T))*100) %>% 
                        ggplot(aes(x=departure_station, y=percentage_delayed_at_arrival))+
                        geom_bar(stat = "identity") +theme(axis.text.x=element_text(angle = -90, hjust = 0))
                }
                else if(y() == "avg_delay_departure_of_all_trains"){
                    trains %>% 
                        group_by(departure_station) %>% 
                        mutate(avg_delay_departure_of_all_trains = sum(avg_delay_all_departing * num_of_trains_carried_out , na.rm = T)/sum(num_of_trains_carried_out, na.rm = T)) %>% 
                        ggplot(aes(x=departure_station, y=avg_delay_departure_of_all_trains))+
                        geom_bar(stat = "identity") +theme(axis.text.x=element_text(angle = -90, hjust = 0))
                }
                else if(y() == "avg_delay_arrival_of_all_trains"){
                    trains %>% 
                        group_by(departure_station) %>% 
                        mutate(avg_delay_arrival_of_all_trains = sum(avg_delay_all_arriving * num_of_trains_carried_out , na.rm = T)/sum(num_of_trains_carried_out, na.rm = T)) %>% 
                        ggplot(aes(x=departure_station, y=avg_delay_arrival_of_all_trains))+
                        geom_bar(stat = "identity") +theme(axis.text.x=element_text(angle = -90, hjust = 0))
                }
                else if(y() == "num_of_canceled_trains"){
                    trains %>% 
                        group_by(departure_station) %>% 
                        mutate(num_of_canceled_trains = (sum(num_of_canceled_trains, na.rm = T))) %>% 
                        ggplot(aes(x=departure_station, y=num_of_canceled_trains))+
                        geom_bar(stat = "identity") +theme(axis.text.x=element_text(angle = -90, hjust = 0))
                }
                else if(y() == "percentage_of_canceled_trains"){
                    trains %>% 
                        group_by(departure_station) %>% 
                        mutate(percentage_of_canceled_trains = (sum(num_of_canceled_trains, na.rm = T)/sum(total_num_trips, na.rm = T))*100) %>% 
                        ggplot(aes(x=departure_station, y=percentage_of_canceled_trains))+
                        geom_bar(stat = "identity") +theme(axis.text.x=element_text(angle = -90, hjust = 0))
                }
            }
        }
        
        
        else if (type() == "Planes"){
            if (x() == "ALL AIRPORTS"){
                plane_data <- flights
            } 
            else{
                plane_data <- flights[flights[, "ORIGIN_AIRPORT"] == input$station,]
            }
            
            if (aggregation()== "Per airline"){
                if(y() == "Number of flights"){
                    plane_data %>%
                        group_by(AIRLINE) %>%
                        summarise(cnt = n()) %>%
                        ggplot(aes(x = (AIRLINE), y = cnt)) +
                        geom_bar(stat = "identity") +
                        theme(axis.text.x=element_text(angle = -90, hjust = 0))
                }
                else if(y() == "Number of delayed flights"){
                    departure_delay<-plane_data[plane_data[, "DEPARTURE_DELAY"] < 0,]
                    
                    departure_delay %>%
                        group_by(AIRLINE) %>%
                        summarise(cnt = n()) %>%
                        ggplot(aes(x = (AIRLINE), y = cnt)) +
                        geom_bar(stat = "identity") +
                        theme(axis.text.x=element_text(angle = -90, hjust = 0))
                }
                else if(y() == "Average flight duration"){
                    plane_data %>%
                        group_by(AIRLINE) %>%
                        summarize(average = mean(AIR_TIME, na.rm=TRUE)) %>%
                        ggplot(aes(x = (AIRLINE), y = average)) +
                        geom_bar(stat = "identity") +
                        theme(axis.text.x=element_text(angle = -90, hjust = 0))
                }
                else if(y() == "Total distance covered by airline flights"){
                    plane_data %>%
                        group_by(AIRLINE) %>%
                        mutate(average = sum(DISTANCE , na.rm = T)) %>% 
                        ggplot(aes(x = (AIRLINE), y = average)) +
                        geom_bar(stat = "identity") +
                        theme(axis.text.x=element_text(angle = -90, hjust = 0))
                }
                else if(y() == "Average departure delay"){
                    plane_data %>%
                        group_by(AIRLINE) %>%
                        summarize(average = mean(DEPARTURE_DELAY, na.rm=TRUE)) %>%
                        ggplot(aes(x = (AIRLINE), y = average)) +
                        geom_bar(stat = "identity") +
                        theme(axis.text.x=element_text(angle = -90, hjust = 0))
                }
                else if (y() == "Average arrival delay"){
                    plane_data %>%
                        group_by(AIRLINE) %>%
                        summarize(average = mean(ARRIVAL_DELAY, na.rm=TRUE)) %>%
                        ggplot(aes(x = (AIRLINE), y = average)) +
                        geom_bar(stat = "identity") +
                        theme(axis.text.x=element_text(angle = -90, hjust = 0))
                }
            }
            else{
                if(y() == "Number of flights"){
                    flights %>%
                        group_by(ORIGIN_AIRPORT) %>%
                        summarise(cnt = n()) %>%
                        ggplot(aes(x = (ORIGIN_AIRPORT), y = cnt)) +
                        geom_bar(stat = "identity") +
                        theme(axis.text.x=element_text(angle = -90, hjust = 0))
                }
                else if(y() == "Number of delayed flights"){
                    departure_delay<-flights[flights[, "DEPARTURE_DELAY"] < 0,]
                    
                    departure_delay %>%
                        group_by(ORIGIN_AIRPORT) %>%
                        summarise(cnt = n()) %>%
                        ggplot(aes(x = (ORIGIN_AIRPORT), y = cnt)) +
                        geom_bar(stat = "identity") +
                        theme(axis.text.x=element_text(angle = -90, hjust = 0))+
                        flip_coord()
                }
                else if(y() == "Average flight duration"){
                    flights %>%
                        group_by(ORIGIN_AIRPORT) %>%
                        summarize(average = mean(AIR_TIME, na.rm=TRUE)) %>%
                        ggplot(aes(x = (ORIGIN_AIRPORT), y = average)) +
                        geom_bar(stat = "identity") +
                        theme(axis.text.x=element_text(angle = -90, hjust = 0))
                }
                else if(y() == "Total distance covered by ORIGIN_AIRPORT flights"){
                    flights %>%
                        group_by(ORIGIN_AIRPORT) %>%
                        mutate(average = sum(DISTANCE , na.rm = T)) %>% 
                        ggplot(aes(x = (ORIGIN_AIRPORT), y = average)) +
                        geom_bar(stat = "identity") +
                        theme(axis.text.x=element_text(angle = -90, hjust = 0))
                }
                else if(y() == "Average departure delay"){
                    flights %>%
                        group_by(ORIGIN_AIRPORT) %>%
                        summarize(average = mean(DEPARTURE_DELAY, na.rm=TRUE)) %>%
                        ggplot(aes(x = (ORIGIN_AIRPORT), y = average)) +
                        geom_bar(stat = "identity") +
                        theme(axis.text.x=element_text(angle = -90, hjust = 0))
                }
                else if (y() == "Average arrival delay"){
                    flights %>%
                        group_by(ORIGIN_AIRPORT) %>%
                        summarize(average = mean(ARRIVAL_DELAY, na.rm=TRUE)) %>%
                        ggplot(aes(x = (ORIGIN_AIRPORT), y = average)) +
                        geom_bar(stat = "identity") +
                        theme(axis.text.x=element_text(angle = -90, hjust = 0))
                }
            }
            
        }
        
    })
    
    map<-reactive({input$checkbox})
    
    output$mymap <- renderLeaflet({
        if (type() == "Planes"){
            if(map()=="TRUE"){
                if (x() == "ALL AIRPORTS" | aggregation()=="Per airport"){
                    lat <- airports2 %>% select(LATITUDE)
                    long <- airports2 %>% select(LONGITUDE) 
                }
                else{
                    lat <- airports2 %>% filter(IATA_CODE == x()) %>% select(LATITUDE)
                    long <- airports2 %>% filter(IATA_CODE == x()) %>% select(LONGITUDE) 
                }
                points <- eventReactive(input$recalc, {
                    cbind(lat, long)
                }, ignoreNULL = FALSE)
                
                
                leaflet() %>%
                    addProviderTiles(providers$Stamen.TonerLite,
                                     options = providerTileOptions(noWrap = TRUE)
                    ) %>%
                    addMarkers(data = points())
            }
        }
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
