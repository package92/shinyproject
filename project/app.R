#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(magrittr)


# Define UI for application that draws a histogram
ui <- dashboardPage(
    
    dashboardHeader(title = "Basic dashboard"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
            menuItem("Widgets", tabName = "widgets", icon = icon("th"))
        )
    ),
    dashboardBody(
        # Boxes need to be put in a row (or column)
        tabItems(
            # First tab content
            tabItem(tabName = "dashboard",
                fluidRow(
                    box(selectInput("var", 
                                    label = "Choose a variable for aggregation",
                                    choices = c("year", 
                                                "departure_station"),
                                    selected = "year")),
                    box(title = "Nb of carried out train",
                        plotOutput("query1a")),
                    box(title = "Nb of cancelled train",
                        plotOutput("query1b")),
                    box(title = "Nb of delayed train",
                        plotOutput("query1c")),
                    box(title = "Average number of delayed train at departure",
                        plotOutput("query1d")),
                    box(title = "Average number of delayed train at arrival",
                        plotOutput("query1e")),
                    box(title = " Total average departure delay time of all trains",
                        plotOutput("query1f")),
                    box(title = " Total average arrival delay time of all trains",
                        plotOutput("query1g")),
                    box(title = " Average departure delay time of delayed trains",
                        plotOutput("query1h")),
                    box(title = "Total number of cancelled train",
                        plotOutput("query1i")),
                    box(title = "Proportion of train cancelled",
                        plotOutput("query1k")),
                    box(title = "Proportion of train delayed",
                        plotOutput("query1l"))
                    
                )
            ),
            
            # Second tab content
            tabItem(tabName = "widgets", 
                    h2("Widgets tab content"), 
                    tableOutput(outputId = 'df'),
            )
        )
    )
)





# Define server logic required to draw a histogram
server <- function(input, output) {

    sncf <- read.csv("~/Desktop/Data Analytics/project/french-sncf-trains-regularities/full_trains.csv", sep=,",", quote = "\"'", dec = ".", fill = TRUE,header = TRUE)
    
    output$query1a <- renderPlot({
        agg_train_carried_out = (sncf%>%
            group_by(input$var)%>%
            summarise(
                Number_of_Trains = sum(as.numeric(total_num_trips)))%>%
                filter(Number_of_Trains>10000))
        myvar<-input$var
        ggplot(agg_train_carried_out, aes(x = agg_train_carried_out$myvar, y = Number_of_Trains)) + geom_bar(stat = "identity") + scale_x_discrete(name ="Year")                       
    })
    
    output$query1b <- renderPlot({
        agg_train_cancelled = (sncf%>%
                                     group_by(year)%>%
                                     summarise(
                                         Number_of_Trains = n(),
                                         Number_of_Cancelation = sum(as.numeric(num_of_canceled_trains)))%>%
                                     filter(Number_of_Trains>100))
        ggplot(agg_train_cancelled, aes(x = agg_train_cancelled$year, y = Number_of_Cancelation)) + geom_bar(stat = "identity") + scale_x_discrete(name ="Year")                       
    })
    
    output$query1c <- renderPlot({
        agg_train_delayed = (sncf%>%
                                   group_by(year)%>%
                                   summarise(
                                       Number_of_Trains = n(),
                                       Number_of_Delayed = sum(as.numeric(num_arriving_late),na.rm = TRUE))%>%
                                   filter(Number_of_Trains>100))
        ggplot(agg_train_delayed, aes(x = agg_train_delayed$year, y = Number_of_Delayed)) + geom_bar(stat = "identity") + scale_x_discrete(name ="Year")                       
    })
    
    output$query1d <- renderPlot({
        avg_train_delayed = (sncf%>%
                                 group_by(year)%>%
                                 summarise(
                                     Number_of_Trains = n(),
                                     Avg_delayed = mean(as.numeric(num_late_at_departure),na.rm = TRUE))%>%
                                 filter(Number_of_Trains>100))
        ggplot(avg_train_delayed, aes(x = avg_train_delayed$year, y = Avg_delayed)) + geom_bar(stat = "identity") + scale_x_discrete(name ="Year")                       
    })
    
    output$query1e <- renderPlot({
        avg_train_delayed = (sncf%>%
                                 group_by(year)%>%
                                 summarise(
                                     Number_of_Trains = n(),
                                     Avg_delayed = mean(as.numeric(num_arriving_late),na.rm = TRUE))%>%
                                 filter(Number_of_Trains>100))
        ggplot(avg_train_delayed, aes(x = avg_train_delayed$year, y = Avg_delayed)) + geom_bar(stat = "identity") + scale_x_discrete(name ="Year")                       
    })
    output$query1f <- renderPlot({
        avg_time_delayed = (sncf%>%
                                 group_by(year)%>%
                                 dplyr::summarise(
                                     Number_of_Trains = n(),
                                     Avg_time_delay = mean(as.numeric(avg_delay_all_departing)))%>%
                                 filter(Number_of_Trains>100))
        
        ggplot(avg_time_delayed, aes(x = avg_time_delayed$year, y = Avg_time_delay)) + geom_bar(stat = "identity") + scale_x_discrete(name ="Year")                       
    })
    output$query1g <- renderPlot({
        avg_delay_all_arriv = (sncf%>%
                                 group_by(year)%>%
                                 summarise(
                                     Number_of_Trains = n(),
                                     avg_delay_all = mean(as.numeric(avg_delay_all_arriving),na.rm = TRUE))%>%
                                 filter(Number_of_Trains>100))
        ggplot(avg_delay_all_arriv, aes(x = avg_delay_all_arriv$year, y = avg_delay_all)) + geom_bar(stat = "identity") + scale_x_discrete(name ="Year")                       
    })
    output$query1h <- renderPlot({
        avg_delay_delay_train = (sncf%>%
                                   group_by(year)%>%
                                   summarise(
                                       Number_of_Trains = n(),
                                       moy_del = mean(as.numeric(avg_delay_late_at_departure),na.rm = TRUE))%>%
                                   filter(Number_of_Trains>100))
        ggplot(avg_delay_delay_train, aes(x = avg_delay_delay_train$year, y = moy_del)) + geom_bar(stat = "identity") + scale_x_discrete(name ="Year")                       
    })
    output$query1i <- renderPlot({
        avg_delay_a_train = (sncf%>%
                                     group_by(year)%>%
                                     summarise(
                                         Number_of_Trains = n(),
                                         moy = mean(as.numeric(avg_delay_late_on_arrival),na.rm = TRUE))%>%
                                     filter(Number_of_Trains>100))
        ggplot(avg_delay_a_train, aes(x = avg_delay_a_train$year, y = moy)) + geom_bar(stat = "identity") + scale_x_discrete(name ="Year")                       
    })
    output$query1j <- renderPlot({
        total_num_cancel = (sncf%>%
                                 group_by(year)%>%
                                 summarise(
                                     Number_of_Trains = n(),
                                     add = sum(as.numeric(num_of_canceled_trains),na.rm = TRUE))%>%
                                 filter(Number_of_Trains>100))
        ggplot(total_num_cancel, aes(x = total_num_cancel$year, y = add)) + geom_bar(stat = "identity") + scale_x_discrete(name ="Year")                       
    })
    output$query1k <- renderPlot({
        proportion_canceled = (sncf%>%
                                group_by(year)%>%
                                summarise(
                                    Number_of_Trains = n(),
                                    percentage_of_cancelation = mean(sum(as.numeric(num_of_canceled_trains))/sum(as.numeric(total_num_trips)),na.rm = TRUE))%>%
                                filter(Number_of_Trains>100))
        ggplot(proportion_canceled, aes(x="", y=percentage_of_cancelation, fill=year))+
            geom_bar(width = 1, stat = "identity")
        })
    output$query1l <- renderPlot({
        proportion_delay = (sncf%>%
                                   group_by(year)%>%
                                   summarise(
                                       Number_of_Trains = n(),
                                       ext_causes = mean(as.numeric(delay_cause_external_cause),na.rm = TRUE),
                                       delay_cause_rail_infrastructure = mean(as.numeric(delay_cause_rail_infrastructure),na.rm = TRUE),
                                       delay_cause_traffic_management = mean(as.numeric(delay_cause_traffic_management),na.rm = TRUE),
                                       delay_cause_traffic_management = mean(as.numeric(delay_cause_traffic_management),na.rm = TRUE),
                                       delay_cause_rolling_stock = mean(as.numeric(delay_cause_rolling_stock),na.rm = TRUE),
                                       delay_cause_station_management = mean(as.numeric(delay_cause_station_management),na.rm = TRUE),
                                       delay_cause_travelers = mean(as.numeric(delay_cause_travelers),na.rm = TRUE),
                                       na.rm = TRUE)%>%
                                   filter(Number_of_Trains>100))
        ggplot(proportion_delay, aes(x="", y=year, fill=delay_cause_rail_infrastructure))+
            coord_polar("y", start=0)
    })
    

    
}

# Run the application 
shinyApp(ui = ui, server = server)
