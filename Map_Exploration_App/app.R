#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyr)
library(dplyr)
library(plotly)
library(RColorBrewer)

data <- read.csv("COVID Data.csv")
df <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv")
colnames(data) <- c("Index", "Date", "State", "COUNTRY", "Update", "Confirmed", "Deaths", "Recovered")

data$COUNTRY[data$COUNTRY == "Mainland China"] <- "China"
data$COUNTRY[data$COUNTRY == "US"] <- "United States"
# for(x in 1:nrow(data)){
#     if(data$COUNTRY[x] == "Mainland China"){
#         data$COUNTRY[x] <- "China"
#     }
#     if(data$COUNTRY[x] == "US"){
#         data$COUNTRY[x] <- "United States"
#     }
#     #data$Date[x] <- paste0("0", data$Date[x]) 
# }
dataByCountry <- data %>% group_by(COUNTRY, Date) %>% summarise(Confirmed = sum(Confirmed), Recovered = sum(Recovered), Deaths = sum(Deaths))
dataByCountry <- merge(dataByCountry, df)
dataByCountry$Date <- as.Date(dataByCountry$Date,format="%m/%d/%Y") 
dataByCountry <- dataByCountry %>% group_by(COUNTRY) %>% arrange(Date, .by_group = TRUE)

dataByCountry$dailyDeaths <- c(dataByCountry$Deaths[1], dataByCountry$Deaths[2:length(dataByCountry$Deaths)] - dataByCountry$Deaths[1:length(dataByCountry$Deaths)-1])
dataByCountry$dailyConfirmed <- c(dataByCountry$Confirmed[1], dataByCountry$Confirmed[2:length(dataByCountry$Confirmed)] - dataByCountry$Confirmed[1:length(dataByCountry$Confirmed)-1])
dataByCountry$dailyRecovered <- c(dataByCountry$Recovered[1], dataByCountry$Recovered[2:length(dataByCountry$Recovered)] - dataByCountry$Recovered[1:length(dataByCountry$Recovered)-1])

# Alternative of replacing the "end points" with the cumulative, but I thought that could be misleading
# dataByCountry$dailyRecovered[dataByCountry$dailyRecovered<0] <- dataByCountry$Recovered[dataByCountry$dailyRecovered<0]
# dataByCountry$dailyConfirmed[dataByCountry$dailyConfirmed<0] <- dataByCountry$Confirmed[dataByCountry$dailyConfirmed<0]
# dataByCountry$dailyDeaths[dataByCountry$dailyDeaths<0] <- dataByCountry$Deaths[dataByCountry$dailyDeaths<0]


dataByCountry$dailyRecovered[dataByCountry$dailyRecovered<0] <- NA
dataByCountry$dailyConfirmed[dataByCountry$dailyConfirmed<0] <- NA
dataByCountry$dailyDeaths[dataByCountry$dailyDeaths<0] <- NA

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Covid Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            radioButtons(
                inputId = "cumulative",
                label = "Cumulative or Daily",
                choices = c("Cumulative", "Daily"),
                selected = "Cumulative"
            ),
            radioButtons(
                inputId = "dataType",
                label = "Type of Data",
                choices = c("Deaths", "Confirmed", "Recovered"),
                selected = "Deaths"
            ),
            sliderInput("Dates",
                        "Date",
                        min = as.Date("2020-1-22","%Y-%m-%d"),
                        max = as.Date("2020-9-9","%Y-%m-%d"),
                        value=as.Date("2020-1-22"),
                        timeFormat="%Y-%m-%d",
                        animate = TRUE)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    subset <- reactive({
        return(dataByCountry[dataByCountry$Date == format(input$Dates, "%m/%d/%y"),])
    })
    
    output$distPlot <- renderPlotly({
        # print(as.character(dataByCountry$Date))
        # print(as.character(format(input$Dates, "%m-%d-%Y")))
        # print(input$Dates)
        # print(min(subsetted$dailyConfirmed))
        # print(min(subsetted$dailyDeaths))
        # print(min(subsetted$dailyRecovered))
        subsetted <- dataByCountry[as.character(dataByCountry$Date) == input$Dates,]
        if(input$cumulative == "Cumulative"){
        if(input$dataType == "Deaths"){
            z <- subsetted$Deaths
        }
        else if(input$dataType == "Confirmed"){
            z <- subsetted$Confirmed
        }
        else{
            z <- subsetted$Recovered
        }}
        else{
            if(input$dataType == "Deaths"){
                z <- subsetted$dailyDeaths
            }
            else if(input$dataType == "Confirmed"){
                z <- subsetted$dailyConfirmed
            }
            else{
                z <- subsetted$dailyRecovered
            }
        }
        return(plot_ly(subsetted, 
                       type='choropleth', 
                       locations=subsetted$CODE, 
                       z=z, 
                       text=subsetted$COUNTRY, 
                       colors = colorRampPalette(c("white", "black"))(50),
                       #colorscale="Greys",
                       cmin = 0,
                       cmax = 200000))
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
