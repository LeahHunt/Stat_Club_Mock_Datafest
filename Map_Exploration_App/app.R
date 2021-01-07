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

for(x in 1:nrow(data)){
    if(data$COUNTRY[x] == "Mainland China"){
        data$COUNTRY[x] <- "China"
    }
    if(data$COUNTRY[x] == "US"){
        data$COUNTRY[x] <- "United States"
    }
    #data$Date[x] <- paste0("0", data$Date[x]) 
}
dataByCountry <- data %>% group_by(COUNTRY, Date) %>% summarise(Confirmed = sum(Confirmed), Recovered = sum(Recovered), Deaths = sum(Deaths))
dataByCountry <- merge(dataByCountry, df)
dataByCountry$Date <- as.Date(dataByCountry$Date,format="%m/%d/%Y") 

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Covid Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
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
        subsetted <- dataByCountry[as.character(dataByCountry$Date) == input$Dates,]
        if(input$dataType == "Deaths"){
            z <- subsetted$Deaths
        }
        else if(input$dataType == "Confirmed"){
            z <- subsetted$Confirmed
        }
        else{
            z <- subsetted$Recovered
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
