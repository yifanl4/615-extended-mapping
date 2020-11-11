library(tidyverse)
library(dplyr)
library(magrittr)
library(readr)
library(scales)
library(ggrepel)
library(base)
library(shiny)
library(maps)


## Importing Date
# The dataset used in this project is from [FEMA](https://www.fema.gov/openfema-data-page/public-assistance-funded-projects-details-v1). 
#d1 = read.csv("PublicAssistanceFundedProjectsDetails.csv")
## Data cleaning
# filter the data from 2009 to 2018 and select related columns
#d1 = separate(col = declarationDate, into = c("declarationYear","others"), sep = "-", remove = FALSE, data = d1)
#d2 = select(d1, c(3,5,8,9,13,15,17,18,19,20))
#d3 = d2 %>% filter(d2$declarationYear %in% c(2009:2018))
#d4 = filter(d3, incidentType=="Hurricane")
d4 = read.csv("d4.csv")
# Creating dataset for plotting the estimated total cost
a1 = aggregate(projectAmount~county, data = d4, FUN = sum)
a2 = aggregate(applicantId~county, data = d4, FUN = unique)
a2$number = lengths(a2$applicantId)
m1 = merge(a1, a2)
for (i in 1:nrow(m1)){
    m1$subregion[i] = tolower(m1$county[i])
}
m1 = select(m1, -c(1,3))
geo = map_data("county")
state = map_data("state")
geo1 = right_join(geo, m1, by = c('subregion'='subregion'))
# Creating dataset for plotting the public assistance grant funding
a3 = aggregate(federalShareObligated~county, data = d4, FUN = sum)
a2$number = lengths(a2$applicantId)
m2 = merge(a3, a2)
for (i in 1:nrow(m2)){
    m2$subregion[i] = tolower(m2$county[i])
}
m2 = select(m2, -c(1,3))
geo = map_data("county")
state = map_data("state")
geo2 = right_join(geo, m2, by = c('subregion'='subregion'))


ui <- fluidPage(
    title = "Examples of Data Tables",
    sidebarLayout(
        tabsetPanel(
             conditionalPanel('input.dataset === "d4"')
        ),
        mainPanel(
            tabsetPanel(
                id = 'dataset',
                tabPanel("Public Assistance Funded Projects Details",
                
                # Create a new Row in the UI for selectInputs
                fluidRow(
                    column(4,
                           selectInput("declarationYear",
                                       "declarationYear:",
                                       c("All",
                                         unique(as.character(d4$declarationYear))))
                    ),
                    column(4,
                           selectInput("state",
                                       "state:",
                                       c("All",
                                         unique(d4$state)))
                    ),
                    column(4,
                           selectInput("county",
                                       "county:",
                                       c("All",
                                         unique(d4$county)))
                    ),
                    column(4,
                           selectInput("damageCategoryCode",
                                       "damageCategoryCode:",
                                       c("All",
                                         unique(d4$damageCategoryCode)))
                ),
                # Create a new row for the table.
                DT::dataTableOutput("table1"))
                
               
                        ),
                tabPanel("Plot for Estimated Total Cost",
                         plotOutput("plot1")),
                tabPanel("Plot for the Public Assistance grant funding",
                         plotOutput("plot2"))

                ), tabsetPanel(tabPanel("Plot for Types of Damage Categories", plotOutput("plot3")
                         )

                      ))
                )
)
                

    

server <- function(input, output) {
    
    # Filter data based on selections
    output$table1 <- DT::renderDataTable(DT::datatable({
        data <- d4
        if (input$declarationYear != "All") {
            data <- data[data$declarationYear == input$declarationYear,]
        }
        if (input$state != "All") {
            data <- data[data$state == input$state,]
        }
        if (input$county != "All") {
            data <- data[data$county == input$county,]
        }
        if (input$damageCategoryCode != "All") {
            data <- data[data$damageCategoryCode == input$damageCategoryCode,]
        }
        data
    }))
   
    # Plot for Estimated Total Cost
    output$plot1 = renderPlot({
        ggplot() + 
            ggtitle("Estimated Total Cost") +
            geom_polygon(data = geo, aes(x = long, y = lat, group = group), color = "grey", fill = "white") +
            geom_polygon(data = geo1, aes(x = long, y = lat, group = group, fill = projectAmount), color = "grey", size = 0.2, alpha = 1.6) +
            labs(x = "Longtitude", y = "Latitude", fill = "Estimated Total Cost")+
            geom_polygon(data = state, aes(x = long, y = lat, group = group), color="black", fill = "lightgray", size = 0.2, alpha = 0.3)
    })
    
    # Plot for the Public Assistance grant funding
    output$plot2 = renderPlot({
        ggplot() + 
            ggtitle("The Public Assistance grant funding") +
            geom_polygon(data = geo, aes(x = long, y = lat, group = group), color = "grey", fill = "white") +
            geom_polygon(data = geo2, aes(x = long, y = lat, group = group, fill = federalShareObligated), color = "grey", size = 0.2, alpha = 1.6) +
            labs(x = "Longtitude", y = "Latitude", fill = "Public Assistance grant funding")+
            geom_polygon(data = state, aes(x = long, y = lat, group = group), color="black", fill = "lightgray", size = 0.2, alpha = 0.3)
    })
    
    #Plot for the damageCategoryCode
    output$plot3 <- renderPlot({
        data <- d4
        if (input$declarationYear != "All") {
            data <- data[data$declarationYear == input$declarationYear,]
        }
        if (input$state != "All") {
            data <- data[data$state == input$state,]
        }
        if (input$county != "All") {
            data <- data[data$county == input$county,]
        }
        if (input$damageCategoryCode != "All") {
            data <- data[data$damageCategoryCode == input$damageCategoryCode,]
        }
        ggplot(data, aes(damageCategoryCode)) +
            geom_histogram(stat = "count")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
