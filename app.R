#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load the required packages
library(shiny)
library(ggplot2)
library(readr)
library(plotly)

# Create a function for plot creation
createVolcanoPlot <- function(outcome, objectDrug, basePrecipitant, estimateRange){
        if(outcome == "hipfracture"){
            outcomeFormatted <- "Hip Fracture"
        }
        else if(outcome == "mvc"){
            outcomeFormatted <- "Motor Vehicle Collision"
        }
        else {
            outcomeFormatted <- "Injury"
        }
        titleText <- paste0("Volcano Plot of Adjusted RRs of ", stringr::str_to_title(outcomeFormatted), " for ", stringr::str_to_sentence(objectDrug), ", ", stringr::str_to_sentence(basePrecipitant), ", and Precipitants")
        filteredData <- dplyr::filter(estimates, Object == objectDrug & Base_Precipitant == basePrecipitant & range == estimateRange & Outcome == outcome)
        `Precipitant Drug` <-  paste0(stringr::str_to_sentence(filteredData$Precipitant), ", Semi-Bayes Risk Ratio = ", round(filteredData$SB_RR, 2), ", 95% CI = (", round(filteredData$SB_RR_lower, 2), ", ", round(filteredData$SB_RR_upper,2),"), p = ", round(filteredData$pSB, 3))
        basePlot <- ggplot(data = filteredData, aes(x = log2RR, y = log10P, label = `Precipitant Drug`)) + 
            geom_point(color = "red") + theme_bw() + scale_y_continuous(name = "log10(1/p)") + scale_x_continuous(name = "log2(semi-Bayes shrunk adjusted rate ratio)") + ggtitle(titleText)
        ggplotly(basePlot, tooltip = "label")
    
}

createHeatMapPlot <- function(outcome, objectDrug, estimateRange){
    if(outcome == "hipfracture"){
        outcomeFormatted <- "Hip Fracture"
    }
    else if(outcome == "mvc"){
        outcomeFormatted <- "Motor Vehicle Collision"
    }
    else {
        outcomeFormatted <- "Injury"
    }
        titleText <- paste0("Heatmap of Adjusted RRs of ", stringr::str_to_title(outcomeFormatted), " for ", stringr::str_to_sentence(objectDrug), ", Base Precipitants, and Precipitants")
        filteredData <- dplyr::filter(estimates, Object == objectDrug & range == estimateRange & Outcome == outcome)
        #`Precipitant Drug` <-  paste0(stringr::str_to_sentence(filteredData$Precipitant), ", Semi-Bayes Risk Ratio = ", round(filteredData$SB_RR, 2), ", 95% CI = (", round(filteredData$SB_RR_lower, 2), ", ", round(filteredData$SB_RR_upper,2),"), p = ", round(filteredData$pSB, 3))
        #basePlot <- ggplot(data = filteredData, aes(x = Precipitant, y = Base_Precipitant, fill = log10P)) + 
            geom_tile() #+ theme_bw() + scale_y_continuous(name = "log10(1/p)") + scale_x_continuous(name = "log2(semi-Bayes shrunk adjusted rate ratio)") + ggtitle(titleText)
        #ggplotly(basePlot)
        plot_ly(x = filteredData$Precipitant, y= filteredData$Base_Precipitant, 
                z = filteredData$log10P, 
                type = "heatmap", 
                colorscale= "Earth",
                showscale = F, ) %>% layout(autosize = F, width = 1000, height = 1000, title = titleText)
    
    
}

# Load the datafile with the risk estimates
estimates <- read_csv("/Users/wfvdmei/Documents/Rotation Fall 2021/Shiny App/3DI Visualization/AdjustedEstimates/adjustedEstimatesProcessed.csv")

# Create the user interface for the visualization
ui <- fluidPage(

    # Application title
    titlePanel("Visualization of 3DI Interactions"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("plotType", "Type of Plot:",
                        list("Heat Map" = "heatMap", 
                             "Volcano Plot" = "volcano"), selected = "volcano"),
            selectInput("outcome", "Outcome:",
                        list("Injury" = "injury", 
                             "Motor Vehicle Collision" = "mvc", 
                             "Hip Fracture" = "hipfracture")),
            selectInput("estimateRange", "Range:",
                        list("7-Fold" = "fold7", 
                             "25-Fold" = "fold25" 
                             )),
            selectizeInput(
                'objectDrug', label = "Object Drug", choices = NULL,
                options = list(maxOptions = 5, placeholder = "Select object drug")
            ),
            uiOutput("basePrecipitant"), 
            actionButton(inputId = "createPlot", label = "Create Plot", icon = NULL, width = NULL),
        ),
        
        mainPanel(
           plotlyOutput("plotInteractive")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    updateSelectizeInput(inputId = 'objectDrug', choices = estimates$Object, server = TRUE)
    updateSelectizeInput(inputId = 'basePrecipitant', choices = estimates$Base_Precipitant, server = TRUE)
    output$basePrecipitant <- renderUI({
        if(input$plotType == "volcano"){
            selectizeInput('basePrecipitant', label = "Base Precipitant", choices = NULL,
                           options = list(maxOptions = 5))
        }
    })
    observeEvent(input$plotType, {
        updateSelectizeInput(inputId = 'basePrecipitant', choices = estimates$Base_Precipitant, server = TRUE)
    })
    observeEvent(input$outcome, {
        updateSelectizeInput(inputId = 'basePrecipitant', choices = estimates$Base_Precipitant[estimates$Outcome == input$outcome], server = TRUE)
    })
    observeEvent(input$outcome, {
        updateSelectizeInput(inputId = 'objectDrug', choices = estimates$Object[estimates$Outcome == input$outcome], server = TRUE)
    })
    observeEvent(input$estimateRange, {
        updateSelectizeInput(inputId = 'basePrecipitant', choices = estimates$Base_Precipitant[estimates$range == input$estimateRange], server = TRUE)
    })
    observeEvent(input$estimateRange, {
        updateSelectizeInput(inputId = 'objectDrug', choices = estimates$Object[estimates$range == input$estimateRange], server = TRUE, selected = NULL)
    })
    output$plotInteractive <- renderPlotly({ 
            input$createPlot
            if(input$plotType == "volcano"){
                isolate(createVolcanoPlot(outcome = input$outcome,  objectDrug = input$objectDrug, basePrecipitant = input$basePrecipitant, estimateRange = input$estimateRange))
            }
            else {
                isolate(createHeatMapPlot(outcome = input$outcome, objectDrug = input$objectDrug, estimateRange = input$estimateRange))}
            })
}
# Run the application 
shinyApp(ui = ui, server = server)
