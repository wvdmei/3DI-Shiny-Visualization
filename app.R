#
# This is a Shiny web application created by Willem van der Mei.
# It displays the results of analyses of the relationship between 3 drug interactions and injury outcomes.
# It allows for the creation of customizable volcano plots and heat maps.
# It also allows for the export of those graphics through plotly.
#

# Load the required packages
library(shiny)
library(ggplot2)
library(readr)
library(plotly)
library(stringr)
library(spsComps)

# Create a function for volcano plot creation
createVolcanoPlot <- function(filteredData, outcome, objectDrug, basePrecipitant, `Precipitant Drug`){
        outcomeFormatted <- renameOutcomes(outcome)
        dotColor <- ifelse(filteredData$log2RR > 0 & filteredData$pSB < 0.05, "red", "black")
        titleText <- paste0("Volcano Plot of Adjusted RRs of ", outcomeFormatted, " for ", str_to_title(objectDrug), ", ", str_to_title(basePrecipitant), ", and Precipitants")
        basePlot <- ggplot(data = filteredData, aes(x = log2RR, y = log10P, label = `Precipitant Drug`)) + 
            geom_point(color = dotColor) + theme_bw() + scale_y_continuous(name = "log10(1/p)") + 
            scale_x_continuous(name = "log2(semi-Bayes shrunk adjusted rate ratio)") + ggtitle(titleText) + 
        geom_hline(yintercept = 1.30103, color = "black", size = 1.5, linetype = "longdash") +
            geom_vline(xintercept = 0, color = "black", size = 1.5, linetype = "longdash")
        ggplotly(basePlot, tooltip = "label")
}

# Rename Outcomes
renameOutcomes <- function(outcome){
    if(outcome %in% "hipfracture"){
        return("Hip Fracture")
    }
    else if(outcome %in% "mvc"){
        return("Motor Vehicle Crash")
    }
    else {
        return("Traumatic Injury")
    }
}

# Create a function to create heat maps
createHeatMapPlot <- function(filteredData, outcome, objectDrug){
    outcomeFormatted <- renameOutcomes(outcome)
    filteredData <- dplyr::arrange(filteredData, Base_Precipitant, Precipitant)
    titleText <- paste0("Heatmap of Adjusted Rate Ratios of ", str_to_title(outcomeFormatted), " for ", str_to_title(objectDrug), ", Base-Pair Precipitants, and Precipitants")
    plot_ly(x = filteredData$Precipitant, y= filteredData$Base_Precipitant, 
        z = round(filteredData$SB_RR, 2), 
        type = "heatmap", 
        colorscale= "RdBu",
        showscale = TRUE, hoverinfo = 'text', width = 1000, height = 1000,
        text = ~paste0("Precipitant: ", filteredData$Precipitant, "<br>", "Base-Pair Precipitant: ", 
        filteredData$Base_Precipitant, "<br>", "Rate Ratio: ", round(filteredData$SB_RR, 2))) %>% 
        layout(autosize = F, title = titleText, 
        xaxis = list(title = "Precipitants"), yaxis = list(title = "Base-Pair Precipitants")) %>% colorbar(title = "Rate Ratio", limits = c(0.0, 10.0), 
        tickvals = c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0, 1.5, 2.0, 3.0, 4.0, 5.0, 6.0, 8.0, 10.0), len = 1.1)
                                                                                                
}

# Read in table with outcomes
estimates <- read_csv("adjustedEstimatesProcessed.csv")

# Create the user interface for the visualization
ui <- fluidPage(

    # Application title
    titlePanel("Visualization of 3DI Interactions"),

    # Sidebar with customization options and create plot button.
    sidebarLayout(
        sidebarPanel(
            selectInput("plotType", "Plot Type:",
                        list("Heat Map" = "heatMap", 
                             "Volcano" = "volcano"), selected = "volcano"),
            selectInput("outcome", "Health Outcome:",
                        list("Traumatic Injury" = "injury", 
                             "Motor Vehicle Crash" = "mvc", 
                             "Hip Fracture" = "hipfracture"), selected = "injury"),
            selectInput("estimateRange", "Variance Parameter for Semi-Bayes Shrinkage",
                        list("0.25 [7-fold range]" = "fold7", 
                             "0.67 [25-fold range]" = "fold25" 
                             ), selected = "fold7"),
            selectizeInput(
                'objectDrug', label = "Object Drug", choices = unique(estimates$Object),
                options = list(maxOptions = 500), multiple = FALSE,
            ),
            uiOutput("basePrecipitant"), # This is displays depending on the value of plotType
            actionButton(inputId = "createPlot", label = "Create Plot", icon = NULL, width = NULL) # This creates the plot after values are selected,
        ),
        
        mainPanel(
           plotlyOutput("plotInteractive"),
           h3("Please click on create plot to generate the plot.")
        )
    )
)

# This is the backend of the web app and handles plot creation and updating of fields.
server <- function(input, output) {
    
    # Conditional display of base precipitant field based on plot type.
    output$basePrecipitant <- renderUI({
        if(input$plotType %in%  "volcano"){
            selectizeInput('basePrecipitant', label = "Base-Pair Precipitant Drug", choices = unique(estimates$Base_Precipitant)[order(unique(estimates$Base_Precipitant))],
                           options = list(maxOptions = 500), multiple = FALSE)
        }
    })
    
    #  Functions to update options when options are changed.
    observeEvent(input$plotType, {
        updateSelectizeInput(inputId = 'basePrecipitant', choices = unique(estimates$Base_Precipitant)[order(unique(estimates$Base_Precipitant))], options = list(maxOptions = 500))
    })
    toListen <- reactive({
        list(input$outcome,input$estimateRange)
    })
    observeEvent(toListen(), {
        updateSelectizeInput(inputId = 'basePrecipitant', choices = unique(estimates$Base_Precipitant[estimates$range %in% input$estimateRange & estimates$Outcome %in% input$outcome])[order(unique(estimates$Base_Precipitant[estimates$range %in% input$estimateRange & estimates$Outcome %in% input$outcome]))], options = list(maxOptions = 500))
        updateSelectizeInput(inputId = 'objectDrug', choices = unique(estimates$Object[estimates$range %in% input$estimateRange & estimates$Outcome %in% input$outcome]), options = list(maxOptions = 500))
    })
    observeEvent(input$objectDrug, {
        updateSelectizeInput(inputId = 'basePrecipitant', choices = unique(estimates$Base_Precipitant[estimates$range %in% input$estimateRange & estimates$Outcome %in% input$outcome & estimates$Object %in% input$objectDrug])[order(unique(estimates$Base_Precipitant[estimates$range %in% input$estimateRange & estimates$Outcome %in% input$outcome & estimates$Object %in% input$objectDrug]))], options = list(maxOptions = 500))
    })
    
    # Create data set to be used for plots based on options selected.
    filteredDataVolcano <- reactive({data = dplyr::filter(estimates, Object %in% input$objectDrug & Base_Precipitant %in% input$basePrecipitant & range %in% input$estimateRange & Outcome %in% input$outcome)
    data})
    filteredDataHeatMap <- reactive({data = dplyr::filter(estimates, Object %in% input$objectDrug & range %in% input$estimateRange & Outcome %in% input$outcome)
    data})
    
    # Create Plot
    output$plotInteractive <- renderPlotly({ 
            input$createPlot
            if(input$plotType %in% "volcano"){
                isolate({
                precipitantDrug <-  paste0(str_to_sentence(filteredDataVolcano()$Precipitant), ", Semi-Bayes Rate Ratio = ", round(filteredDataVolcano()$SB_RR, 2), ", 95% CI = (", round(filteredDataVolcano()$SB_RR_lower, 2), ", ", round(filteredDataVolcano()$SB_RR_upper,2),"), p = ", round(filteredDataVolcano()$pSB, 3))
                shinyCatch(createVolcanoPlot(filteredDataVolcano(), input$outcome, input$objectDrug, input$basePrecipitant, precipitantDrug), blocking_level = "warning", shiny = FALSE) # This allows us to stop execution and suppress warning messages in case of errors.
                })
            }
            else {
                isolate(createHeatMapPlot(filteredDataHeatMap(), input$outcome, input$objectDrug))}
            })
}

# Run the application 
shinyApp(ui = ui, server = server)
