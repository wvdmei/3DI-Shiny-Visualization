#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(readr)
library(plotly)

createPlot <- function(outcome, plotType, objectDrug, basePrecipitant, estimateRange){
    if(plotType == "volcano"){
        basePlot <- ggplot(data = dplyr::filter(estimates, Object == objectDrug & Base_Precipitant == basePrecipitant & range == estimateRange & Outcome == outcome), aes(x = log2RR, y = log10P, label = paste0(SB_RR, ", 95% CI = (", SB_RR_lower, ", ", SB_RR_upper,"), p = ", pSB))) + 
            geom_point(color = "red") + theme_bw() + scale_y_continuous(name = "log10(1/p)") + scale_x_continuous(name = "log2(semi-Bayes shrunk adjusted rate ratio)")
        ggplotly(basePlot)
        
    }
    else if(plotType == "heatMap"){
        
    }
    
}

estimates <- read_csv("/Users/wfvdmei/Documents/Rotation Fall 2021/Shiny App/3DI Visualization/AdjustedEstimates/adjustedEstimatesProcessed.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Visualization of 3DI Interactions"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("plotType", "Type of Plot:",
                        list("Heat Map" = "heatMap", 
                             "Volcano Plot" = "volcano")),
            selectInput("outcome", "Outcome:",
                        list("Injury" = "injury", 
                             "Motor Vehicle Collision" = "mvc", 
                             "Hip Fracture" = "hipfracture")),
            selectInput("estimateRange", "Range:",
                        list("7-Fold" = "fold7", 
                             "25-Fold" = "fold25" 
                             )),
            # allow creation of new items in the drop-down list
            selectizeInput(
                'objectDrug', label = "Object Drug", choices = NULL,
                options = list(maxOptions = 5, placeholder = "Select object drug")
            ),
            selectizeInput(
                'basePrecipitant', label = "Base Precipitant", choices = NULL,
                options = list(maxOptions = 5)
            ),
        ),
        
        

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("plotInteractive")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    updateSelectizeInput(inputId = 'objectDrug', choices = estimates$Object, server = TRUE)
    updateSelectizeInput(inputId = 'basePrecipitant', choices = estimates$Base_Precipitant, server = TRUE)
    #observeEvent(input$outcome, {
     #   updateSelectInput(inputId = "estimateRange", selected = ifelse(input$outcome != "injury", "fold25", NULL))
    #})
    #observeEvent(input$outcome, {
        #updateSelectizeInput(inputId = "objectDrug", options = ifelse(input$outcome != "injury",list(create = TRUE, maxOptions = 5), list(create = TRUE, maxOptions = 5)))
    #})
    output$plotInteractive <- renderPlotly(createPlot(outcome = input$outcome, plotType = input$plotType, objectDrug = input$objectDrug, basePrecipitant = input$basePrecipitant, estimateRange = input$estimateRange))
}

# Run the application 
shinyApp(ui = ui, server = server)
