#=============================ML Web Application=============================================================
#--- Building an Rshiny application that accepts slider input and returns ML predictions
#============================================================================================================

#============================Rshiny web app check points =====================================================
# --- Loads an already built ML model
# --- Configure shiny to load input data - Restrict Input data range to test data
# --- Load test set and make model predictions
#=============================================================================================================
#-------libraries------------
library(shiny)
library(shinythemes)
library(randomForest)
library(data.table)


RF_model <- readRDS("rf_model.rds")      #load ML  model

testdata <- read.csv("testing.csv", header = T) # Restrict Input data range to test data only
testdata <- testdata[,-1]


# TrainSet <- read.csv("training.csv", header = TRUE)
# TrainSet <- TrainSet[,-1]

# ------ UI Block ----
ui <- fluidPage(theme = shinytheme("darkly"),
                
                navbarPage(title = "ML Predictions - IRIS",
                           
                           tabPanel(title = "RF Prediction Model",
                                    sidebarPanel(
                                      HTML("<h3> Input Parameters</h3>"),
                                      sliderInput(inputId = "Sepal.Length", label = "Sepal Length:",
                                                  min = min(testdata$Sepal.Length),
                                                  max = max(testdata$Sepal.Length),
                                                  value = median(testdata$Sepal.Length)),
                                      sliderInput(inputId = "Sepal.Width", label = "Sepal Width",
                                                  min = min(testdata$Sepal.Width),
                                                  max = max(testdata$Sepal.Width),
                                                  value = median(testdata$Sepal.Width)),
                                      sliderInput(inputId = "Petal.Length", label = "Petal Length",
                                                  min = min(testdata$Petal.Length),
                                                  max = max(testdata$Petal.Length),
                                                  value = median(testdata$Petal.Length)),
                                      sliderInput(inputId = "Petal.Width", label = "Petal Width",
                                                  min = min(testdata$Petal.Width),
                                                  max = max(testdata$Petal.Width),
                                                  value = median(testdata$Petal.Width)),
                                      actionButton(inputId = "submitbutton", label = "Submit",
                                                   class = "btn btn-primary")  #btn btn-primary 
                                    ),
                                    
                                    mainPanel(
                                      HTML("<h3> ML Model Predictions </h3>"), # Text box for Status/Output
                                      verbatimTextOutput("contents"),
                                      tableOutput("tabledata")        # Prediction results table
                                    )
                             
                           )
                  
                )
  
)



# -----Server-----
server <- function(input, output, session){
  
  #Input Data
  datasetInput <- reactive({
    
    df <- data.frame(Name = c("Sepal Length", "Sepal Width", "Petal Length", "Petal Width"),
                     Value = as.character(input$Sepal.Length, input$Sepal.Width, input$Petal.Length, input$Petal.Width),
                     stringsAsFactors = F)
    Species <- 0
    df <- rbind(df,Species)
    input <- transpose(df)
    write.table(x = input, file = "input.csv", sep = ",", quote = F, row.names = F, col.names = F)
    
    test <- read.csv(paste("input", ".csv", sep = ""), header = T)
    
    Output <- data.frame(Prediction = predict(RF_model,test), round(predict(RF_model,test,type = "prob"), 2))
    print(Output)
    
  })
  
  #--Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton>0) {
      isolate("Prediction complete!")
    } else {
      return("Shiny Server is ready for Prediction")
    }
  })
  
  
  #---Prediction Results Table
  output$tabledata <- renderTable({
    if (input$submitbutton > 0) {
      isolate(datasetInput())
    }
  })
  
  
}


# 
# # ---- Launch Shiny App ----
# shinyApp(ui = ui, server = server)

