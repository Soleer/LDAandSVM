Classifier <- selectInput("Classifier", "Select classifier", choices=c("LDA", "QDA", "PDA"))
Base <- selectInput("Base", "Select Basis Expansion  (only for PDA)", choices = c("none", "quad", "cube", "sqrt", "log", "abs"))
Background <- radioButtons("Background", "Plot classification Grid (greatly increases calculation time)", c("FALSE", "TRUE"))
Calc_button <- actionButton("Calc_button", "Classifiy")

## ADD OPTION TO SAVE IMAGE TO FILE
##
##

Param <- sliderInput("Param", "Number of Dimensions", value = 2, min = 2, max = 10, step = 1)
Classes <- sliderInput("Classes", "Number of Classes", value = 2, min = 2, max  = 10, step = 1)
Sigma1 <- numericInput("Sigma1", "std. of Class 1", value = 1, min = 0.01, max = 5)
Sigma2 <- numericInput("Sigma2", "std. of Class 2", value = 1.5, min = 0.01, max = 5)
Sigma3 <- numericInput("Sigma3", "std. of Class 3", value = 2, min = 0.01, max = 5)
Sigma4 <- numericInput("Sigma4", "std. of Class 4", value = 2.5, min = 0.01, max = 5)
Sigma5 <- numericInput("Sigma5", "std. of Class 5", value = 1.3, min = 0.01, max = 5)
Sigma6 <- numericInput("Sigma6", "std. of Class 6", value = 1.1, min = 0.01, max = 5)
Sigma7 <- numericInput("Sigma7", "std. of Class 7", value = 2.1, min = 0.01, max = 5)
Sigma8 <- numericInput("Sigma8", "std. of Class 8", value = 1.8, min = 0.01, max = 5)
Sigma9 <- numericInput("Sigma9", "std. of Class 9", value = 0.7, min = 0.01, max = 5)
Sigma10 <- numericInput("Sigma10", "std. of Class 10", value = 1.1, min = 0.01, max = 5)
Test_button <- actionButton("Test_button", "Generate random testdata")

Plot1 <- plotOutput("Classification")
Plot2 <- plotOutput("Error")

server_LDA_SVM <- function(input, output){
  observeEvent(input$Test_button, {
    nparam <- input$Param
    nclasses <- input$Classes
    print(paste("Creating a test with", nclasses, "classes in", nparam, "dimensions."))
    test_shiny <<- make_test(100,
                      nparam = nparam,
                      nclasses = nclasses,
                      sigma = c(input$Sigma1, input$Sigma2, input$Sigma3, input$Sigma4, input$Sigma5, 
                                input$Sigma6, input$Sigma7, input$Sigma8, input$Sigma9, input$Sigma10))
    print("test created")
  })
  testplot <- ggplot()
  liste <- ggplot()
  observeEvent(input$Calc_button, {
    Classfun <- input$Classifier
    BG <- input$Background
    Base <- input$Base
    print(paste("classifying with:", Classfun, "and basis expansion:", Base))
    print("Please wait for the calculations to finish")
    
    if(Classfun == "LDA"){
      f <- classify(unique(test_shiny$class), LDA(test_shiny[1:(ncol(test_shiny)-1)], test_shiny$class))
      liste <- plot_error(test_shiny[1:(ncol(test_shiny)-1)], test_shiny$class, f)
      testplot <-
        make_2D_plot(test_shiny[1:(ncol(test_shiny)-1)],
                     test_shiny$class,
                     f,
                     ppu = 5,
                     bg = BG)
    }
    
    if(Classfun == "QDA"){
      f <- classify(unique(test_shiny$class), QDA(test_shiny[1:(ncol(test_shiny)-1)], test_shiny$class))
      liste <<- plot_error(test_shiny[1:(ncol(test_shiny)-1)], test_shiny$class, f)
      testplot <<-
        make_2D_plot(test_shiny[1:(ncol(test_shiny)-1)],
                     test_shiny$class,
                     f,
                     ppu = 5,
                     bg = BG)
    }
    
    if(Classfun == "PDA"){
      f <- classify(unique(test_shiny$class), PDA(test_shiny[1:(ncol(test_shiny)-1)], test_shiny$class, base = Base))
      liste <<- plot_error(test_shiny[1:(ncol(test_shiny)-1)], test_shiny$class, f)
      testplot <<-
        make_2D_plot(test_shiny[1:(ncol(test_shiny)-1)],
                     test_shiny$class,
                     f,
                     ppu = 5,
                     bg = BG)
    }
    if(Classfun == "RDA"){
      #TODO
    }
    output$Classification <- renderPlot({testplot})
    output$Error <- renderPlot({do.call(grid.arrange, liste)})
    print("finished!")
  })
}


ui_LDA_SVM <- fluidPage(
  headerPanel("RProject LDA & SVM"),
  tabsetPanel(
    tabPanel("Options", 
             fluidRow(column(width=4, titlePanel("Create Test"), Param, Classes, Test_button),
                      column(width=4, titlePanel("Test Variables"), Sigma1, Sigma2, Sigma3, Sigma4, Sigma5, Sigma6, Sigma7, Sigma8, Sigma9, Sigma10),
                      column(width=4, titlePanel("Classification"), Classifier, Base, Background, Calc_button))),
    tabPanel("Classification", Plot1),
    tabPanel("Error", Plot2)
  )
)


classify_app <- function(){
  shinyApp(ui_LDA_SVM, server_LDA_SVM)
}
