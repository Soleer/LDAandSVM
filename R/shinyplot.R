Classifier <- selectInput("Classifier", "Select classifier", choices=c("LDA", "QDA", "PDA","SVM"))
Base <- selectInput("Base", "Select Basis Expansion  (only for PDA)", choices = c("none", "quad", "cube", "sqrt", "log", "abs"))
Background <- radioButtons("Background", "Plot classification Grid (greatly increases calculation time)", c("FALSE", "TRUE"))
Calc_button <- actionButton("Calc_button", "Classifiy")
Classify <- wellPanel(Classifier, Base, Background, Calc_button)

Load_test <- textInput("Load_test", "Load an R6 Dataset", value = "Name of the object", width = NULL, placeholder = NULL)
Load_button <- actionButton("Load_button", "Load Object")
Load <- wellPanel(Load_test, Load_button)

Save_name1 <- textInput("Save_name1", "Save image to .png", value = "ShinyClassification")
Save_DPI1 <- sliderInput("Save_DPI1", "DPI of Image", value = 400, min = 100, max = 1000, step = 1)
Save_button1 <- actionButton("Save_button1", "Save image")
Save1 <- wellPanel(Save_name1, Save_DPI1, Save_button1)

Save_name2 <- textInput("Save_name2", "Save image to .png", value = "ShinyError")
Save_DPI2 <- sliderInput("Save_DPI2", "DPI of Image", value = 400, min = 100, max = 1000, step = 1)
Save_button2 <- actionButton("Save_button2", "Save image")
Save2 <- wellPanel(Save_name2, Save_DPI2, Save_button2)

Param <- sliderInput("Param", "Number of Dimensions", value = 2, min = 2, max = 10, step = 1)
Classes <- sliderInput("Classes", "Number of Classes", value = 2, min = 2, max  = 10, step = 1)
Test_button <- actionButton("Test_button", "Generate random testdata")
Create <- wellPanel(Param, Classes, Test_button)

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
Sigma <- wellPanel(Sigma1, Sigma2, Sigma3, Sigma4, Sigma5, Sigma6, Sigma7, Sigma8, Sigma9, Sigma10)


Plot1 <- plotOutput("Classification")
Plot2 <- plotOutput("Error")

server_LDA_SVM <- function(input, output){
  shiny_env <- new_environment()
  
  observeEvent(input$Load_button, {
    dataset <- input$Load_test
    print(paste("Loading Dataset Object", dataset))
    test_shiny <<- eval(parse_expr(dataset))
  })
  
  observeEvent(input$Test_button, {
    nparam <- input$Param
    nclasses <- input$Classes
    print(paste("Creating a test with", nclasses, "classes in", nparam, "dimensions."))
    test_shiny <<- make_test(100,
                      nparam = nparam,
                      nclasses = nclasses,
                      sigma = c(input$Sigma1, input$Sigma2, input$Sigma3, input$Sigma4, input$Sigma5, 
                                input$Sigma6, input$Sigma7, input$Sigma8, input$Sigma9, input$Sigma10))
    
    # shiny_test <<- make_set$new(test_shiny,
    #                         by = "class",
    #                         title = "Shiny Test",
    #                         description = "This is a randomly generated Test for shiny")
    print("test created")
  })
  
  observeEvent(input$Calc_button, {
    Classfun <- input$Classifier
    BG <- input$Background
    Base <- input$Base
    print(paste("classifying with:", Classfun, "and basis expansion:", Base))
    print("Please wait for the calculations to finish")
    
    if(Classfun == "LDA"){
      f <- classify(unique(test_shiny$class), LDA(test_shiny[1:(ncol(test_shiny)-1)], test_shiny$class))
      Error_plot_shiny <- plot_error(test_shiny[1:(ncol(test_shiny)-1)], test_shiny$class, f)
      Class_plot_shiny <-
        make_2D_plot(test_shiny[1:(ncol(test_shiny)-1)],
                     test_shiny$class,
                     f,
                     ppu = 5,
                     bg = BG)
      Error_plot_shiny <- do.call(grid.arrange, Error_plot_shiny)
    }
    
    if(Classfun == "QDA"){
      f <- classify(unique(test_shiny$class), QDA(test_shiny[1:(ncol(test_shiny)-1)], test_shiny$class))
      Error_plot_shiny <- plot_error(test_shiny[1:(ncol(test_shiny)-1)], test_shiny$class, f)
      Class_plot_shiny <-
        make_2D_plot(test_shiny[1:(ncol(test_shiny)-1)],
                     test_shiny$class,
                     f,
                     ppu = 5,
                     bg = BG)
      Error_plot_shiny <- do.call(grid.arrange, Error_plot_shiny)
    }
    
    if(Classfun == "PDA"){
      f <- classify(unique(test_shiny$class), PDA(test_shiny[1:(ncol(test_shiny)-1)], test_shiny$class, base = Base))
      Error_plot_shiny <- plot_error(test_shiny[1:(ncol(test_shiny)-1)], test_shiny$class, f)
      Class_plot_shiny <-
        make_2D_plot(test_shiny[1:(ncol(test_shiny)-1)],
                     test_shiny$class,
                     f,
                     ppu = 5,
                     bg = BG)
      Error_plot_shiny <- do.call(grid.arrange, Error_plot_shiny)
    }
    if(Classfun == "SVM"){
      f <- svm_classify(unique(test_shiny$class), svm(test_shiny[1:(ncol(test_shiny)-1)], test_shiny$class))
      Error_plot_shiny <- plot_error(test_shiny[1:(ncol(test_shiny)-1)], test_shiny$class, f)
      Class_plot_shiny <-
        make_2D_plot(test_shiny[1:(ncol(test_shiny)-1)],
                     test_shiny$class,
                     f,
                     ppu = 5,
                     bg = BG)
      Error_plot_shiny <- do.call(grid.arrange, Error_plot_shiny)
    }
    
    if(Classfun == "RDA"){
      #TODO
    }
    
    assign("Class_plot_shiny", Class_plot_shiny, envir=shiny_env)
    assign("Error_plot_shiny", Error_plot_shiny, envir=shiny_env)
    
    output$Classification <- renderPlot({Class_plot_shiny})
    output$Error <- renderPlot({do.call(grid.arrange, Error_plot_shiny)})
    print("finished!")
  })
  
  observeEvent(input$Save_button1, {
    shiny_name <- input$Save_name1
    DPI <- input$Save_DPI1
    Shiny_plot <- shiny_env$Class_plot_shiny
    print(paste0("Saving plot with ", DPI, " DPI to \"", shiny_name, "\""))
    ggsave(paste0(shiny_name, ".png"),
           plot = Shiny_plot,
           device = 'png',
           dpi = DPI)
    print("Image saved")
  })
  
  observeEvent(input$Save_button2, {
    shiny_name <- input$Save_name2
    DPI <- input$Save_DPI2
    Shiny_plot <- shiny_env$Error_plot_shiny
    print(paste0("Saving plot with ", DPI, " DPI to: ", shiny_name))
    ggsave(paste0(shiny_name, ".png"),
           plot = Shiny_plot,
           device = 'png',
           dpi = DPI)
    print("Image saved")
  })
}


ui_LDA_SVM <- fluidPage(
  headerPanel("RProject LDA & SVM"),
  tabsetPanel(
    tabPanel("Options", 
             fluidRow(column(width=4, titlePanel("Dataset"), Load, Create),
                      column(width=4, titlePanel("Test Variables"), Sigma),
                      column(width=4, titlePanel("Classification"), Classify))),
    tabPanel("Classification", Plot1, Save1),
    tabPanel("Error", Plot2, Save2)
  )
)


classify_app <- function(){
  shinyApp(ui_LDA_SVM, server_LDA_SVM)
}
