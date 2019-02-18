Classifier <- selectInput("Classifier", "Select classifier", choices=c("LDA", "QDA", "PDA","SVM"))                                  ##Dropdown menu where the classifier functions can be selected
Base <- selectInput("Base", "Select Basis Expansion  (only for PDA)", choices = c("id", "quad", "cube", "sqrt", "log", "abs"))      ##Dropdown menu where the basis expansion for PDA can be selected
Background <- radioButtons("Background", "Plot classification Grid (greatly increases calculation time)", c("TRUE", "FALSE"))       ##Radio buttons with the option to print the background or not
Calc_button <- actionButton("Calc_button", "Classifiy")                                                                             ##Button that starts the process of classifying
Classify <- wellPanel(Classifier, Base, Background, Calc_button)                                                                    ##Merges all objects above into one big Object

Load_test <- textInput("Load_test", "Load an R6 Dataset", value = "Name of the object")               ##Text input where the name of an object can be written. This object will then be loaded into shiny
Load_button <- actionButton("Load_button", "Load Object")                                             ##Button that loads in th object specified above
Load <- wellPanel(Load_test, Load_button)                                                             ##Merges the two objects above into one

Param <- sliderInput("Param", "Number of Dimensions", value = 2, min = 2, max = 10, step = 1)         ##Slider that lets the user chose the number of dimensions in a randomly generated test
Classes <- sliderInput("Classes", "Number of Classes", value = 2, min = 2, max  = 10, step = 1)       ##Slider that lets the user chose the number of classes in a randomly generated test
Test_button <- actionButton("Test_button", "Generate random testdata")                                ##Button to start generating a random test
Create <- wellPanel(Param, Classes, Test_button)                                                      ##Merges the objects into one 

Sigma1 <- numericInput("Sigma1", "std. of Class 1", value = 1, min = 0.01, max = 5)                   ##Input that lets the user chose the standard deviations of all classes
Sigma2 <- numericInput("Sigma2", "std. of Class 2", value = 1.5, min = 0.01, max = 5)
Sigma3 <- numericInput("Sigma3", "std. of Class 3", value = 2, min = 0.01, max = 5)
Sigma4 <- numericInput("Sigma4", "std. of Class 4", value = 2.5, min = 0.01, max = 5)
Sigma5 <- numericInput("Sigma5", "std. of Class 5", value = 1.3, min = 0.01, max = 5)
Sigma6 <- numericInput("Sigma6", "std. of Class 6", value = 1.1, min = 0.01, max = 5)
Sigma7 <- numericInput("Sigma7", "std. of Class 7", value = 2.1, min = 0.01, max = 5)
Sigma8 <- numericInput("Sigma8", "std. of Class 8", value = 1.8, min = 0.01, max = 5)
Sigma9 <- numericInput("Sigma9", "std. of Class 9", value = 0.7, min = 0.01, max = 5)
Sigma10 <- numericInput("Sigma10", "std. of Class 10", value = 1.1, min = 0.01, max = 5)
Sigma <- wellPanel(Sigma1, Sigma2, Sigma3, Sigma4, Sigma5, Sigma6, Sigma7, Sigma8, Sigma9, Sigma10)   ##Merges all Sigmas into one Objects

Plot1 <- plotOutput("Classification")                                                                 ##Object where the Classification plot will be shown
Save_name1 <- textInput("Save_name1", "Save image to .png", value = "ShinyClassification")            ##Text input where the user can name the file he wants the plot to be saved in
Save_DPI1 <- sliderInput("Save_DPI1", "DPI of Image", value = 400, min = 100, max = 1000, step = 1)   ##Slider to change the DPI of the saved image
Save_button1 <- actionButton("Save_button1", "Save image")                                            ##Button that saves the image
Save1 <- wellPanel(Save_name1, Save_DPI1, Save_button1)                                               ##Merges the objects above into one

Plot2 <- plotOutput("Error")                                                                          ##Object where the Error plot will be shown
Save_name2 <- textInput("Save_name2", "Save image to .png", value = "ShinyError")                     ##Text input where the user can name the file he wants the plot to be saved in
Save_DPI2 <- sliderInput("Save_DPI2", "DPI of Image", value = 400, min = 100, max = 1000, step = 1)   ##Slider to change the DPI of the saved image
Save_button2 <- actionButton("Save_button2", "Save image")                                            ##Button that saves the image
Save2 <- wellPanel(Save_name2, Save_DPI2, Save_button2)                                               ##Merges the objects above into one

## Server Function --------------------------------------------------------------------------------------------------------------------

server_LDA_SVM <- function(input, output){
  shiny_env <- new_environment()                          ##Creates a new environment where the plots will be saved in later for easier access
      
  observeEvent(input$Load_button, {                       ##What happens when the Load button is pressed:
    dataset <- input$Load_test                            ##loads in the dataset from the name given in the text input
    print(paste("Loading Dataset Object", dataset))       ##Console output confing that the object is being loaded
    shiny_set <- eval(parse_expr(dataset))                ##Object test_shiny gets overwritten by the loaded dataset
    assign("shiny_set", shiny_set, envir=shiny_env)
    print("Data loaded")
  })
  
  observeEvent(input$Test_button, {                       ##What happens when the "create random Test" button is pressed:
    nparam <- input$Param                                 ##Number of Dimensions and
    nclasses <- input$Classes                             ##number of Classes gets loaded
    print(paste("Creating a test with", nclasses, "classes in", nparam, "dimensions.")) ##Confirmation console output
    test_shiny <- make_test(100,                         ##Creating a test via make_test() with classes, dimensions and sigma values
                      nparam = nparam,
                      nclasses = nclasses,
                      sigma = c(input$Sigma1, input$Sigma2, input$Sigma3, input$Sigma4, input$Sigma5, 
                                input$Sigma6, input$Sigma7, input$Sigma8, input$Sigma9, input$Sigma10))
    shiny_set <- make_set(test_shiny, by = "class", title = "Shiny Test", 
                          description = "This is a randomly generated test for shiny")
    assign("shiny_set", shiny_set, envir=shiny_env)
    print("test created")                                 ##Confirmation that the test has been created
  })
  
  observeEvent(input$Calc_button, {                       ##What happens when the "classify" button is pressed:
    Classfun <- input$Classifier                          
    BG <- input$Background
    Base <- input$Base
    print(paste("classifying with:", Classfun, "and basis expansion:", Base))
    print("Please wait for the calculations to finish")
    shiny_set <- shiny_env$shiny_set
    
    if(Classfun == "LDA"){
      func_shiny <- LDA(shiny_set)[['name']]
      Error_plot_shiny <- plot_error(shiny_set, func_shiny)
      Class_plot_shiny <- make_2D_plot(shiny_set,
                          func_shiny,
                          ppu = 5)
      Error_plot_shiny <- do.call(grid.arrange, Error_plot_shiny)
    }
    
    if(Classfun == "QDA"){
      func_shiny <- QDA(shiny_set)[['name']]
      Error_plot_shiny <- plot_error(shiny_set, func_shiny)
      Class_plot_shiny <- make_2D_plot(shiny_set,
                                       func_shiny,
                                       ppu = 5)
      Error_plot_shiny <- do.call(grid.arrange, Error_plot_shiny)
    }
    
    if(Classfun == "PDA"){
      func_shiny <- PDA(shiny_set, base = Base)[['name']]
      Error_plot_shiny <- plot_error(shiny_set, func_shiny)
      Class_plot_shiny <- make_2D_plot(shiny_set,
                                       func_shiny,
                                       ppu = 5)
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
    
    assign("Class_plot_shiny", Class_plot_shiny, envir=shiny_env)           ##Putting the plots into the environment created at the beginning to access the later
    assign("Error_plot_shiny", Error_plot_shiny, envir=shiny_env)
    
    output$Classification <- renderPlot({Class_plot_shiny})                 ##Plotting the classification in the second panel
    output$Error <- renderPlot({do.call(grid.arrange, Error_plot_shiny)})   ##Plotting the error in the thrid panel
    print("finished!")                                                      ##Confirmation message that all calculations are finished
  })
  
  observeEvent(input$Save_button1, {                                        ##What happens when the first "Save Image" button is pressed:
    shiny_name <- input$Save_name1                                          ##Reading the name from the text input
    DPI <- input$Save_DPI1                                                  ##Reading the DPI value from the slider
    Shiny_plot <- shiny_env$Class_plot_shiny                                ##Getting the plot out of the environment created above
    print(paste0("Saving plot with ", DPI, " DPI to: ", shiny_name))        ##Console output telling the user that the image is now beeing saved
    ggsave(paste0(shiny_name, ".png"),                                      ##Saving the image via ggsave using the name and DPI from above
           plot = Shiny_plot,
           device = 'png',
           dpi = DPI)
    print("Image saved")                                                    ##Confirmation message that the image is now saved
  })
  
  observeEvent(input$Save_button2, {                                        ##What happens when the second "Save Image" button is pressed:
    shiny_name <- input$Save_name2                                          ##Reading the name from the text input
    DPI <- input$Save_DPI2                                                  ##Reading the DPI value from the slider
    Shiny_plot <- shiny_env$Error_plot_shiny                                ##Getting the plot out of the environment created above
    print(paste0("Saving plot with ", DPI, " DPI to: ", shiny_name))        ##Console output telling the user that the image is now beeing saved
    ggsave(paste0(shiny_name, ".png"),                                      ##Saving the image via ggsave using the name and DPI from above
           plot = Shiny_plot,
           device = 'png',
           dpi = DPI)
    print("Image saved")                                                    ##Confirmation message that the image is now saved
  })
}

                                                                                  
ui_LDA_SVM <- fluidPage(                                                          ## Creating the fluid page for shiny with all preinitialized objects
  headerPanel("RProject LDA & SVM"),                                              ##Name of the shiny App
  tabsetPanel(                                                                    ##Splitting the app into 3 tabs
    tabPanel("Options",                                                           ##First  page with all options
             fluidRow(column(width=4, titlePanel("Dataset"), Load, Create),       ##Splitting the first page into 3 columns,  1st for reading creating/reading in tests
                      column(width=4, titlePanel("Test Variables"), Sigma),       ##                                          2nd for changing the paramters of a randomly generated test
                      column(width=4, titlePanel("Classification"), Classify))),  ##                                          3rd for the classification options
    tabPanel("Classification", Plot1, Save1),                                     ##Second page with the classification plot output
    tabPanel("Error", Plot2, Save2)                                               ##Third page with the error plot output
  ) 
)


classify_app <- function(){
  shinyApp(ui_LDA_SVM, server_LDA_SVM)
}
