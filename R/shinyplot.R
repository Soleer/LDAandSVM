Classifier <- selectInput("Classifier", "Select classifier", choices=c("LDA", "QDA", "PDA", "RDA", "SVM"))                                  ##Dropdown menu where the classifier functions can be selected
Base <- selectInput("Base", "Select Basis Expansion", choices = c("id", "quad", "cube", "sqrt", "log", "abs"))                              ##Dropdown menu where the basis expansion for PDA can be selected
Background <- radioButtons("Background", "Plot classification Grid", c("TRUE", "FALSE"))                                                    ##Radio buttons with the option to print the background or not
Project <- radioButtons("Project", "Project the data onto the main components", c("FALSE", "TRUE"))                                         ##Selection if the plot shoul be projected
PPU <- sliderInput("PPU", "Select the points per unit for the background", value = 5, min = 1, max = 10, step = 1)                           ##Selection for the PPU of the plot

Calc_button <- actionButton("Calc_button", "Classifiy")                                                                                     ##Button that starts the process of classifying
Kernel <- selectInput("Kernel", "Select the Kernel", choices = c("id", "poly", "radial", "neural"))                                         ##Dropdown menu for the Kernel to be used
Margin <- numericInput("Margin", "Select how exact the support vector lines should be", value = 1, min = 0.1, max = 1000)                   ##numeric input for the margin c in SVM
Var_Pol <- sliderInput("Var_pol", "Select the power of the polynomial", value = 2, min = 1, max = 15, step = 1)                             ##slider input for the degree of the polynomial
Radial <- sliderInput("Radial", "Select the factor of the radial basis", value = 1, min = 0.1, max = 50, step = 0.01)                       ##slider input for the multiplication factor in the radial kernel
Var_Pol_neu <- sliderInput("Var_pol_neu", "Select the factor for the neural Kernel", value = 2, min = 0.1, max = 15, step = 0.01)           ##slider input for the factor in the neural kernel
Radial_neu <- sliderInput("Radial_neu", "Select the summand for the neural Kernel", value = 1, min = 0.1, max = 100, step = 0.01)           ##slider input for the summand in the neural kernel
Alpha <- sliderInput("Alpha", "Select the Alpha value", value = 0, min = 0, max = 1, step = 0.01)                                           ##slider input for the Alpha value in RDA
Gamma <- sliderInput("Gamma", "Select the Gamma value", value = 0, min = 0, max = 1, step = 0.01)                                           ##slider input for the Gamma value in RDA

Classify <- wellPanel(Classifier, Background, PPU, Project, Calc_button)                                       ##Merging the Objects above into one for every group
PDA_pan <- conditionalPanel(condition = "input.Classifier == 'PDA'", Base)                                     ##Panel that appears if PDA is selected
RDA_pan <- conditionalPanel(condition = "input.Classifier == 'RDA'", Alpha, Gamma)                             ##Panel that appears if RDA is selected
SVM_pan <-  conditionalPanel(condition = "input.Classifier == 'SVM'", Kernel, Margin,                          ##Panel that appears if SVM is selected with subpanels for the different Kernels
                             conditionalPanel(condition = "input.Kernel == 'poly'", Var_Pol) ,
                             conditionalPanel(condition = "input.Kernel == 'radial'", Radial),
                             conditionalPanel(condition = "input.Kernel == 'neural'", Var_Pol_neu, Radial_neu))

Load_test <- textInput("Load_test", "Load an R6 Dataset", value = "Name of the object")               ##Text input where the name of an object can be written. This object will then be loaded into shiny
Load_button <- actionButton("Load_button", "Load Object")                                             ##Button that loads in th object specified above
Load <- wellPanel(Load_test, Load_button)                                                             ##Merges the two objects above into one

Observations <- sliderInput("Observations", "Number of observations per class", value = 100, min = 2, max = 250) ##Slider that lets the user chose the number of observations per class
Param <- sliderInput("Param", "Number of Dimensions", value = 2, min = 2, max = 10, step = 1)         ##Slider that lets the user chose the number of dimensions in a randomly generated test
Classes <- sliderInput("Classes", "Number of Classes", value = 2, min = 2, max  = 10, step = 1)       ##Slider that lets the user chose the number of classes in a randomly generated test
Test_button <- actionButton("Test_button", "Generate random testdata")                                ##Button to start generating a random test
Create <- wellPanel(Observations, Param, Classes, Test_button)                                        ##Merges the objects into one 

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
  
  observeEvent(input$Test_button, {                                                       ##What happens when the "create random Test" button is pressed:
    nparam <- input$Param                                                                 ##Number of Dimensions and
    nclasses <- input$Classes                                                             ##number of Classes gets loaded
    Observations <- input$Observations                                                    ##Number of observations per class
    print(paste("Creating a test with", nclasses, "classes in", nparam, "dimensions."))   ##Confirmation console output
    test_shiny <- make_test(Observations,                                                 ##Creating a test via make_test() with classes, dimensions and sigma values
                      nparam = nparam,
                      nclasses = nclasses,
                      sigma = c(input$Sigma1, input$Sigma2, input$Sigma3, input$Sigma4, input$Sigma5, 
                                input$Sigma6, input$Sigma7, input$Sigma8, input$Sigma9, input$Sigma10))
    shiny_set <- make_set(test_shiny, by = "class", title = "Shiny Test", 
                          description = "This is a randomly generated test for shiny")
    assign("shiny_set", shiny_set, envir=shiny_env)                                       ##Adding the set into the environment for easier access
    print("test created")                                                                 ##Confirmation that the test has been created
  })
  
  observeEvent(input$Calc_button, {                                                       ##What happens when the "classify" button is pressed:
    Classfun <- input$Classifier                                                          ##Function by which the set should be classified               
    BG <- input$Background                                                                ##Reading whether the plot should be with a background
    Project <- input$Project                                                              ##Reading whether the plot should be projected onto the maincomponents
    PPU <- input$PPU
    
    print("Please wait for the calculations to finish")
    shiny_set <- shiny_env$shiny_set                                                      ##Reading the shiny set out of the environment
    
    if(Classfun == "LDA"){                                                                ##What happens when the set is classified with LDA:
      print("classifying with: LDA")                                                      ##Console output for transparency
      
      func_shiny <- LDA(shiny_set)[['name']]                                              ##Classfifying the set with LDA
      Error_plot_shiny <- plot_error(shiny_set, func_shiny)                               ##Plotting the Error
      Class_plot_shiny <- make_2D_plot(shiny_set,                                         ##Plotting the Classification results
                          func_shiny,
                          ppu = PPU,
                          bg = BG,
                          project = Project)
      Error_plot_shiny <- do.call(grid.arrange, Error_plot_shiny)                         ##Arranging and saving the Error plot in an object 
    }
    
    if(Classfun == "QDA"){                                                                ##What happens when the set is classified with QDA (for further comments see LDA)
      print("classifying with: QDA")                                                      ##Console output for transparency
      
      func_shiny <- QDA(shiny_set)[['name']]
      Error_plot_shiny <- plot_error(shiny_set, func_shiny)
      Class_plot_shiny <- make_2D_plot(shiny_set,
                                       func_shiny,
                                       ppu = PPU,
                                       bg = BG,
                                       project = Project)
      Error_plot_shiny <- do.call(grid.arrange, Error_plot_shiny)
    }
    
    if(Classfun == "PDA"){                                                                ##What happens when the set is classified with PDA (for further comments see LDA)
      Base <- input$Base                                                                  ##Reading which basis expansion should be used (only PDA)
      print(paste("classifying with PDA and basis expansion =", Base))                    ##Console output for transparency
      
      func_shiny <- PDA(shiny_set, base = Base)[['name']]                                 ##Classifying the set with PDA and the given basis expansion
      Error_plot_shiny <- plot_error(shiny_set, func_shiny)
      Class_plot_shiny <- make_2D_plot(shiny_set,
                                       func_shiny,
                                       ppu = PPU,
                                       bg = BG,
                                       project = Project)
      Error_plot_shiny <- do.call(grid.arrange, Error_plot_shiny)
    }
    
    if(Classfun == "RDA"){
      Alpha <- input$Alpha                                                                ##Reading the alpha value
      Gamma <- input$Gamma                                                                ##Reading the gamma value
      print(paste("classifying with RDA, gamma =", Gamma, "and alpha =", Alpha))          ##Console output for transparency
      
      func_shiny <- RDA(shiny_set, alpha = Alpha, gamma = Gamma)[['name']]                ##Classifying the set with RDA and the given Alpha and Gamma values
      Error_plot_shiny <- plot_error(shiny_set, func_shiny)
      Class_plot_shiny <- make_2D_plot(shiny_set,
                                       func_shiny,
                                       ppu = PPU,
                                       bg = BG,
                                       project = Project)
      Error_plot_shiny <- do.call(grid.arrange, Error_plot_shiny)
    }
    
    if(Classfun == "SVM"){
      Kernel <- input$Kernel                                                              ##Reading in the Kernel 
      Margin <- input$Margin                                                              ##Reading in the margin
      if(Kernel == "neural"){                                                             ##If the neural Kernel is selected the values mean a different thing as with the other Kernels
        Var_Pol <- input$Var_Pol_neu                                                      ##Reading in the values for the Kernels
        Radial <- input$Radial_neu
      }
      else{
        Var_Pol <- input$Var_Pol
        Radial <- input$Radial
      }
        
      print(paste("classifying with SVM, Kernel =", Kernel, ", Margin =", Margin, ", Degree =", Var_Pol, ", Radial Multiplier =", Radial)) ##Console output for transparency                    ##Console output for transparency
      
      func_shiny <- SVM(shiny_set, C = Margin, kernel = Kernel, d = Var_Pol, g = Radial)[['name']]
      Error_plot_shiny <- plot_error(shiny_set, func_shiny)
      Class_plot_shiny <- make_2D_plot(shiny_set,
                                       func_shiny,
                                       ppu = PPU,
                                       bg = BG,
                                       project = Project)
      Error_plot_shiny <- do.call(grid.arrange, Error_plot_shiny)
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
                      column(width=4, titlePanel("Classification"), Classify, PDA_pan, RDA_pan, SVM_pan))),##                 3rd for the classification options
    tabPanel("Classification", Plot1, Save1),                                     ##Second page with the classification plot output
    tabPanel("Error", Plot2, Save2)                                               ##Third page with the error plot output
  ) 
)


#' classify_app
#'
#' A shiny application for classifying data with a graphic interface. Use the "Load Test" option to 
#' load in a R6 data_set object by its name or generate a random test using the options on the 
#' left and the paramters for the std in the middle. For classifying use the column on the right 
#' and chose the classifier and potentially and further parameters. To view the classification 
#' plot go to the second tab and to see the error plot go to the thrid tab. In both these the
#' image can be saved with the options  at the bottom. The images will be saved as .png
#' @return Nothing
#' @examples
#'
#' @export
classify_app <- function(){
  shinyApp(ui_LDA_SVM, server_LDA_SVM)
}
