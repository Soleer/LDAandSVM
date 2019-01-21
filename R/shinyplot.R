Classifier <- selectInput("Classifier", "Select classifier", choices=c("LDA", "QDA", "PDA"))
Base <- selectInput("Base (only for PDA)", "Select Basis Expansion", choices = c("none", "quad", "cube", "sqrt", "log", "abs"))
Background <- radioButtons("Background", "Plot classification Grid (greatly increases calculation time)", c("FALSE", "TRUE"))
Calc_button <- actionButton("Calc_button", "Classifiy")
Plot1 <- plotOutput("Classification")
Plot2 <- plotOutput("Error")

sig <- c(1,1.5)
test <- make_test(100,
                  nparam = 2,
                  nclasses = 2,
                  sigma = sig)

server <- function(input, output){
  observeEvent(input$Calc_button, {
    Classfun <- input$Classifier
    BG <- input$Background
    Base <- input$Base
    print(Classfun)
    
    if(Classfun == "LDA"){
      f <- classify(unique(test$class), LDA(test[1:(ncol(test)-1)], test$class))
      liste <- plot_error(test[1:(ncol(test)-1)], test$class, f)
      testplot <-
        make_2D_plot(test[1:(ncol(test)-1)],
                     test$class,
                     f,
                     ppu = 5,
                     bg = BG)
    }
    
    if(Classfun == "QDA"){
      f <- classify(unique(test$class), QDA(test[1:(ncol(test)-1)], test$class))
      liste <- plot_error(test[1:(ncol(test)-1)], test$class, f)
      testplot <-
        make_2D_plot(test[1:(ncol(test)-1)],
                     test$class,
                     f,
                     ppu = 5,
                     bg = BG)
    }
    
    if(Classfun == "PDA"){
      f <- classify(unique(test$class), PDA(test[1:(ncol(test)-1)], test$class, base = Base))
      liste <- plot_error(test[1:(ncol(test)-1)], test$class, f)
      testplot <-
        make_2D_plot(test[1:(ncol(test)-1)],
                     test$class,
                     f,
                     ppu = 5,
                     bg = BG)
    }
    output$Classification <- renderPlot({testplot})
    output$Error <- renderPlot({do.call(grid.arrange, liste)})
  })
}


ui <- fluidPage(
  tabsetPanel(
    tabPanel("Options", Classifier, Base, Background, Calc_button),
    tabPanel("Classification", Plot1),
    tabPanel("Error", Plot2)
  )
)