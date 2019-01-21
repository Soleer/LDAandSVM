library(ggplot2)
library(gridExtra)
library(quadprog)
library("NlcOptim")
library("shiny")
source("R/Basis_expansion.R")
source('R/Test.R')
source("R/Estimators.R")
source("R/Classifier_funs.R")
source("R/plot_functions.R")
<<<<<<< HEAD
#source("R/svm.R")
=======
source("R/svm.R")
source("R/shinyplot.R")
>>>>>>> 2fc55a8a1295caabf708ab5181621fbb4beb92a3
set.seed(0)

##Analyse
sig <- c(1.5,2,2.5,1.3,1.1,2.1,1.8)
dimension <- 3

test <- make_test(100,
                  nparam = dimension,
                  nclasses = 6,
                  sigma = sig)

<<<<<<< HEAD
=======
### Shiny-Interface

shinyApp(ui, server)

>>>>>>> 2fc55a8a1295caabf708ab5181621fbb4beb92a3
### PDA
f <- classify(unique(test$class), PDA(test[1:dimension], test$class, base = "quad"))
liste <- plot_error(test[1:dimension], test$class, f)
p1 <- do.call(grid.arrange, liste)
testplot <-
  make_2D_plot(test[1:dimension],
               test$class,
               f,
               ppu = 5,
               bg = FALSE)
plotlist <- list(p1, testplot)
nice <- do.call("grid.arrange", c(plotlist, ncol = 2, top = "PDA"))

ggsave('PDA.png',
       plot = nice,
       device = 'png',
       dpi = 400)


### LDA
f2 <- classify(unique(test$class), LDA(test[1:dimension], test$class))
liste2 <- plot_error(test[1:dimension], test$class, f2)
p2 <- do.call(grid.arrange, liste2)
testplot2 <-
  make_2D_plot(test[1:dimension],
               test$class,
               f2,
               ppu = 5)
plotlist2 <- list(p2, testplot2)

nice2 <-
  do.call("grid.arrange", c(plotlist2, ncol = 2, top = "LDA"))
ggsave('LDA.png',
       plot = nice2,
       device = 'png',
       dpi = 400)


### QDA
f3 <- classify(unique(test$class), QDA(test[1:dimension], test$class))
liste3 <- plot_error(test[1:dimension], test$class, f3)
p3 <- do.call(grid.arrange, liste3)
testplot3 <-
  make_2D_plot(test[1:dimension],
               test$class,
               f3,
               ppu = 5)
plotlist3 <- list(p3, testplot3)

nice3 <-
  do.call("grid.arrange", c(plotlist3, ncol = 2, top = "QDA"))
ggsave('QDA.png',
       plot = nice3,
       device = 'png',
       dpi = 400)
