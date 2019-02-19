library(ggplot2)
library(gridExtra)
library(quadprog)
library(R6)
library(MASS)
library(NlcOptim)
library(e1071)
library(shiny)
library(rlang)
source("R/Basis_expansion.R")
source("R/Test.R")
source("R/oop.R")
source("R/Estimators.R")
source("R/Classifier_funs.R")
source("R/plot_functions.R")
#source("R/svm_alt.R")
source("R/shinyplot.R")
source("R/RDA.R")

set.seed(0)

##Analyse
sig <- c(1.5, 2, 2.5, 1.3, 1.1, 2.1, 1.8)
dimension <- 2

test <- make_test(100,
                  nparam = dimension,
                  nclasses = 3,
                  sigma = sig)

set <-
  make_set(test,
           by = "class",
           title = "TEST",
           description = "Weil ich kann!")
### Shiny-Interface
classify_app( )
### PDA
func_name0 <-  PDA(set, base = "quad")[['name']]
liste0 <- plot_error(set, func_name0)
p0 <- do.call(grid.arrange, liste0)
testplot0 <-
  make_2D_plot(set,
               func_name0,
               ppu = 5,
               bg = FALSE)
plotlist0 <- list(p0, testplot0)
nice0 <- do.call("grid.arrange", c(plotlist0, ncol = 2, top = "PDA"))

ggsave('PDA.png',
       plot = nice0,
       device = 'png',
       dpi = 400)


### LDA
func_name1 <- LDA(set)[['name']]
liste1 <- plot_error(set, func_name1)
p1 <- do.call(grid.arrange, liste1)
testplot1 <-
  make_2D_plot(set,
               func_name1,
               ppu = 5)
plotlist1 <- list(p1, testplot1)

nice1 <-
  do.call("grid.arrange", c(plotlist1, ncol = 2, top = "LDA"))
ggsave('LDA.png',
       plot = nice1,
       device = 'png',
       dpi = 400)


### QDA
func_name2 <- QDA(set)[['name']]
liste2 <- plot_error(set, func_name2)
p2 <- do.call(grid.arrange, liste2)
testplot2 <-
  make_2D_plot(set,
               func_name2,
               ppu = 5)
plotlist2 <- list(p2, testplot2)

nice2 <-
  do.call("grid.arrange", c(plotlist2, ncol = 2, top = "QDA"))
ggsave('QDA.png',
       plot = nice2,
       device = 'png',
       dpi = 400)

### RDA
func_name3 <- RDA(set)[['name']]
liste3 <- plot_error(set, func_name3)
p3 <- do.call(grid.arrange, liste3)
testplot3 <-
  make_2D_plot(set,
               func_name3,
               ppu = 5)
plotlist3 <- list(p3, testplot3)

nice3 <-
  do.call("grid.arrange", c(plotlist3, ncol = 2, top = "RDA"))
ggsave('RDA.png',
       plot = nice3,
       device = 'png',
       dpi = 400)

###svm
f4 <-
  svm_classify(uresults = set$classes,
               t = svm(set$data, set$results))
liste4 <- plot_error(set, f4)
p4 <- do.call(grid.arrange, liste4)
testplot4 <-
  make_2D_plot(set,
               f4,
               ppu = 5)
plotlist4 <- list(p4, testplot4)

nice4 <-
  do.call("grid.arrange", c(plotlist4, ncol = 2, top = "svm"))
ggsave('svm.png',
       plot = nice4,
       device = 'png',
       dpi = 400)

