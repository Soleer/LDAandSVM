library(ggplot2)
library(gridExtra)
library(quadprog)
library(R6)
library(MASS)
library(NlcOptim)
library(e1071)
library(shiny)
library(NlcOptim)
library(shiny)
library(rlang)
source("R/Basis_expansion.R")
source("R/Test.R")
source("R/oop.R")
source("R/Estimators.R")
source("R/Classifier_funs.R")
source("R/plot_functions.R")
source("R/svm.R")
source("R/shinyplot.R")
set.seed(0)

##Real Data
d1 <- read.csv("student-alcohol-consumption/student-mat.csv",header=TRUE)
d2 <- read.csv("student-alcohol-consumption/student-por.csv",header=TRUE)


d3=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))
str(d3)

data <- data.frame(results = d3$G1.x, absences = d3$absences.x, sex = d3$sex)
data
SAC_G1 <- make_set(data, by = "sex", title = "Student Alcohol consumption", description = "This is the student alcohol consumption for the G1 Test classified by sex")
classify_app()
LDA(SAC_G1)
make_2D_plot(SAC_G1,"LDA_1",ppu=1)

##Analyse
sig <- c(1.5, 2, 2.5, 1.3, 1.1, 2.1, 1.8)
dimension <- 2

test <- make_test(100,
                  nparam = dimension,
                  nclasses = 4,
                  sigma = sig)

set <-
  make_set(test,
           by = "class",
           title = "TEST",
           description = "Weil ich kann!")
### Shiny-Interface
classify_app()
### PDA
func_name0 <-  PDA(set, base = "quad")[['name']]
plot_summary(set,func_name0)
### LDA
func_name1 <- LDA(set)[['name']]
plot_summary(set,func_name1)


### QDA
func_name2 <- QDA(set)[['name']]
plot_summary(set,func_name2)
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



## Motivating example
set.seed(2)
height_B <- rnorm(115, mean = 30, sd = 4)
decibel_B <- rnorm(115, mean = 70, sd = 10)
classes_B <- rep("Barock", 115)

height_R <- rnorm(78, mean = 17, sd = 6)
decibel_R <- rnorm(78, mean = 102, sd = 7)
classes_R <- rep("Renaissance", 78)

height_C <- rnorm(96, mean = 42, sd = 7)
decibel_C <- rnorm(96, mean = 64, sd = 8)
classes_C <- rep("Classic", 96)

height <- c(height_B, height_R, height_C)
decibel <- c(decibel_B, decibel_R, decibel_C)
class <- c(classes_B, classes_R, classes_C)

Rockets <- data.frame(height = height, decibel = decibel, class = class)

Rocket_set <- make_set(Rockets, by = 'class', title = "Rockets", description = "Rockets with their flight heigth")

func_name1 <- LDA(Rocket_set)[['name']]
liste1 <- plot_error(Rocket_set, func_name1)
p1 <- do.call(grid.arrange, liste1)
testplot1 <-
  make_2D_plot(Rocket_set,
               func_name1,
               ppu = 2)
plotlist1 <- list(p1, testplot1)

func_name2 <- QDA(Rocket_set)[['name']]
liste2 <- plot_error(Rocket_set, func_name2)
p2 <- do.call(grid.arrange, liste2)
testplot2 <-
  make_2D_plot(Rocket_set,
               func_name2,
               ppu = 2)
plotlist2 <- list(p1, testplot2)

ggplot(mtcars, aes(x=hp, y=mpg)) + geom_point(aes(y=qsec), color="red") 
testplot1 <- testplot1 + geom_point(aes(x = 60, y = -80))
testplot1
testplot2

Rocket_set$func[[func_name2]](c(60, -80))
