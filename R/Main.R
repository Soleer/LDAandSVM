# library(ggplot2)
# library(gridExtra)
# library(quadprog)
# library(R6)
# library(MASS)
# library(NlcOptim)
# library(e1071)
# library(shiny)
# library(NlcOptim)
# library(shiny)
# library(rlang)
# source("R/Basis_expansion.R")
# source("R/Test.R")
# source("R/oop.R")
# source("R/Estimators.R")
# source("R/Classifier_funs.R")
# source("R/plot_functions.R")
# source("R/svm_oop.R")
# source("R/shinyplot.R")
# set.seed(0)

# 
# ##Real Data
# d1 <- read.csv("student-alcohol-consumption/student-mat.csv",header=TRUE)
# d2 <- read.csv("student-alcohol-consumption/student-por.csv",header=TRUE)
# 
# 
# d3=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))
# str(d3)
# 
# data <- data.frame(results = d3$G1.x, absences = d3$absences.x, sex = d3$sex)
# data
# SAC_G1 <- make_set(data, by = "sex", title = "Student Alcohol consumption", description = "This is the student alcohol consumption for the G1 Test classified by sex")
# classify_app()
# LDA(SAC_G1)
# make_2D_plot(SAC_G1,"LDA_1",ppu=1)
# 
# ##Analyse
# sig <- c(1.5, 2, 2.5, 1.3, 1.1, 2.1, 1.8)
# dimension <- 2
# 
# test <- make_test(100,
#                   nparam = dimension,
#                   nclasses = 4,
#                   sigma = sig)
# 
# set <-
#   make_set(test,
#            by = "class",
#            title = "TEST",
#            description = "Weil ich kann!")
# ### Shiny-Interface
# classify_app()
# ### PDA
# set$parnames
# func_name0 <-  PDA(set, base = "quad")[['name']]
# plot_summary(set,func_name0)
# make_2D_plot(set,func_name0,project = c('a','b'))
# ### LDA
# func_name1 <- LDA(set)[['name']]
# plot_summary(set,func_name1)
# 
# 
# ### QDA
# func_name2 <- QDA(set)[['name']]
# plot_summary(set,func_name2)
# 
# ###svm
# f4 <-
#   svm_classify(uresults = set$classes,
#                t = svm(set$data, set$results))
# liste4 <- plot_error(set, f4)
# p4 <- do.call(grid.arrange, liste4)
# testplot4 <-
#   make_2D_plot(set,
#                f4,
#                ppu = 5)
# plotlist4 <- list(p4, testplot4)
# 
# nice4 <-
#   do.call("grid.arrange", c(plotlist4, ncol = 2, top = "svm"))
# ggsave('svm.png',
#        plot = nice4,
#        device = 'png',
#        dpi = 400)
# 
# 
# 
# ## Motivating example
# set.seed(2)
# height_B <- rnorm(115, mean = 30, sd = 4)
# decibel_B <- rnorm(115, mean = 70, sd = 10)
# classes_B <- rep("Barock", 115)
# 
# height_R <- rnorm(78, mean = 17, sd = 6)
# decibel_R <- rnorm(78, mean = 102, sd = 7)
# classes_R <- rep("Renaissance", 78)
# 
# height_C <- rnorm(96, mean = 42, sd = 7)
# decibel_C <- rnorm(96, mean = 64, sd = 8)
# classes_C <- rep("Classic", 96)
# 
# height <- c(height_B, height_R, height_C)
# decibel <- c(decibel_B, decibel_R, decibel_C)
# class <- c(classes_B, classes_R, classes_C)
# 
# Rockets <- data.frame(height = height, decibel = decibel, class = class)
# 
# Rocket_set <- make_set(Rockets, by = 'class', title = "Rockets", description = "Rockets with their flight heigth")
# 
# func_name1 <- LDA(Rocket_set)[['name']]
# liste1 <- plot_error(Rocket_set, func_name1)
# p1 <- do.call(grid.arrange, liste1)
# testplot1 <-
#   make_2D_plot(Rocket_set,
#                func_name1,
#                ppu = 2,
#                project = FALSE)
# plotlist1 <- list(p1, testplot1)
# 
# func_name2 <- QDA(Rocket_set)[['name']]
# liste2 <- plot_error(Rocket_set, func_name2)
# p2 <- do.call(grid.arrange, liste2)
# 
# testplot2 <-
#   make_2D_plot(Rocket_set,
#                func_name2,
#                ppu = 2,
#                project = FALSE)
# 
# testplot2
# 
# plotlist2 <- list(p1, testplot2)
# 
# ggplot(mtcars, aes(x=hp, y=mpg)) + geom_point(aes(y=qsec), color="red") 
# testplot1 <- testplot1 + geom_point(aes(x = 60, y = -80))
# testplot1
# testplot2
# 
# blub <- plot_summary(Rocket_set,func_name1)
# blub
# 
# Rocket_set$func[[func_name1]](c(40, 103))
# 
# 
# ## Shiny SVM
# set.seed(2)
# sig <- c(1.5, 2, 2.5, 1.3)  ##Creating a vector of standard deviations for the the make_test() function
# dimension <- 2   ##Number of dimensions the test should have. 2 for simlicity
# 
# test <- make_test(100, ##Creating a random test where each class has 100 observations
#                   nparam = dimension, ##in 2 Dimensions
#                   nclasses = 4, ##with 4 classes
#                   sigma = sig) ##That are distributed as specified above
# 
# set <- make_set(test, ##Creating a R6 dataset object with the generated data
#                 by = "class", ##Column in which the classes of the observations are listed
#                 title = "R Markdown ",
#                 description = "R Markdown presentation file")
# classify_app()


# test <- make_test(nclasses = 3,ninputs = 50)
# test <- make_set(test,"class","TITEL",description = "Description")
# test$func_names
# results <- test$results
# data <- test$data
# dd <- SVM(test,C = 50,kernel = "neural",d=5,g=3)[['name']]
# f3 <- test$func[[dd]]
# 
# list("SVM_2")
# calc_error(test,dd)
# dd
# 
# 
# gg <- 0
# for (i in 1:150) {
#   gg[i] <- (f3(as.double(data[i,])))
# }
# gg

# # 
# # ##Real Data
# # d1 <- read.csv("student-alcohol-consumption/student-mat.csv",header=TRUE)
# # d2 <- read.csv("student-alcohol-consumption/student-por.csv",header=TRUE)
# # 
# # 
# # d3=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))
# # str(d3)
# # 
# # data <- data.frame(results = d3$G1.x, absences = d3$absences.x, sex = d3$sex)
# # data
# # SAC_G1 <- make_set(data, by = "sex", title = "Student Alcohol consumption", description = "This is the student alcohol consumption for the G1 Test classified by sex")
# # classify_app()
# # LDA(SAC_G1)
# # make_2D_plot(SAC_G1,"LDA_1",ppu=1)
# # 
# # ##Analyse
# # sig <- c(1.5, 2, 2.5, 1.3, 1.1, 2.1, 1.8)
# # dimension <- 2
# # 
# # test <- make_test(100,
# #                   nparam = dimension,
# #                   nclasses = 4,
# #                   sigma = sig)
# # 
# # set <-
# #   make_set(test,
# #            by = "class",
# #            title = "TEST",
# #            description = "Weil ich kann!")
# # ### Shiny-Interface
# # classify_app()
# # ### PDA
# # set$parnames
# # func_name0 <-  PDA(set, base = "quad")[['name']]
# # plot_summary(set,func_name0)
# # make_2D_plot(set,func_name0,project = c('a','b'))
# # ### LDA
# # func_name1 <- LDA(set)[['name']]
# # plot_summary(set,func_name1)
# # 
# # 
# # ### QDA
# # func_name2 <- QDA(set)[['name']]
# # plot_summary(set,func_name2)
# # 
# # ###svm
# # f4 <-
# #   svm_classify(uresults = set$classes,
# #                t = svm(set$data, set$results))
# # liste4 <- plot_error(set, f4)
# # p4 <- do.call(grid.arrange, liste4)
# # testplot4 <-
# #   make_2D_plot(set,
# #                f4,
# #                ppu = 5)
# # plotlist4 <- list(p4, testplot4)
# # 
# # nice4 <-
# #   do.call("grid.arrange", c(plotlist4, ncol = 2, top = "svm"))
# # ggsave('svm.png',
# #        plot = nice4,
# #        device = 'png',
# #        dpi = 400)
# # 
# # 
# # 
# # ## Motivating example
# # set.seed(2)
# # height_B <- rnorm(115, mean = 30, sd = 4)
# # decibel_B <- rnorm(115, mean = 70, sd = 10)
# # classes_B <- rep("Barock", 115)
# # 
# # height_R <- rnorm(78, mean = 17, sd = 6)
# # decibel_R <- rnorm(78, mean = 102, sd = 7)
# # classes_R <- rep("Renaissance", 78)
# # 
# # height_C <- rnorm(96, mean = 42, sd = 7)
# # decibel_C <- rnorm(96, mean = 64, sd = 8)
# # classes_C <- rep("Classic", 96)
# # 
# # height <- c(height_B, height_R, height_C)
# # decibel <- c(decibel_B, decibel_R, decibel_C)
# # class <- c(classes_B, classes_R, classes_C)
# # 
# # Rockets <- data.frame(height = height, decibel = decibel, class = class)
# # 
# # Rocket_set <- make_set(Rockets, by = 'class', title = "Rockets", description = "Rockets with their flight heigth")
# # 
# # func_name1 <- LDA(Rocket_set)[['name']]
# # liste1 <- plot_error(Rocket_set, func_name1)
# # p1 <- do.call(grid.arrange, liste1)
# # testplot1 <-
# #   make_2D_plot(Rocket_set,
# #                func_name1,
# #                ppu = 2,
# #                project = FALSE)
# # plotlist1 <- list(p1, testplot1)
# # 
# # func_name2 <- QDA(Rocket_set)[['name']]
# # liste2 <- plot_error(Rocket_set, func_name2)
# # p2 <- do.call(grid.arrange, liste2)
# # 
# # testplot2 <-
# #   make_2D_plot(Rocket_set,
# #                func_name2,
# #                ppu = 2,
# #                project = FALSE)
# # 
# # testplot2
# # 
# # plotlist2 <- list(p1, testplot2)
# # 
# # ggplot(mtcars, aes(x=hp, y=mpg)) + geom_point(aes(y=qsec), color="red") 
# # testplot1 <- testplot1 + geom_point(aes(x = 60, y = -80))
# # testplot1
# # testplot2
# # 
# # blub <- plot_summary(Rocket_set,func_name1)
# # blub
# # 
# # Rocket_set$func[[func_name1]](c(40, 103))
# # 
# # 
# # ## Shiny SVM
# # set.seed(2)
# # sig <- c(1.5, 2, 2.5, 1.3)  ##Creating a vector of standard deviations for the the make_test() function
# # dimension <- 2   ##Number of dimensions the test should have. 2 for simlicity
# # 
# # test <- make_test(100, ##Creating a random test where each class has 100 observations
# #                   nparam = dimension, ##in 2 Dimensions
# #                   nclasses = 4, ##with 4 classes
# #                   sigma = sig) ##That are distributed as specified above
# # 
# # set <- make_set(test, ##Creating a R6 dataset object with the generated data
# #                 by = "class", ##Column in which the classes of the observations are listed
# #                 title = "R Markdown ",
# #                 description = "R Markdown presentation file")
# # classify_app()
# 
# 
# d1 <- read.csv("exams/StudentsPerformance.csv",header=TRUE)
# 
# Performance_math_writing_gender  <- data.frame(math = d1$math.score, write = d1$writing.score, gender = d1$gender)
# Performance_math_writing_pared  <- data.frame(math = d1$math.score, write = d1$reading.score, pared = d1$parental.level.of.education)
# 
# Performance_math_writing_pared_set <- make_set(Performance_math_writing_pared, by = "pared", title = "Students Performance in test", "Test real data")
# 
# fun <- LDA(Performance_math_writing_pared_set)[['name']]
# plot <- make_2D_plot(Performance_math_writing_pared_set, fun, ppu = 1, project = FALSE)
# plot
# classify_app()
#  
# # test <- make_test(nclasses = 3,ninputs = 50)
# # test <- make_set(test,"class","TITEL",description = "Description")
# # test$func_names
# # results <- test$results
# # data <- test$data
# # dd <- SVM(test,C = 50,kernel = "neural",d=5,g=3)[['name']]
# # f3 <- test$func[[dd]]
# # 
# # list("SVM_2")
# # calc_error(test,dd)
# # dd
# # 
# # 
# # gg <- 0
# # for (i in 1:150) {
# #   gg[i] <- (f3(as.double(data[i,])))
# # }
# # gg
