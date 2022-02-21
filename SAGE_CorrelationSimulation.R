#########################
## Simulating Correlated Data - Sage Topic 7
## Script by: Abbey Hammell 
##
## Description: Script generates correlated data, which are used as concept examples in 
## SAGE - Student Success: Data Literacy Module: A Few Basic Statistical Concepts by Alicia Hofelich Mohr & Abbey Hammell (in the process of being published as of Feb 2022) 
#########################

#####
######## PREP WORKSPACE
#####


## clear workspace
rm(list=ls())

## attach packages 
pacman::p_load(MASS,
               ggplot2,
               gridExtra)


#create correlation simulation function 
#mu.vector is a vector of distribution means you want your data to fall around
#sigma.matrix is the correlation matrix you want to simulate 
#n is the number of data points (observations) you want to simulate

corsim <- function(mu.vector, sigma.matrix, n){
  x <- as.data.frame(mvrnorm(n=n, mu=mu.vector, 
                             Sigma=sigma.matrix, empirical = TRUE))
  return(x)
}



###########################################################################
######## GENERATE CORRELATED DATA - COEFFICIENT EXPLANATION SECTION 
###########################################################################


# create different correlation matrices
sigma.pos1 <- rbind(c(1,1), c(1,1)) # r = 1
sigma.neg1 <- rbind(c(1,-1), c(-1,1)) # r = -1
sigma.0 <- rbind(c(1,0), c(0,1)) # r = 0
sigma.pos.1 <- rbind(c(1,.1), c(.1,1)) # r = .1 
sigma.neg.1 <- rbind(c(1,-.1), c(-.1,1)) # r = -.1 
sigma.pos.3 <- rbind(c(1,.3), c(.3,1)) # r = .3 
sigma.neg.3 <- rbind(c(1,-.3), c(-.3,1)) # r = -.3
sigma.pos.5 <- rbind(c(1,.5), c(.5,1)) # r = .5 
sigma.neg.5 <- rbind(c(1,-.5), c(-.5,1)) # r = -.5 


# create the mean vector for correlation simulation 
mu.vector <-c(3.5, 3.5) 

#set seed for randomization
set.seed(23456)

# generate the multivariate normal distribution with 30 data points
# using correlation matrices and mu.vector above
cor.pos1 <- corsim(mu.vector, sigma.pos1, n = 30)
cor.neg1 <- corsim(mu.vector, sigma.neg1, n = 30) 
cor.0  <- corsim(mu.vector, sigma.0, n = 30) 
cor.pos.1 <- corsim(mu.vector, sigma.pos.1, n = 30)
cor.neg.1 <- corsim(mu.vector, sigma.neg.1, n = 30)
cor.pos.3 <- corsim(mu.vector, sigma.pos.3, n = 30)
cor.neg.3 <- corsim(mu.vector, sigma.neg.3, n = 30)
cor.pos.5 <- corsim(mu.vector, sigma.pos.5, n = 30)
cor.neg.5 <- corsim(mu.vector, sigma.neg.5, n = 30)


#####
######## PLOT THE DATA FOR COMPARISON


## first, save the theme as an object, for use with a bunch of the graphs
themesave <- theme(plot.title = element_text(hjust = 0.5, size = 30, face = "bold", 
                                margin = margin(b = 20)),
      axis.line = element_line(colour = "black"),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.title = element_text(size=14, face="bold", colour = "black"),
      axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
      axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
      axis.text=element_text(size=12, colour = "black"),
      plot.margin=unit(c(.5,.5,.5,.5),"cm")
)



#r = 1
(graph.pos1 <- ggplot(data = cor.pos1, aes(x = (V1*3.9), y = (V2*3.5))) +
  geom_point() + 
  labs(x = "Total Hours Playing Video Games",
       y = "Servings of Vegetables Eaten") + 
  scale_x_continuous(limits = c(0,30)) + 
  scale_y_continuous(limits = c(0,25)) + 
  ggtitle(expression(paste(italic("r"), " = 1"))) +
  themesave
  )

#save to folder
ggsave(filename = "Graphs/gamesveges_pos1.png", device = "png", width = 6, height = 5, units = "in")


#r = -1
(graph.neg1 <- ggplot(data = cor.neg1, aes(x = (V1*4.5), y = (V2*3.5))) +
    geom_point() + 
    labs(x = "Total Hours Playing Video Games",
         y = "Servings of Vegetables Eaten") + 
    scale_x_continuous(limits = c(0,30)) + 
    scale_y_continuous(limits = c(0,25)) + 
    ggtitle(expression(paste(italic("r"), " = -1"))) +
    themesave
    )

#save to folder
ggsave(filename = "Graphs/gamesveges_neg1.png", device = "png", width = 6, height = 5, units = "in")


#r = 0
(graph.0 <- ggplot(data = cor.0, aes(x = (V1*4.5), y = (V2*3.5))) +
    geom_point() + 
    labs(x = "Total Hours Playing Video Games",
         y = "Servings of Vegetables Eaten") + 
    scale_x_continuous(limits = c(0,30)) + 
    scale_y_continuous(limits = c(0,25)) + 
    ggtitle(expression(paste(italic("r"), " = 0"))) +
    themesave
)

#save to folder
ggsave(filename = "Graphs/gamesveges_0.png", device = "png", width = 6, height = 5, units = "in")



#r = .10
(graph.pos.1 <- ggplot(data = cor.pos.1, aes(x = (V1*4.5), y = (V2*3.5))) +
    geom_point() + 
    labs(x = "Total Hours Playing Video Games",
         y = "Servings of Vegetables Eaten") + 
    scale_x_continuous(limits = c(0,30)) + 
    scale_y_continuous(limits = c(0,25)) + 
    ggtitle(expression(paste(italic("r"), " = .10"))) +
    themesave
)

#r = -.10
(graph.neg.1 <- ggplot(data = cor.neg.1, aes(x = (V1*4.5), y = (V2*3.5))) +
    geom_point() + 
    labs(x = "Total Hours Playing Video Games",
         y = "Servings of Vegetables Eaten") + 
    scale_x_continuous(limits = c(0,30)) + 
    scale_y_continuous(limits = c(0,25)) + 
    ggtitle(expression(paste(italic("r"), " = -.10"))) +
    themesave
)


#save to folder
ggsave(plot = grid.arrange(graph.pos.1 , graph.neg.1, ncol=2), filename = "Graphs/gamesveges_point1.png", device = "png", width = 12, height = 5, units = "in")



#r = .30
(graph.pos.3 <- ggplot(data = cor.pos.3, aes(x = (V1*4.5), y = (V2*3.5))) +
    geom_point() + 
    labs(x = "Total Hours Playing Video Games",
         y = "Servings of Vegetables Eaten") + 
    scale_x_continuous(limits = c(0,30)) + 
    scale_y_continuous(limits = c(0,25)) + 
    ggtitle(expression(paste(italic("r"), " = .30"))) +
    themesave
    )

#r = -.30
(graph.neg.3 <- ggplot(data = cor.neg.3, aes(x = (V1*4.5), y = (V2*3.5))) +
    geom_point() + 
    labs(x = "Total Hours Playing Video Games",
         y = "Servings of Vegetables Eaten") + 
    scale_x_continuous(limits = c(0,30)) + 
    scale_y_continuous(limits = c(0,25)) + 
    ggtitle(expression(paste(italic("r"), " = -.30"))) +
    themesave
)


#save to folder
ggsave(plot = grid.arrange(graph.pos.3 , graph.neg.3, ncol=2), filename = "Graphs/gamesveges_point3.png", device = "png", width = 12, height = 5, units = "in")


#r = .50
(graph.pos.5 <- ggplot(data = cor.pos.5, aes(x = (V1*4.5), y = (V2*3.5))) +
    geom_point() + 
    labs(x = "Total Hours Playing Video Games",
         y = "Servings of Vegetables Eaten") + 
    scale_x_continuous(limits = c(0,30)) + 
    scale_y_continuous(limits = c(0,25)) + 
    ggtitle(expression(paste(italic("r"), " = .50"))) +
    themesave
)

#r = -.50
(graph.neg.5 <- ggplot(data = cor.neg.5, aes(x = (V1*4.5), y = (V2*3.5))) +
    geom_point() + 
    labs(x = "Total Hours Playing Video Games",
         y = "Servings of Vegetables Eaten") + 
    scale_x_continuous(limits = c(0,30)) + 
    scale_y_continuous(limits = c(0,25)) + 
    ggtitle(expression(paste(italic("r"), " = -.50"))) +
    themesave
)


#save to folder
ggsave(plot = grid.arrange(graph.pos.5, graph.neg.5, ncol=2), filename = "Graphs/gamesveges_point5.png", device = "png", width = 12, height = 5, units = "in")


#plot the data 
(graph.pos.5 <- ggplot(data = cor.pos.5, aes(x = (V1*4.5), y = (V2*3.5))) +
    geom_point() + 
    labs(x = "Total Hours Playing Video Games",
         y = "Servings of Vegetables Eaten") + 
    scale_x_continuous(limits = c(0,30)) + 
    scale_y_continuous(limits = c(0,25)) + 
    ggtitle("Positive") +
    themesave
)

(graph.neg.5 <- ggplot(data = cor.neg.5, aes(x = (V1*4.5), y = (V2*3.5))) +
    geom_point() + 
    labs(x = "Total Hours Playing Video Games",
         y = "Servings of Vegetables Eaten") + 
    scale_x_continuous(limits = c(0,30)) + 
    scale_y_continuous(limits = c(0,25)) + 
    ggtitle("Negative") +
    themesave
)

#save to folder
ggsave(plot = grid.arrange(graph.pos.5, graph.neg.5, ncol=2), filename = "Graphs/gamesveges_positivenegative.png", device = "png", width = 12, height = 5, units = "in")





###########################################################################
######## GENERATE CORRELATED DATA - COEFFICIENT "TRY YOUR HAND" ACTIVITY 
###########################################################################


# create the correlation matrix for different correlations
sigma.pos.95 <- rbind(c(1,.95), c(.95,1)) # r = .95
sigma.neg.20 <- rbind(c(1,-.20), c(-.20,1)) # r = -.20
sigma.pos.35 <- rbind(c(1,.35), c(.35,1)) # r = .35
sigma.neg.70 <- rbind(c(1,-.70), c(-.70,1)) # r = -.70

# create the mean vector
mu.vector <-c(10, 15) 

# generate the multivariate normal distribution with a different number of data points

# first, set seed for randomization
set.seed(2345)

cor.pos.95 <- corsim(mu.vector, sigma.pos.95, n = 30)
cor.neg.20 <- corsim(mu.vector, sigma.neg.20, n = 30)
cor.pos.35 <- corsim(mu.vector, sigma.pos.35, n = 30)
cor.neg.70 <- corsim(mu.vector, sigma.neg.70, n = 30)


#####
######## PLOT THE DATA

## save the theme for the graphs below
themesave2 <- theme(plot.title = element_text(hjust = 0.5, size = 26, face = "bold", 
                                              margin = margin(b = 20)),
                    axis.line = element_line(colour = "black"),
                    panel.border = element_blank(),
                    panel.background = element_blank(),
                    axis.title = element_text(size=14, face="bold", colour = "black"),
                    axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
                    axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
                    axis.text=element_text(size=12, colour = "black"),
                    plot.margin=unit(c(.5,.5,.5,.5),"cm"))
                    


#graph for r = .95
(graph.pos.95 <- ggplot(data = cor.pos.95, aes(x = V1, y = V2)) +
    geom_point() + 
    labs(x = "Variable X",
         y = "Variable Y") + 
    themesave2
  )

#save to folder
ggsave(filename = "Graphs/cortryhand_pos95.png", device = "png", width = 6, height = 5, units = "in")

#graph for r = -.20
(graph.neg.20 <- ggplot(data = cor.neg.20, aes(x = V1, y = V2)) +
    geom_point() + 
    labs(x = "Variable X",
         y = "Variable Y") + 
    themesave2
)

#save to folder
ggsave(filename = "Graphs/cortryhand_neg20.png", device = "png", width = 6, height = 5, units = "in")


#graph for r = .35
(graph.pos.35 <- ggplot(data = cor.pos.35, aes(x = V1, y = V2)) +
    geom_point() + 
    labs(x = "Variable X",
         y = "Variable Y") + 
    themesave2
)

#save to folder
ggsave(filename = "Graphs/cortryhand_pos35.png", device = "png", width = 6, height = 5, units = "in")


#graph for r = -.70
(graph.neg.70 <- ggplot(data = cor.neg.70, aes(x = V1, y = V2)) +
    geom_point() + 
    labs(x = "Variable X",
         y = "Variable Y") + 
    themesave2
)

#save to folder
ggsave(filename = "Graphs/cortryhand_neg70.png", device = "png", width = 6, height = 5, units = "in")




###########################################################################
######## GENERATE CORRELATED DATA - CORRELATION SIGNIFICANCE GRAPHS
###########################################################################

# create the correlation matrix
sigma.pos.20 <- rbind(c(1,.20), c(.20,1)) # r = .20

# create the mean vector 
# 15 = mean number of desserts eaten per month
# 50 = monthly rated happiness
mu.vector <- c(15, 50)

# generate the multivariate normal distribution with a different number of data points

# first, set seed for randomization 
set.seed(25235)

cor.pos.20.n30 <- corsim(mu.vector, sigma.pos.20, n = 30)
cor.pos.20.n100 <- corsim(mu.vector, sigma.pos.20, n = 100)

#cor test for each:
cor.test(cor.pos.20.n30$V1, cor.pos.20.n30$V2)
cor.test(cor.pos.20.n100$V1, cor.pos.20.n100$V2)

#####
######## PLOT THE DATA

#graph for n = 30
(graph.pos.20.n30 <- ggplot(data = cor.pos.20.n30, aes(x = V1, y = V2)) +
    geom_point() + 
    labs(x = "Variable X",
         y = "Variable Y") + 
    scale_x_continuous(limits = c(12,18)) + 
    scale_y_continuous(limits = c(47,53)) + 
    theme(plot.title = element_text(hjust = 0.5, size = 26, face = "bold", 
                                    margin = margin(b = 20)),
          axis.line = element_line(colour = "black"),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.title = element_text(size=14, face="bold", colour = "black"),
          axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
          axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
          axis.text=element_text(size=12, colour = "black"),
          plot.margin=unit(c(.5,.5,.5,.5),"cm")
    ))


#save to folder
ggsave(filename = "Graphs/corsig_n30.png", device = "png", width = 6, height = 5, units = "in")

#graph for n = 100
(graph.pos.20.n100 <- ggplot(data = cor.pos.20.n100, aes(x = V1, y = V2)) +
    geom_point() + 
    labs(x = "Variable X",
         y = "Variable Y") + 
    scale_x_continuous(limits = c(12,18)) + 
    scale_y_continuous(limits = c(47,53)) + 
    theme(plot.title = element_text(hjust = 0.5, size = 26, face = "bold", 
                                    margin = margin(b = 20)),
          axis.line = element_line(colour = "black"),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.title = element_text(size=14, face="bold", colour = "black"),
          axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
          axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
          axis.text=element_text(size=12, colour = "black"),
          plot.margin=unit(c(.5,.5,.5,.5),"cm")
    ))


#save to folder
ggsave(filename = "Graphs/corsig_n100.png", device = "png", width = 6, height = 5, units = "in")




###########################################################################
######## GENERATE CORRELATED DATA - OUTLIER EDITION 
###########################################################################

## Create correlations, and then look at the difference between r values when an outlier is added into the mix

# create the correlation matrices
sigma.pos.40 <- rbind(c(1,.40), c(.40,1)) # r = .40
sigma.neg.90 <- rbind(c(1,-.90), c(-.90,1)) # r = -.90

# create the mean vector 
mu.vector <- c(5, 50)

# generate the multivariate normal distribution with a different number of data points

#first, set seed for randomization
set.seed(23434)

cor.pos.40 <- corsim(mu.vector, sigma.pos.40, n = 25)
cor.neg.90 <- corsim(mu.vector, sigma.neg.90, n = 25)

#####
######## CORRELATION TESTS & OUTLIER ADDITION

#multiply x variable by 10
cor.pos.40$V1 <- cor.pos.40$V1 * 10
cor.neg.90$V1 <- cor.neg.90$V1 * 10

#cor test for each to double check:
cor.test(cor.pos.40$V1, cor.pos.40$V2)
cor.test(cor.neg.90$V1, cor.neg.90$V2)

# add an outlier to each 
cor.pos.40.out <- rbind(cor.pos.40, c(78,45))
cor.neg.90.out <- rbind(cor.neg.90, c(35, 49))

#cor test for each to double check:
cor.test(cor.pos.40.out$V1, cor.pos.40.out$V2)
cor.test(cor.neg.90.out$V1, cor.neg.90.out$V2)

#####
######## PLOT THE DATA

## outline theme for each plot 
themesave3 <- theme(axis.line = element_line(colour = "black"),
                        panel.border = element_blank(),
                        panel.background = element_blank(),
                        axis.title = element_text(size=14, face="bold", colour = "black"),
                        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
                        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
                        axis.text=element_text(size=12, colour = "black"),
                        plot.margin=unit(c(.5,.5,.5,.5),"cm"))



#graph without outlier, r = .40
(nooutlier.40 <- ggplot(data = cor.pos.40, aes(x = V1, y = V2)) +
    geom_point() + 
    labs(x = "Variable X",
         y = "Variable Y") + 
    scale_x_continuous() + 
    scale_y_continuous() + 
    themesave3
  )

ggsave(filename = "Graphs/regression_outliersim40.png", device = "png", width = 6, height = 5, units = "in")

#graph with outlier, r = .40
(outlier.40 <- ggplot(data = cor.pos.40.out, aes(x = V1, y = V2)) +
    geom_point() + 
    labs(x = "Variable X",
         y = "Variable Y") + 
    scale_x_continuous() + 
    scale_y_continuous() + 
    themesave3
)

ggsave(plot = grid.arrange(nooutlier.40, outlier.40, ncol=2), filename = "Graphs/coroutlier_40.png", device = "png", width = 12, height = 5, units = "in")


#graph without outlier, r = .90
(nooutlier.90 <- ggplot(data = cor.neg.90, aes(x = V1, y = V2)) +
    geom_point() + 
    labs(x = "Variable X",
         y = "Variable Y") + 
    scale_x_continuous() + 
    scale_y_continuous() + 
    themesave3
)

ggsave(filename = "Graphs/regression_outliersim40.png", device = "png", width = 6, height = 5, units = "in")

#graph with outlier, r = .90 
(outlier.90 <- ggplot(data = cor.neg.90.out, aes(x = V1, y = V2)) +
    geom_point() + 
    labs(x = "Variable X",
         y = "Variable Y") + 
    scale_x_continuous() + 
    scale_y_continuous() + 
    themesave3
)

ggsave(plot = grid.arrange(nooutlier.90, outlier.90, ncol=2), filename = "Graphs/coroutlier_90.png", device = "png", width = 12, height = 5, units = "in")


###########################################################################
######## GENERATE CORRELATED DATA - NON-LINEAR EXAMPLE WITH FLOWERS
###########################################################################

# generate data that is related, just not in a linear fashion 
# this is used to show that a classic correlation coefficient isn't always the best statistic to model the relationship between variables

#set seed for randomization 
set.seed(414)

# create a sequence of x values 
x <- seq(from = 0, to = 5, by=.1) #rain inches
#generate true function of y-values
y_true <- dnorm(seq(from = 0, to = 5, by=.1), mean=2, sd=.75) 
#add some noise to the true function to get the y-values
y_rand <- y_true + rnorm(51, sd = 0.08)

#plot the data to check it out 
plot(x,y_true,type="l",lwd = 1)
plot(x,y_rand)

#create a data frame with non-linear values 
flowersdat <- data.frame(x = x,
                         y_true = y_true,
                         y_rand = y_rand)

#get the classic correlation value for the non-linear data (for example's sake)
cor.test(flowersdat$x, flowersdat$y_rand)


#####
######## PLOT THE DATA


#get theme for the flower graph 
themesave4 <- theme(axis.line = element_line(colour = "black"),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.title = element_text(size=14, face="bold", colour = "black"),
      axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
      axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
      axis.text=element_text(size=12, colour = "black"),
      plot.margin=unit(c(.5,.5,.5,.5),"cm"))


#graph the data by itself
(flowerbasicgraph <- ggplot(data = flowersdat, aes(x = x*3, y = y_rand*100)) +
    geom_point() + 
    labs(x = "Rainfall Flower Received in May (in cm) ",
         y = "Flower Growth (in cm)") + 
    scale_x_continuous() + 
    scale_y_continuous() + 
    themesave4
)

ggsave(filename = "Graphs/flowerbasic.png", device = "png", width = 6, height = 5, units = "in")

#graph the data with a linear line of best fit
(flowerbasiclmline <- ggplot(data = flowersdat, aes(x = x*3, y = y_rand*100)) +
    geom_point() + 
    geom_smooth(method = "lm", se = FALSE, color = "black", size = 1.5) +
    labs(x = "Rainfall Flower Received in May (in cm) ",
         y = "Flower Growth (in cm)") + 
    scale_x_continuous() + 
    scale_y_continuous() + 
    themesave4
)

ggsave(filename = "Graphs/flowerlmline.png", device = "png", width = 6, height = 5, units = "in")

#graph the data with a non-linear line of best fit 
(flowerbasicnormline <- ggplot(data = flowersdat) +
    geom_point(aes(x = x*3, y = y_rand*100)) + 
    geom_line(aes(x = x*3, y = y_true*100), size = 1.5) +
    labs(x = "Rainfall Flower Received in May (in cm) ",
         y = "Flower Growth (in cm)") + 
    scale_x_continuous() + 
    scale_y_continuous() + 
    themesave4
)

#save the plot 
ggsave(filename = "Graphs/flowernormline.png", device = "png", width = 6, height = 5, units = "in")


###########################################################################
######## GENERATE CORRELATED DATA - NON-LINEAR PIZZA EXAMPLE 
###########################################################################

# generate data that is related, just not in a linear fashion 
# this is used to show that a classic correlation coefficient isn't always the best statistic to model the relationship between variables

#set seed for randomization 
set.seed(414)

#get a sequence of x-values 
x <- seq(from = 0, to = 200, by= 5) #square inches of pizza eaten
#generate true function for y-values
y_true <- (x^4 / 10000000) + 50 
#add some noise to get our y-values 
y_rand <- y_true + rnorm(41, sd = 12)

#plot the data, just to check
plot(x,y_true,type="l",lwd = 1)
plot(x,y_rand)

#put the data into a dataframe 
pizzadat <- data.frame(x = x,
                         y_true = y_true,
                         y_rand = y_rand)

#####
######## PLOT THE DATA


#plot the data 
(pizzabasic <- ggplot(data = pizzadat, aes(x = x, y = (y_rand/20))) +
    geom_point() +
    labs(x = "Amount of Pizza Eaten (in square inches) ",
         y = "Self-Reported Stomach Discomfort") + 
    scale_x_continuous() + 
    scale_y_continuous(limits = c(0,10)) + 
    theme(axis.line = element_line(colour = "black"),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.title = element_text(size=14, face="bold", colour = "black"),
          axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
          axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
          axis.text=element_text(size=12, colour = "black"),
          plot.margin=unit(c(.5,.5,.5,.5),"cm")))

#save the plot 
ggsave(filename = "Graphs/pizzabasic.png", device = "png", width = 6, height = 5, units = "in")