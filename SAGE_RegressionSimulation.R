#########################
## Simulating Regression Data - Sage Topic 8
## Script by: Abbey Hammell 
##
## ## Description: Script generates correlated data and data around specific lines of best fit, 
## which are used as concept examples in SAGE - Student Success: Data Literacy Module: 
## A Few Basic Statistical Concepts by Alicia Hofelich Mohr & Abbey Hammell 
## (in the process of being published as of Feb 2022) 
#########################

#####
######## PREP WORKSPACE
#####

## clear workspace
rm(list=ls())

## attach packages 
pacman::p_load(MASS,
               ggplot2,
               scales,
               gridExtra)

#set working directory
setwd("") #set this to wherever you wish


#create correlation (multivariate normal distribution) simulation function 
#mu.vector is a vector of distribution means you want your data to fall around
#sigma.matrix is the correlation matrix you want to simulate 
#n is the number of data points (observations) you want to simulate

corsim <- function(mu.vector, sigma.matrix, n){
  x <- as.data.frame(mvrnorm(n=n, mu=mu.vector, 
                             Sigma=sigma.matrix, empirical = TRUE))
  return(x)
}

#create a function to create points around a line of best fit 
simulate <- function(x, beta, r.2) {
  sigma <- sqrt(var(x) * beta[2]^2 * (1/r.2 - 1))
  e <- residuals(lm(rnorm(length(x)) ~ x))
  return (y.0 <- beta[1] + beta[2]*x + sigma * scale(e))
}
#https://stats.stackexchange.com/questions/152028/i-have-a-line-of-best-fit-i-need-data-points-that-will-not-change-my-line-of-be


###########################################################################
######## GENERATE REGRESSION DATA - ONLINE ADVERTISEMENTS
###########################################################################

# create the correlation matrix 
sigma.catart <- rbind(c(1,.60), c(.60,1)) # r = .60, R^2 = .36

# create the mean vector
mu.vector <-c(3, 7.5) 

# generate the multivariate normal distribution with 60 data points (5 * 12 months)

# first, set seed for randomization
set.seed(2345)

cor.catart <- corsim(mu.vector, sigma.catart, n=60)

#multiply x by 10 to stretch out the distribution
cor.catart$V1 <- cor.catart$V1 * 1000
#multiply y by 100 to stretch out the distribution
cor.catart$V2 <- cor.catart$V2 * 2500


#####
######## PLOT THE DATA

#plot the scatterplot of data that was just generated above
(catartorig <- ggplot(data = cor.catart, aes(x = V1, y = V2)) +
  geom_point() + 
  labs(x = "Money Spent on Online Advertising \n (per month)",
       y = "Total Cat Art Sales \n (per month)") + 
  scale_x_continuous(labels=scales::dollar_format()) + 
  scale_y_continuous(labels=scales::dollar_format()) + 
  theme(axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title = element_text(size=14, face="bold", colour = "black"),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.text=element_text(size=12, colour = "black"),
        plot.margin=unit(c(.5,.5,.5,.5),"cm")
        ))

#save to folder
ggsave(filename = "Graphs/catart_original.png", device = "png", width = 6, height = 5, units = "in")


#####
######## GET REGRESSION RESULTS

# use the data generated above in a linear model to obtain regression analysis specifics
mod.catart <- lm(cor.catart$V2 ~ cor.catart$V1)
summary.lm(mod.catart)
coef(lm(cor.catart$V2 ~ cor.catart$V1))

## regression equation is (approximately) y = 1.5x + 14250


#####
######## PLOT THE LINE OF BEST FIT GRAPH

(catartline <- catartorig + 
    geom_abline(intercept = 14250, slope = 1.5, color = "black", size = 1.5))

#save the graph
ggsave(filename = "Graphs/catart_linebestfit.png", device = "png", width = 6, height = 5, units = "in")


#####
######## PLOT BAD > BETTER > BEST LINES 

# cat art, with a line that does not fit the data well at all 
(catartbad <- catartorig + 
    geom_abline(intercept = 21500, slope = -1.5, color = "darkred", size = 1.5) +
    ggtitle("Bad")+
    theme(plot.title = element_text(hjust = 0.5, size = 25, face = "bold", 
                                    margin = margin(b = 20))))

#save the graph
ggsave(filename = "Graphs/catart_badfit.png", device = "png", width = 6, height = 5, units = "in")

# cat art, with line of an okay fit 
(catartokay <- catartorig + 
    geom_abline(intercept = 17500, slope = 0.25, color = "goldenrod3", size = 1.5) +
    ggtitle("Okay")+
    theme(plot.title = element_text(hjust = 0.5, size = 25, face = "bold", 
                                    margin = margin(b = 20))))

#save the graph
ggsave(filename = "Graphs/catart_okayfit.png", device = "png", width = 6, height = 5, units = "in")

# cat art, with the line of best fit
(catartbest <- catartorig + 
    geom_abline(intercept = 14250, slope = 1.5, color = "forestgreen", size = 1.5) +
    ggtitle("Best")+
    theme(plot.title = element_text(hjust = 0.5, size = 25, face = "bold", 
                                    margin = margin(b = 20))))

#save the graph
ggsave(filename = "Graphs/catart_bestfit.png", device = "png", width = 6, height = 5, units = "in")



###########################################################################
###### GENERATE DIFFERENT R-SQUAREDS AROUND 1.5x + 14250 
###########################################################################


#first, set seed for randomization 
set.seed (1532)

#set your beta values
beta <- c(14250, 1.5)

#generate random x values 
xrand <- rnorm(n = 60, mean = 3500, sd = 1000)

#simulate y-values around your generated x values that follow your beta & R^2 specifications
#we're using the same beta values...comparing what different R^2 might look like if they have the same line of best fit 
yrand_1 <- simulate(xrand, beta, r.2 = .10)
yrand_2 <- simulate(xrand, beta, r.2 = .45)
yrand_3 <- simulate(xrand, beta, r.2 = .70)

#add all simulated y-values to the same data frame for easy plotting
simdat <- data.frame(xrand = xrand,
                     yrand1 = yrand_1,
                     yrand2 = yrand_2, 
                     yrand3 = yrand_3)

## double check results
##summary(lm(yrand1 ~ xrand, data = simdat))
##summary(lm(yrand2 ~ xrand, data = simdat))
##summary(lm(yrand3 ~ xrand, data = simdat))


#####
######## PLOT THE DATA


## first, set the theme 
themesave <- theme(axis.line = element_line(colour = "black"),
                      panel.border = element_blank(),
                      panel.background = element_blank(),
                      axis.title = element_text(size=14, face="bold", colour = "black"),
                      axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
                      axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
                      axis.text=element_text(size=12, colour = "black"),
                      plot.margin=unit(c(.5,.5,.5,.5),"cm")
)

# plot R^2 = .10
(simstrength1 <- ggplot(data = simdat, aes(x = xrand, y = yrand1)) +
    geom_point() + 
    geom_abline(intercept = 14250, slope = 1.5, color = "black", size = 1.5) +
    labs(x = "Money Spent on Online Advertising \n (per month)",
         y = "Total Cat Art Sales \n (per month)") + 
    scale_x_continuous(labels=scales::dollar_format(), limits = c(0, 6000)) + 
    scale_y_continuous(labels=scales::dollar_format(), limits = c(9000, 30000)) + 
    themesave
)

# save the graph
ggsave(filename = "Graphs/catart_simstrength1.png", device = "png", width = 6, height = 5, units = "in")


# plot R^2 = .45
(simstrength2 <- ggplot(data = simdat, aes(x = xrand, y = yrand2)) +
    geom_point() + 
    geom_abline(intercept = 14250, slope = 1.5, color = "black", size = 1.5) +
    labs(x = "Money Spent on Online Advertising \n (per month)",
         y = "Total Cat Art Sales \n (per month)") + 
    scale_x_continuous(labels=scales::dollar_format(), limits = c(0, 6000)) + 
    scale_y_continuous(labels=scales::dollar_format(), limits = c(9000, 30000)) + 
    themesave
)

# save the graph
ggsave(filename = "Graphs/catart_simstrength2.png", device = "png", width = 6, height = 5, units = "in")

# plot R^2 = .70
(simstrength3 <- ggplot(data = simdat, aes(x = xrand, y = yrand3)) +
    geom_point() + 
    geom_abline(intercept = 14250, slope = 1.5, color = "black", size = 1.5) +
    labs(x = "Money Spent on Online Advertising \n (per month)",
         y = "Total Cat Art Sales \n (per month)") + 
    scale_x_continuous(labels=scales::dollar_format(), limits = c(0, 6000)) + 
    scale_y_continuous(labels=scales::dollar_format(), limits = c(9000, 30000)) + 
    themesave
)

# save the graph
ggsave(filename = "Graphs/catart_simstrength3.png", device = "png", width = 6, height = 5, units = "in")


###########################################################################
###### CREATE A REGRESSION LINE W/ SCATTER PLOT THAT INCLUDES AN OUTLIER 
###########################################################################

## Create correlation matrix, and then look at the difference in regression analysis results when an outlier is added into the mix


# create the correlation matrix 
sigma.pos.40 <- rbind(c(1,.40), c(.40,1)) # r = .40

# create the mean vector 
mu.vector <- c(5, 50)

# generate the multivariate normal distribution with a different number of data points
set.seed(23434)
cor.pos.40 <- corsim(mu.vector, sigma.pos.40, n = 25)


#multiply x variable by 10 to increase the spread
cor.pos.40$V1 <- cor.pos.40$V1 * 10

#linear model & regression results (without outlier)
mod.pos.40 <- lm(V2 ~ V1, data = cor.pos.40)
coef(mod.pos.40)
summary(mod.pos.40)


# add an outlier 
cor.pos.40.out <- rbind(cor.pos.40, c(78,46))

#linear model & regression results (with outlier)
mod.pos.40.out <- lm(V2 ~ V1, data = cor.pos.40.out)
coef(mod.pos.40.out)
summary(mod.pos.40.out)



#####
######## PLOT THE DATA 

#scatterplot & line of best fit without the outlier
(nooutlier.40 <- ggplot(data = mod.pos.40, aes(x = V1, y = V2)) +
    geom_point() + 
    geom_abline(intercept = 48, slope = 0.04, color = "black", size = 1.5) +
    labs(x = "Variable X",
         y = "Variable Y") + 
    scale_x_continuous(limits = c(35, 80)) + 
    scale_y_continuous(limits = c(46, 52)) + 
    theme(axis.line = element_line(colour = "black"),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.title = element_text(size=14, face="bold", colour = "black"),
          axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
          axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
          axis.text=element_text(size=12, colour = "black"),
          plot.margin=unit(c(.5,.5,.5,.5),"cm")))

#save the graph
ggsave(filename = "Graphs/regression_outliersim40.png", device = "png", width = 6, height = 5, units = "in")

#scatterplot & line of best fit WITH the outlier
(outlier.40 <- ggplot(data = mod.pos.40.out, aes(x = V1, y = V2)) +
    geom_point() + 
    geom_abline(intercept = 50.03, slope = -0.0037, color = "black", size = 1.5) +
    labs(x = "Variable X",
         y = "Variable Y") + 
    scale_x_continuous(limits = c(35, 80)) + 
    scale_y_continuous(limits = c(46, 52)) + 
    theme(axis.line = element_line(colour = "black"),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.title = element_text(size=14, face="bold", colour = "black"),
          axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
          axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
          axis.text=element_text(size=12, colour = "black"),
          plot.margin=unit(c(.5,.5,.5,.5),"cm")))

#save the graph
ggsave(filename = "Graphs/regression_outliersim40out.png", device = "png", width = 6, height = 5, units = "in")

