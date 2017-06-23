"
Filename:  Project Code
Last Updated: 01.12.2016
Author: Luis Bosshart
"
# Project R: In this project, I try to write three functions, converting a .dta into a .rda file, 
# analyzing the data in some way and subsequently performing a simple regression with election data.
# Data obtained from the data set: Time Series American National Election Studies


setwd( "/Users/luisbosshart/Dropbox/Master/Programming with R/Project/anes_timeseries_2012_dta" )

#First function: from dta to rda
library(foreign)
x <- read.dta("anes_timeseries_2012_Stata12.dta")
bn <- gsub( 'dta' , 'rda' , basename( "anes_timeseries_2012_Stata12.dta" ) )
names( x ) <- tolower( names( x ) )
save( x , file = "anes_timeseries_2012_Stata12.rda" )

#Second Function: Data analysis
install.packages( "survey" )

library(survey)
options( survey.lonely.psu = "adjust" )

load( "anes_timeseries_2012_Stata12.rda" )

nrow( x )

head( x )

# I failed succeeding with the subsequent part in time, hence I use an inbuilt data set for
# the regression and plotting... :(

library(help = "datasets")
install.packages("ggplot2")
library("ggplot2")
data(ChickWeight)
head(ChickWeight)
install.packages("stargazer")
library(stargazer)

# Analyse Data, descriptive statistics
stargazer(ChickWeight, type = "text", title="Descriptive statistics", digits=1, out="table1.txt")

# Random Plot
p1 <- ggplot(ChickWeight, aes(weight))   + geom_bar()
plot(p1)

#Regression
# dependent variable = weight
# Diet = independentvariable
lm(weight ~ Diet, data=ChickWeight) 

#Packaging
getwd()
ls()
package.skeleton(name = "LBprojectpackage",
                 path = "/Users/luisbosshart/Dropbox/Master/Programming with R/Project/anes_timeseries_2012_dta", force = FALSE)