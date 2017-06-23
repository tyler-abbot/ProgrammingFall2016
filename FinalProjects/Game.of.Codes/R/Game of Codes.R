#Game of Codes
#A Song of R and Python
#Made by Daniel Barreto and Johannes Seebauer

library(devtools)
library(roxygen2)

#First, we create a new variable to measure for how many chapters
#each character has survived. To do so we define the following
#variables as the accumulated number of chapters at the end of each book.
nchaptersb1 <- 73
nchaptersb2 <- sum(nchaptersb1 + 70)
nchaptersb3 <- sum(nchaptersb2 + 82)
nchaptersb4 <- sum(nchaptersb3 + 46)
nchaptersb5 <- sum(nchaptersb4 + 73)
#One problem we have in our data sheet is that for some observations
#it is specified the book in which the character dies, but not the chapter.
#We fix this by assuming that in all of those cases the character died at the prologue.
data_got4=read.csv("character-deaths.csv")
data_got4$Death.Chapter <- with(data_got4, ifelse(!is.na(Book.of.Death) & is.na(Death.Chapter),0, Death.Chapter))
#Another problem we faced is that in the column "Allegiances" in our data there was both
#entries for "House Tully" and simply "Tully", for example. We solve this by:
data_got4$Allegiances <- lapply(data_got4$Allegiances,gsub,pattern="House ",replacement="")
#Then we create a column in our data sheet named "Cumulative Death Chapter"
#and fill it up.
data_got4["Cumulative.Death.Chapter"] <- NA
data_got4$Cumulative.Death.Chapter <- with(data_got4, ifelse(Book.of.Death == 1, Death.Chapter,
                                                             ifelse(Book.of.Death == 2, Death.Chapter + nchaptersb1,
                                                                    ifelse(Book.of.Death == 3, Death.Chapter + nchaptersb2,
                                                                           ifelse(Book.of.Death == 4, Death.Chapter + nchaptersb3,
                                                                                  ifelse(Book.of.Death == 5, Death.Chapter + nchaptersb4, NA))))))
#In order to be able to compute the life expectancy of the characters,
#we'll now replace the NA's for nchapterb5 + 50. That is, we assume that
#50 chapters into book 6 Cersei is going to kill everyone using the wildfire.
data_got4$Cumulative.Death.Chapter<- with(data_got4, ifelse(is.na(Cumulative.Death.Chapter),nchaptersb5+50, Cumulative.Death.Chapter))

# Creating a function for life expectancy conditional on allegiance.
#' Life expectancy by allegiance
#' @param x String: The name of the house
#' @seealso \code{\link{mean}} which this function wraps
#' @export
lexpectancy.allegiance <- function(x){
  mean(data_got4[data_got4$Allegiances==x, "Cumulative.Death.Chapter"])
}
lexpectancy.allegiance("Lannister")
lexpectancy.allegiance("Stark")
lexpectancy.allegiance("Night's Watch")
lexpectancy.allegiance("Tully")
lexpectancy.allegiance("Wildling")
lexpectancy.allegiance("None")

#Creating a function for life expectancy conditional on gender.
#' Life expectancy by gender
#' @param x Dummy: 0 for female and 1 for male
#' @seealso \code{\link{mean}} which this function wraps
#' @export
lexpectancy.gender <- function(x){
  mean(data_got4[data_got4$Gender==x, "Cumulative.Death.Chapter"])
}
lexpectancy.gender(0)
lexpectancy.gender(1)

#Creating a function for life expectancy conditional on nobility (0 for not noble and 1 for noble).
#' Life expectancy by nobility
#' @param x Dummy: 0 for not noble and 1 for noble
#' @seealso \code{\link{mean}} which this function wraps
#' @export
lexpectancy.nobility <- function(x){
  mean(data_got4[data_got4$Nobility==x, "Cumulative.Death.Chapter"])
}
lexpectancy.nobility(0)
lexpectancy.nobility(1)

#setwd("./Game.of.Codes")
#document()
#?lexpectancy.gender
