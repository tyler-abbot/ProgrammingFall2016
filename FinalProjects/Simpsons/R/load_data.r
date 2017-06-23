
#' A Function to load the datasets. 
#'
#' This function loads the three datasets you download from kaggle: https://www.kaggle.com/wcukierski/the-simpsons-by-the-data
#' @export
#' @examples
#' load_data()

load_data <- function(data_script, data_gender, data_imdb){
    data_script <- read.csv(paste(mydir, "simpsons_script_lines.csv", sep=""))
    data_gender <- read.csv(paste(mydir, "simpsons_characters.csv", sep=""))
    data_imdb <- read.csv(paste(mydir, "simpsons_episodes.csv", sep=""))
}
