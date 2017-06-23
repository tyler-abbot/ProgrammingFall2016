#' Predict Ligue 1 Football championship function 
#'
#' This is the master function of our package that will call all other functions and data
#' needed to output a prediction standings of the French football championship using previous season standings
#' and the first ten matches of the season.
#'
#'
#' @export
#' @examples
#' predict_L1()

predict_L1 <- function(){

  # Clear working space
  rm(list=ls())
  
  # Create dataframe (data was already formatted by hand)
  data("Ligue1-data")
  raw_data <- data.frame(`Ligue1-data`)
  data <- raw_data[1:20,c("ID","Team.Name","Standings.2015.16","Points.2015.16","Standings.2016.17","Points.2016.17","M1","M2","M3","M4","M5","M6","M7","M8","M9","M10")]
  
  ## 1. Define variables
  n = 20              #number of teams
  alpha_1 <- 0.5      #arbitrary coefficients for form/quality scores
  alpha_2 <- 0.5
  diff_coeff <- c(rep(0,20))
  
  Points_toul_16 <- data[4,4] #Last non-relegated team in previous year
  Points_psg_16 <- data[3,4]  #Leader number of points
  
  ## 2. Assign a quality score to each team
  data$Qual.Score <- score_team_qual(data$Points.2015.16, Points_psg_16, Points_toul_16)
  
  ## 3. Compute match difficulty coefficient
  
  # Convert data into correct format for function use (don't really know why but otherwise it didn't work)
  match_matrix <- as.matrix(data[1:20,7:16])
  current_rank <- as.matrix(data$ID)
  
  # Compute difficulty coefficient
  diff_coeff <- match_diff_coeff(current_rank, match_matrix)
  
  # 4. Assign Current form score
  data$Form[1:20] <- (diff_coeff*data$Points.2016.17 + data$Points.2016.17)
  
  ## 5. Assign point prediction
  data$PointsPredicted <- round(alpha_1*data$Qual.Score + alpha_2*data$Form, digits = 0)
  
  ## 6. Plot league standing
  output_data <- data[ ,c("ID","Team.Name","Points.2015.16","Points.2016.17","Qual.Score","Form","PointsPredicted")]
  output_data <- output_data[order(output_data$PointsPredicted, decreasing = TRUE), ]
  write.csv2(output_data, "~/Desktop/Ligue1-predictions.csv")
  output_data
}