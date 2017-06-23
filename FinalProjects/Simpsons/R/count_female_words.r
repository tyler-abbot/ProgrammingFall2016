
#' A Function to count the female words per episode and save it in a dataframe
#'
#' This function tracks the absolute number of words spoken by women. It first adds up all words and finally stores the final amount of female and male words of every episode in a new dataset. However, as the function stands now the ordering did not work very well so that it creates an incomplete summary of word counts. 
#' @export
#' @examples
#' count_female_words()

count_female_words <- function(i, j, data_scgen, data_wpe){
# creating a data set to save the female "words per episode" (wpe)  in
    data_scgen[,"words_per_episode"] <- 0
    head(data_scgen)
    data_wpe <- data.frame("episode"=0, "words_male"=0, "words_female"=0)
    data_wpe
# creating setting vectors for row and episode
    i <- 1 # episode 
    j <- 1 # row
    addmen <- 0 
    addwoman <- 0  
# starting to count female words per episode
    while(i <= 400){
        if(data_scgen[j, 'gender'] == "m"){
            addmen <- as.numeric(as.character(data_scgen[j,'word_count'])) + addmen
            data_scgen[j,'words_per_episode'] <- addmen
            j <- j + 1
        }
        if(data_scgen[j,'gender'] == "f"){ # If line j is a women, then add wordcount of line to sum of the words in the whole episode and add this number to column words_by_episode 
            addwoman <- as.numeric(as.character(data_scgen[j,'word_count'])) + addwoman
            data_scgen[j,'words_per_episode'] <- addwoman
            j <- j + 1
        }
        if (i != as.numeric(as.character(data_scgen[j,'episode_id']))){ # If new episode
        infos <- c(i, addmen, addwoman) # saving the calculated word counts in the infos vector
        data_wpe <- rbind(data_wpe, infos) # adding the infos into the dataset
        addwoman <- 0 # setting the counting variable back to 0
        addmen <- 0 #setting the counting variable back to 0
        i <- as.numeric(as.character(data_scgen[j,'episode_id'])) #start over with next episode
      }
    }
head(data_scgen, n=10L)
head(data_wpe)
}

