
#' A function to merge and clean unimportant variables and observations
#'
#' This function clears all unimportant oberservations and variables and merges the two important sets data_script and data_gender
#' @export
#' @examples
#' merge_and_clean()

merge_and_clean <- function(data_gender, data_script){
# only observations with gender: data_gender2
    data_gender2 <- subset(data_gender, gender == "m" | gender == "f") 
    names(data_gender2)[1] <- "character_id"
# getting rid of all the trash in the big data set: data_script2
    data_script2 <- data_script[, c("id", "number", "episode_id", "timestamp_in_ms", "character_id", "raw_character_text", "raw_location_text", "word_count")]
    head(data_script2)
# merging by character_id
    data_scgen <- merge(data_script2, data_gender2, by="character_id")
    data_scgen <- data_scgen[order(data_scgen$episode_id),]
# cleaning the first two rows and observations without row_count 
    data_scgen <- data_scgen[3:72585, ]
    data_scgen <- subset(data_scgen, word_count != "") 
    head(data_scgen)
}
merge_and_clean(data_gender, data_script)
