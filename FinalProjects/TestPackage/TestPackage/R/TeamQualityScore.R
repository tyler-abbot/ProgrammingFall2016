#' Team Quality Score function 
#'
#' This function assigns a score evaluating the general quality 
#' of a team based on its previous season relative standings.
#'   
#' [INSERT MOTIVATION OF QUALITY SCORE FORMULA]
#'  
#' Formula: 
#' Team_i score = (1 - (Leader points - Team_i points) / leader points ) * last team points
#'
#' @export
#' @examples
#' score_team_qual()

score_team_qual <- function(team_points, leader_points, last_team_points){
  qual_score <- (1-((leader_points - team_points)/ leader_points))*last_team_points
  return(qual_score)
}
