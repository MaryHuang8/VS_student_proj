# reduces the number of observations for a certain period of time. Biases
# can be imposed depending on where data is removed from
limitObservations <- function(observations, newObservationLimit, leftEnd = 0, rightEnd = NULL) {
  if (is.null(rightEnd)) rightEnd <- dim(observations)[2]
  
  observations %>% 
    mutate( 
      across(
        any_of(leftEnd:rightEnd), 
        ~if_else(row_number() > newObservationLimit, NA, .)
      )
    )
}

# removes observations based on the `removalCriteria`, a function that can be used
# to impose some bias
makePatches <- function(observations, removalCriteria, leftEnd = 0, rightEnd = NULL) {
  if (is.null(rightEnd)) rightEnd <- dim(observations)[2]
  
  observations %>% 
    mutate( 
      across(
        any_of(leftEnd:rightEnd), 
        ~if_else(removalCriteria(.), NA, .)
      )
    )
}

# mutates `observations` as per operator
operate <- function(observations, operator, leftEnd = 0, rightEnd = NULL) {
  if (is.null(rightEnd)) rightEnd <- dim(observations)[2]
  
  observations %>% 
    mutate( 
      across(
        any_of(leftEnd:rightEnd), 
        ~operator(.)
      )
    )
}