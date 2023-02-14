# Take a validated nested list Demes structure, name each element of the list
# after each deme and return the modified structure back in the named form
name_demes <- function(demes){
  deme_names <- c()
  for (i in 1:length(demes$demes)){
    deme_names[i] <- demes$demes[[1]]$name
  }
  names(demes$demes) <- deme_names
  return(demes)
}
