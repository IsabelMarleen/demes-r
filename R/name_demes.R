
#' Convert demes into a named list
#'
#' @param demes
#'
#' @return \code{named deme}
#' @noRd
#' @examples
#' a <- yaml::read_yaml(text="time_units: generations
#' demes:
#'   - name: A
#' epochs:
#'   - start_size: 1000")
#' a <- demes::name_demes(a)
name_demes <- function(demes){
  deme_names <- c()
  for (i in 1:length(demes$demes)){
    deme_names[i] <- demes$demes[[1]]$name
  }
  names(demes$demes) <- deme_names
  return(demes)
}
