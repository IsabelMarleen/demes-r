
#' Convert deme into a named list
#'
#' @param deme
#'
#' @return \code{named deme}
#'
#' @examples
#' a <- load_yaml(text="time_units: generations
#' demes:
#'   - name: A
#' epochs:
#'   - start_size: 1000")
#' a <- name_deme(a)
name_deme <- function(deme){
  deme_names <- c()
  for (i in 1:length(deme$demes)){
    deme_names[i] <- deme$demes[[1]]$name
  }
  names(deme$demes) <- deme_names
  return(deme)
}
