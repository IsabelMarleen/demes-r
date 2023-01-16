#' Convert Infinity Strings to Proper Inf Values
#'
#' @param inp
#'
#' @return inp
#' @noRd
#'
#' @examples
convert_infinity <- function(inp){

  for (i in 1:length(inp$demes)){
    if (inp$demes[[i]]$start_time == "Infinity") {
      inp$demes[[i]]$start_time <- Inf
    }
  }

  if (length(inp$migrations) > 0) {
    for (i in 1:length(inp$migrations)){
        if(inp$migrations[[i]]$start_time == "Infinity"){
          inp$migrations[[i]]$start_time <- Inf
      }
    }
  }

  return(inp)
}
