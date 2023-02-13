# Iterate over the entire Demes nested list structure and convert infinity
# strings which can appear in a Demes YAML file into proper Inf values
# (this is more idiomatic because R has a support for infinite numbers built in).
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
