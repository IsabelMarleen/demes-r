order_demes <- function(deme){
  ordered_deme <- list()

  ordered_deme$time_units <- deme$time_units
  ordered_deme$generation_time <- deme$generation_time
  ordered_deme$doi <- deme$doi
  ordered_deme$description <- deme$description
  ordered_deme$metadata <- deme$metadata

  ordered_deme$demes <- list()
  for (i in 1:length(deme$demes)){
    ordered_deme$demes[i][[1]] <- list()

    ordered_deme$demes[[i]]$name <- deme$demes[[i]]$name
    ordered_deme$demes[[i]]$description <- deme$demes[[i]]$description
    ordered_deme$demes[[i]]$start_time <- deme$demes[[i]]$start_time

    ordered_deme$demes[[i]]$epochs <- list()
    for (j in 1:length(deme$demes[[i]]$epochs)){
      ordered_deme$demes[[i]]$epochs[j][[1]] <- list()

      ordered_deme$demes[[i]]$epochs[[j]]$end_time <- deme$demes[[i]]$epochs[[j]]$end_time
      ordered_deme$demes[[i]]$epochs[[j]]$start_size <- deme$demes[[i]]$epochs[[j]]$start_size
      ordered_deme$demes[[i]]$epochs[[j]]$end_size <- deme$demes[[i]]$epochs[[j]]$end_size
      ordered_deme$demes[[i]]$epochs[[j]]$size_function <- deme$demes[[i]]$epochs[[j]]$size_function
      ordered_deme$demes[[i]]$epochs[[j]]$selfing_rate <- deme$demes[[i]]$epochs[[j]]$selfing_rate
      ordered_deme$demes[[i]]$epochs[[j]]$cloning_rate <- deme$demes[[i]]$epochs[[j]]$cloning_rate
    }
    ordered_deme$demes[[i]]$proportions <- deme$demes[[i]]$proportions
    ordered_deme$demes[[i]]$ancestors <- deme$demes[[i]]$ancestors
  }

  ordered_deme$migrations <- list()
  if (length(deme$migrations) > 0){
    for (p in 1:length(deme$migrations)){
      ordered_deme$migrations[p][[1]] <- list()

      ordered_deme$migrations[[p]]$start_time <- deme$migrations[[p]]$start_time
      ordered_deme$migrations[[p]]$end_time <- deme$migrations[[p]]$end_time
      ordered_deme$migrations[[p]]$source <- deme$migrations[[p]]$source
      ordered_deme$migrations[[p]]$dest <- deme$migrations[[p]]$dest
      ordered_deme$migrations[[p]]$rate <- deme$migrations[[p]]$rate
    }
  }

  ordered_deme$pulses <- list()
  if (length(deme$pulses) > 0){
    for (p in 1:length(deme$pulses)){
      ordered_deme$pulses[p][[1]] <- list()

      ordered_deme$pulses[[p]]$sources <- deme$pulses[[p]]$sources
      ordered_deme$pulses[[p]]$dest <- deme$pulses[[p]]$dest
      ordered_deme$pulses[[p]]$time <- deme$pulses[[p]]$time
      ordered_deme$pulses[[p]]$proportions <- deme$pulses[[p]]$proportions
    }
  }

  return(ordered_deme)
}

conv_prop_vec <- function(demes){
  for (i in 1:length(demes$demes)){
    if (length(demes$demes[[i]]$proportions) > 0){
      demes$demes[[i]]$proportions <- as.double(unlist(demes$demes[[i]]$proportions))
    } else{
      demes$demes[[i]]$proportions <- vector(mode="double")
    }
  }

  return(demes)
}

post_process_expected <- function(exp){
  exp <- order_demes(exp)
  exp <- convert_infinity(exp)
  exp <- conv_prop_vec(exp)

  return(exp)
}



