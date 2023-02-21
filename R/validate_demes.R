validate_demes <- function(inp){
  out <- inp

  # Check for time_units and generation_time
  if (is.null(inp$time_units)){
    out$time_units <- "generations"
  }

  if (is.null(inp$generation_time)){
    out$generation_time <- as.double(1)
  } else {
    out$generation_time <- as.double(out$generation_time)
  }


  if (is.null(inp$doi)){
    out$doi<- list()
  }

  if (is.null(inp$description)){
    out$description <- ""
  }

  if (is.null(inp$metadata)){
    out$metadata <- list()
    names(out$metadata) <- character()
  }

  for (i in 1:length(inp$demes)){

    if (is.null(inp$demes[[i]]$name)){
      # Throw error?
      out$demes[[i]]$name <-  ''
    }

    # Check for description or set default
    if (is.null(inp$demes[[i]]$description)){
      out$demes[[i]]$description <-  ''
    }

    if (is.null(inp$demes[[i]]$start_time)){
      out$demes[[i]]$start_time <-  Inf
    } else {
      out$demes[[i]]$start_time <- as.double(inp$demes[[i]]$start_time)
    }

    if (length(inp$demes[[i]]$epochs) == 0){
      inp$demes[[i]]$epochs <- list()
      out$demes[[i]]$epochs <- list()
    }

    if (length(inp$demes[[i]]$epochs) > 0){
      num_epochs <- 1:length(inp$demes[[i]]$epochs)
    } else {
      num_epochs <- 1
    }
    for(j in num_epochs){ # iterate through all epochs in deme i
      if (length(inp$demes[[i]]$epochs[j][[1]]) == 0){
        inp$demes[[i]]$epochs[j][[1]] <- list()
        out$demes[[i]]$epochs[j][[1]] <- list()
      }
      inp_curr_epoch <- inp$demes[[i]]$epochs[[j]]
      out_curr_epoch <- out$demes[[i]]$epochs[[j]]

      if (is.null(out_curr_epoch)){
        out_curr_epoch <- list()
      }

      # if(is.null(inp_curr_epoch$start_size) & !is.null(inp_curr_epoch$end_size)){
      #   out_curr_epoch$start_size <- as.double(inp_curr_epoch$end_size)
      # } else if(!is.null(inp_curr_epoch$start_size) & is.null(inp_curr_epoch$end_size)){
      #   out_curr_epoch$end_size <- as.double(inp_curr_epoch$start_size)
      # } else if(!is.null(inp_curr_epoch$start_size) & !is.null(inp_curr_epoch$end_size)){
      #   out_curr_epoch$end_size <- as.double(inp_curr_epoch$end_size)
      #   out_curr_epoch$start_size <- as.double(inp_curr_epoch$start_size)
      # } else{
      #   out_curr_epoch$end_size <- as.double(0)
      #   out_curr_epoch$start_size <- as.double(0)
      # }

      if (is.null(out_curr_epoch$start_size)){
        if (!is.null(inp$defaults$epoch$start_size)){
          out_curr_epoch$start_size <- inp$defaults$epoch$start_size
        } else if(!is.null(inp_curr_epoch$end_size))  {
          out_curr_epoch$start_size <- as.double(inp_curr_epoch$end_size)
        } else {
          out_curr_epoch$start_size <- as.double(0)
        }
      }


      if (is.null(out_curr_epoch$end_size)){
        if (!is.null(inp$defaults$epoch$end_size)){
          out_curr_epoch$end_size <- inp$defaults$epoch$end_size
        } else if(!is.null(out_curr_epoch$start_size))  {
          out_curr_epoch$end_size <- as.double(out_curr_epoch$start_size)
        } else {
          out_curr_epoch$end_size <- as.double(0)
        }
      }


      if (is.null(out_curr_epoch$end_time)){
        if (!is.null(inp$defaults$epoch$end_time)){
          out_curr_epoch$end_time <- inp$defaults$epoch$end_time
        } else {
          out_curr_epoch$end_time <- as.double(0)
        }
      }


      if (is.null(out_curr_epoch$size_function)){
        if (!is.null(inp$defaults$epoch$size_function)){
          out_curr_epoch$size_function <- inp$defaults$epoch$size_function
        } else {
            # size_function
            #
            # A function describing the population size change between start_time and end_time. This may be any string, but the values “constant” and “exponential” are explicitly acknowledged to have the following meanings.
            #
            # constant: the deme’s size does not change over the epoch. start_size and end_size must be equal.
            # exponential: the deme’s size changes exponentially from start_size to end_size over the epoch. If t is a time within the span of the epoch, the deme size N at time t can be calculated as:
            #   dt = (epoch.start_time - t) / (epoch.start_time - epoch.end_time)
            # r = log(epoch.end_size / epoch.start_size)
            # N = epoch.start_size * exp(r * dt)
            #
            # size_function must be constant if the epoch has an infinite start_time.
            if(out_curr_epoch$end_size == out_curr_epoch$start_size){
              out_curr_epoch$size_function <- "constant"
            } else{
              out_curr_epoch$size_function <- "exponential" # xxx quick fix
            }
          }
        }

        if (is.null(out_curr_epoch$selfing_rate)){
          if (!is.null(inp$defaults$epoch$selfing_rate)){
            out_curr_epoch$selfing_rate <- inp$defaults$epoch$selfing_rate
          } else {
            out_curr_epoch$selfing_rate <- as.double(0)
          }
        }

        if (is.null(out_curr_epoch$cloning_rate)){
          if (!is.null(inp$defaults$epoch$cloning_rate)){
            out_curr_epoch$cloning_rate <- inp$defaults$epoch$cloning_rate
          } else {
            out_curr_epoch$cloning_rate <- as.double(0)
          }
        }

        out$demes[[i]]$epochs[[j]] <- out_curr_epoch
      }


    if (is.null(inp$demes[[i]]$proportions) & length(inp$demes[[i]]$ancestors) == 1){
      out$demes[[i]]$proportions <- as.double(1)
    } else if(is.null(inp$demes[[i]]$proportions) & length(inp$demes[[i]]$ancestors) > 1){
      # Throw error?
      print("no proportions even though several ancestors, that can't be right")
    }else{
      out$demes[[i]]$proportions <- as.double(unlist(inp$demes[[i]]$proportions))
    }

    if (is.null(inp$demes[[i]]$ancestors)){
      out$demes[[i]]$ancestors <- list()
    } else {
      out$demes[[i]]$ancestors <- inp$demes[[i]]$ancestors
    }
  }

  if (length(inp$migrations) == 0){
    out$migrations <- list()
  } else {
    for (i in 1:length(inp$migrations)){
      out$migrations[[i]]$start_time <- as.double(out$migrations[[i]]$start_time)
      out$migrations[[i]]$end_time <- as.double(out$migrations[[i]]$end_time)
      out$migrations[[i]]$rate <- as.double(out$migrations[[i]]$rate)
    }
  }

  if (is.null(inp$pulses)){
    out$pulses <- list()
  }

  return(out)
}

