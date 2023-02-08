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

    comparison_group <- c("end_time", "end_size", "start_size", "size_function", "selfing_rate", "cloning_rate")

    for(j in 1:length(inp$demes[[i]]$epochs)){ # iterate through all epochs in deme i
      inp_curr_epoch <- inp$demes[[i]]$epochs[[j]]
      out_curr_epoch <- out$demes[[i]]$epochs[[j]]

      present_epoch <- c(names(inp_curr_epoch))
      missing_epoch <- setdiff(comparison_group, present_epoch)

      if (is.null(inp$demes[[i]]$epochs)){
        # should probably throw an error, there should be at least one epoch per deme, right?
        out_curr_epoch <- list()
        out_curr_epoch$start_size <- as.double(0)
        out_curr_epoch$end_size <- as.double(0)
        out_curr_epoch$end_time <- as.double(0)
        out_curr_epoch$size_function <- "exponential"
        out_curr_epoch$selfing_rate <- as.double(0)
        out_curr_epoch$cloning_rate <- as.double(0)
      } else {

        if (is.null(inp_curr_epoch$end_time)){
          out_curr_epoch$end_time <- as.double(0)
        }

        if(is.null(inp_curr_epoch$start_size) & !is.null(inp_curr_epoch$end_size)){
          out_curr_epoch$start_size <- as.double(inp_curr_epoch$end_size)
        } else if(!is.null(inp_curr_epoch$start_size) & is.null(inp_curr_epoch$end_size)){
          out_curr_epoch$end_size <- as.double(inp_curr_epoch$start_size)
        } else if(!is.null(inp_curr_epoch$start_size) & !is.null(inp_curr_epoch$end_size)){
          out_curr_epoch$end_size <- as.double(inp_curr_epoch$end_size)
          out_curr_epoch$start_size <- as.double(inp_curr_epoch$start_size)
        }

        if(is.null(inp_curr_epoch$size_function)){
          # This is a quick fix, test file args_from_file_01.yaml suggests that the size function is calculated from the size values?
          out_curr_epoch$size_function <- "constant"
        }

        if(is.null(inp_curr_epoch$selfing_rate)){
          out_curr_epoch$selfing_rate <- as.double(0)
        } else {
          out_curr_epoch$selfing_rate <- as.double(inp_curr_epoch$selfing_rate)
        }

        if(is.null(inp_curr_epoch$cloning_rate)){
          out_curr_epoch$cloning_rate <- as.double(0)
        } else {
          out_curr_epoch$cloning_rate <- as.double(inp_curr_epoch$cloning_rate)
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

