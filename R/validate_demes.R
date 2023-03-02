validate_demes <- function(inp){
  out <- inp

  # Check for time_units and generation_time
  if (is.null(inp$time_units)){
    stop("time_units must be specified but was missing.", .call = FALSE)
  }

  if (is.null(inp$generation_time) & out$time_units == "generations"){
    out$generation_time <- as.double(1)
  } else if(out$time_units == "generations" & out$generation_time != 1) {
    stop("When time_units is 'generations', generation_time must be equal to 1 and can be omitted.", .call = FALSE)
  } else {
    stop("generation_time must be specified, unless time_units is 'generations'.", .call = FALSE)
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

  # Attempting to validate top-level defaults insofar as possible
  if (inp$defaults$epoch$start_size < 0){
    stop("Epoch start_size cannot be negative, but a negative top-level default value was specified.", .call=FALSE)
  }

  # Demes validation
  # Attempting to validate deme-level defaults insofar as possible
  if (inp$demes$defaults$epoch$start_size < 0){
    stop("Epoch start_size cannot be negative, but a negative deme-level default value was specified.", .call=FALSE)
  }

  for (i in 1:length(inp$demes)){
    # Name
    if (is.null(inp$demes[[i]]$name)){
      stop(paste("Every deme must have a name, but deme", i, "does not have one.", .call=F))
    }

    # Description
    if (is.null(inp$demes[[i]]$description) & !is.null(inp$defaults$deme$description)){
      out$demes[[i]]$description <- inp$defaults$deme$description
    } else {
      out$demes[[i]]$description <- ""
    }

    # Ancestors
    if (is.null(inp$demes[[i]]$ancestors) & !is.null(inp$defaults$deme$ancestors)){
      out$demes[[i]]$ancestors <- inp$defaults$deme$ancestors
    } else {
      out$demes[[i]]$ancestors <- list()
    }

    # Proportions
    if (is.null(inp$demes[[i]]$proportions) & !is.null(inp$defaults$deme$proportions)){
      out$demes[[i]]$proportions <- inp$defaults$deme$proportions
    } else if (is.null(inp$demes[[i]]$proportions)){
      if (length(inp$demes[[i]]$ancestors) == 1){
        out$demes[[i]]$proportions <- list(as.double(1))
      } else if(length(inp$demes[[i]]$ancestors) == 0){
        out$demes[[i]]$proportions <- list()
      } else{
        stop("proportions cannot be determined with the information provided. proportions must either be specified explicitly, via defaults or have one or less ancestors.", .call=FALSE)
      }
    } else{
      out$demes[[i]]$proportions <- as.double(inp$demes[[i]]$proportions)
    }

    # Start time
    if (is.null(inp$demes[[i]]$start_time)){
      if(!is.null(inp$demes$defaults$start_time)){
        out$demes[[i]]$start_time <-  inp$demes$defaults$start_time
      } else if (!is.null(inp$defaults$demes$start_time)){
        out$demes[[i]]$start_time <-  inp$defaults$demes$start_time
      } else if (!is.null(inp$defaults$epoch$start_time)){
        out$demes[[i]]$start_time <-  inp$defaults$epoch$start_time
      } else if (length(out$demes[[i]]$ancestors) == 1 & out$demes[[out$demes[[i]]$ancestors[[1]]]]$end_time > 0){
        out$demes[[i]]$start_time <- out$demes[[out$demes[[i]]$ancestors[[1]]]]$end_time
      } else if (length(out$demes[[i]]$ancestors) == 0){
        out$demes[[i]]$start_time <- Inf
      } else {
        stop(paste("start_time of deme", i, "cannot be determined from the provided information."), .call=FALSE)
      }
    } else {
      out$demes[[i]]$start_time <- as.double(inp$demes[[i]]$start_time)
    }

    # Epochs
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

      # Epoch start time
      if (j == 1){
        out_curr_epoch$start_time <- out$demes[[i]]$start_time
      } else {
        out_curr_epoch$start_time <- out$demes[[i]]$epochs[[j-1]]$end_time
      }

      # Epoch end time
      if (is.null(out_curr_epoch$end_time)){
        if (!is.null(inp$demes$defaults$epoch$end_time)){
          out_curr_epoch$end_time <- inp$demes$defaults$epoch$end_time
        } else if (!is.null(inp$defaults$epoch$end_time)){
          out_curr_epoch$end_time <- inp$defaults$epoch$end_time
        } else if (j == length(out$demes[[i]]$epochs)) {
          out_curr_epoch$end_time <- as.double(0)
        } else {
          stop(paste("Epoch end_time", j, "in deme", i, "cannot be determined."), .call=FALSE)
        }
      } else if (out_curr_epoch$end_time >= out$demes[[i]]$start_time){
        stop(paste("The end_time of epoch", j, "is larger or equal to the start_time of deme", i, "but needs to be strictly smaller."), .call=FALSE)
      } else if (j > 1 & out_curr_epoch$end_time >= out$demes[[i]]$epochs[[j-1]]$end_time){
        stop(paste("The end_time values of successive epochs must be strictly decreasing, but in deme", i, "epoch", j, "is larger or equal to the end_time of epoch", j-1, "."), .call=FALSE)
      }

      # Epoch start size and end size
      if (is.null(out_curr_epoch$start_size)){ # Resolve start size defaults
        if (!is.null(inp$defaults$epoch$start_size)){
          out_curr_epoch$start_size <- inp$defaults$epoch$start_size
        } else if (!is.null(inp$demes$defaults$epoch$start_size)){
          out_curr_epoch$start_size <- inp$demes$defaults$epoch$start_size
        }
      }

      if (is.null(out_curr_epoch$end_size)){ # Resolve end size defaults
        if (!is.null(inp$defaults$epoch$end_size)){
          out_curr_epoch$end_size <- inp$defaults$epoch$end_size
        } else if (!is.null(inp$demes$defaults$epoch$end_size)){
          out_curr_epoch$end_size <- inp$demes$defaults$epoch$end_size
        }
      }

      if (j == 1){
        if (is.null(out_curr_epoch$end_size) & is.null(out_curr_epoch$start_size)){
          stop(paste("In the first epoch in each deme, at least one of start_size or end_size must be specified, possibly via default values. In deme", i, "both values were missing."), .call=FALSE)
        } else if (is.null(out_curr_epoch$start_size)){
          out_curr_epoch$start_size <- out_curr_epoch$end_size
        } else if (is.null(out_curr_epoch$end_size)){
          out_curr_epoch$end_size <- out_curr_epoch$start_size
        }

        if (out$demes[[i]]$start_time == Inf & out_curr_epoch$start_size != out_curr_epoch$end_size){
          stop(paste("If a deme has an infinite start_time, its first epoch must have identical start and end sizes, but in deme", i, "this is violated."), .call=FALSE)
        }
      } else {
        if (is.null(out_curr_epoch$start_size)){
          out_curr_epoch$start_size <- out$demes[[i]]$epochs[[j-1]]$end_size
        }
        if (is.null(out_curr_epoch$end_size)){
          out_curr_epoch$end_size <- out_curr_epoch$start_size
        }
      }

      # Size function
      if (is.null(out_curr_epoch$size_function)){
        if (!is.null(inp$demes$defaults$epoch$size_function)){
          out_curr_epoch$size_function <- inp$demes$defaults$epoch$size_function
        } else if (!is.null(inp$defaults$epoch$size_function)){
          out_curr_epoch$size_function <- inp$defaults$epoch$size_function
        } else {
            # size_function must be constant if the epoch has an infinite start_time.
            if(out_curr_epoch$end_size == out_curr_epoch$start_size){
              out_curr_epoch$size_function <- "constant"
            } else{
              out_curr_epoch$size_function <- "exponential"
            }
        }
      }

      if (out_curr_epoch$start_time == Inf & out_curr_epoch$size_function != "constant"){
        stop(paste("If epoch start_time infinite, the size_function must be constant. In deme", i, "epoch", j, "the size function is", out_curr_epoch$size_function, "instead."), .call=FALSE)
      }

      # Selfing rate
      if (is.null(out_curr_epoch$selfing_rate)){
        if (!is.null(inp$demes$defaults$epoch$selfing_rate)){
          out_curr_epoch$selfing_rate <- inp$demes$defaults$epoch$selfing_rate
        } else if (!is.null(inp$defaults$epoch$selfing_rate)){
          out_curr_epoch$selfing_rate <- inp$defaults$epoch$selfing_rate
        } else {
          out_curr_epoch$selfing_rate <- as.double(0)
        }
      }

        # Cloning rate
      if (is.null(out_curr_epoch$cloning_rate)){
        if (!is.null(inp$demes$defaults$epoch$cloning_rate)){
          out_curr_epoch$cloning_rate <- inp$demes$defaults$epoch$cloning_rate
        } else if (!is.null(inp$defaults$epoch$cloning_rate)){
          out_curr_epoch$cloning_rate <- inp$defaults$epoch$cloning_rate
        } else {
          out_curr_epoch$cloning_rate <- as.double(0)
        }
      }

      out$demes[[i]]$epochs[[j]] <- out_curr_epoch
    }
  }

  out <- name_demes(out)

  # Migrations
  if (length(inp$migrations) == 0){
    out$migrations <- list()
  } else {
    start_num_migr <- length(inp$migrations)
    for (i in 1:length(start_num_migr)){
      # Rate
      if (is.null(out$migrations[[i]]$rate) & !is.null(inp$defaults$migrations$rate)){
        out$migrations[[i]]$rate <- inp$defaults$migrations$rate
      } else if (is.null(out$migrations[[i]]$rate)){
        stop(paste0("If a migration is specified, a migration rate must be specified, possibly via default values. This is violated in migration", i, "."), .call=FALSE)
      } else {
        out$migrations[[i]]$rate <- as.double(out$migrations[[i]]$rate)
      }

      if (sum(out$migrations[[i]]$rate) > 1){
        stop(paste0("Sum of migration rates must be less than or equal to 1, violated in demes$migration[[", i, "]]."), call. = FALSE)
      }

      # Source
      if (is.null(out$migrations[[i]]$source) & !is.null(inp$defaults$migrations$source)){
        out$migrations[[i]]$source <- inp$defaults$migrations$source
      }

      # Dest
      if (is.null(out$migrations[[i]]$dest) & !is.null(inp$defaults$migrations$dest)){
        out$migrations[[i]]$dest<- inp$defaults$migrations$dest
      }

      # if (!(out$migrations[[i]]$dest %in% names(out$demes) ))
      # TODO: Throw error if dest / source does not match a population

      # Demes
      if (is.null(out$migrations[[i]]$demes) & !is.null(inp$defaults$migrations$demes)){
        out$migrations[[i]]$demes <- inp$defaults$migrations$demes
      }

      # Mode of migration
      if (is.null(out$migrations[[i]]$demes) & !is.null(out$migrations[[i]]$dest) & !is.null(out$migrations[[i]]$source)){
        migr_mode <- "asymmetric"
      } else if (!is.null(out$migrations[[i]]$demes) & is.null(out$migrations[[i]]$dest) & is.null(out$migrations[[i]]$source)){
        migr_mode <- "asymmetric"
      } else {
        stop(paste("Mode of migration (either symmetric or asymmetric) could not be determined for migration", i, ". For asymmetric migration,
                   'dest' and 'source' must be specified and 'demes' must be NULL and for symmetric migration it is the other way around."), .call=FALSE)
      }

      # Symmetric Migration
      if (migr_mode == "symmetric"){
        if (length(out$migrations[[i]]$demes) < 2){
          stop(paste("Symmetric migration requires at least two populations to be specified in 'migrations$demes', but fewer than 2 populations were specified for symmetric migration in migration", i, ". This cannot be resolved."), .call=FALSE)
        } else if (length(out$migrations[[i]]$demes) != length(unique(out$migrations[[i]]$demes))){
          stop(paste("Symmetric migration requires distinct populations to be specified in 'migrations$demes', but in migration", i, " not all population names were unique."), .call=FALSE)
        } else if (!all(out$migrations[[i]]$demes %in% names(out$demes))){
          stop(paste("Symmetric migration requires all specified populations to correspond to resolved demes. This was not the case in migration", i, ", where populations", out$migrations[[i]]$demes[out$migrations[[i]]$demes %in% names(out$demes)], "did not correspond to defined demes."), .call=FALSE)
        }

        # Saving symmetric migration at pos i and resetting migration at pos i
        sym_migration <- out$migrations[[i]]
        out$migrations[[i]] <- NULL
        sym_combs <- combn(out$migrations[[i]]$demes, 2) # Getting all combinations

        for (k in ncol(sym_combs)){
          if (k == 1){ # The very first asymmetric migration replaces the original symmetric migration
            # First asymmetric migration of pair
            out$migrations[[i]]$source <- sym_combs[1, k]
            out$migrations[[i]]$dest <- sym_combs[2, k]
            out$migrations[[i]]$rate <- sym_migration$rate
            out$migrations[[i]]$start_time <- sym_migration$start_time
            out$migrations[[i]]$end_time <- sym_migration$end_time
            # start_time
            out$migrations[[i]]$start_time <- validate_migration_times(out, i, time="start")
            # end_time
            out$migrations[[i]]$end_time <- validate_migration_times(out, i, time="end")

            # Second asymmetric migration of pair
            pos_counter <- length(out$migrations)+1
            out$migrations[[pos_counter]]$dest <- sym_combs[1, k]
            out$migrations[[pos_counter]]$source <- sym_combs[2, k]
            out$migrations[[pos_counter]]$rate <- sym_migration$rate
            out$migrations[[pos_counter]]$start_time <- sym_migration$start_time
            out$migrations[[pos_counter]]$end_time <- sym_migration$end_time
            # start_time
            out$migrations[[i]]$start_time <- validate_migration_times(out, pos_counter, time="start")
            # end_time
            out$migrations[[i]]$end_time <- validate_migration_times(out, pos_counter, time="end")

          } else {
            pos_counter <- pos_counter + 1
            # First asymmetric migration of pair
            out$migrations[[pos_counter]]$source <- sym_combs[1, k]
            out$migrations[[pos_counter]]$dest <- sym_combs[2, k]
            out$migrations[[pos_counter]]$rate <- sym_migration$rate
            out$migrations[[pos_counter]]$start_time <- sym_migration$start_time
            out$migrations[[pos_counter]]$end_time <- sym_migration$end_time
            # start_time
            out$migrations[[i]]$start_time <- validate_migration_times(out, pos_counter, time="start")
            # end_time
            out$migrations[[i]]$end_time <- validate_migration_times(out, pos_counter, time="end")

            pos_counter <- pos_counter + 1
            # Second asymmetric migration of pair
            out$migrations[[pos_counter]]$dest <- sym_combs[1, k]
            out$migrations[[pos_counter]]$source <- sym_combs[2, k]
            out$migrations[[pos_counter]]$rate <- sym_migration$rate
            out$migrations[[pos_counter]]$start_time <- sym_migration$start_time
            out$migrations[[pos_counter]]$end_time <- sym_migration$end_time
            # start_time
            out$migrations[[i]]$start_time <- validate_migration_times(out, pos_counter, time="start")
            # end_time
            out$migrations[[i]]$end_time <- validate_migration_times(out, pos_counter, time="end")
          }
        }
      } else { # asymmetric migrations
        # start_time
        out$migrations[[i]]$start_time <- validate_migration_times(out, i, time="start")
        # end_time
        out$migrations[[i]]$end_time <- validate_migration_times(out, i, time="end")
      }
    }
  }

  if (is.null(inp$pulses)){
    out$pulses <- list()
  }

  return(out)
}



validate_migration_times <- function(out, i, time){
  if (time == "start"){
    if (!is.null(out$migrations[[i]]$start_time)){
      start_time <- as.double(out$migrations[[i]]$start_time)
    } else if (!is.null(out$defaults$migration$start_time)) {
      start_time <- as.double(out$defaults$migration$start_time)
    } else {
      start_time <- as.double(
        min(out$demes$demes[[out$migrations[[i]]$source]]$start_time,
            out$demes$demes[[out$migrations[[i]]$dest]]$start_time))
    }
    return(start_time)
  } else if (time == "end"){
    if (!is.null(out$migrations[[i]]$end_time)){
      end_time <- as.double(out$migrations[[i]]$end_time)
    } else if (!is.null(out$defaults$migration$end_time)) {
      end_time <- as.double(out$defaults$migration$end_time)
    } else {
      end_time <- as.double(
        max(out$demes$demes[[out$migrations[[i]]$source]]$end_time,
            out$demes$demes[[out$migrations[[i]]$dest]]$end_time))
    }
    return(end_time)
  }
}
