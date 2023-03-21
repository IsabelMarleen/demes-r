validate_demes <- function(inp){
  out <- inp
  named_inp <- name_demes(inp)
  deme_names <- names(named_inp$demes)

  # Check for time_units and generation_time
  if (is.null(inp$time_units)){
    stop("time_units must be specified but was missing.", call. = FALSE)
  }

  if (is.null(inp$generation_time) & out$time_units == "generations"){
    out$generation_time <- as.double(1)
  } else if(out$time_units == "generations" & out$generation_time != 1) {
    stop("When time_units is 'generations', generation_time must be equal to 1 and can be omitted.", call. = FALSE)
  } else if (is.null(inp$generation_time)) {
    stop("generation_time must be specified, unless time_units is 'generations'.", call. = FALSE)
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
  if (!all(inp$defaults$epoch$start_size >= 0 & !is.null(inp$defaults$epoch$start_size))){
    stop("Epoch start_size cannot be negative, but a negative top-level default value was specified.", call.=FALSE)
  }

  # Demes validation
  for (i in 1:length(inp$demes)){
    # Attempting to validate deme-level defaults insofar as possible
    if (!all(inp$demes[[i]]$defaults$epoch$start_size >= 0 & !is.null(inp$demes[[i]]$defaults$epoch$start_size))){
      stop("Epoch start_size cannot be negative, but a negative deme-level default value was specified.", call.=FALSE)
    }

    # Name
    if (is.null(inp$demes[[i]]$name)){
      stop(paste("Every deme must have a name, but deme", i, "does not have one.", call.=F))
    }

    # Description
    if (is.null(inp$demes[[i]]$description) & !is.null(inp$defaults$deme$description)){
      out$demes[[i]]$description <- inp$defaults$deme$description
    } else if (is.null(inp$demes[[i]]$description)) {
      out$demes[[i]]$description <- ""
    }

    # Ancestors
    if (length(inp$demes[[i]]$ancestors) > 0){
      out$demes[[i]]$ancestors <- unlist(out$demes[[i]]$ancestors)
    } else if (is.null(inp$demes[[i]]$ancestors) & !is.null(inp$defaults$deme$ancestors)){
      out$demes[[i]]$ancestors <- unlist(inp$defaults$deme$ancestors)
    } else {
      out$demes[[i]]$ancestors <- vector(mode="character")
    }
    if (out$demes[[i]]$name %in% out$demes[[i]]$ancestors){
      stop(paste0("No deme may appear in its own ancestors list. This was violated in deme ", i, "."), call.=F)
    } else if (length(out$demes[[i]]$ancestors) != length(unique(out$demes[[i]]$ancestors))){
      stop(paste0("Each element of the ancestors list must be unique. This was violated in deme ", i, "."), call.=F)
    }
    is_char_ancestor <- sapply(out$demes[[i]]$ancestors, is.character)
    if (any(!is_char_ancestor)){
      stop(paste0("Each element of the ancestors list must be a string. This was violated in deme ", i, ", where an ancestor had type ", typeof(out$demes[[i]]$ancestors[!is_char_ancestor]), "."), call.=F)
    }

    # Proportions
    if (!is.null(out$demes[[i]]$proportions)){
      if(length(out$demes[[i]]$proportions) > 0){
        out$demes[[i]]$proportions <- as.double(unlist(inp$demes[[i]]$proportions))
      } else {
        out$demes[[i]]$proportions <- vector(mode="double")
      }
    } else if (!is.null(inp$defaults$deme$proportions)){
      out$demes[[i]]$proportions <- as.double(unlist(inp$defaults$deme$proportions))
    } else if (length(out$demes[[i]]$ancestors) == 1){
      out$demes[[i]]$proportions <- as.double(1)
    } else if(length(out$demes[[i]]$ancestors) == 0){
        out$demes[[i]]$proportions <- vector(mode="double")
    } else{
        stop("proportions cannot be determined with the information provided. proportions must either be specified explicitly, via defaults or have one or less ancestors.", call.=FALSE)
    }

    if (sum(out$demes[[i]]$proportions) > 1){
      stop(paste0("If the proportions list is not empty, then the values must sum to 1. This was violated in deme ", i, "."), call. = FALSE)
    } else if (length(out$demes[[i]]$proportions) != length(out$demes[[i]]$ancestors)){
      stop(paste0("The proportions list must have the same length as the ancestors list. This was violated in deme ", i, "."), call.=FALSE)
    }

    # Start time
    if (!is.null(inp$demes[[i]]$start_time)){
      out$demes[[i]]$start_time <- as.double(out$demes[[i]]$start_time)
    } else if(!is.null(inp$demes[[i]]$defaults$start_time)){
      out$demes[[i]]$start_time <-  as.double(inp$demes[[i]]$defaults$start_time)
    } else if (!is.null(inp$defaults$deme$start_time)){
      out$demes[[i]]$start_time <-  as.double(inp$defaults$deme$start_time)
    } else if (!is.null(inp$defaults$epoch$start_time)){
      out$demes[[i]]$start_time <-  as.double(inp$defaults$epoch$start_time)
    } else if (length(out$demes[[i]]$ancestors) == 0){
      out$demes[[i]]$start_time <- Inf
    } else if (length(out$demes[[i]]$ancestors) == 1 & get_ancestors_endtime(out, i, deme_names) > 0){
      out$demes[[i]]$start_time <- as.double(get_ancestors_endtime(out, i, deme_names))
    } else {
      stop(paste("start_time of deme", i, "cannot be determined from the provided information."), call.=FALSE)
    }

    if (out$demes[[i]]$start_time == Inf & length(out$demes[[i]]$ancestors) != 0){
      stop(paste0("If the start_time of a deme is infinity, ancestors must be an empty list. This is violated in deme ", i, "."), call.=F)
    } else if (out$demes[[i]]$start_time != Inf & length(out$demes[[i]]$ancestors) == 0){
      stop(paste0("If a deme has a finite start_time, it must have specified ancestors. This is violated in deme ", i, "."), call.=F)
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
        out_curr_epoch$start_time <- as.double(out$demes[[i]]$start_time)
      } else if (j > 1) {
        out_curr_epoch$start_time <- as.double(out$demes[[i]]$epochs[[j-1]]$end_time)
      }

      # Epoch end time
      if (is.null(out_curr_epoch$end_time)){
        if (!is.null(inp$demes[[i]]$defaults$epoch$end_time)){
          out_curr_epoch$end_time <- as.double(inp$demes[[i]]$defaults$epoch$end_time)
        } else if (!is.null(inp$defaults$epoch$end_time)){
          out_curr_epoch$end_time <- as.double(inp$defaults$epoch$end_time)
        } else if (j == length(out$demes[[i]]$epochs)) {
          out_curr_epoch$end_time <- as.double(0)
        } else {
          stop(paste("Epoch end_time", j, "in deme", i, "cannot be determined."), call.=FALSE)
        }
      } else if (as.double(out_curr_epoch$end_time) >= out$demes[[i]]$start_time){
        stop(paste("The end_time of epoch", j, "is larger or equal to the start_time of deme", i, "but needs to be strictly smaller."), call.=FALSE)
      } else if (j > 1){
        if (out_curr_epoch$end_time >= out$demes[[i]]$epochs[[j-1]]$end_time){
          stop(paste("The end_time values of successive epochs must be strictly decreasing, but in deme", i, "epoch", j, "is larger or equal to the end_time of epoch", j-1, "."), call.=FALSE)
        }
      } else {
        out_curr_epoch$end_time <- as.double(out_curr_epoch$end_time)
      }

      # Epoch start size and end size
      if (!is.null(out_curr_epoch$start_size)){
        out_curr_epoch$start_size <- as.double(out_curr_epoch$start_size)
      } else if (!is.null(inp$demes[[i]]$defaults$epoch$start_size)){ # Resolve start size defaults
        out_curr_epoch$start_size <- as.double(inp$demes[[i]]$defaults$epoch$start_size)
      } else if (!is.null(inp$defaults$epoch$start_size)){
        out_curr_epoch$start_size <- as.double(inp$defaults$epoch$start_size)
      }

      if (!is.null(out_curr_epoch$end_size)){
        out_curr_epoch$end_size <- as.double(out_curr_epoch$end_size)
      } else if (!is.null(inp$demes[[i]]$defaults$epoch$end_size)){ # Resolve end size defaults
        out_curr_epoch$end_size <- as.double(inp$demes[[i]]$defaults$epoch$end_size)
      } else if (!is.null(inp$defaults$epoch$end_size)){
          out_curr_epoch$end_size <- as.double(inp$defaults$epoch$end_size)
      }

      if (j == 1){
        if (is.null(out_curr_epoch$end_size) & is.null(out_curr_epoch$start_size)){
          stop(paste("In the first epoch in each deme, at least one of start_size or end_size must be specified, possibly via default values. In deme", i, "both values were missing."), call.=FALSE)
        } else if (is.null(out_curr_epoch$start_size)){
          out_curr_epoch$start_size <- as.double(out_curr_epoch$end_size)
        } else if (is.null(out_curr_epoch$end_size)){
          out_curr_epoch$end_size <- as.double(out_curr_epoch$start_size)
        }

        if (out$demes[[i]]$start_time == Inf & out_curr_epoch$start_size != out_curr_epoch$end_size){
          stop(paste("If a deme has an infinite start_time, its first epoch must have identical start and end sizes, but in deme", i, "this is violated."), call.=FALSE)
        }
      } else {
        if (is.null(out_curr_epoch$start_size)){
          out_curr_epoch$start_size <- as.double(out$demes[[i]]$epochs[[j-1]]$end_size)
        }
        if (is.null(out_curr_epoch$end_size)){
          out_curr_epoch$end_size <- as.double(out_curr_epoch$start_size)
        }
      }

      # Size function
      if (is.null(out_curr_epoch$size_function)){
        if (!is.null(inp$demes[[i]]$defaults$epoch$size_function)){
          out_curr_epoch$size_function <- inp$demes[[i]]$defaults$epoch$size_function
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
        stop(paste("If epoch start_time infinite, the size_function must be constant. In deme", i, "epoch", j, "the size function is", out_curr_epoch$size_function, "instead."), call.=FALSE)
      }

      # Selfing rate
      if (is.null(out_curr_epoch$selfing_rate)){
        if (!is.null(inp$demes[[i]]$defaults$epoch$selfing_rate)){
          out_curr_epoch$selfing_rate <- inp$demes[[i]]$defaults$epoch$selfing_rate
        } else if (!is.null(inp$defaults$epoch$selfing_rate)){
          out_curr_epoch$selfing_rate <- inp$defaults$epoch$selfing_rate
        } else {
          out_curr_epoch$selfing_rate <- as.double(0)
        }
      }

      # Cloning rate
      if (is.null(out_curr_epoch$cloning_rate)){
        if (!is.null(inp$demes[[i]]$defaults$epoch$cloning_rate)){
          out_curr_epoch$cloning_rate <- inp$demes[[i]]$defaults$epoch$cloning_rate
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
    migs <- NULL
    for (i in 1:start_num_migr){
      # Rate
      if (is.null(out$migrations[[i]]$rate) & !is.null(inp$defaults$migration$rate)){
        out$migrations[[i]]$rate <- as.double(inp$defaults$migration$rate)
      } else if (is.null(out$migrations[[i]]$rate)){
        stop(paste0("If a migration is specified, a migration rate must be specified, possibly via default values. This is violated in migration", i, "."), call.=FALSE)
      } else {
        out$migrations[[i]]$rate <- as.double(out$migrations[[i]]$rate)
      }

      if (sum(out$migrations[[i]]$rate) > 1){
        stop(paste0("Sum of migration rates must be less than or equal to 1, violated in demes$migration[[", i, "]]."), call. = FALSE)
      }

      # Source
      if (is.null(out$migrations[[i]]$source) & !is.null(inp$defaults$migration$source) & is.null(out$migrations[[i]]$demes)){
        out$migrations[[i]]$source <- inp$defaults$migration$source
      }

      # Dest
      if (is.null(out$migrations[[i]]$dest) & !is.null(inp$defaults$migration$dest) & is.null(out$migrations[[i]]$demes)){
        out$migrations[[i]]$dest<- inp$defaults$migration$dest
      }

      # if (!(out$migrations[[i]]$dest %in% names(out$demes) ))
      # TODO: Throw error if dest / source does not match a population

      # Demes
      if (is.null(out$migrations[[i]]$demes) & !is.null(inp$defaults$migration$demes) & is.null(out$migrations[[i]]$dest) & is.null(out$migrations[[i]]$source)){
        out$migrations[[i]]$demes <- inp$defaults$migration$demes
      }

      # Mode of migration
      if (is.null(out$migrations[[i]]$demes) & !is.null(out$migrations[[i]]$dest) & !is.null(out$migrations[[i]]$source)){
        migr_mode <- "asymmetric"
      } else if (!is.null(out$migrations[[i]]$demes) & is.null(out$migrations[[i]]$dest) & is.null(out$migrations[[i]]$source)){
        migr_mode <- "symmetric"
      } else {
        stop(paste("Mode of migration (either symmetric or asymmetric) could not be determined for migration", i, ". For asymmetric migration,
                   'dest' and 'source' must be specified and 'demes' must be NULL and for symmetric migration it is the other way around."), call.=FALSE)
      }

      # Symmetric Migration
      if (migr_mode == "symmetric"){
        if (length(out$migrations[[i]]$demes) < 2){
          stop(paste("Symmetric migration requires at least two populations to be specified in 'migrations$demes', but fewer than 2 populations were specified for symmetric migration in migration", i, ". This cannot be resolved."), call.=FALSE)
        } else if (length(out$migrations[[i]]$demes) != length(unique(out$migrations[[i]]$demes))){
          stop(paste("Symmetric migration requires distinct populations to be specified in 'migrations$demes', but in migration", i, " not all population names were unique."), call.=FALSE)
        } else if (!all(out$migrations[[i]]$demes %in% names(out$demes))){
          stop(paste("Symmetric migration requires all specified populations to correspond to resolved demes. This was not the case in migration", i, ", where populations", out$migrations[[i]]$demes[out$migrations[[i]]$demes %in% names(out$demes)], "did not correspond to defined demes."), call.=FALSE)
        }

        # Saving symmetric migration at pos i and resetting migration at pos i
        sym_migration <- out$migrations[[i]]
        out$migrations[[i]]$demes <- NULL
        sym_combs <- combn(sym_migration$demes, 2) # Getting all combinations
        asym_from_sym <- list()
        mig_counter <- 1

        for (k in 1:ncol(sym_combs)){
          asym_from_sym[[mig_counter]] <- list()

          # First asymmetric migration of pair
          asym_from_sym[[mig_counter]]$source <- sym_combs[1, k]
          asym_from_sym[[mig_counter]]$dest <- sym_combs[2, k]
          asym_from_sym[[mig_counter]]$rate <- sym_migration$rate
          asym_from_sym[[mig_counter]]$start_time <- sym_migration$start_time
          asym_from_sym[[mig_counter]]$end_time <- sym_migration$end_time
          # start_time
          asym_from_sym[[mig_counter]]$start_time <- validate_migration_times(out, asym_from_sym[[mig_counter]], "start", deme_names)
          # end_time
          asym_from_sym[[mig_counter]]$end_time <- validate_migration_times(out, asym_from_sym[[mig_counter]], "end", deme_names)

          # Second asymmetric migration of pair
          mig_counter <- mig_counter+1
          asym_from_sym[[mig_counter]] <- list()
          asym_from_sym[[mig_counter]]$dest <- sym_combs[1, k]
          asym_from_sym[[mig_counter]]$source <- sym_combs[2, k]
          asym_from_sym[[mig_counter]]$rate <- sym_migration$rate
          asym_from_sym[[mig_counter]]$start_time <- sym_migration$start_time
          asym_from_sym[[mig_counter]]$end_time <- sym_migration$end_time
          # start_time
          asym_from_sym[[mig_counter]]$start_time <- validate_migration_times(out, asym_from_sym[[mig_counter]], "start", deme_names)
          # end_time
          asym_from_sym[[mig_counter]]$end_time <- validate_migration_times(out, asym_from_sym[[mig_counter]], "end", deme_names)
          mig_counter <- mig_counter+1
        }
        if (is.null(migs)){
          migs <- asym_from_sym
        } else{
          migs <- c(migs, asym_from_sym)
        }
      } else { # asymmetric migrations
        # start_time
        out$migrations[[i]]$start_time <- validate_migration_times(out, out$migrations[[i]], "start", deme_names)
        # end_time
        out$migrations[[i]]$end_time <- validate_migration_times(out, out$migrations[[i]], "end", deme_names)
        if (is.null(migs)){
          migs <- list(out$migrations[[i]])
        } else{
          migs <- c(migs, list(out$migrations[[i]]))
        }
      }
    }
    out$migrations <- migs
  }

  # Pulses
  pulse_times <- c()
  if (length(inp$pulses) == 0 | is.null(inp$pulses)){
    out$pulses <- list()
  } else {
    for (i in 1:length(inp$pulses)){
      # Sources
      if (is.null(inp$pulses[[i]]$sources) & !is.null(inp$defaults$pulse$sources)){
        out$pulses[[i]]$sources <- inp$defaults$pulse$sources
      } else if (is.null(inp$pulses[[i]]$sources)){
        stop(paste("No sources were found for pulse", i, ". They are required for pulse resolution, however."), call.=F)
      }
      # Proportions
      if (is.null(inp$pulses[[i]]$proportions) & !is.null(inp$defaults$pulse$proportions)){
        out$pulses[[i]]$proportions <- inp$defaults$pulse$proportions
      } else if (is.null(inp$pulses[[i]]$proportions)){
        stop(paste("No proportions were found for pulse", i, ". They are required for pulse resolution, however."), call.=F)
      }
      # Dest
      if (is.null(inp$pulses[[i]]$dest) & !is.null(inp$defaults$pulse$dest)){
        out$pulses[[i]]$dest <- inp$defaults$pulse$dest
      } else if (is.null(inp$pulses[[i]]$dest)){
        stop(paste("No dest was found for pulse", i, ". It is required for pulse resolution, however."), call.=F)
      }
      # Time
      if (is.null(inp$pulses[[i]]$time) & !is.null(inp$defaults$pulse$time)){
        out$pulses[[i]]$time <- inp$defaults$pulse$time
      } else if (is.null(inp$pulses[[i]]$time)){
        stop(paste("No time was found for pulse", i, ". It is required for pulse resolution, however."), call.=F)
      }
      pulse_times[i] <- out$pulses[[i]]$time
    }

    # Sort pulses
    out$pulses <- out$pulses[order(pulse_times, decreasing = TRUE)]
  }

  # Final Validation
  # Demes
  if (length(out$demes) == 0){
    stop("There must be at least one deme, but none were specified.", call.=F)
  }
  deme_names <- unlist(lapply(out$demes, function(x){return(x$name)}))
  if (length(deme_names) != length(unique(deme_names))){
    stop(paste("Deme names must be unique in the model, but deme(s)", unique(deme_names[duplicated(deme_names)]), "appear(s) several times"))
  }

  return(out)
}



validate_migration_times <- function(out, mig, time, deme_names){
  if (time == "start"){
    if (!is.null(mig$start_time)){
      start_time <- as.double(mig$start_time)
    } else if (!is.null(out$defaults$migration$start_time)) {
      start_time <- as.double(out$defaults$migration$start_time)
    } else {
      source_index <- match(mig$source, deme_names)
      dest_index <- match(mig$dest, deme_names)
      start_time <- as.double(
        min(out$demes[[source_index]]$start_time,
            out$demes[[dest_index]]$start_time))
    }
    return(start_time)

  } else if (time == "end"){
    if (!is.null(mig$end_time)){
      end_time <- as.double(mig$end_time)
    } else if (!is.null(out$defaults$migration$end_time)) {
      end_time <- as.double(out$defaults$migration$end_time)
    } else {
      source_index <- match(mig$source, deme_names)
      dest_index <- match(mig$dest, deme_names)
      source_last_epoch <- length(out$demes[[source_index]]$epochs)
      dest_last_epoch <- length(out$demes[[dest_index]]$epochs)

      end_time <- as.double(
        max(out$demes[[source_index]]$epoch[[source_last_epoch]]$end_time,
            out$demes[[dest_index]]$epoch[[dest_last_epoch]]$end_time))
    }
    return(as.double(end_time))
  }
}

get_ancestors_endtime <- function(out, i, deme_names){
  ancestor_name <- out$demes[[i]]$ancestors[1]
  ancestor_index <- match(ancestor_name, deme_names)
  last_epoch <- length(out$demes[[ancestor_index]]$epochs)

  return(as.double(out$demes[[ancestor_index]]$epochs[[last_epoch]]$end_time))
}
