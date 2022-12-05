filepath <- "../ex03.yaml"


#' Load and Validate a Demes Model
#'
#' More description
#'
#' @param filepath
#'
#' @return \code{out}
#' @export
#'
#' @examples
load_deme <- function(filepath){

  inp <- yaml::read_yaml(filepath)
  out <- inp


  # Check for time_units and generation_time
  if (is.null(inp$time_units)){
    out$time_units <- "generations"
  }


  if (is.null(inp$doi)){
    out$doi<- list()
  }

  if (is.null(inp$metadata)){
    out$metadata <- list()
  }

  for (i in 1:length(inp$demes)){
    if (is.null(inp$demes[[i]]$ancestors)){
      out$demes[[i]]$ancestors <- list()
    }

    # Check for description or set default
    if (is.null(inp$demes[[i]]$description)){
      out$demes[[i]]$description <-  ''
    }

    if (is.null(inp$demes[[i]]$proportions)){
      out$demes[[i]]$proportions <- list()
    }

    if (is.null(inp$demes[[i]]$epochs)){
      out$demes[[i]]$epochs <- list()
      # inp$epochs <- data.frame(end_time=c(0), start_size=c(1000), size_function="constant", selfing_rate=c(0), cloning_rate=c(0))
      # Create empty sub items for all the
    }

    if (is.null(inp$demes[[i]]$migrations)){
      out$demes[[i]]$migrations <- list()
    }

    if (is.null(inp$demes[[i]]$pulses)){
      out$demes[[i]]$pulses <- list()
    }
  }

  return(out)
}

test_out <- load_deme(filepath)
#
# metadata: {}
# demes:
#   - name: A
# description: ''
# start_time: .inf
# ancestors: []
# proportions: []
# epochs:
#   - {end_time: 0, start_size: 1000, end_size: 1000, size_function: constant, selfing_rate: 0,
#     cloning_rate: 0}
# migrations: []
# pulses: []

# Currently the python does not print the doi, but it should, the internal python representation and the printed python thing do not match, so use that for comparison
