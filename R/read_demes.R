#' Load and validate a Demes model
#'
#' This function reads a Demes model from a YAML file or a literal YAML string,
#' validates the model, and returns it as a fully instantiated R object.
#'
#' See the Demes specification document for more details about the Demes format
#' <https://github.com/popsim-consortium/demes-spec/>.
#'
#' @param file Either a path to a YAML file or a connection open for writing
#' @param text If a \code{file} is not provided, Demes YAML input is
#'   read as a literal string given in the \code{text}
#' @return A fully instantiated Demes model as a nested R list
#' @export
#'
#' @examples
#' path <- system.file("extdata/yaml", "ex03.yaml", package = "demes")
#' demes1 <- read_demes(file = path)
#'
#' yaml_string <- "time_units: generations\ndemes:\n  - name: a\n    epochs:\n    - start_size: 100"
#' demes2 <- read_demes(text = yaml_string)
read_demes <- function(file, text){
  if (missing(file) && !missing(text)) {
    file <- NULL
    inp <- yaml::read_yaml(text = text)
  } else if (!missing(file) && missing(text)) {
    inp <- yaml::read_yaml(file = file)
    text <- NULL
  } else {
    stop("Either filepath or text must be supplied.", call. = FALSE)
  }

  demes <- validate_demes(inp)
  demes <- name_demes(demes)
  demes <- convert_infinity(demes)

  return(demes)
}
