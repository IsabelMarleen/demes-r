#' Load and Validate a Demes Model
#'
#' \code{read_demes} reads a Demes model yaml file or string, validates the input and creates a fully specified R object from it
#'
#' @param file either a character string naming a file or a connection open for writing
#' @param text character string: if file is not supplied and this is, then data are read from the value of text via a text connection. Notice that a literal string can be used to include (small) data sets within R code.
#' @return A fully specified Demes model, a nested list. See the Demes specifications for more details https://github.com/popsim-consortium/demes-spec/.
#' @export
#'
#' @examples
#' \dontrun{
#' file_name <- "test_file.yaml"
#' a <- read_demes(file=file_name)}
#'
#' yaml_string <- "time_units: generations\ndemes:\n  - name: a\n    epochs:\n    - start_size: 100"
#' b <- read_demes(text=yaml_string)
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

  deme <- validate_demes(inp)
  deme <- name_demes(deme)
  deme <- convert_infinity(deme)

  return(deme)
}
