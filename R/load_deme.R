#' Load and Validate a Demes Model
#'
#' More description
#'
#' @param file either a character string naming a file or a connection open for writing
#' @param text character string: if file is not supplied and this is, then data are read from the value of text via a text connection. Notice that a literal string can be used to include (small) data sets within R code.
#' @return \code{out}
#' @export
#'
#' @examples
#' \dontrun{file_name <- "test_file.yaml"
#' a <- load_deme(file=file_name)}
#'
#' yaml_string <- "time_units: generations\ndemes:\n  - name: a\n    epochs:\n    - start_size: 100"
#' b <- load_deme(text=yaml_string)
load_deme <- function(file, text){
  if (missing(file) && !missing(text)) {
    file <- NULL
    inp <- yaml::read_yaml(text = text)
  } else if (!missing(file) && missing(text)) {
    inp <- yaml::read_yaml(file = file)
    text <- NULL
  } else {
    stop("Either filepath or text must be supplied.", call. = FALSE)
  }

  deme <- validate_deme(inp)
  deme <- name_deme(deme)

  return(deme)
}
