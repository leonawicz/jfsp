#' Generate a report on a data set
#'
#' Generate a pdf, Word or HTML report on any data set in the jfsp package.
#'
#' Create a report for a specific data set. The report summarizes the variables in the data set. It is a wrapper around and based on \code{dataMaid::makeDataReport}, though it renders output with some differences not available in \code{dataMaid}.
#'
#' Pandoc must be available on the system for rendering to pdf. It is recommended to run from RStudio, which includes the necessary components, if you have difficulty.
#' For Word documents (docx) it is necessary to be on a Windows system with Microsoft Word installed.
#' If no file name is supplied, the output defaults to HTML and the file name is the name of the data set.
#'
#' If \code{metadata = TRUE}, information about the system environment and conditions at run time when the document was generated is included at the bottom of the document. If \code{FALSE} (default), they are removed.
#' Because \code{datadoc} does not directly render the document with \code{dataMaid} and makes some internal changes to the Rmd file, the function call line in the metadata is always removed due to its resulting lack of clarity.
#'
#' @param data jfsp data set, e.g., \code{fmoba}.
#' @param file character, output file, must end in .pdf, .docx, or .html. See details.
#' @param title character, document title.
#' @param subtitle character, document subtitle. Set to \code{NULL} to recover the \code{dataMaid} attribution.
#' @param metadata logical, if \code{TRUE}, include metadata. See details.
#' @param open_doc logical, open document after rendering. Defaults to \code{TRUE}.
#' @param keep_rmd logical, retain the intermediary Rmd document. Defaults to \code{FALSE}.
#' @param details logical, if set to \code{FALSE}, do not print any progress.
#'
#' @return nothing is returned, but a file is written to disk.
#' @export
#'
#' @examples
#' \dontrun{datadoc(fmoba)}
datadoc <- function(data, file, title = "", subtitle = "", metadata = FALSE,
                    open_doc = TRUE, keep_rmd = FALSE, details = TRUE){
  data_name <- deparse(substitute(data))
  if(missing(file)) file <- paste0(data_name, ".html")
  ext <- utils::tail(strsplit(file, "\\.")[[1]], 1)
  if(!ext %in% c("pdf", "docx", "html")) stop("`file` must end in .pdf, .docx or .html.")
  rmd <- gsub(ext, "Rmd", file)
  if(title == "") title <- data_name
  if(details) cat("Genrating Rmd for ", data_name, "...\n", sep = "")
  suppressMessages({
    dataMaid::makeDataReport(data, gsub("docx", "word", ext), render = FALSE,
                             file = rmd, replace = TRUE, openResult = FALSE,
                             codebook = TRUE, reportTitle = paste0("\"", title, "\""))
  })
  if(details) cat("Rendering ", ext, " report...\n", sep = "")
  .intercept_report(rmd, subtitle, metadata)
  if(!keep_rmd) unlink(rmd)
  if(open_doc) pander::openFileInOS(file)
  if(details) cat("Completed.\n")
  invisible()
}

.intercept_report <- function(file, subtitle = "", metadata = FALSE){
  file_lines <- readLines(file)
  file_lines <- gsub("# Codebook summary table", "# Variable summary", file_lines)
  idx_stl <- which(substr(file_lines, 1, 9) == "subtitle:")
  idx_thm <- which(substr(file_lines, 1, 16) == "library(ggplot2)")
  idx_end <- which(substr(file_lines, 1, 30) == "Report generation information:")
  idx_end2 <- which(substr(file_lines, 1, 18) == " *  Function call:")
  file_lines[idx_thm] <- paste0("library(ggplot2); theme_set(theme_bw())")
  if(length(idx_stl) & !is.null(subtitle)) file_lines[idx_stl] <- paste0("subtitle: \"", subtitle, "\"")
  n <- length(file_lines)
  file_lines <- if(metadata) file_lines[-c(idx_end2:n)] else file_lines[-c(idx_end:n)]
  writeLines(file_lines, file)
  rmarkdown::render(file, quiet = TRUE)
  invisible()
}
