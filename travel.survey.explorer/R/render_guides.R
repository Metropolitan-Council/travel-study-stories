#' Render all helper modals, introduction, and notes documents
#'
#' @param .output_dir character, directory location for output markdowns
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' render_guides()
#' }
#' @importFrom rmarkdown render
render_guides <- function(.output_dir = "inst/app/www") {
  rmarkdown::render("inst/guides/ABOUT.Rmd",
    output_format = "github_document", # -----
    output_file = "ABOUT.md",
    output_dir = .output_dir
  )
}
