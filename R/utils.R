#' Check if we are currently running as an rstudio addin
#' @import rstudioapi

ifunc_run_as_addin <- function() {
  rstudioapi::isAvailable() && rstudioapi::getActiveDocumentContext()$id != "#console"
}


#' Display an alert, only on first launch for the current session
#' @param run_as_addin TRUE if the function is running as an rstudio addin

ifunc_show_alert <- function(run_as_addin) {
  ## Display the alert only on first time launch
  show_alert <- is.null(getOption("sssplugin_hide_alert"))
  if (show_alert) {
    options(sssplugin_hide_alert = TRUE)
    shiny::div(class = "alert alert-warning alert-dismissible",
        shiny::HTML('<button type="button" class="close" data-dismiss="alert" aria-label="Close"><span aria-hidden="true">&times;</span></button>'),
        HTML(gettext("<strong>Warning :</strong> This interface doesn't do anything by itself.", domain = "sssplugin")),
        if (run_as_addin) {
          shiny::HTML(gettext("It will generate R code, insert it in your current R script, and you'll have to run it yourself.", domain = "sssplugin"))
        } else {
          shiny::HTML(gettext("It only generates R code you'll have to copy/paste into your script and run yourself.", domain = "sssplugin"))
        }
    )}
}

#' Returns custom CSS content

ifunc_get_css <- function() {
  css.file <- system.file(file.path("shiny", "css", "ifuncs.css"), package = "sssplugin")
  out <- paste(readLines(css.file),collapse="\n")
  shiny::HTML(out)
}

#' Return first non-null of two values
#' @name first_non_null
#' @param x first object
#' @param y second object

`%||%` <- function(x, y) {
  if (!is.null(x)) x else y
}


#' Wrap an expr to suppress warnings and errors
#'
#' @param expr
#'
#' @return a list object containing output, warnings and errors
#' @export
#'
#' @examples
#' myTryCatch(log(-1))
myTryCatch <- function(expr) {
  warn <- err <- NULL
  value <- withCallingHandlers(
    tryCatch(expr, error=function(e) {
      err <<- e
      NULL
    }), warning=function(w) {
      warn <<- w
      invokeRestart("muffleWarning")
    })
  list(value=value, warning=warn, error=err)
}
