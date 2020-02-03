#library(shiny)
#library(miniUI)
source("R/utils.R")
#library(glue)
#library(papaja)


##' Interactive t-test
##'
##' This function launches a shiny app in a web browser in order to do
##' interactive t-test
##'
##' @param obj data frame to operate on
##' @param ivar_name name of the column for independent variable
##' @param dvar_name name of the column for dependent variable
##' @return
##' The function launches a shiny app in the system web browser. The recoding code is returned in the console
##' when the app is closed with the "Done" button.
##' @author André Calero Valdez <andrecalerovaldez@@gmail.com>
##' @examples
##' \dontrun{data(ToothGrowth)
##' sssttest(ToothGrowth)
##' }
##' @import shiny
##' @import rstudioapi
##' @import miniUI
##' @importFrom highr hi_html
## @importFrom htmltools htmlEscape
##' @export
sssttest <- function(obj=NULL, ivar_name = NULL, dvar_name = NULL) {

  run_as_addin <- ifunc_run_as_addin()

  # is a param given??
  if (is.null(obj)) {
    if (run_as_addin) {
      context <- rstudioapi::getActiveDocumentContext()
      obj <- context$selection[[1]]$text
      if (obj == "") obj <- NULL
    }
    obj_name <- NULL
    ivar_name <- NULL
  }

  if (!is.null(obj)) {
    ## If first arg is a string
    if (is.character(obj) && length(obj) == 1) {
      obj_name <- obj
      try({
        obj <- get(obj_name, envir = sys.parent())
      }, silent = TRUE)
    }
    else {
      obj_name <- deparse(substitute(obj))
    }
    ## If first arg is of the form d$x
    if (inherits(obj, "tbl_df") || inherits(obj, "data.table")) obj <- as.data.frame(obj)

    ## Check if obj is a data frame or a vector
    if (!is.data.frame(obj)) {
      stop(sQuote(paste0(obj_name, ' must be a data frame.')))
    }
  }


  ui <- miniUI::miniPage(

    ## Page title
    miniUI::gadgetTitleBar(gettext("Interactive independent sample t-Test", domain="ssssplugin")),
    ## Custom CSS
    tags$style(ifunc_get_css()),
    #tags$style(type='text/css', '#txt_out {white-space: pre-wrap;}'),
    miniUI::miniTabstripPanel(
      miniUI::miniTabPanel(
        value = "settings",
        gettext("Data-frame and variables", domain="ssssplugin"), icon = icon("sliders"),
        miniUI::miniContentPanel(

          ifunc_show_alert(run_as_addin),

          ## First panel : new variable name and recoding style ----
          tags$h4(icon("columns"), gettext("Data-frame and variables", domain="ssssplugin")),
          wellPanel(
            fluidRow(
              column(12,
                     selectizeInput(
                       "obj_name",
                       gettext("Select data frame", domain="ssssplugin"),
                       choices = Filter(
                         function(x) {
                           inherits(get(x, envir = sys.parent()), "data.frame")
                         }, ls(.GlobalEnv)),
                       selected = obj_name, multiple = FALSE)),
              column(6, uiOutput("ivarInput")),
              column(6, uiOutput("dvarInput")))),
          uiOutput("nblevelsAlert"),
          tags$h4(icon("sliders"), gettext("Test settings", domain="ssssplugin")),
          wellPanel(
            fluidRow(
              column(6, selectInput("alternative", "Alternative",
                                    c(`two-sided test` = "two.sided",
                                      `Level 1 is less than level 2` = "less",
                                      `Level 1 is greater than level 2` = "greater"))),
              column(6, selectInput("conf", "Confidence level", c(0.95, 0.99, 0.999))),
              # add further selctizeInputs here
              column(4, uiOutput("newvarInput"))
            )),
          uiOutput("ivNotTwoLevels"),
          uiOutput("loadedforcatsAlert"),
          uiOutput("loadeddplyrAlert")
        )),

      ## Second panel : recoding fields, dynamically generated ----
      miniUI::miniTabPanel(
        value = "Preview",
        gettext("Preview"), icon = icon("wrench"),
        miniUI::miniContentPanel(
          wellPanel(fluidRow(
            column(12, verbatimTextOutput("preview")),
            column(6, textOutput("msgs")))
          ))),
      ## Third panel : generated code and results checking ----
      miniUI::miniTabPanel(
        value = "code",
        gettext("Code and result"), icon = icon("code"),
        miniUI::miniContentPanel(
          tags$h4(icon("code"), gettext("Code", domain="ssssplugin")),
          htmlOutput("testOut")))
    )
  )

  # Server ----

  server <- function(input, output, session) {
    if (!is.null(obj_name)) {
      updateSelectizeInput(session, "obj_name", selected = obj_name)
    }


    robj <- reactive({
      obj <- get(req(input$obj_name %||% obj_name), envir = .GlobalEnv)
      if (inherits(obj, "tbl_df") || inherits(obj, "data.table")) obj <- as.data.frame(obj)
      obj
    })

    rfactors <- reactive({
      d <- robj()
      a <- names(Filter(is.factor, d))
      b <- names(Filter(is.character, d))
      c(a,b)
    })

    rnumerics <- reactive({
      d <- robj()
      names(Filter(is.numeric, d))
    })


    # rcall generator ----
    rcall <- reactive({
      iv <- input$ivar_name
      dv <- input$dvar_name
      ob <- input$obj_name
      conf_level <- input$conf
      alter <- input$alternative
      # construct function call
      glue::glue("t.test({dv} ~ {iv}, data = {ob}, conf.level = {conf_level}, alternative = \"{alter}\")")
    })

    # ivar selecter ----
    output$ivarInput <- renderUI({
      if (is.data.frame(robj())) {
        selectizeInput("ivar_name",
                       gettext("Independent variable", domain="sssplugin"),
                       choices = rfactors(),
                       selected = ivar_name,
                       multiple = FALSE)
      }
    })

    # dvar selector ----
    output$dvarInput <- renderUI({
      if (is.data.frame(robj())) {

        selectizeInput("dvar_name",
                       gettext("Dependent variable", domain="sssplugin"),
                       choices = rnumerics(),
                       selected = dvar_name,
                       multiple = FALSE)
      }
    })

    # Warning messages ----
    output$ivNotTwoLevels <- renderUI({
      req(input$ivar_name)
      req(input$obj_name)
      iv <- input$ivar_name
      ob <- input$obj_name
      d <- paste0("length(levels(factor(",ob,"$",iv,")))")

      x <- eval(parse(text = d))
      if(x != 2){
        return(div(class = "alert alert-warning alert-dismissible",
                   shiny::HTML('<button type="button" class="close" data-dismiss="alert" aria-label="Close"><span aria-hidden="true">&times;</span></button>'),
                   shiny::HTML(gettext("<strong>Warning :</strong> The independent variable must have exactly 2 levels.", domain="sssplugin"))))
      } else {
        #rcall()
      }

    })



    # Preview render ----
    output$preview <- renderText({

      d <- rcall()

      # try to evaluate the call
      res <- myTryCatch(eval(parse(text = d)))

      # if errors capture
      if(!is.null(res$error)){
        msg <- as.character(res$error)
        op <- glue::glue("This produces an Error:\n{msg}")
        return(op )
      }


      # get all the output to render in verbatim
      oput <- paste(capture.output(res$value
        #t.test(len ~ supp, data = ToothGrowth)
        ),collapse = "\n")
      glue::glue("> ",d,"\nOutput:\n",oput)
    })

    generate_code <- reactive({
      rcall()
    })

    output$testOut <- renderText({
      ## Header
      if (is.data.frame(robj())) {
        header <- shiny::HTML(gettextf("<p class='header'>Running t-Test on <tt>%s</tt> with IV <tt>%s</tt> and DV <tt>%s</tt>.</p>",
                                req(input$obj_name), req(input$ivar_name), req(input$dvar_name), domain = "R-questionr"))
      }
      ## Generate code
      out <- generate_code()
      out <- styler::style_text(out)
      ## Generated code syntax highlighting
      out <- paste(highr::hi_html(out), collapse = "\n")
      ## Final paste
      out <- paste0(header, "<pre class='r'><code class='r' id='codeout'>",out,"</code></pre>")
      out
    })


    # Handle the Cancel button being pressed.
    observeEvent(input$cancel, {
      invisible(stopApp())
    })

    # Handle the Done button being pressed.
    observeEvent(input$done, {
      ## Generate code
      out <- generate_code()
      out <- styler::style_text(out)
      out <- paste(out, collapse = "\n")
      if (run_as_addin) {
        rstudioapi::insertText(text = out)
      } else {
        out <- paste0(gettext("\n-------- Start t-Test code --------\n\n", domain="sssplugin"),
                      out,
                      gettext("\n--------- End t-Test code ---------\n", domain="sssplugin"))
        cat(out)
      }
      stopApp()
    })
  }



  runGadget(ui, server, viewer = dialogViewer("sssplugin - t-test", height = 800, width = 800))
}

# TESTING ---
if(F) {
  df <- ToothGrowth
  df2 <- dataforsocialscience::robo_care
  library(dplyr)
  df2$gender_rec <- recode(df2$gender, "rather not say" = "male")
  df2$gender_rec <- as.character(df2$gender_rec)

  #sssttest()
}

