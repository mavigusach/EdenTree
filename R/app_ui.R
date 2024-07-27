#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    shinyjs::useShinyjs(),
    # Your application UI logic
    bs4Dash::bs4DashPage(sidebar = ,dark = NULL,fullscreen = TRUE,help = NULL,scrollToTop = TRUE, footer = bs4Dash::bs4DashFooter(right =
                                                                                                             div(
                                                                                                               fluidRow(
                                                                                                                 textOutput("render_Rodape_Direitos_Ano")),
                                                                                                               fluidRow(
                                                                                                                 uiOutput("render_Rodape_Versao_Codename")
                                                                                                               )
                                                                                                             )

    ,
    left = div(
      div(
        uiOutput("render_Rodape_Idioma"),
      ),
      div(
        uiOutput("render_Rodape_Tutorial")
      ),
      div(
        uiOutput("render_Rodape_Sobre_Nos")
      )
    )
    ),
    bs4Dash::bs4DashNavbar(titlePanel(title = actionLink(inputId="refresh_home",div(icon("apple-whole",lib = "font-awesome"),"EdenTree"))),skin="light"),
    bs4Dash::bs4DashSidebar(skin = "light",status = "primary",disable = TRUE),

    bs4Dash::bs4DashBody(

      fresh::use_theme(fresh::create_theme(
        fresh::bs4dash_status(
          primary = "#850000"

        ))),

    shinycssloaders::withSpinner(uiOutput("render_pagina"),type=2,color = "#850000",color.background = "white")
    )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )
  #Link CSS
  tags$link(rel = "stylesheet", type="text/css", href="www/CSS/folhadeestilo_edentree.css")

  tags$head(
    golem::favicon(ico="eden_tree"),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "EdenTree"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
