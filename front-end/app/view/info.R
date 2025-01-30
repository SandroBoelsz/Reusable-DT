box::use(
  shiny[NS, fillPage, div, p, moduleServer, tags, h1, fluidRow, observeEvent, actionLink],
  bslib[card, card_header, card_body, bs_theme, nav_select],
)

box::use(app / view / honeybee / honeybee_main[honeybee_ui, honeybee_server], )


#' @export
mod_info_ui <- function(id, i18n) {
  ns <- NS(id)
  fluidRow(
    class = "index-info",
    tags$div(
      class = "col mx-auto col-lg-8",
      tags$h1(
        tags$span(i18n$translate("Prototype"),
          class = "text-primary",
          id = ns("heading-first-part")
        ),
        tags$span(i18n$translate("Digital Twins"),
          id = ns("heading-second-part")
        ),
        class = "justify-content-center",
      ),
      # THIS DESIGN IS STORAGED HERE FOR THE TIME BEING, problems shown in issue #56 on Github
      tags$div(
        class = "info-text justify-content-center",
        tags$p(
          i18n$translate(
            "The Biodiversity Digital Twin prototype will provide advanced models for simulation and prediction capabilities, through practical use cases addressing critical issues related to global biodiversity dynamics."
          )
        ),
        tags$p(
          i18n$translate(
            "BioDT exploits the LUMI Supercomputer and employs FAIR data combined with digital infrastructure, predictive modelling and AI solutions, facilitating evidence-based solutions for biodiversity protection and restoration."
          )
        ),
        tags$p(
          i18n$translate(
            "The project responds to key EU and international policy initiatives, including the EU Biodiversity Strategy 2030, EU Green Deal, UN Sustainable Development Goals, Destination Earth."
          )
        ),
      ),
      tags$div(
        class = "row gap-3 justify-content-center",
        style = "margin-top: 3em;",
        tags$div(
          class = "landing-pdt-wrap col-sm-5",
          tags$div(
            class = "title",
            tags$h2(
              i18n$translate("Species interactions with each other and with humans")
            )
          ),
          tags$div(
            class = "description",
            tags$div(
              class = "img-box",
              tags$img(
                src = "static/img/usecase4.png",
                width = 140,
                height = 140,
                loading = "lazy",
                `aria-hidden` = "true",
                alt = ""
              )
            ),
            tags$div(
              class = "views-element-container",
              tags$ul(
                tags$li(
                  actionLink(
                    class = "w-100",
                    inputId = ns("honeybee_selector"),
                    label = i18n$translate("Pollinators (Honeybee)")
                  )
                ),
                tags$li(
                  class = "w-100",
                  i18n$translate("Disease Outbreaks")
                )
              )
            )
          )
        )
      )
    )
  )
}

#' @export
mod_info_server <- function(id, r, main_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(input$honeybee_selector, {
      print("actionlink selected")
      nav_select("navbar",
        selected = "Honeybee",
        session = main_session
      )
      nav_select("honeybee_main-tab",
        selected = "Beekeeper",
        session = main_session
      )
    })
  })
}
