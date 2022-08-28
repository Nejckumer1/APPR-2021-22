shinyUI(fluidPage(
  titlePanel(""),
  sidebarLayout(position = "left",
                sidebarPanel(
                  radioButtons(
                    "razred", 
                    label = "Razred:",
                    choices = list("1" = "1", 
                                   "2" = "2",
                                   "3" = "3",
                                   "4" = "4",
                                   "5" = "5",
                                   "6" = "6",
                                   "7" = "7",
                                   "8" = "8",
                                   "9" = "9"),
                    selected = 9),
                  selectInput(
                    "disciplina",
                    label = "Disciplina:",
                    choices = c(
                      "visina",
                      "teza",
                      "kozna_guba",
                      "dotikanje_plosce_z_roko",
                      "skos_z_mesta",
                      "premagovanje_ovir_nazaj",
                      "trebusnjaki",
                      "predklon_naprej",
                      "vesa_v_zgibi",
                      "tek_na_60m"
                    ),
                    selected = "teza"
                  ))
                ,
                mainPanel(plotOutput("graf"))),
  uiOutput("izborTabPanel")))




