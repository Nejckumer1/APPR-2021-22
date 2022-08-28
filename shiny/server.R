library(shiny)

#sportni_kart = read_csv("podatki/sportni_karton_urejen")

shinyServer(
  function(input, output)
  {
    output$graf <- renderPlot({
      narisi.graf(input$razred, input$disciplina)
    })
  }
)

narisi.graf <- function(razredF, disciplinaF)
{# pripravim tabelo:
  tabela_shiny =  sportni_kart %>% filter(razred == razredF,vrsta == "povprecje", leto != "vsa_leta") %>% 
    pivot_longer(
      cols = c(
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
      names_to = "disciplina",
      values_to = "vrednosti"
    ) %>%
    filter(disciplina == disciplinaF) %>%
    ggplot(
      mapping = aes(x = leto, y = vrednosti, color = spol, group = spol)
    ) +
    geom_point()+
    geom_line() +
    theme_bw()+
    theme(
      axis.text.x = element_text(vjust = 0.5),
      axis.title.x = element_text(vjust = 0)
    ) + 
    labs(
      title = "Speminjanje rezultatov skozi leta za posamezen razred",
      subtitle = sprintf("Disciplina: %s   Razred: %s", disciplinaF, razredF),
      x = "Leto",
      y = disciplinaF,
      color = "Spol"
    )+
    scale_color_manual(name = 'Spol', 
                       values = c('fantje' = 'aquamarine2','punce' = 'coral1'), labels = c('fantje','punce'))
  
  print(tabela_shiny)
}

