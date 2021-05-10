#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  
  exp_sh2 <- economia::exp_sh2
  media_movel <- exp_sh2 %>%
    dplyr::mutate(fator = dplyr::case_when(codigo_sh2 > 15 & codigo_sh2 < 68 ~ "Produto Industrializado",
                                           codigo_sh2 > 71 & codigo_sh2 < 90 ~ "Produto Industrializado",
                                           TRUE ~ "Outro")) %>%
    dplyr::group_by(ano_mes) %>%
    dplyr::mutate(exp_totais = sum(value)) %>%
    dplyr::group_by(fator, ano_mes, exp_totais) %>% 
    dplyr::filter(fator != "Outro") %>% 
    dplyr::summarise(exp_industria = sum(value)) %>%
    dplyr::ungroup() %>% 
    dplyr::mutate(media_movel_total= data.table::frollapply(exp_totais, 12, sum, align = "right", fill = NA)) %>%
    dplyr::mutate(media_movel_industria = data.table::frollapply(exp_industria, 12, sum, align = "right", fill = NA))
  
  output$plot1 <- shiny::renderPlot({
    media_movel %>%
      tidyr::pivot_longer(media_movel_total:media_movel_industria, names_to = "media", values_to = "valor") %>% 
      ggplot2::ggplot() +
      ggplot2::geom_line(ggplot2::aes(ano_mes, valor, color = media)) +
      ggplot2::scale_y_continuous(labels = scales::label_number_si()) +
      ggplot2::scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
      ggplot2::labs(title = "Exportações Totais e Exportações de Bens Industriais",
                    subtitle = "Soma do acumulado de 12 meses",
                    caption = "Formulação: MRE, Fonte: Ministério da Economia",
                    x = NULL,
                    y = NULL,
                    color = "Média") +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "bottom") +
      ggthemes::scale_color_tableau(labels = c("Indústria", "Total"))})
  
  output$plot2 <- shiny::renderPlot({
    media_movel %>% 
      dplyr::mutate(diferenca = media_movel_industria/media_movel_total) %>%
      ggplot2::ggplot() +
      ggplot2::geom_line(ggplot2::aes(ano_mes, diferenca, color = diferenca), show.legend = F) +
      ggplot2::labs(title = "Parcela de Bens Industriais nas Exportações Totais",
           subtitle = "Acumulado 12 meses",
           caption = "Formulação: MRE, Fonte: Ministério da Economia",
           x = NULL,
           y = NULL) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "bottom") +
      ggthemes::scale_color_gradient_tableau() +
      ggplot2::scale_y_continuous(labels = scales::percent_format()) +
      ggplot2::scale_x_date(date_breaks = "2 year", date_labels = "%Y")
  })
    
}

