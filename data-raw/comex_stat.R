ultimo_ano <- barao::comerciobr_get_ulimoano()

url <- paste0("http://api.comexstat.mdic.gov.br/general?filter=%7B%22yearStart%22:%221997%22,%22yearEnd%22:%22", ultimo_ano,"%22,%22typeForm%22:1,%22typeOrder%22:2,%22filterList%22:%5B%5D,%22filterArray%22:%5B%5D,%22rangeFilter%22:%5B%5D,%22detailDatabase%22:%5B%7B%22id%22:%22noSh2pt%22,%22text%22:%22Cap%C3%ADtulo%20(SH2)%22,%22parentId%22:%22coSh2%22,%22parent%22:%22Codigo%20SH2%22%7D%5D,%22monthDetail%22:true,%22metricFOB%22:true,%22metricKG%22:false,%22metricStatistic%22:false,%22monthStart%22:%2201%22,%22monthEnd%22:%2212%22,%22formQueue%22:%22general%22,%22langDefault%22:%22pt%22,%22monthStartName%22:%22Janeiro%22,%22monthEndName%22:%22Dezembro%22%7D")

exp <- httr::GET(url)

exp_sh2 <- httr::content(exp, as = "text") %>% 
  jsonlite::fromJSON(simplifyDataFrame = T) %>% 
  purrr::pluck(1) %>% 
  as.data.frame() %>% 
  tibble::as_tibble() %>% 
  dplyr::rename(ano = `list.coAno`,
                mes = list.coMes,
                codigo_sh2 = list.coSh2,
                sh2_desc = list.noSh2pt,
                value = list.vlFob) %>% 
  dplyr::mutate(value = as.numeric(value)) %>% 
  tidyr::unite("ano_mes", ano:mes) %>% 
  dplyr::mutate(ano_mes = paste0(ano_mes, "_01")) %>% 
  dplyr::mutate(ano_mes = lubridate::as_date(ano_mes))


# media_movel %>% 
#   View()


usethis::use_data(exp_sh2, overwrite = T)  
  