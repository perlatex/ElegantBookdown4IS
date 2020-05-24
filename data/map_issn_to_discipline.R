map_issn_to_discipline <- function(var) {
  
  library(dplyr)
  
  esilist <- readxl::read_excel(
    here::here("data", "esi_jcr_list", "esi-master-journal-list-5-2020.xlsx") 
    )

  tb <- esilist %>% 
    dplyr::select(ISSN, discipline = `Category name`) %>% 
    dplyr::mutate(discipline = stringr::str_to_title(discipline)) %>%
    #dplyr::filter(!is.na(ISSN)) %>% 
    dplyr::filter(stringr::str_detect(ISSN, "\\d{4}-\\d{4}")) #%>% 
    #janitor::get_dupes(ISSN)
  
  vector_key <- tb %>% tibble::deframe()
  
  dplyr::recode(var, !!!vector_key, .default = NA_character_)
}


