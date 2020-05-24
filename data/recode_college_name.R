recode_college_name <- function(var) {
  
  library(dplyr)
  
  collname <- readxl::read_excel(
     here::here("data", "sicnu_college_name", "sicnu_coll_name_en2cn.xlsx")
     ) #%>% 
    #janitor::get_dupes(coll)
  
  vector_key <- collname %>% tibble::deframe()
  
  dplyr::recode(var, !!!vector_key, .default = NA_character_)
}


