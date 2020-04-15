library(tidyverse)



######################################################
# tidy university/discipline data from 2010-2019
read_plus <- function(flnm) {
  readr::read_csv(flnm,
    col_types = list(
      学科 = col_character(),
      名称 = col_character(),
      排名 = col_double(),
      出版年 = col_double(),
      `Web of Science 论文数` = col_double(),
      学科规范化的引文影响力 = col_double(),
      被引频次 = col_double(),
      论文被引百分比 = col_double()
    )
  ) %>%
    dplyr::rename(
      discipline = `学科`,
      univ = `名称`,
      rank = `排名`,
      year = `出版年`,
      n_paper = `Web of Science 论文数`,
      impact = `学科规范化的引文影响力`,
      n_cited = `被引频次`,
      percent_cited = `论文被引百分比`
    ) %>%
    dplyr::filter(!is.na(discipline)) %>%
    dplyr::mutate(
      school = flnm %>% stringr::str_extract(., "(?<=UnivTimeSerial/).*?(?=\\.csv)")
    ) %>%
    dplyr::select(school, discipline, year, n_paper, n_cited)  
}


univ_discip_timeserial <- 
  here::here("data", "UnivTimeSerial") %>%
  fs::dir_ls(regexp = "*.csv", recurse = FALSE) %>%
  purrr::map_dfr(~ read_plus(.)) %>%
  dplyr::mutate(
    discipline = dplyr::recode(discipline,
      `Mathematics`             = "数学",
      `Physics`                 = "物理学",
      `Chemistry`               = "化学",
      `Engineering`             = "工程学",
      `Computer Science`        = "计算机科学",
      `Materials Science`       = "材料科学",
      `Economics & Business`    = "经济与商业",
      `Psychiatry/Psychology`   = "精神病学与心理学"
    )
  )
univ_discip_timeserial


univ_discip_timeserial %>% 
	filter(	discipline != "ALL") %>% 
	filter(stringr::str_detect(discipline, "^A"))

######################################################



######################################################
# univ discipline development data.frame summary from last ten years
# 2010-2020 
univ_discip_summary <- 
	readr::read_csv(here::here("data", "UnivSummary", "university_summary.xlsx"))
univ_discip_summary
######################################################






######################################################
# tidy newest ESI Threshold
esi22 <- readxl::read_excel(here::here("data", "ESI", "ESI_22.xlsx")) %>% 
	dplyr::mutate(discipline = stringr::str_to_title(discipline))
esi22


Threshold <- 
	readxl::read_excel(here::here("data", "ESI", "ThresholdESI0326.xlsx"), 
					   skip = 2, n_max = 22) %>% 
	janitor::clean_names() %>% 
	dplyr::select(discipline = research_fields, Threshold0326 = institution) %>% 
	dplyr::mutate(discipline = stringr::str_to_title(discipline))
Threshold



ThresholdESI <- left_join(esi22, Threshold, by = "discipline")
ThresholdESI
######################################################




######################################################
# save to Rdata
save(univ_discip_timeserial, ThresholdESI, #univ_discip_summary, 
	 file = "myData.Rdata")

#load("myData.Rdata")
######################################################


