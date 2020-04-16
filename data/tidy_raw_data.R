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
      `Mathematics` = "数学",
      `Physics` = "物理学",
      `Chemistry` = "化学",
      `Engineering` = "工程学",
      `Computer Science` = "计算机科学",
      `Materials Science` = "材料科学",
      `Economics & Business` = "经济与商业",
      `Psychiatry/Psychology` = "精神病学与心理学"
    )
  )
univ_discip_timeserial


univ_discip_timeserial %>%
  filter(discipline != "ALL") %>%
  filter(stringr::str_detect(discipline, "^A"))

univ_discip_timeserial %>% count(school, discipline)

univ_discip_timeserial %>%
  filter(school == "北京师范大学") %>%
  count(discipline)

# here::here("data", "UnivTimeSerial", "北京师范大学.csv") %>% read_csv()
######################################################







######################################################
# tidy newest ESI Threshold
esi22 <- readxl::read_excel(here::here("data", "ESI", "ESI_22.xlsx")) %>%
  dplyr::mutate(discipline = stringr::str_to_title(discipline))
esi22

esi22 %>%
	is_within_discipline(discipline) %>% 
	filter(!is_within_discipline)


Threshold <-
  readxl::read_excel(here::here("data", "ESI", "ThresholdESI0326.xlsx"),
    skip = 2, n_max = 22
  ) %>%
  janitor::clean_names() %>%
  dplyr::select(discipline = research_fields, Threshold0326 = institution) %>%
  dplyr::mutate(discipline = stringr::str_to_title(discipline))
Threshold



ThresholdESI <- left_join(esi22, Threshold, by = "discipline")
ThresholdESI

ThresholdESI %>%
    is_within_discipline(discipline) %>% 
	filter(!is_within_discipline)
######################################################






######################################################
# univ discipline development data.frame summary from last ten years
# 2010-2020
univ_discip_df <-
  readxl::read_excel(here::here("data", "UnivSummary", "university_summary.xlsx")) %>%
  dplyr::filter(!is.na(school)) %>%
  dplyr::mutate(discipline = stringr::str_to_title(discipline))
univ_discip_df

univ_discip_df %>% 
	is_within_discipline(discipline) %>% 
	filter(!is_within_discipline)



univ_discip_stat <- univ_discip_df %>%
  dplyr::left_join(
    ThresholdESI %>% select(discipline, discipline_cn, Threshold0326),
    by = "discipline"
  ) %>%
  dplyr::select(school, univ, discipline_cn, discipline, cum_paper, cum_cited, Threshold0326)

univ_discip_stat
univ_discip_stat %>% dplyr::filter(is.na(discipline_cn)) # have 5 NA


univ_discip_stat %>% 
	is_within_discipline(discipline) %>% 
	filter(!is_within_discipline)
######################################################







######################################################
# tidy top one percent
read_top <- function(flnm) {
  readxl::read_excel(flnm, skip = 5) %>%
    janitor::clean_names() %>%
    dplyr::filter(!is.na(institutions)) %>%
    filter(stringr::str_detect(countries_regions, "CHINA")) %>%
    filter(stringr::str_detect(institutions, "NORMAL")) %>%
    dplyr::mutate(
      discipline = flnm %>% stringr::str_extract(., "(?<=TopOnePercent/).*?(?=\\.xlsx)")
    ) %>%
    mutate(is_enter_one_percent = TRUE) %>%
    mutate_at(vars(institutions), ~ stringr::str_extract(., ".*?UNIVERSITY")) %>%
    mutate_at(vars(institutions), ~ stringr::str_to_title(.)) %>%
    select(discipline,
      univ = institutions, web_of_science_documents,
      cites, top_papers, is_enter_one_percent
    )
}


top_one_percent <-
  here::here("data", "TopOnePercent") %>%
  fs::dir_ls(regexp = "*.xlsx", recurse = FALSE) %>%
  purrr::map_dfr(~ read_top(.)) %>%
  dplyr::mutate(
    discipline = dplyr::recode(discipline,
      `Environment_Ecology` = "Environment/Ecology",
      `Psychiatry_Psychology` = "Psychiatry/Psychology",
      `Social Sciences, general` = "Social Sciences, General"
    )
  )

top_one_percent
top_one_percent %>% 
	count(univ) %>% 
	arrange(-n)

top_one_percent %>%
    is_within_discipline(discipline) %>% 
	filter(!is_within_discipline)


univ_discip_summary <- univ_discip_stat %>%
  dplyr::left_join(top_one_percent, by = c("discipline", "univ"))
univ_discip_summary


univ_discip_summary %>% 
	group_by(school) %>% 
	summarise( 
		num = sum(is_enter_one_percent, na.rm = T)
	) %>% 
	arrange(desc(num))

######################################################










######################################################
# save to Rdata
save(univ_discip_timeserial, ThresholdESI, univ_discip_summary,
  file = "myData.Rdata"
)

# load("myData.Rdata")
######################################################





######################################################
# backup
univ_discip_stat %>%
  distinct(univ) %>%
  dplyr::mutate(univ = stringr::str_to_title(univ))
######################################################
