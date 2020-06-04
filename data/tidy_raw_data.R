library(tidyverse)
library(tidyESI)



######################################################
Normal_University_Top30 <- 
	readr::read_csv(here::here("data", "UniversityName", "schoolname.csv"))
Normal_University_Top30


Normal_University_Top30 %>% 
	tidyESI::add_univ_name_cn(univ)
######################################################




######################################################
# ESI22
esi22 <- readxl::read_excel(here::here("data", "ThresholdESI", "ESI_22.xlsx")) %>%
	dplyr::mutate(discipline = stringr::str_to_title(discipline))
esi22


t <- esi22 %>% tidyESI::add_esi_threshold(discipline, date = "last")
t
######################################################







######################################################
# tidy university30/discipline22/TimeSerial21 data from 2000-2020last
read_plus <- function(flnm) {
  readr::read_csv(flnm) %>%
    dplyr::select(
      discipline = "名称",
      year = "出版年",
      n_paper = "Web of Science 论文数",
      n_cited = "被引频次"
    ) %>%
    dplyr::filter(!is.na(year)) %>%
	dplyr::mutate(discipline = stringr::str_to_title(discipline)) %>%
    dplyr::mutate(
      univ = flnm %>% stringr::str_extract(., "(?<=UnivDisciplineTimeSerial/).*?(?=\\.csv)"))
}

univ_discip_timeserial <-
  here::here("data", "UnivDisciplineTimeSerial") %>%
  fs::dir_ls(regexp = "*.csv", recurse = FALSE) %>%
  purrr::map_dfr(~ read_plus(.)) %>%
  dplyr::select(univ, discipline, year, n_paper, n_cited)


univ_discip_timeserial
univ_discip_timeserial %>% count(univ)

colnames(univ_discip_timeserial)


univ_discip_timeserial %>% 
	tidyESI::add_discipline_cn(discipline) %>% 
	tidyESI::add_univ_name_cn(univ)

##########################################################################







##########################################################################
# tidy top one percent
read_top <- function(flnm) {
  readxl::read_excel(flnm, skip = 5) %>%
    janitor::clean_names() %>%
    dplyr::filter(!is.na(research_fields)) %>%
    dplyr::filter(research_fields != "ALL FIELDS") %>%
    dplyr::mutate(discipline = stringr::str_to_title(research_fields)) %>%
    dplyr::mutate(is_enter_one_percent = TRUE) %>%
    dplyr::mutate(
      univ = flnm %>% stringr::str_extract(., "(?<=TopOnePercent/).*?(?=\\.xlsx)")
    ) %>%
    select(
      univ, discipline,
      web_of_science_documents,
      cites, top_papers, is_enter_one_percent
    )
}


top_one_percent <-
  here::here("data", "TopOnePercent") %>%
  fs::dir_ls(regexp = "*.xlsx", recurse = FALSE) %>%
  purrr::map_dfr(~ read_top(.))

top_one_percent
top_one_percent %>% 
    tidyESI::add_discipline_cn(discipline) %>% 
	tidyESI::add_univ_name_cn(univ)


t <- top_one_percent %>%
  dplyr::count(discipline) %>%
  dplyr::arrange(-n) %>%
  tidyESI::add_discipline_cn(discipline) 

t
t %>% tidyESI::add_esi_threshold(discipline)
##########################################################################





##########################################################################
read_wos <- function(flnm) {
    read_tsv(flnm, quote = "", col_names = TRUE) %>% 
    select(AU, AF, SO, DE, C1, RP, FU, CR, TC, SN, PY, UT) 
}


tbl <- here::here("data", "wos", "sichuan_normal_univ") %>%
  fs::dir_ls(regexp = "*.txt", recurse = FALSE) %>%
  purrr::map_dfr(~ read_wos(.)) 
tbl

t <- tbl %>% 
  dplyr::select(C1, TC, SN) %>% 
  dplyr::mutate(discipline = tidyESI::map_issn_to_esi_discipline(SN)) %>% 
  rowwise() %>% 
  dplyr::mutate(coll = stringr::str_extract_all(C1, "Sichuan Normal Univ,\\s+([^,]*),") 
  ) %>% 
  tidyr::unnest(coll) %>% 
  dplyr::ungroup()


sicnu_contribution_by_college <- t %>% 
  dplyr::select(discipline, TC, coll) %>% 
  dplyr::mutate(
    across(coll, stringr::str_squish)
  ) %>%
  dplyr::mutate(coll_name_cn = tidyESI::map_addr_to_college_name(coll)) %>% 
  dplyr::filter(!is.na(coll_name_cn), !is.na(discipline)) %>% 
  dplyr::group_by(discipline, coll_name_cn) %>% 
  dplyr::summarise(
    n_paper = n(),
    n_cited = sum(TC),
    .groups = "drop"
  ) %>% 
  dplyr::arrange(-n_cited)  %>% 
  tidyESI::add_discipline_cn(discipline) %>% 
  dplyr::relocate(discipline_cn)
##########################################################################





##########################################################################
highcited_timeserial <- 
	here::here("data","top_paper", "top_paper.xlsx") %>%
	readxl::read_excel(., skip = 1) %>%
	janitor::clean_names() %>%
	dplyr::filter(stringr::str_detect(accession_number, "^WOS")) %>% 
	dplyr::mutate(discipline = stringr::str_to_title(research_field)) %>%
	dplyr::mutate(times_cited = as.numeric(times_cited)) %>%
	dplyr::select(
		discipline, 
		year = publication_date,
		times_cited
	)  %>%  
	dplyr::group_by(discipline, year) %>% 
	dplyr::summarise(
		n_paper_high = n(),
		n_cited_high = sum(times_cited),
		.groups = "drop"
	) 
highcited_timeserial



##########################################################################











######################################################
# wrangle for save
# 1)  school / univ
Normal_University_Top30


# 2) category / discipline_mix / discipline_cn /  discipline / Threshold0326
ESI_and_Threshold <- esi22 %>% tidyESI::add_esi_threshold(discipline)


# 3) univ / discipline / year / n_paper / n_cited
univ_discip_timeserial_2000_to_2019 <- 
	univ_discip_timeserial %>% 
	dplyr::filter(between(year, 2000, 2019)) %>% 
	tidyESI::add_discipline_cn(discipline) %>% 
	tidyESI::add_univ_name_cn(univ)



univ_discip_timeserial_2010_to_2019 <- 
	univ_discip_timeserial %>% 
	dplyr::filter(between(year, 2010, 2019)) %>% 
	tidyESI::add_discipline_cn(discipline) %>% 
	tidyESI::add_univ_name_cn(univ)


univ_discip_timeserial_2010_to_2020 <- 
	univ_discip_timeserial %>% 
	dplyr::filter(between(year, 2010, 2020)) %>% 
	tidyESI::add_discipline_cn(discipline) %>% 
	tidyESI::add_univ_name_cn(univ)



# 4) univ / discipline / is_enter_one_percent
univ_discip_enter_top_one_percent <- top_one_percent %>% 
	tidyESI::add_discipline_cn(discipline) %>% 
	tidyESI::add_univ_name_cn(univ)



# 5) univ / discipline / year / n_paper / n_cited
univ_discip_cum_last_ten_year <- univ_discip_timeserial %>%
	dplyr::filter(between(year, 2010, 2020)) %>% # last ten year
	dplyr::group_by(univ, discipline) %>%
	dplyr::summarise(
		cum_paper = sum(n_paper),
		cum_cited = sum(n_cited)
	) %>%
	dplyr::ungroup() %>% 
	dplyr::left_join(top_one_percent, by = c("discipline", "univ")) %>% 
	tidyESI::add_esi_threshold(discipline) %>% 
	tidyESI::add_discipline_cn(discipline) %>% 
	tidyESI::add_univ_name_cn(univ)


# 6) sichuan normal univ: (college) / (discipline) / (n_paper) / (n_cited)
sicnu_contribution_by_college


# 7) sichuan normal univ: highcited_timeserial
highcited_timeserial



# save to Rdata
save(Normal_University_Top30,
	 ESI_and_Threshold,
	 univ_discip_timeserial_2000_to_2019, 
	 univ_discip_timeserial_2010_to_2019, 
	 univ_discip_timeserial_2010_to_2020,
	  
	 univ_discip_enter_top_one_percent,
	 univ_discip_cum_last_ten_year,
	 
	 sicnu_contribution_by_college,
	 
	 highcited_timeserial,
	 file = "myData.Rdata"
)

# load("myData.Rdata")
######################################################
