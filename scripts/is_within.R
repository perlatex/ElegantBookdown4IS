is_within_univ <- function(df, var) {
	library(dplyr)
	
	alist <- 
		c("Beijing Normal University", "East China Normal University", "Central China Normal University", "Nanjing Normal University", "Hunan Normal University", "Northeast Normal University", "South China Normal University", "Shaanxi Normal University", "Capital Normal University", "Zhejiang Normal University", "Shandong Normal University", "Tianjin Normal University", "Fujian Normal University", "Henan Normal University", "Jiangxi Normal University", "Shanghai Normal University", "Anhui Normal University", "Northwest Normal University", "Guangxi Normal University", "Hangzhou Normal University", "Yunnan Normal University", "Harbin Normal University", "Hebei Normal University", "Jiangsu Normal University", "Sichuan Normal University", "Liaoning Normal University", "Chongqing Normal University", "Qufu Normal University", "Guizhou Normal University", "Hainan Normal University")
	
	
	df %>% mutate(
		is_within_univ = if_else({{var}} %in% alist, TRUE, FALSE)
	)
}


is_within_discipline <- function(df, var) {
	library(dplyr)
	
	alist <- c("Computer Science", 
			   "Engineering", 
			   "Materials Science", 
			   "Biology & Biochemistry", 
			   "Environment/Ecology", 
			   "Microbiology", 
			   "Molecular Biology & Genetics", 
			   "Social Sciences, General", 
			   "Economics & Business", 
			   "Chemistry", 
			   "Geosciences", 
			   "Mathematics", 
			   "Physics", 
			   "Space Science", 
			   "Agricultural Sciences", 
			   "Plant & Animal Science", 
			   "Clinical Medicine", 
			   "Immunology", 
			   "Neuroscience & Behavior", 
			   "Pharmacology & Toxicology", 
			   "Psychiatry/Psychology", 
			   "Multidisciplinary")
	
	df %>% mutate(
		is_within_discipline = if_else({{var}} %in% alist, TRUE, FALSE)
	)
}


