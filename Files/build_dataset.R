library(tidyverse)

options(remove(list=ls()))
setwd("/Users/tuckerboynton/Desktop/R/ArxEd")

enrollment <- readxl::read_excel("enrollment.xlsx")
teacher_fte <- readxl::read_excel("teacher_fte.xlsx")
avg_teacher_salary <- readxl::read_excel("avg_teacher_salary.xlsx")
per_pupil_exp <- readxl::read_excel("per_pupil_exp.xlsx")
towns_counties <- readxl::read_excel("towns_counties.xlsx")
leagues <- readxl::read_excel("leagues.xlsx")
bio_mcas <- readxl::read_excel("bio_mcas.xlsx")
phy_mcas <- readxl::read_excel("phy_mcas.xlsx")
tec_mcas <- readxl::read_excel("tec_mcas.xlsx")
che_mcas <- readxl::read_excel("che_mcas.xlsx")
ela_mcas <- readxl::read_excel("ela_mcas.xlsx")
sci_mcas <- readxl::read_excel("sci_mcas.xlsx")
mth_mcas <- readxl::read_excel("mth_mcas.xlsx")
sat <- readxl::read_excel("sat.xlsx")
ap <- readxl::read_excel("ap.xlsx")
contracts <- readxl::read_excel("contracts.xlsx")
populations <- readxl::read_excel("populations.xlsx")

dart <- read_csv("ArxApp/all_dart.csv") %>%
  select(-c(
    `Berlin-Boylston...36`,
    `Brooke Charter School (District)...58`,
    `Edward M. Kennedy Academy for Health Careers (Horace Mann Charter) (District)...107`,
    `Greenfield Commonwealth Virtual District...136`
  )) %>%
  distinct() %>%
  rename(district_name = `...1`) %>%
  as.data.frame()

rownames(dart) <- dart$district_name

dart <- dart %>% select(-district_name)

full_df <- enrollment %>%
  merge(teacher_fte, all = T) %>%
  merge(avg_teacher_salary, all = T) %>%
  merge(per_pupil_exp, all = T) %>%
  merge(sci_mcas, all = T) %>%
  merge(ela_mcas, all = T) %>%
  merge(mth_mcas, all = T) %>%
  merge(sat, all = T) %>%
  merge(ap, all = T) %>%
  merge(contracts, all = T) %>%
  merge(populations, all = T) %>%
  merge(leagues, all = T) %>%
  merge(towns_counties, all = T) %>%
  select(-last_year) %>%
  mutate(
    pct_teachers_licensed = pct_teachers_licensed/100,
    pct_experienced_teachers = pct_experienced_teachers/100,
    pct_teachers_without_waiver = pct_teachers_without_waiver/100,
    pct_teaching_in_field = pct_teaching_in_field/100,
    pct_core_classes_taught_by_exp_teachers = pct_core_classes_taught_by_exp_teachers/100
  ) %>%
  select(-fte_count)

ncol(full_df)

colnames(full_df)

write.csv(full_df, "ArxApp/school_info.csv", row.names = F)

