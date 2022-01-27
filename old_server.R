#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(gt)
library(webshot)
library(shinyTree)
library(lazyeval)
library(shinyjs)

options(scipen = 99999)

color_palette <- c("darkred", "white", "darkgreen")

gt_theme_538 <- function(data,...) {
  data %>%
    opt_all_caps()  %>%
    opt_table_font(
      font = list(default_fonts())
    ) %>%
    tab_style(
      style = cell_borders(
        sides = "bottom", color = "black", weight = px(3)
      ),
      locations = cells_body(
        columns = TRUE,
        # This is a relatively sneaky way of changing the bottom border
        # Regardless of data size
        rows = nrow(data$`_data`)
      )
    )  %>% 
    tab_options(
      column_labels.background.color = "white",
      table.border.top.width = px(3),
      table.border.top.color = "white",
      table.border.bottom.color = "white",
      table.border.bottom.width = px(3),
      table_body.border.bottom.color = "black",
      column_labels.border.top.width = px(3),
      column_labels.border.top.color = "black",
      column_labels.border.lr.color = "grey",
      column_labels.vlines.style = "solid",
      table_body.vlines.style = "solid",
      table_body.vlines.color = "lightgrey",
      column_labels.border.bottom.width = px(3),
      column_labels.border.bottom.color = "black",
      data_row.padding = px(3),
      source_notes.font.size = 12,
      table.font.size = 16,
      heading.align = "left",
      ...
    ) 
}

dart <- read_csv("all_dart.csv") %>%
  select(-c(
    `Berlin-Boylston...36`,
    `Brooke Charter School (District)...58`,
    `Edward M. Kennedy Academy for Health Careers (Horace Mann Charter) (District)...107`,
    `Greenfield Commonwealth Virtual District...136`
  )) %>%
  distinct() %>%
  rename(district_name = `...1`) %>%
  as.data.frame()

school_info <- read_csv("school_info.csv") %>%
  mutate(
    dart_name = case_when(
      district_name == "Seven Hills Charter Public (District)" ~ "Learning First Charter Public School (District)",
      district_name == "Massachusetts Virtual Academy at Greenfield Commonwealth Virtual District" ~ "Greater Commonwealth Virtual District",
      district_name == "Hampden Charter School of Science (District)" ~ "Hampden Charter School of Science East (District)",
      district_name == "Tri County Regional Vocational Technical" ~ "Tri-County Regional Vocational Technical",
      district_name == "Brooke Charter School East Boston (District)" ~ "Brooke Charter School (District)",
      district_name == "Greater Commonwealth Virtual District" ~ "Greenfield Commonwealth Virtual District",
      district_name == "Southern Worcester County Regional Vocational School District" ~ "Southern Worcester County Regional Vocational Technical",
      district_name == "Boylston" ~ "Berlin-Boylston",
      district_name == "Berlin" ~ "Berlin-Boylston",
      TRUE ~ district_name),
    district_name = case_when(
      district_name == "Seven Hills Charter Public (District)" ~ "Learning First Charter Public School (District)",
      district_name == "Massachusetts Virtual Academy at Greenfield Commonwealth Virtual District" ~ "Greater Commonwealth Virtual District",
      district_name == "Tri County Regional Vocational Technical" ~ "Tri-County Regional Vocational Technical",
      district_name == "Greater Commonwealth Virtual District" ~ "Greenfield Commonwealth Virtual District",
      district_name == "Southern Worcester County Regional Vocational School District" ~ "Southern Worcester County Regional Vocational Technical",
      TRUE ~ district_name))

district_names <- school_info %>%
  select(district_name) %>%
  distinct()

years <- school_info %>%
  select(year) %>%
  distinct()

statistic_names <- c(
  "Total Enrollment",
  "Total Teacher FTE",
  "% Teachers Licensed",
  "Student:Teacher Ratio",
  "% Teachers Experienced",
  "% Teachers w/o Waiver",
  "% Teachers Teaching in Field",
  "Total Core Academic Classes",
  "% Core Classes Taught by Exp Teachers",
  "Total Salary",
  "Average Salary",
  "In-District Expenditure",
  "In-District Pupil FTE",
  "In-District Expenditure per Pupil",
  "Total Expenditure",
  "Total Pupil FTE",
  "Total Expenditure per Pupil"
)

statistic_names_full <- c(
  "District",
  "District Code",
  "Year",
  "1st Year",
  "2nd Year",
  "PK Enrollment",
  "K Enrollment",
  "G1 Enrollment",
  "G2 Enrollment",
  "G3 Enrollment",
  "G4 Enrollment",
  "G5 Enrollment",
  "G6 Enrollment",
  "G7 Enrollment",
  "G8 Enrollment",
  "G9 Enrollment",
  "G10 Enrollment",
  "G11 Enrollment",
  "G12 Enrollment",
  "SP Enrollment",
  "Total Enrollment",
  "Total Teacher FTE",
  "% Teachers Licensed",
  "Student:Teacher Ratio",
  "% Teachers Experienced",
  "% Teachers w/o Waiver",
  "% Teachers Teaching in Field",
  "Total Core Academic Classes",
  "% Core Classes Taught by Exp Teachers",
  "Total Salary",
  "Average Salary",
  "In-District Expenditure",
  "In-District Pupil FTE",
  "In-District Expenditure per Pupil",
  "Total Expenditure",
  "Total Pupil FTE",
  "Total Expenditure per Pupil",
  "Students Advanced/Proficient in Bio MCAS",
  "% Students Advanced/Proficient in Bio MCAS",
  "Students Advanced in Bio MCAS",
  "% Students Advanced in Bio MCAS",
  "Students Proficient in Bio MCAS",
  "% Students Proficient in Bio MCAS",
  "Students Need Imp Bio MCAS",
  "% Students Need Imp Bio MCAS",
  "Students Warning/Failing Bio MCAS",
  "% Students Warning/Failing Bio MCAS",
  "Students Taking Bio MCAS",
  "Students Advanced/Proficient in Phy MCAS",
  "% Students Advanced/Proficient in Phy MCAS",
  "Students Advanced in Phy MCAS",
  "% Students Advanced in Phy MCAS",
  "Students Proficient in Phy MCAS",
  "% Students Proficient in Phy MCAS",
  "Students Need Imp Phy MCAS",
  "% Students Need Imp Phy MCAS",
  "Students Warning/Failing Phy MCAS",
  "% Students Warning/Failing Phy MCAS",
  "Students Taking Phy MCAS",
  "Students Advanced/Proficient in Tec MCAS",
  "% Students Advanced/Proficient in Tec MCAS",
  "Students Advanced in Tec MCAS",
  "% Students Advanced in Tec MCAS",
  "Students Proficient in Tec MCAS",
  "% Students Proficient in Tec MCAS",
  "Students Need Imp Tec MCAS",
  "% Students Need Imp Tec MCAS",
  "Students Warning/Failing Tec MCAS",
  "% Students Warning/Failing Tec MCAS",
  "Students Taking Tec MCAS",
  "Students Advanced/Proficient in Che MCAS",
  "% Students Advanced/Proficient in Che MCAS",
  "Students Advanced in Che MCAS",
  "% Students Advanced in Che MCAS",
  "Students Proficient in Che MCAS",
  "% Students Proficient in Che MCAS",
  "Students Need Imp Che MCAS",
  "% Students Need Imp Che MCAS",
  "Students Warning/Failing Che MCAS",
  "% Students Warning/Failing Che MCAS",
  "Students Taking Che MCAS",
  "SAT Tests Taken",
  "SAT Reading/Writing Score",
  "SAT Math Score",
  "AP Tests Taken",
  "AP 1s",
  "AP 2s",
  "AP 3s",
  "AP 4s",
  "AP 5s",
  "AP 1-2 %",
  "AP 3-5 %",
  "DART Name",
  "City",
  "County",
  "League"
)

axis_options <- set_names(colnames(school_info), statistic_names_full)

# Define server logic required to draw a histogram
function(input, output, session) {
  
  comp_df <- reactive({
    
    district <- input$district
    comp_districts <- input$comp_districts
    comp_year <- input$comp_year
    
    if (input$individuals == "No"){
      
      comp_df <- school_info %>%
        filter(district_name %in% comp_districts,
               year == comp_year) %>%
        select(total_enrollment:total_exp_per_pupil) %>%
        magrittr::set_colnames(statistic_names) %>%
        as.data.frame()
      
      district_df <- school_info %>%
        filter(district_name == district,
               year == comp_year) %>%
        select(total_enrollment:total_exp_per_pupil) %>%
        magrittr::set_colnames(statistic_names) %>%
        as.data.frame()
      
      for (i in 1:ncol(comp_df)){
        
        col_mean <- mean(comp_df[,i], na.rm = T)
        col_name <- names(comp_df)[i]
        district_value <- district_df[,i][1]
        
        if (i == 1){
          
          
          diff <- ifelse(!is.na(district_value) & !is.na(col_mean),
                         district_value-col_mean, NA_real_)
          
          pct_diff <- ifelse(!is.na(diff), diff/col_mean, NA_real_)
          
          differences_df <- data.frame(column = c(col_name),
                                       value = c(district_value),
                                       mean = c(col_mean),
                                       difference = c(diff),
                                       pct_difference = c(pct_diff))
          
          colnames(differences_df) <- c("column",
                                        "value",
                                        "mean",
                                        "diff",
                                        "pct_diff")
        }
        else{
          
          diff <- ifelse(!is.na(district_value) & !is.na(col_mean),
                         district_value-col_mean, NA_real_)
          
          pct_diff <- ifelse(!is.na(diff), diff/col_mean, NA_real_)
          
          new_row <- data.frame(column = c(col_name),
                                value = c(district_value),
                                mean = c(col_mean),
                                difference = c(diff),
                                pct_difference = c(pct_diff))
          
          colnames(new_row) <- c("column",
                                 "value",
                                 "mean",
                                 "diff",
                                 "pct_diff")
          
          differences_df <- rbind(new_row, differences_df)
          
        }
      }
    }
    else{
      
      comp_df <- school_info %>%
        filter(district_name %in% comp_districts | district_name == district,
               year == comp_year)
      
      district_names <- comp_df$district_name
      
      new_df_names <- c()
      
      for (i in 1:nrow(comp_df)){
        new_df_names <- append(new_df_names, comp_df$district_name[i])
        new_df_names <- append(new_df_names, paste0(comp_df$district_name[i], " Diff"))
      }
      
      comp_df <- comp_df %>%
        select(total_enrollment:total_exp_per_pupil) %>%
        t() %>%
        magrittr::set_colnames(district_names) %>%
        magrittr::set_rownames(statistic_names) %>%
        as.data.frame()
      
      value_column <- as.vector(comp_df[district])
      
      differences_df <- data.frame(value = c(statistic_names),
                                   diff = c(value_column))
      
      colnames(differences_df) <- c(district, paste0(district, " Diff"))
      
      for (j in 1:ncol(comp_df)){
        
        current_district <- district_names[j]
        current_df_names <- colnames(differences_df)
        
        if (current_district != district){
          
          value_column <- as.vector(comp_df[current_district])
          diff_column <- (value_column-comp_df[district])/comp_df[district]
          
          differences_df <- cbind(differences_df, value_column) %>%
            cbind(diff_column)
          
          colnames(differences_df) <- append(current_df_names, c(current_district, paste0(current_district, " Diff")))
          
        }
      }
      
    }
    
    differences_df
  })
  
  comp_table <- reactive({
    
    district <- input$district
    comp_districts <- input$comp_districts
    comp_year <- input$comp_year
    
    if (input$individuals == "No"){
      
      len_comps <- length(comp_districts)
      
      for (i in 1:len_comps){
        if (i == 1){
          comp_list <- comp_districts[i]
        }
        else{
          comp_list <- paste0(comp_list, ", ", comp_districts[i])
        }
      }
      
      comp_df() %>%
        mutate(
          value = ifelse(is.nan(value), NA_real_, as.numeric(value)),
          mean = ifelse(is.nan(mean), NA_real_, as.numeric(mean)),
          diff = ifelse(is.nan(diff), NA_real_, as.numeric(diff))
        ) %>%
        arrange(match(column,
                      c("Total Expenditure",
                        "Total Expenditure per Pupil",
                        "In-District Expenditure",
                        "In-District Expenditure per Pupil",
                        
                        "Total Teacher FTE",
                        "Total Salary",
                        "Average Salary",
                        
                        "Total Enrollment",
                        "Total Pupil FTE",
                        "In-District Pupil FTE",
                        "Student:Teacher Ratio",
                        
                        "Total Core Academic Classes",
                        "% Teachers Licensed",
                        "% Teachers Experienced",
                        "% Teachers w/o Waiver",
                        "% Teachers Teaching in Field",
                        "% Core Classes Taught by Exp Teachers")),
                desc(column)) %>%
        gt() %>%
        cols_label(
          column = "",
          value = "Your District",
          mean = "Avg of Comparison Districts",
          diff = "Difference",
          pct_diff = "% Difference"
        ) %>%
        cols_align("center", columns = 2:5) %>%
        fmt_number(columns = 2:4, rows = c(8,12), decimals = 0) %>%
        fmt_number(columns = 2:4, rows = c(5,9:11), decimals = 1) %>%
        fmt_currency(columns = 2:4, rows = c(1:4,6:7), decimals = 0) %>%
        fmt_percent(columns = 2:4, rows = c(13:17), decimals = 1) %>%
        fmt_percent(columns = 5, decimals = 1) %>%
        data_color(
          columns = 5, 
          colors =
            scales::col_numeric(
              palette = (
                palette = color_palette
              ) %>% as.character(),
              domain = c(-1,1)
            )
        ) %>%
        tab_style(cell_fill(color = color_palette[3]), locations = list(
          cells_body(columns = 5, rows = pct_diff > 1))) %>%
        tab_style(cell_fill(color = color_palette[2]), locations = list(
          cells_body(columns = 5, rows = is.na(pct_diff)))) %>%
        tab_style(cell_fill(color = color_palette[1]), locations = list(
          cells_body(columns = 5, rows = pct_diff < -1))) %>%
        tab_style(cell_text(color = "white"), locations = list(
          cells_body(columns = 5, rows = abs(pct_diff) > 1))) %>%
        tab_row_group(
          label = "Teacher Stats",
          rows = c(12:17)
        ) %>%
        tab_row_group(
          label = "Enrollment",
          rows = c(8:11)
        ) %>%
        tab_row_group(
          label = "Teacher Salaries",
          rows = c(5:7)
        ) %>%
        tab_row_group(
          label = "Expenditure",
          rows = c(1:4)
        ) %>%
        gt_theme_538() %>%
        tab_source_note(
          source_note = "Note: Only enrollment data available for 2021/22;
                                    Teacher salaries available beginning in 2019/20;
                                    Teacher stats vary in availability by year/district") %>%
        tab_header(
          title = paste0(district, " vs. Comparison Districts"),
          subtitle = paste0("Stats for ", comp_year, "; See footnote for data availability details")
        ) %>%
        tab_footnote(footnote="Dark red = ≤ -100%; Dark green = ≥ 100%", locations=cells_column_labels(columns=5)) %>%
        tab_footnote(comp_list, locations=cells_column_labels(columns=3))
    }
    
    else{
      
      if (ncol(comp_df()) > 2){
        pct_cols <- seq(4, ncol(comp_df()), 2)
      }
      else{
        pct_cols <- c()
      }
      
      if (ncol(comp_df()) > 2){
        value_cols <- append(2, seq(3, ncol(comp_df()), 2))
      }
      else{
        value_cols <- c(2)
      }
      
      col_indexes <- c(seq(ncol(comp_df())))
      
      # Comparison districts WITHOUT the district itself
      comp_districts_adjusted <- c()
      
      if (ncol(comp_df()) > 2){
        
        for (i in 1:length(comp_districts)){
          
          if (comp_districts[i] != district & comp_districts[i] %in% colnames(comp_df())){
            
            comp_districts_adjusted <- append(comp_districts_adjusted, comp_districts[i])
            
          }
          
        }
      }
      
      col_labs <- c("",district)
      
      if (length(comp_districts_adjusted) > 0){
        
        for (i in 1:length(comp_districts_adjusted)){
          
          col_labs <- append(col_labs, c(comp_districts_adjusted[i], "% Diff"))
        }
      }
      
      print(col_labs)
      print(col_indexes)
      
      col_labs <- set_names(col_labs, col_indexes)
      
      comp_df() %>%
        arrange(match(get(district),
                      c("Total Expenditure",
                        "Total Expenditure per Pupil",
                        "In-District Expenditure",
                        "In-District Expenditure per Pupil",
                        
                        "Total Teacher FTE",
                        "Total Salary",
                        "Average Salary",
                        
                        "Total Enrollment",
                        "Total Pupil FTE",
                        "In-District Pupil FTE",
                        "Student:Teacher Ratio",
                        
                        "Total Core Academic Classes",
                        "% Teachers Licensed",
                        "% Teachers Experienced",
                        "% Teachers w/o Waiver",
                        "% Teachers Teaching in Field",
                        "% Core Classes Taught by Exp Teachers")),
                desc(get(district))) %>%
        magrittr::set_colnames(col_indexes) %>%
        gt() %>%
        cols_label(.list = col_labs) %>%
        gt_theme_538() %>%
        tab_row_group(
          label = "Teacher Stats",
          rows = c(12:17)
        ) %>%
        tab_row_group(
          label = "Enrollment",
          rows = c(8:11)
        ) %>%
        tab_row_group(
          label = "Teacher Salaries",
          rows = c(5:7)
        ) %>%
        tab_row_group(
          label = "Expenditure",
          rows = c(1:4)
        ) %>%
        fmt_number(columns = value_cols, rows = c(8,12), decimals = 0) %>%
        fmt_number(columns = value_cols, rows = c(5,9:11), decimals = 1) %>%
        fmt_currency(columns = value_cols, rows = c(1:4,6:7), decimals = 0) %>%
        fmt_percent(columns = value_cols, rows = c(13:17), decimals = 1) %>%
        fmt_percent(columns = pct_cols, decimals = 1) %>%
        data_color(
          columns = pct_cols, 
          colors =
            scales::col_bin(
              palette = (
                palette = color_palette
              ) %>% as.character(),
              domain = c(-1000,1000),
              bins = c(-1000,-1,0,1,1000),
              na.color = "white"
            )
        ) %>%
        cols_align("center", columns = 2:max(col_indexes)) %>%
        tab_style(
          style = list(
            cell_borders(
              sides = c("left"),
              color = "black",
              weight = px(2)
            ),
            cell_borders(
              sides = c("left"),
              color = "black",
              weight = px(2)
            )
          ),
          locations = list(
            cells_body(
              columns = value_cols
            ),
            cells_column_labels(
              columns = value_cols
            )
          )
        ) %>%
        tab_source_note(
          source_note = "Note: Only enrollment data available for 2021/22;
                                    Teacher salaries available beginning in 2019/20;
                                    Teacher stats vary in availability by year/district") %>%
        tab_footnote(footnote="Dark red = ≤ -100%; Dark green = ≥ 100%", locations=cells_column_labels(columns=pct_cols)) %>%
        tab_header(
          title = paste0(district, " vs. Comparison Districts"),
          subtitle = paste0("Stats for ", comp_year, "; See footnote for data availability details")
        ) %>%
        cols_width(`1` ~ px(275),
                   ends_with("12") ~ px(75),
                   ends_with("22") ~ px(75),
                   ends_with("32") ~ px(75),
                   ends_with("42") ~ px(75),
                   ends_with("52") ~ px(75),
                   ends_with("62") ~ px(75),
                   ends_with("72") ~ px(75),
                   ends_with("82") ~ px(75),
                   ends_with("92") ~ px(75),
                   ends_with("02") ~ px(75),
                   ends_with("4") ~ px(75),
                   ends_with("6") ~ px(75),
                   ends_with("8") ~ px(75),
                   ends_with("0") ~ px(75),
                   everything() ~ px(150)) %>%
        tab_options(table.font.size = 14)
      
    }
  })
  
  toListen <- reactive({
    list(input$autoselect, input$district, input$comp_year,
         input$budget[1], input$budget[2], input$enrollment, input$fte, input$salary)
  })
  
  observeEvent(toListen(), {
    
    district <- input$district
    
    if ("DART" %in% input$autoselect & district != ""){
      
      dart_districts <- dart %>%
        select(district_name, as.name(district)) %>%
        filter(get(district) == 1, district_name != district) %>%
        pull(district_name)
      
      # print(paste0("DART districts:", dart_districts))
      
    }
    else{
      dart_districts <- c()
    }
    
    if ("County" %in% input$autoselect & district != ""){
      
      school_county <- school_info %>% filter(district_name == district) %>% pull(county)
      
      county_districts <- school_info %>%
        select(district_name, county) %>%
        filter(district_name != district, county == school_county[1]) %>%
        distinct() %>%
        pull(district_name)
      
      # print(paste0("County districts:", county_districts))
      
    }
    else{
      county_districts <- c()
    }
    
    if ("League" %in% input$autoselect & district != ""){
      
      school_league <- school_info %>% filter(district_name == district) %>% pull(league)
      
      if (!is.na(school_league[1])){
        
        league_districts <- school_info %>%
          select(district_name, league) %>%
          filter(district_name != district, league == school_league[1]) %>%
          distinct() %>%
          pull(district_name)
      }
      else{
        league_districts <- c()
      }
      
      # print(paste0("County districts:", county_districts))
      
    }
    else{
      league_districts <- c()
    }
    
    if ("Municipality" %in% input$autoselect & district != ""){
      
      school_municipality <- school_info %>% filter(district_name == district) %>% pull(city)
      
      town_districts <- school_info %>%
        select(district_name, city) %>%
        filter(district_name != district, city == school_municipality[1]) %>%
        distinct() %>%
        pull(district_name)
      
      # print(paste0("Municipality districts:", town_districts))
      
    }
    else{
      town_districts <- c()
    }
    
    if ("Total Budget within 5%" %in% input$autoselect & district != "" & input$comp_year != ""){
      
      district_budget <- school_info %>%
        filter(district_name == district, year == input$comp_year) %>%
        pull(total_exp)
      
      budget_hi <- 1.05*district_budget[1]
      budget_lo <- 0.95*district_budget[1]
      
      budget_districts <- school_info %>%
        select(district_name, total_exp) %>%
        filter(district_name != district, between(total_exp, budget_lo, budget_hi)) %>%
        distinct() %>%
        pull(district_name)
      
      # print(paste0("Budget districts:", budget_districts))
      
    }
    else{
      budget_districts <- c()
    }
    
    if ("Enrollment within 5%" %in% input$autoselect & district != "" & input$comp_year != ""){
      
      district_enrollment <- school_info %>%
        filter(district_name == district, year == input$comp_year) %>%
        pull(total_enrollment)
      
      enrollment_hi <- 1.05*district_enrollment[1]
      enrollment_lo <- 0.95*district_enrollment[1]
      
      enrollment_districts <- school_info %>%
        select(district_name, total_enrollment) %>%
        filter(district_name != district, between(total_enrollment, enrollment_lo, enrollment_hi)) %>%
        distinct() %>%
        pull(district_name)
      
      # print(paste0("Enrollment districts:", budget_districts))
      
    }
    else{
      enrollment_districts <- c()
    }
    
    if ("Teacher FTE within 5%" %in% input$autoselect & district != "" & input$comp_year != ""){
      
      district_fte <- school_info %>%
        filter(district_name == district, year == input$comp_year) %>%
        pull(total_teachers)
      
      fte_hi <- 1.05*district_fte[1]
      fte_lo <- 0.95*district_fte[1]
      
      fte_districts <- school_info %>%
        select(district_name, total_teachers) %>%
        filter(district_name != district, between(total_teachers, fte_lo, fte_hi)) %>%
        distinct() %>%
        pull(district_name)
      
      # print(paste0("Budget districts:", budget_districts))
      
    }
    else{
      fte_districts <- c()
    }
    
    if ("Avg Teacher Salary within 5%" %in% input$autoselect & district != "" & input$comp_year != ""){
      
      district_salary <- school_info %>%
        filter(district_name == district, year == input$comp_year) %>%
        filter(!is.na(average_salary)) %>%
        pull(average_salary)
      
      salary_hi <- 1.05*district_salary[1]
      salary_lo <- 0.95*district_salary[1]
      
      salary_districts <- school_info %>%
        select(district_name, average_salary) %>%
        filter(district_name != district, between(average_salary, salary_lo, salary_hi)) %>%
        distinct() %>%
        pull(district_name)
      
    }
    else{
      salary_districts <- c()
    }
    
    selected_districts <- append(input$comp_districts, town_districts)
    selected_districts <- append(selected_districts, county_districts)
    selected_districts <- append(selected_districts, dart_districts)
    selected_districts <- append(selected_districts, budget_districts)
    selected_districts <- append(selected_districts, enrollment_districts)
    selected_districts <- append(selected_districts, fte_districts)
    selected_districts <- append(selected_districts, salary_districts)
    selected_districts <- append(selected_districts, league_districts)
    
    # print(selected_districts)
    
    budget_hi <- input$budget[2]*1000000
    budget_lo <- input$budget[1]*1000000
    
    if (length(selected_districts) > 0 & "Total Budget" %in% input$filters){
      
      selected_districts <- school_info %>%
        filter(district_name %in% selected_districts,
               year %in% input$comp_year,
               between(total_exp, budget_lo, budget_hi), !is.na(total_exp)) %>%
        distinct() %>%
        pull(district_name)
      
      # print(paste0("Budget:", selected_districts))
      
    }
    
    enrollment_hi <- input$enrollment[2]*100
    enrollment_lo <- input$enrollment[1]*100
    
    if (length(selected_districts) > 0 & "Enrollment" %in% input$filters){
      
      selected_districts <- school_info %>%
        filter(district_name %in% selected_districts,
               year %in% input$comp_year,
               between(total_enrollment, enrollment_lo, enrollment_hi), !is.na(total_enrollment)) %>%
        distinct() %>%
        pull(district_name)
      
      # print(paste0("Enrollment:", selected_districts))
      
      
    }
    
    fte_hi <- input$fte[2]
    fte_lo <- input$fte[1]
    
    if (length(selected_districts) > 0 & "Teacher FTE" %in% input$filters){
      
      selected_districts <- school_info %>%
        filter(district_name %in% selected_districts,
               year %in% input$comp_year,
               between(total_teachers, fte_lo, fte_hi), !is.na(total_teachers)) %>%
        distinct() %>%
        pull(district_name)
      
      # print(paste0("FTE:", selected_districts))
      
      
    }
    
    salary_hi <- input$salary[2]*1000
    salary_lo <- input$salary[1]*1000
    
    if (length(selected_districts) > 0 & "Avg Teacher Salary" %in% input$filters){
      
      selected_districts <- school_info %>%
        filter(district_name %in% selected_districts,
               year %in% input$comp_year,
               between(average_salary, salary_lo, salary_hi), !is.na(average_salary)) %>%
        distinct() %>%
        pull(district_name)
      
      # print(paste0("Salary:", selected_districts))
      
      
    }
    
    updateSelectizeInput(session, input = "comp_districts",
                         selected = selected_districts)
    
  })
  
  observeEvent(input$reset, {
    
    updateSelectizeInput(session, input = "comp_districts", selected = "")
    updateCheckboxGroupInput(session, input = "autoselect", selected = "")
    
  })
  
  output$comparisons_table <- render_gt({comp_table()})
  
  userplot <- reactive({
    
    plot_data <- school_info %>%
      filter(district_name %in% input$comp_districts | district_name == input$district,
             year == input$comp_year, !is.na(get(input$y)))
    
    if (input$plot_type == "Scatter"){
      
      plot_data %>%
        ggplot(aes_string(input$x, input$y)) +
        ggimage::geom_image(image = paste0("Logos/", plot_data$district_code, ".png"), asp = 1.7) +
        labs(
          title = paste0(names(axis_options)[axis_options == input$y], " vs. ",
                         names(axis_options)[axis_options == input$x],
                         " for Selected Comparison Districts"),
          subtitle = input$comp_year,
          x = names(axis_options)[axis_options == input$x],
          y = names(axis_options)[axis_options == input$y]
        ) +
        ggthemes::theme_fivethirtyeight() +
        theme(
          legend.position = "none",
          plot.title = element_text(size = 24),
          plot.subtitle = element_text(size = 20),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12)
        )
    }
    else{
      
      y_vals <- plot_data %>% pull(get(input$y))
      y_mean <- mean(y_vals, na.rm = T)
      
      plot_data %>%
        ggplot(aes_string(paste0("reorder(district_name, ", input$y, ")"), input$y)) +
        geom_bar(stat = "identity", fill = ifelse(plot_data$district_name == input$district, "darkred", "darkgrey")) +
        ggimage::geom_image(image = paste0("Logos/", plot_data$district_code, ".png"), asp = 1.7) +
        geom_hline(yintercept = y_mean, color = "red", linetype = "dashed") +
        annotate("label", x = 1.5, y = y_mean, label = paste0("Avg: ", round(y_mean,2))) +
        labs(
          title = paste0(names(axis_options)[axis_options == input$y],
                         " for Selected Comparison Districts"),
          subtitle = input$comp_year,
          x = "District",
          y = names(axis_options)[axis_options == input$y]
        ) +
        ggthemes::theme_fivethirtyeight() +
        theme(
          legend.position = "none",
          plot.title = element_text(size = 24),
          plot.subtitle = element_text(size = 20),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          axis.text.x = element_blank(),
          axis.text.y = element_text(size = 12)
        )
    }
    
  })
  
  output$userplot <- renderPlot({userplot()}, height = 750)
  
  output$download_table <- downloadHandler(
    filename = "district_comparison.png",
    content = function(file) {
      gtsave(comp_table(), file)
    }
  )
  
  output$download_csv <- downloadHandler(
    filename = "district_comparison.csv",
    content = function(file) {
      write.csv(comp_df(), file)
    }
  )
  
  output$download_plot <- downloadHandler(
    filename = function() {"comparison_plot.png"},
    content = function(file) {
      device <- function(..., width, height) grDevices::png(..., width = 10, height = 6, res = 300, units = "in")
      ggsave(file, plot = userplot(), device = device)
    }
  )
  
  observeEvent(input$toggle_sidebar, {
    shinyjs::toggle(id = "Sidebar")
  })
  
}
