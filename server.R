
library(shiny)
library(tidyverse)
library(gt)
library(webshot)
library(shinyTree)
library(lazyeval)
library(shinyjs)
library(plotly)

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

## Read in DART schools, manually removing duplicate columns (created by virtue of scraper)
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

## Read in school info data frame, adding new column called "DART name" for district's name in DART database
# NOTE: for most districts, this is just their name, but some have special cases (see below)
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

overview_statistics <- c(
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
    "District", # 1
    "District Code",
    "1st Year",
    "Year",
    "2nd Year",
    "PK Enrollment",
    "K Enrollment",
    "G1 Enrollment",
    "G2 Enrollment",
    "G3 Enrollment", # 10
    "G4 Enrollment",
    "G5 Enrollment",
    "G6 Enrollment",
    "G7 Enrollment",
    "G8 Enrollment",
    "G9 Enrollment",
    "G10 Enrollment",
    "G11 Enrollment",
    "G12 Enrollment",
    "SP Enrollment", # 20
    "Total Enrollment",
    "Total Teacher FTE",
    "% Teachers Licensed",
    "Student:Teacher Ratio",
    "% Teachers Experienced",
    "% Teachers w/o Waiver",
    "% Teachers Teaching in Field",
    "Total Core Academic Classes",
    "% Core Classes Taught by Exp Teachers",
    "Total Salary", # 30
    "Average Salary",
    "In-District Expenditure",
    "In-District Pupil FTE",
    "In-District Expenditure per Pupil",
    "Total Expenditure",
    "Total Pupil FTE",
    "Total Expenditure per Pupil",
    "Students Advanced/Proficient in Science MCAS",
    "% Students Advanced/Proficient in Science MCAS",
    "Students Advanced in Science MCAS", # 40
    "% Students Advanced in Science MCAS",
    "Students Proficient in Science MCAS",
    "% Students Proficient in Science MCAS",
    "Students Need Imp Science MCAS",
    "% Students Need Imp Science MCAS",
    "Students Warning/Failing Science MCAS",
    "% Students Warning/Failing Science MCAS",
    "Students Taking Science MCAS",
    "Students Advanced/Proficient in ELA MCAS",
    "% Students Advanced/Proficient in ELA MCAS", # 50
    "Students Advanced in ELA MCAS",
    "% Students Advanced in ELA MCAS",
    "Students Proficient in ELA MCAS",
    "% Students Proficient in ELA MCAS",
    "Students Need Imp ELA MCAS",
    "% Students Need Imp ELA MCAS",
    "Students Warning/Failing ELA MCAS",
    "% Students Warning/Failing ELA MCAS",
    "Students Taking ELA MCAS", # 60
    "Students Advanced/Proficient in Math MCAS",
    "% Students Advanced/Proficient in Math MCAS",
    "Students Advanced in Math MCAS",
    "% Students Advanced in Math MCAS",
    "Students Proficient in Math MCAS",
    "% Students Proficient in Math MCAS",
    "Students Need Imp Math MCAS",
    "% Students Need Imp Math MCAS",
    "Students Warning/Failing Math MCAS",
    "% Students Warning/Failing Math MCAS",
    "Students Taking Math MCAS", # 70
    "SAT Tests Taken",
    "SAT Reading/Writing Score",
    "SAT Math Score",
    "AP Tests Taken",
    "AP 1s",
    "AP 2s",
    "AP 3s",
    "AP 4s",
    "AP 5s",
    "AP 1-2 %", # 80
    "AP 3-5 %",
    "Contract Length",
    "Contract Y1",
    "Contract Y2",
    "Contract Y3",
    "2015-16 COLA",
    "2016-17 COLA",
    "2017-18 COLA",
    "2018-19 COLA",
    "2019-20 COLA", # 90
    "2020-21 COLA",
    "2021-22 COLA",
    "2022-23 COLA",
    "2023-24 COLA",
    "2015-16 Upper-Left Salary",
    "2016-17 Upper-Left Salary",
    "2017-18 Upper-Left Salary",
    "2018-19 Upper-Left Salary",
    "2019-20 Upper-Left Salary",
    "2020-21 Upper-Left Salary", # 100
    "2021-22 Upper-Left Salary",
    "2022-23 Upper-Left Salary",
    "2023-24 Upper-Left Salary",
    "2015-16 Lower-Right Salary",
    "2016-17 Lower-Right Salary",
    "2017-18 Lower-Right Salary",
    "2018-19 Lower-Right Salary",
    "2019-20 Lower-Right Salary",
    "2020-21 Lower-Right Salary",
    "2021-22 Lower-Right Salary", # 110
    "2022-23 Lower-Right Salary",
    "2023-24 Lower-Right Salary",
    "2015-16 Average Salary",
    "2016-17 Average Salary",
    "2017-18 Average Salary",
    "2018-19 Average Salary",
    "2019-20 Average Salary",
    "2020-21 Average Salary",
    "2021-22 Average Salary",
    "2022-23 Average Salary", # 120
    "2023-24 Average Salary",
    "Contract Steps",
    "Contract Lanes",
    "Average Step % Increase",
    "Last Step % Increase",
    "Average Lane % Increase",
    "Bachelors Masters % Increase",
    "Masters Doctorate % Increase",
    "Longevity Start Year",
    "Longevity End Year", # 130
    "Longevity Min Pay",
    "Longevity Max Pay",
    "# Days with Students",
    "# Days w/o Students Teachers",
    "# Days w/o Students Guidance",
    "# Days w/o Students Nurses",
    "Nurses Unionized",
    "% Health Insurance Paid",
    "Personal Days Annually",
    "Personal Days Carry Over", # 140
    "Personal Days Carry Over Max",
    "Unused Personal Days Paid for",
    "Unused Personal Days Paid for Rate",
    "Sick Days",
    "Sick Days Buy Back",
    "Sick Days Buy Back Rate",
    "Sick Leave Bank",
    "Sick Leave Bank Cap",
    "Bereavement Days",
    "Religious Days", # 150
    "Other Days",
    "Comp for Attendance",
    "Guidance Max Ratio High School",
    "Max Class Size Ratio Elementary",
    "Max Student Ratio High School",
    "Special Ed Ratio Elementary",
    "Special Ed Ratio High School",
    "Children of Teachers Attending",
    "Length of Workday Elementary",
    "Length of Workday High School", # 160
    "Early Retirement Incentives",
    "Unit B",
    "Tuition Reimbursement",
    "Students 1st Language not English",
    "% Students 1st Language not English",
    "English Learners",
    "% Students English Learners",
    "Students w/ Disabilities",
    "% Students Disabled",
    "High-Need Students", # 170
    "% Students High-Need",
    "Low-Income Students",
    "% Students Low-Income",
    "DART Name",
    "City",
    "County",
    "League"
)

axis_options <- set_names(colnames(school_info), statistic_names_full)

# Define server logic required to draw a histogram
function(input, output, session) {
    
    ### DISTRICT SELECTOR TAB
    
    # Events to listen to for comparison district updating
    toListen <- reactive({
        list(input$autoselect, input$autoselect2, input$district, input$comp_year,
             input$budget[1], input$budget[2], input$enrollment, input$fte, input$salary)
    })
    
    # What to do when one of the listened-to events changes
    observeEvent(toListen(), {
        
        district <- input$district
        
        ### At a high level, the autoselect portion of this observeEvent() creates
        ### individual vectors of the districts that meet at least one of the selected
        ### criteria (lines 322-488). When an autoselect option is not selected, the
        ### script creates an empty vector. Then, these vectors are appended to make
        ### one vector of districts, and this vector is pared based on the additional
        ### filters the user adds (lines 502-560). Finally, the selected districts are updated
        ### to reflect the user inputs.
        
        ## Update same DART districts
        # NOTE: this process is a bit different than the ones below because DART names != district names necessarily
        if ("DART" %in% input$autoselect & district != ""){
            
            # Get data frame with DART names and district names
            dart_names <- school_info %>%
                select(district_name, dart_name) %>%
                distinct()
            
            # Get individual district's DART name
            dart_district <- dart_names %>%
                filter(district_name == district) %>%
                pull(dart_name)
            
            # Get DART districts
            darts <- dart %>%
                select(district_name, as.name(district)) %>%
                filter(get(dart_district) == 1, district_name != dart_district) %>%
                pull(district_name)
            
            # Convert those DART names back to district names
            dart_districts <- c()
            
            for (i in 1:length(darts)){
                
                new_dart <- dart_names %>%
                    filter(dart_name == darts[i]) %>%
                    pull(district_name)
                
                dart_districts <- append(dart_districts, new_dart)
            }
            
        }
        else{
            dart_districts <- c()
        }
        
        ## Update same county districts
        if ("County" %in% input$autoselect & district != ""){
            
            school_county <- school_info %>% filter(district_name == district) %>% pull(county)
            
            county_districts <- school_info %>%
                select(district_name, county) %>%
                filter(district_name != district, county == school_county[1]) %>%
                distinct() %>%
                pull(district_name)
        }
        else{
            county_districts <- c()
        }
        
        ## Update same league districts
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
            
        }
        else{
            league_districts <- c()
        }
        
        ## Update same municipality districts
        if ("Municipality" %in% input$autoselect & district != ""){
            
            school_municipality <- school_info %>% filter(district_name == district) %>% pull(city)
            
            town_districts <- school_info %>%
                select(district_name, city) %>%
                filter(district_name != district, city == school_municipality[1]) %>%
                distinct() %>%
                pull(district_name)
        }
        else{
            town_districts <- c()
        }
        
        ## Update similar budget districts
        if ("Total Budget within 5%" %in% input$autoselect2 & district != "" & input$comp_year != ""){
            
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
        }
        else{
            budget_districts <- c()
        }
        
        ## Update similar enrollment districts
        if ("Enrollment within 5%" %in% input$autoselect2 & district != "" & input$comp_year != ""){
            
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
        }
        else{
            enrollment_districts <- c()
        }
        
        ## Update similar teacher FTE districts
        if ("Teacher FTE within 5%" %in% input$autoselect2 & district != "" & input$comp_year != ""){
            
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
        }
        else{
            fte_districts <- c()
        }
        
        ## Update similar salary districts
        if ("Avg Teacher Salary within 5%" %in% input$autoselect2 & district != "" & input$comp_year != ""){
            
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
        
        ## YOU NOW HAVE A VECTOR OF DISTRICTS THAT SHOULD BE AUTOSELECTED AS WELL
        ## AS THOSE THAT WERE ALREADY SELECTED - TIME TO PARE DOWN BASED ON FILTERS
        
        ## Filter based on budget
        budget_hi <- input$budget[2]*1000000
        budget_lo <- input$budget[1]*1000000
        
        if (length(selected_districts) > 0 & "Total Budget" %in% input$filters){
            
            selected_districts <- school_info %>%
                filter(district_name %in% selected_districts,
                       year %in% input$comp_year,
                       between(total_exp, budget_lo, budget_hi), !is.na(total_exp)) %>%
                distinct() %>%
                pull(district_name)
        }
        
        ## Filter based on enrollment
        enrollment_hi <- input$enrollment[2]*100
        enrollment_lo <- input$enrollment[1]*100
        
        if (length(selected_districts) > 0 & "Enrollment" %in% input$filters){
            
            selected_districts <- school_info %>%
                filter(district_name %in% selected_districts,
                       year %in% input$comp_year,
                       between(total_enrollment, enrollment_lo, enrollment_hi), !is.na(total_enrollment)) %>%
                distinct() %>%
                pull(district_name)
        }
        
        ## Filter based on number of teachers (FTE)
        fte_hi <- input$fte[2]
        fte_lo <- input$fte[1]
        
        if (length(selected_districts) > 0 & "Teacher FTE" %in% input$filters){
            
            selected_districts <- school_info %>%
                filter(district_name %in% selected_districts,
                       year %in% input$comp_year,
                       between(total_teachers, fte_lo, fte_hi), !is.na(total_teachers)) %>%
                distinct() %>%
                pull(district_name)
        }
        
        ## Filter based on salary
        salary_hi <- input$salary[2]*1000
        salary_lo <- input$salary[1]*1000
        
        if (length(selected_districts) > 0 & "Avg Teacher Salary" %in% input$filters){
            
            selected_districts <- school_info %>%
                filter(district_name %in% selected_districts,
                       year %in% input$comp_year,
                       between(average_salary, salary_lo, salary_hi), !is.na(average_salary)) %>%
                distinct() %>%
                pull(district_name)
        }
        
        ### FINALLY, UPDATE THE SELECTIONS
        updateSelectizeInput(session, input = "comp_districts",
                             selected = selected_districts)
        
    })
    
    # Unselect comparison districts if user clicks 'reset'
    observeEvent(input$reset, {
        
        updateSelectizeInput(session, input = "comp_districts", selected = "")
        updateCheckboxGroupInput(session, input = "autoselect", selected = "")
        updateCheckboxGroupInput(session, input = "autoselect2", selected = "")
        updateCheckboxGroupInput(session, input = "filters", selected = "")
        
    })
    
    ### HOME TAB
    
    # Title of home panel
    output$district <- renderText({
        
        district <- input$district
        
        paste0(district, " at a Glance")
        
    })
    
    # Subtitle of home panel
    output$comps <- renderText({
        
        comp_districts <- input$comp_districts
        
        len_comps <- length(comp_districts)
        
        for (i in 1:len_comps){
            if (i == 1){
                comp_list <- comp_districts[i]
            }
            else{
                comp_list <- paste0(comp_list, ", ", comp_districts[i])
            }
        }
        
        paste0("Comparing to: ", comp_list)
        
    })
    
    # Summary table for upper-left section of homepage
    output$summary_table <- render_gt({
        
        district <- input$district
        
        district_code <- school_info %>%
            filter(district_name == input$district) %>%
            pull(district_code)
        
        district_code <- district_code[1]
        
        district_df <- school_info %>%
            filter(district_name == input$district,
                   year == input$comp_year) %>%
            mutate(
                total_enrollment = paste("Enrollment:", total_enrollment),
                total_teachers = paste("Teacher FTE:", total_teachers),
                year = paste("Year:", year)) %>%
            select(year, total_enrollment, total_teachers) %>%
            t() %>%
            as.data.frame()
        
        district_df %>%
            gt() %>%
            cols_width(everything() ~ px(250)) %>%
            gt_theme_538() %>%
            cols_label(V1 = gtExtras::img_header(label = district,
                                                 img_url = paste0("https://github.com/tuckerboynton22/arxed_ddd/blob/main/Logos/", district_code, ".png?raw=true"),
                                                 height = 90))
        
    })
    
    # Create single data frame with all stats of interest and how the district compares
    # to the average of the selected comparison districts
    plot_comp_df <- reactive({
        
        district <- input$district
        comp_districts <- input$comp_districts
        comp_year <- input$comp_year
        
        school_info %>%
            filter(district_name %in% comp_districts | district_name == district,
                   year == comp_year) %>%
            mutate(is_district = ifelse(district_name == district, district, "Others")) %>%
            group_by(is_district) %>%
            summarize(
                total_exp = mean(total_exp, na.rm = T),
                salary_total = mean(salary_total, na.rm = T),
                non_salary_exp = total_exp - salary_total,
                total_exp_per_pupil = mean(total_exp_per_pupil, na.rm = T),
                average_salary = mean(average_salary, na.rm = T),
                total_enrollment = mean(total_enrollment, na.rm = T),
                total_teachers = mean(total_teachers, na.rm = T),
                sat_reading_writing = mean(sat_reading_writing, na.rm = T),
                sat_math = mean(sat_math, na.rm = T),
                english_learner_pct = mean(english_learner_pct, na.rm = T),
                low_income_pct = mean(low_income_pct, na.rm = T),
                ela_advanced_proficient_pct = mean(ela_advanced_proficient_pct, na.rm = T),
                mth_advanced_proficient_pct = mean(mth_advanced_proficient_pct, na.rm = T),
                sci_advanced_proficient_pct = mean(sci_advanced_proficient_pct, na.rm = T),
                ap_score_3_5_pct = mean(ap_score_3_5_pct, na.rm = T)
            ) %>%
            mutate(short_district = stringr::str_trunc(is_district, 20, "right"))
        
    })
    
    # Create single data frame with all contract stats of interest and how the district compares
    # to the average of ALL OTHER districts for which we have contract info - not just the selected
    # comp districts
    plot_contract_df <- reactive({
        
        district <- input$district
        
        school_info %>%
            mutate(is_district = ifelse(district_name == district, district, "Others")) %>%
            group_by(is_district) %>%
            summarize(
                cola_2015_16 = mean(cola_2015_16, na.rm = T),
                cola_2016_17 = mean(cola_2016_17, na.rm = T),
                cola_2017_18 = mean(cola_2017_18, na.rm = T),
                cola_2018_19 = mean(cola_2018_19, na.rm = T),
                cola_2019_20 = mean(cola_2019_20, na.rm = T),
                cola_2020_21 = mean(cola_2020_21, na.rm = T),
                cola_2021_22 = mean(cola_2021_22, na.rm = T),
                cola_2022_23 = mean(cola_2022_23, na.rm = T),
                cola_2023_24 = mean(cola_2023_24, na.rm = T),
                lower_right_2015_16 = mean(lower_right_2015_16, na.rm = T),
                lower_right_2016_17 = mean(lower_right_2016_17, na.rm = T),
                lower_right_2017_18 = mean(lower_right_2017_18, na.rm = T),
                lower_right_2018_19 = mean(lower_right_2018_19, na.rm = T),
                lower_right_2019_20 = mean(lower_right_2019_20, na.rm = T),
                lower_right_2020_21 = mean(lower_right_2020_21, na.rm = T),
                lower_right_2021_22 = mean(lower_right_2021_22, na.rm = T),
                lower_right_2022_23 = mean(lower_right_2022_23, na.rm = T),
                lower_right_2023_24 = mean(lower_right_2023_24, na.rm = T),
                upper_left_2015_16 = mean(upper_left_2015_16, na.rm = T),
                upper_left_2016_17 = mean(upper_left_2016_17, na.rm = T),
                upper_left_2017_18 = mean(upper_left_2017_18, na.rm = T),
                upper_left_2018_19 = mean(upper_left_2018_19, na.rm = T),
                upper_left_2019_20 = mean(upper_left_2019_20, na.rm = T),
                upper_left_2020_21 = mean(upper_left_2020_21, na.rm = T),
                upper_left_2021_22 = mean(upper_left_2021_22, na.rm = T),
                upper_left_2022_23 = mean(upper_left_2022_23, na.rm = T),
                upper_left_2023_24 = mean(upper_left_2023_24, na.rm = T),
                salary_avg_2015_16 = mean(salary_avg_2015_16, na.rm = T),
                salary_avg_2016_17 = mean(salary_avg_2016_17, na.rm = T),
                salary_avg_2017_18 = mean(salary_avg_2017_18, na.rm = T),
                salary_avg_2018_19 = mean(salary_avg_2018_19, na.rm = T),
                salary_avg_2019_20 = mean(salary_avg_2019_20, na.rm = T),
                salary_avg_2020_21 = mean(salary_avg_2020_21, na.rm = T),
                salary_avg_2021_22 = mean(salary_avg_2021_22, na.rm = T),
                salary_avg_2022_23 = mean(salary_avg_2022_23, na.rm = T),
                salary_avg_2023_24 = mean(salary_avg_2023_24, na.rm = T)
            )
        
    })
    
    # Total budget/expenditure plot
    output$total_exp_plot <- renderPlotly({
        
        ## Manually set color scale depending on whether the district, the average of the comparison
        ## districts, or both have complete data (doing this makes district always gold and comp districts
        ## always blue regardless of data availability)
        if (sum(!is.nan(plot_comp_df()$total_exp)) == 2){
            palette <- c("#DB9743","#2A3A6E")
        }
        else if (!is.nan(plot_comp_df()$total_exp[plot_comp_df()$is_district == input$district])){
            palette <- c("#DB9743")
        }
        else{
            palette <- c("#2A3A6E")
        }
        
        userplot <- plot_ly(
            colors = palette,
            x = plot_comp_df()$total_exp,
            y = plot_comp_df()$is_district,
            color = plot_comp_df()$short_district,
            type = "bar",
            hovertemplate = paste(plot_comp_df()$is_district, "Total<br>$%{x:.3s}<extra></extra>"),
            texttemplate = '$%{x:.2s}', textposition = 'inside',
            textfont = list(color = "white", size = 18),
            height = 350) %>%
            layout(
                barmode = "overlay",
                title = "Total Budget",
                yaxis = list(showticklabels = F),
                showlegend = F
            ) %>%
            # This trace adds the salary outline to show the % of the budget going to teacher salaries
            add_trace(x = plot_comp_df()$salary_total, name = "Salaries",
                      marker = list(color = 'transparent', line = list(color = 'black', width = 3)),
                      texttemplate = "", hovertemplate = paste0("Salaries<br>", round(plot_comp_df()$salary_total/plot_comp_df()$total_exp*100), "%<extra></extra>"))
        
    })
    
    # Per-pupil expenditure plot
    output$per_pupil_exp_plot <- renderPlotly({
        
        ## Manually set color scale depending on whether the district, the average of the comparison
        ## districts, or both have complete data (doing this makes district always gold and comp districts
        ## always blue regardless of data availability)
        if (sum(!is.nan(plot_comp_df()$total_exp_per_pupil)) == 2){
            palette <- c("#DB9743","#2A3A6E")
        }
        else if (!is.nan(plot_comp_df()$total_exp_per_pupil[plot_comp_df()$is_district == input$district])){
            palette <- c("#DB9743")
        }
        else{
            palette <- c("#2A3A6E")
        }
        
        userplot <- plot_ly(
            colors = palette,
            y = plot_comp_df()$total_exp_per_pupil,
            x = plot_comp_df()$is_district,
            color = plot_comp_df()$is_district,
            type = "bar",
            hovertemplate = paste(plot_comp_df()$is_district, "<br>$%{y:.3s}<extra></extra>"),
            texttemplate = '$%{y:.2s}', textposition = 'inside',
            textfont = list(color = "white", size = 18),
            height = 225) %>%
            layout(
                title = "Per-Pupil Expenditure",
                xaxis = list(showticklabels = F),
                showlegend = F
            )
        
    })
    
    # Average teacher salary plot
    output$average_salary_plot <- renderPlotly({
        
        ## Manually set color scale depending on whether the district, the average of the comparison
        ## districts, or both have complete data (doing this makes district always gold and comp districts
        ## always blue regardless of data availability)
        if (sum(!is.nan(plot_comp_df()$average_salary)) == 2){
            palette <- c("#DB9743","#2A3A6E")
        }
        else if (!is.nan(plot_comp_df()$average_salary[plot_comp_df()$is_district == input$district])){
            palette <- c("#DB9743")
        }
        else{
            palette <- c("#2A3A6E")
        }
        
        userplot <- plot_ly(
            colors = palette,
            y = plot_comp_df()$average_salary,
            x = plot_comp_df()$is_district,
            color = plot_comp_df()$is_district,
            type = "bar",
            hovertemplate = paste(plot_comp_df()$is_district, "<br>$%{y:.3s}<extra></extra>"),
            texttemplate = '$%{y:.2s}', textposition = 'inside',
            textfont = list(color = "white", size = 18),
            height = 225) %>%
            layout(
                title = "Average Teacher Salary",
                xaxis = list(showticklabels = F),
                showlegend = F
            )
        
    })
    
    # Total number of students plot
    output$num_students_plot <- renderPlotly({
        
        ## Manually set color scale depending on whether the district, the average of the comparison
        ## districts, or both have complete data (doing this makes district always gold and comp districts
        ## always blue regardless of data availability)
        if (sum(!is.nan(plot_comp_df()$total_enrollment)) == 2){
            palette <- c("#DB9743","#2A3A6E")
        }
        else if (!is.nan(plot_comp_df()$total_enrollment[plot_comp_df()$is_district == input$district])){
            palette <- c("#DB9743")
        }
        else{
            palette <- c("#2A3A6E")
        }
        
        userplot <- plot_ly(
            colors = palette,
            y = plot_comp_df()$total_enrollment,
            x = plot_comp_df()$is_district,
            color = plot_comp_df()$is_district,
            type = "bar",
            hovertemplate = paste(plot_comp_df()$is_district, "<br>", round((plot_comp_df() %>% pull(total_enrollment))), "<extra></extra>"),
            texttemplate = '%{y:.2s}', textposition = 'inside',
            textfont = list(color = "white", size = 18),
            height = 225) %>%
            layout(
                title = "Total Enrollment",
                xaxis = list(showticklabels = F),
                showlegend = F
            )
        
    })
    
    # AP scores plot
    output$ap_plot <- renderPlotly({
        
        ## Manually set color scale depending on whether the district, the average of the comparison
        ## districts, or both have complete data (doing this makes district always gold and comp districts
        ## always blue regardless of data availability)
        if (sum(!is.nan(plot_comp_df()$ap_score_3_5_pct)) == 2){
            palette <- c("#DB9743","#2A3A6E")
        }
        else if (!is.nan(plot_comp_df()$ap_score_3_5_pct[plot_comp_df()$is_district == input$district])){
            palette <- c("#DB9743")
        }
        else{
            palette <- c("#2A3A6E")
        }
        
        userplot <- plot_ly(
            colors = palette,
            y = plot_comp_df()$ap_score_3_5_pct,
            x = plot_comp_df()$is_district,
            color = plot_comp_df()$is_district,
            type = "bar",
            hovertemplate = paste(plot_comp_df()$is_district, "<br>%{y:.3s}%<extra></extra>"),
            texttemplate = '%{y:.2s}%', textposition = 'inside',
            textfont = list(color = "white", size = 18),
            height = 225) %>%
            layout(
                title = "Advanced Placement",
                xaxis = list(showticklabels = F),
                yaxis = list(title = "% of Scores 3-5"),
                showlegend = F
            )
        
    })
    
    # Total teachers plot
    output$teacher_fte_plot <- renderPlotly({
        
        ## Manually set color scale depending on whether the district, the average of the comparison
        ## districts, or both have complete data (doing this makes district always gold and comp districts
        ## always blue regardless of data availability)
        if (sum(!is.nan(plot_comp_df()$total_teachers)) == 2){
            palette <- c("#DB9743","#2A3A6E")
        }
        else if (!is.nan(plot_comp_df()$total_teachers[plot_comp_df()$is_district == input$district])){
            palette <- c("#DB9743")
        }
        else{
            palette <- c("#2A3A6E")
        }
        
        userplot <- plot_ly(
            colors = palette,
            y = plot_comp_df()$total_teachers,
            x = plot_comp_df()$is_district,
            color = plot_comp_df()$is_district,
            type = "bar",
            hovertemplate = paste(plot_comp_df()$is_district, "<br>", round((plot_comp_df() %>% pull(total_teachers)),1), "<extra></extra>"),
            texttemplate = '%{y:.3s}', textposition = 'inside',
            textfont = list(color = "white", size = 18),
            height = 225) %>%
            layout(
                title = "Total Teachers",
                xaxis = list(showticklabels = F),
                showlegend = F
            )
        
    })
    
    # SAT scores plot
    output$sat_plot <- renderPlotly({
        
        ## Manually set color scale depending on whether the district, the average of the comparison
        ## districts, or both have complete data (doing this makes district always gold and comp districts
        ## always blue regardless of data availability)
        if (sum(!is.nan(plot_comp_df()$sat_reading_writing)) == 2 | sum(!is.nan(plot_comp_df()$sat_math)) == 2){
            palette <- c("#DB9743","#2A3A6E")
        }
        else if (!is.nan(plot_comp_df()$sat_reading_writing[plot_comp_df()$is_district == input$district]) | !is.nan(plot_comp_df()$sat_math[plot_comp_df()$is_district == input$district])){
            palette <- c("#DB9743")
        }
        else{
            palette <- c("#2A3A6E")
        }
        
        userplot <- plot_ly(
            colors = palette,
            x = plot_comp_df()$sat_reading_writing,
            y = plot_comp_df()$sat_math,
            type = "scatter",
            mode = "markers",
            marker = list(
                size = 25,
                opacity = .75
            ),
            color = plot_comp_df()$short_district,
            hovertemplate = paste((plot_comp_df() %>% pull(is_district)),
                                  "<br>",
                                  "SAT Reading/Writing:",
                                  (round(plot_comp_df() %>% pull(sat_reading_writing),1)),
                                  "<br>",
                                  "SAT Math:",
                                  (round(plot_comp_df() %>% pull(sat_math),1)),
                                  "<extra></extra>"),
            height = 225) %>%
            layout(yaxis = list(title = "Math",
                                range = list(0,800)),
                   xaxis = list(title = "Reading/Writing",
                                range = list(0,800)),
                   title = "SAT Performance",
                   showlegend = F)
    })
    
    # MCAS scores plot
    output$mcas_plot <- renderPlotly({
        
        ## Pivot data longer for grouped bars
        mcas_df <- plot_comp_df() %>%
            select(is_district, ela_advanced_proficient_pct:sci_advanced_proficient_pct) %>%
            pivot_longer(cols = ends_with("_advanced_proficient_pct"),
                         names_to = "test",
                         names_pattern = "(.*)_advanced_proficient_pct",
                         values_to = "score") %>%
            mutate(test = case_when(
                test == "mth" ~ "Math",
                test == "sci" ~ "Science",
                test == "ela" ~ "ELA"
            ))
        
        ## Manually set color scale depending on whether the district, the average of the comparison
        ## districts, or both have complete data (doing this makes district always gold and comp districts
        ## always blue regardless of data availability)
        if (sum(!is.nan(mcas_df$score[mcas_df$is_district == "Others"])) > 0 & sum(!is.nan(mcas_df$score[mcas_df$is_district == input$district])) > 0){
            palette <- c("#DB9743","#2A3A6E")
        }
        else if (sum(!is.nan(mcas_df$score[mcas_df$is_district == input$district])) > 0){
            palette <- c("#DB9743")
        }
        else{
            palette <- c("#2A3A6E")
        }
        
        userplot <- plot_ly(
            colors = palette,
            data = mcas_df,
            x = mcas_df$test,
            y = mcas_df$score,
            color = mcas_df$is_district,
            type = "bar",
            hovertemplate = paste(mcas_df$is_district, "<br>%{y:.3s}%<extra></extra>"),
            texttemplate = '%{y:.2s}%', textposition = 'inside',
            textfont = list(color = "white", size = 18),
            height = 225) %>%
            layout(
                yaxis = list(title = "% Advanced/Proficient"),
                title = "10th Grade MCAS",
                showlegend = F
            )
        
    })
    
    # Student demographics plot
    output$student_demo_plot <- renderPlotly({
        
        ## Manually set color scale depending on whether the district, the average of the comparison
        ## districts, or both have complete data (doing this makes district always gold and comp districts
        ## always blue regardless of data availability)
        if (sum(!is.nan(plot_comp_df()$english_learner_pct)) == 2 | sum(!is.nan(plot_comp_df()$low_income_pct)) == 2){
            palette <- c("#DB9743","#2A3A6E")
        }
        else if (!is.nan(plot_comp_df()$english_learner_pct[plot_comp_df()$is_district == input$district]) | !is.nan(plot_comp_df()$low_income_pct[plot_comp_df()$is_district == input$district])){
            palette <- c("#DB9743")
        }
        else{
            palette <- c("#2A3A6E")
        }
        
        
        userplot <- plot_ly(
            colors = palette,
            x = plot_comp_df()$english_learner_pct,
            y = plot_comp_df()$low_income_pct,
            type = "scatter",
            mode = "markers",
            marker = list(
                size = 25,
                opacity = .75
            ),
            color = plot_comp_df()$short_district,
            hovertemplate = paste((plot_comp_df() %>% pull(is_district)),
                                  "<br>",
                                  "English Learner:",
                                  (round(plot_comp_df() %>% pull(english_learner_pct),1)),
                                  "%<br>",
                                  "Low-Income:",
                                  (round(plot_comp_df() %>% pull(low_income_pct),1)),
                                  "%<extra></extra>"),
            height = 225) %>%
            layout(yaxis = list(title = "Low-Income %",
                                range = list(0,100)),
                   xaxis = list(title = "English Learner %",
                                range = list(0,100)),
                   title = "Student Demographics",
                   showlegend = F)
    })
    
    # Contract average salary plot
    output$yearly_salary_plot <- renderPlotly({
        
        year_n <- str_replace(input$comp_year, "-", "_")
        first_year <- as.numeric(substring(input$comp_year, 1, 4)) + 2
        second_year <- as.numeric(substring(input$comp_year, 6, 8)) + 2
        year_n_2 <- paste0(first_year, "_", second_year)
        
        ## Pivot data longer for grouped bars
        salary_avg_df <- plot_contract_df() %>%
            select(is_district, as.name(paste0("salary_avg_", year_n)):as.name(paste0("salary_avg_", year_n_2))) %>%
            pivot_longer(cols = starts_with("salary_avg_"),
                         names_to = "year",
                         names_prefix = "salary_avg_",
                         values_to = "salary") %>%
            mutate(year = str_replace(year, "_", "-"))
        
        ## Manually set color scale depending on whether the district, the average of the comparison
        ## districts, or both have complete data (doing this makes district always gold and comp districts
        ## always blue regardless of data availability)
        if (sum(!is.nan(salary_avg_df$salary[salary_avg_df$is_district == input$district])) > 0 & sum(!is.nan(salary_avg_df$salary[salary_avg_df$is_district == "Others"])) > 0){
            palette <- c("#DB9743","#2A3A6E")
        }
        else if (sum(!is.nan(salary_avg_df$salary[salary_avg_df$is_district == input$district])) > 0){
            palette <- c("#DB9743")
        }
        else{
            palette <- c("#2A3A6E")
        }
        
        userplot <- plot_ly(
            colors = palette,
            x = salary_avg_df$year,
            y = salary_avg_df$salary,
            color = salary_avg_df$is_district,
            type = "scatter",
            mode = "lines+markers",
            hovertemplate = paste(salary_avg_df$is_district, "<br>$%{y:.3s}<extra></extra>"),
            textfont = list(color = "white", size = 18),
            height = 225) %>%
            layout(
                yaxis = list(title = "",
                             range = list(65000,80000)),
                title = "Average Salary",
                xaxis = list(title = ""),
                showlegend = F
            )
    })
    
    # COLA plot
    output$cola_plot <- renderPlotly({
        
        year_n <- str_replace(input$comp_year, "-", "_")
        first_year <- as.numeric(substring(input$comp_year, 1, 4)) + 2
        second_year <- as.numeric(substring(input$comp_year, 6, 8)) + 2
        year_n_2 <- paste0(first_year, "_", second_year)
        
        ## Pivot longer for grouped bars
        cola_df <- plot_contract_df() %>%
            select(is_district, as.name(paste0("cola_", year_n)):as.name(paste0("cola_", year_n_2))) %>%
            pivot_longer(cols = starts_with("cola_"),
                         names_to = "year",
                         names_prefix = "cola_",
                         values_to = "cola") %>%
            mutate(year = str_replace(year, "_", "-"))
        
        print(cola_df$is_district == "Others")
        
        ## Manually set color scale depending on whether the district, the average of the comparison
        ## districts, or both have complete data (doing this makes district always gold and comp districts
        ## always blue regardless of data availability)
        if (sum(!is.nan(cola_df$cola[cola_df$is_district == input$district])) > 0 & sum(!is.nan(cola_df$cola[cola_df$is_district == "Others"])) > 0){
            palette <- c("#DB9743","#2A3A6E")
        }
        else if (sum(!is.nan(cola_df$cola[cola_df$is_district == input$district])) > 0){
            palette <- c("#DB9743")
        }
        else{
            palette <- c("#2A3A6E")
        }
        
        userplot <- plot_ly(
            colors = palette,
            x = cola_df$cola,
            y = cola_df$year,
            color = cola_df$is_district,
            type = "bar",
            hovertemplate = paste(cola_df$is_district, "<br>%{x:.2%}<extra></extra>"),
            texttemplate = '%{x:.1%}', textposition = 'inside',
            textfont = list(color = "white", size = 18),
            height = 225) %>%
            layout(
                yaxis = list(title = ""),
                title = "COLA",
                showlegend = F
            )
        
    })
    
    # Upper-left salary plot
    output$upper_left_salary_plot <- renderPlotly({
        
        year_n <- str_replace(input$comp_year, "-", "_")
        first_year <- as.numeric(substring(input$comp_year, 1, 4)) + 2
        second_year <- as.numeric(substring(input$comp_year, 6, 8)) + 2
        year_n_2 <- paste0(first_year, "_", second_year)
        
        upper_left_df <- plot_contract_df() %>%
            select(is_district, as.name(paste0("upper_left_", year_n)):as.name(paste0("upper_left_", year_n_2))) %>%
            pivot_longer(cols = starts_with("upper_left_"),
                         names_to = "year",
                         names_prefix = "upper_left_",
                         values_to = "upper_left") %>%
            mutate(year = str_replace(year, "_", "-"))
        
        ## Manually set color scale depending on whether the district, the average of the comparison
        ## districts, or both have complete data (doing this makes district always gold and comp districts
        ## always blue regardless of data availability)
        if (sum(!is.nan(upper_left_df$upper_left[upper_left_df$is_district == input$district])) > 0 & sum(!is.nan(upper_left_df$upper_left[upper_left_df$is_district == "Others"])) > 0){
            palette <- c("#DB9743","#2A3A6E")
        }
        else if (sum(!is.nan(upper_left_df$upper_left[upper_left_df$is_district == input$district])) > 0){
            palette <- c("#DB9743")
        }
        else{
            palette <- c("#2A3A6E")
        }
        
        userplot <- plot_ly(
            colors = palette,
            x = upper_left_df$upper_left,
            y = upper_left_df$year,
            color = upper_left_df$is_district,
            type = "bar",
            hovertemplate = paste(upper_left_df$is_district, "<br>$%{x:.3s}<extra></extra>"),
            texttemplate = '$%{x:.2s}', textposition = 'inside',
            textfont = list(color = "white", size = 18),
            height = 225) %>%
            layout(
                yaxis = list(title = ""),
                title = "Upper-Left Salary",
                showlegend = F
            )
        
    })
    
    # Lower-right salary plot
    output$lower_right_salary_plot <- renderPlotly({
        
        year_n <- str_replace(input$comp_year, "-", "_")
        first_year <- as.numeric(substring(input$comp_year, 1, 4)) + 2
        second_year <- as.numeric(substring(input$comp_year, 6, 8)) + 2
        year_n_2 <- paste0(first_year, "_", second_year)
        
        ## Pivot longer for grouped bars
        lower_right_df <- plot_contract_df() %>%
            select(is_district, as.name(paste0("lower_right_", year_n)):as.name(paste0("lower_right_", year_n_2))) %>%
            pivot_longer(cols = starts_with("lower_right_"),
                         names_to = "year",
                         names_prefix = "lower_right_",
                         values_to = "lower_right") %>%
            mutate(year = str_replace(year, "_", "-"))
        
        ## Manually set color scale depending on whether the district, the average of the comparison
        ## districts, or both have complete data (doing this makes district always gold and comp districts
        ## always blue regardless of data availability)
        if (sum(!is.nan(lower_right_df$lower_right[lower_right_df$is_district == input$district])) > 0 & sum(!is.nan(lower_right_df$lower_right[lower_right_df$is_district == "Others"])) > 0){
            palette <- c("#DB9743","#2A3A6E")
        }
        else if (sum(!is.nan(lower_right_df$lower_right[lower_right_df$is_district == input$district])) > 0){
            palette <- c("#DB9743")
        }
        else{
            palette <- c("#2A3A6E")
        }
        
        userplot <- plot_ly(
            colors = palette,
            x = lower_right_df$lower_right,
            y = lower_right_df$year,
            color = lower_right_df$is_district,
            type = "bar",
            hovertemplate = paste(lower_right_df$is_district, "<br>$%{x:.4s}<extra></extra>"),
            texttemplate = '$%{x:.3s}', textposition = 'inside',
            textfont = list(color = "white", size = 18),
            height = 225) %>%
            layout(
                yaxis = list(title = ""),
                title = "Lower-Right Salary",
                showlegend = F
            )
        
    })
    
    ### OVERVIEW TAB
    
    # Create data frame with comparison of district and comparison districts
    # NOTE: User can choose to see individual comp districts or aggregated
    # comp districts using the input$individuals parameter (used in if statements below)
    comp_df <- reactive({
        
        district <- input$district
        comp_districts <- input$comp_districts
        comp_year <- input$comp_year
        
        # If comp districts are grouped...
        if (input$individuals == "No"){
            
            # Create data frame of comparison districts and rename columns
            comp_df <- school_info %>%
                filter(district_name %in% comp_districts,
                       year == comp_year) %>%
                select(total_enrollment:total_exp_per_pupil) %>%
                magrittr::set_colnames(overview_statistics) %>%
                as.data.frame()
            
            # Create data frame of user's district and rename columns
            district_df <- school_info %>%
                filter(district_name == district,
                       year == comp_year) %>%
                select(total_enrollment:total_exp_per_pupil) %>%
                magrittr::set_colnames(overview_statistics) %>%
                as.data.frame()
            
            # Iterate thru columns and for each, create a row with the average of
            # comp districts, value for the user's district, and % difference between
            # the two - as I said on our call, there's probably a vectorized way to
            # do this, but I never had time to make more efficient
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
        # If comp districts are not grouped...
        else{
            
            # Create data frame with only user's district and chosen comp districts
            comp_df <- school_info %>%
                filter(district_name %in% comp_districts | district_name == district,
                       year == comp_year)
            
            district_names <- comp_df$district_name
            new_df_names <- c()
            
            # Create names for new data frame with '{district name}' and '{district name} Diff'
            # to record both value and % difference from user's district
            for (i in 1:nrow(comp_df)){
                new_df_names <- append(new_df_names, comp_df$district_name[i])
                new_df_names <- append(new_df_names, paste0(comp_df$district_name[i], " Diff"))
            }
            
            # Transpose comparison df so columns are the districts and rows are the statistics of interest
            comp_df <- comp_df %>%
                select(total_enrollment:total_exp_per_pupil) %>%
                t() %>%
                magrittr::set_colnames(district_names) %>%
                magrittr::set_rownames(overview_statistics) %>%
                as.data.frame()
            
            # Create data frame to store comp district values and % differences
            value_column <- as.vector(comp_df[district])
            differences_df <- data.frame(value = c(overview_statistics),
                                         diff = c(value_column))
            colnames(differences_df) <- c(district, paste0(district, " Diff"))
            
            # Iterate thru districts and for each, create a column for the district
            # values and % differences
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
        
        ## YOU NOW HAVE A DF WITH A COLUMN FOR EACH DISTRICT AND EACH DISTRICT % DIFF FROM USER'S DISTRICT
        differences_df
    })
    
    # Now use the data frame from above to generate a pretty {gt} table for comparisons
    comp_table <- reactive({
        
        district <- input$district
        comp_districts <- input$comp_districts
        comp_year <- input$comp_year
        
        # If comp districts are grouped...
        if (input$individuals == "No"){
            
            len_comps <- length(comp_districts)
            
            # Create list of comp districts for table source note
            # NOTE: There might be a better way to do this
            for (i in 1:len_comps){
                if (i == 1){
                    comp_list <- comp_districts[i]
                }
                else{
                    comp_list <- paste0(comp_list, ", ", comp_districts[i])
                }
            }
            
            # Build {gt} table
            comp_df() %>%
                mutate(
                    # Replace NaNs with NAs (this was causing problems earlier)
                    value = ifelse(is.nan(value), NA_real_, as.numeric(value)),
                    mean = ifelse(is.nan(mean), NA_real_, as.numeric(mean)),
                    diff = ifelse(is.nan(diff), NA_real_, as.numeric(diff))
                ) %>%
                # Arrange in order of columns so we can do row groupings later
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
                # Color palette is such that anything 1-100% is lighter hue; anything
                # greater in magnitude is dark
                data_color(
                    columns = 5, 
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
        
        # If comp districts aren't grouped...
        else{
            
            # Create list of columns that will store % values
            # Need for table formatting later
            if (ncol(comp_df()) > 2){
                pct_cols <- seq(4, ncol(comp_df()), 2)
            }
            else{
                pct_cols <- c()
            }
            
            # Create list of columns that will store measured statistics
            # Need for table formatting later
            if (ncol(comp_df()) > 2){
                value_cols <- append(2, seq(3, ncol(comp_df()), 2))
            }
            else{
                value_cols <- c(2)
            }
            
            # Get column indexes for renaming purposes later
            # Easier to refer to index with dynamic user input
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
            
            # Column labels
            # NOTE: First two columns are the names of the statistics and the 
            # values for the district itself, so we set those right off the bat
            col_labs <- c("",district)
            
            # Again, we use the convention of '{district name}' and '{district name} % Diff'
            if (length(comp_districts_adjusted) > 0){
                for (i in 1:length(comp_districts_adjusted)){
                    col_labs <- append(col_labs, c(comp_districts_adjusted[i], "% Diff"))
                }
            }
            
            # Finally, create named vector where column indexes correspond to names from above
            col_labs <- set_names(col_labs, col_indexes)
            
            # Build {gt} table
            comp_df() %>%
                # Arrange in order of columns so we can do row groupings later
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
                # Set column names to indexes
                magrittr::set_colnames(col_indexes) %>%
                gt() %>%
                # Set column labels
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
                # Color palette is such that anything 1-100% is lighter hue; anything
                # greater in magnitude is dark
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
                # Hacky way to ensure that all '% Diff' columns are the same size no matter how
                # many comp districts the user enters
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
    
    # Actually render the {gt} table (need {gt} object to save in next function)
    output$comparisons_table <- render_gt({comp_table()})
    
    # Allow table download as png
    output$download_table <- downloadHandler(
        filename = "district_comparison.png",
        content = function(file) {
            gtsave(comp_table(), file)
        }
    )
    
    # Allow table download as csv
    output$download_csv <- downloadHandler(
        filename = "district_comparison.csv",
        content = function(file) {
            write.csv(comp_df(), file)
        }
    )
    
    ### PLOTS TAB
    
    # Observe X-axis category selected and populate dropdown
    observeEvent(input$x_cat, {
        
        if (input$x_cat == "Enrollment"){
            updateSelectizeInput(session, input = "x", choices = axis_options[6:21])
        }
        else if (input$x_cat == "Teachers/Classes"){
            updateSelectizeInput(session, input = "x", choices = axis_options[22:29])
        }
        else if (input$x_cat == "Salaries/Expenditure"){
            updateSelectizeInput(session, input = "x", choices = axis_options[30:37])
        }
        else if (input$x_cat == "10th Grade MCAS"){
            updateSelectizeInput(session, input = "x", choices = axis_options[38:70])
        }
        else if (input$x_cat == "SAT/AP"){
            updateSelectizeInput(session, input = "x", choices = axis_options[71:81])
        }
        else if (input$x_cat == "Basic Contract"){
            updateSelectizeInput(session, input = "x", choices = axis_options[82:132])
        }
        else if (input$x_cat == "Working Conditions"){
            updateSelectizeInput(session, input = "x", choices = axis_options[133:163])
        }
        else if (input$x_cat == "Student Population"){
            updateSelectizeInput(session, input = "x", choices = axis_options[164:173])
        }
    })
    
    # Observe Y-axis category selected and populate dropdown
    observeEvent(input$y_cat, {
        
        if (input$y_cat == "Enrollment"){
            updateSelectizeInput(session, input = "y", choices = axis_options[6:21])
        }
        else if (input$y_cat == "Teachers/Classes"){
            updateSelectizeInput(session, input = "y", choices = axis_options[22:29])
        }
        else if (input$y_cat == "Salaries/Expenditure"){
            updateSelectizeInput(session, input = "y", choices = axis_options[30:37])
        }
        else if (input$y_cat == "10th Grade MCAS"){
            updateSelectizeInput(session, input = "y", choices = axis_options[38:70])
        }
        else if (input$y_cat == "SAT/AP"){
            updateSelectizeInput(session, input = "y", choices = axis_options[71:81])
        }
        else if (input$y_cat == "Basic Contract"){
            updateSelectizeInput(session, input = "y", choices = axis_options[82:132])
        }
        else if (input$y_cat == "Working Conditions"){
            updateSelectizeInput(session, input = "y", choices = axis_options[133:163])
        }
        else if (input$y_cat == "Student Population"){
            updateSelectizeInput(session, input = "y", choices = axis_options[164:173])
        }
        
    })
    
    # Put data in df to build plots
    plotdata <- reactive({
        
        district <- input$district
        comp_districts <- input$comp_districts
        comp_year <- input$comp_year
        y <- input$y
        
        plot_data <- school_info %>%
            filter(district_name %in% comp_districts | district_name == district,
                   year == comp_year, !is.na(get(y))) %>%
            mutate(is_district = ifelse(district_name == district, district, "Others"),
                   short_district = stringr::str_trunc(district_name, 20, "right"))
        
    })
    
    # Build plots
    # NOTE: User can select bar, scatter, or pie
    output$userplot <- renderPlotly({
        
        district <- input$district
        
        if (input$plot_type == "Scatter"){
            
            x <- plotdata() %>% pull(input$x)
            y <- plotdata() %>% pull(input$y)

            userplot <- plot_ly(
                x = x,
                y = y,
                type = "scatter",
                mode = "markers",
                marker = list(
                    size = 30,
                    opacity = .5
                ),
                color = plotdata()$short_district,
                hovertemplate = paste((plotdata() %>% pull(district_name)),
                                      "<br>",
                                      names(axis_options)[axis_options == input$x], ":",
                                      (plotdata() %>% pull(input$x)),
                                      "<br>",
                                      names(axis_options)[axis_options == input$y], ":",
                                      (plotdata() %>% pull(input$y)),
                                      "<extra></extra>"),
                height = 750) %>%
                layout(yaxis = list(title = names(axis_options)[axis_options == input$y]),
                       xaxis = list(title = names(axis_options)[axis_options == input$x]),
                       title = paste(district, "vs. Comparison Districts",
                                     "<br><sup>", names(axis_options)[axis_options == input$y],
                                     "vs.", names(axis_options)[axis_options == input$x], input$comp_year, "</sup>"),
                       images = list(
                           source = base64enc::dataURI(file = "www/small_logo.png"),
                           x = 0, y = 1,
                           sizex = 0.2, sizey = 0.1
                       ))
            
        }
        
        else if (input$plot_type == "Pie"){
            
            y <- plotdata() %>% pull(input$y)
            x <- factor(plotdata() %>% pull(district_name), levels = unique(plotdata() %>% pull(district_name))[order(plotdata() %>% pull(input$y))])
            y_mean <- mean(y, na.rm = T)
            
            userplot <- plot_ly(
                labels = x,
                values = y,
                type = "pie",
                # color = plotdata()$short_district,
                hovertemplate = paste((plotdata() %>% pull(district_name)),
                                      "<br>",
                                      names(axis_options)[axis_options == input$y], ":",
                                      y, "<extra></extra>"),
                height = 750) %>%
                layout(title = paste(district, "vs. Comparison Districts",
                                     "<br><sup>", names(axis_options)[axis_options == input$y], input$comp_year, "</sup>"),
                       legend = list(font = list(size = 11)),
                       images = list(
                           source = base64enc::dataURI(file = "www/small_logo.png"),
                           x = 0, y = 1,
                           sizex = 0.2, sizey = 0.1
                       ))
            
        }
        
        else{
            
            y <- plotdata() %>% pull(input$y)
            x <- factor(plotdata() %>% pull(district_name), levels = unique(plotdata() %>% pull(district_name))[order(plotdata() %>% pull(input$y))])
            district_short <- factor(plotdata() %>% pull(short_district), levels = unique(plotdata() %>% pull(short_district))[order(plotdata() %>% pull(input$y))])
            y_mean <- mean(y, na.rm = T)
            
            userplot <- plot_ly(
                x = x,
                y = y,
                type = "bar",
                color = district_short,
                hovertemplate = paste((plotdata() %>% pull(district_name)),
                                      "<br>",
                                      names(axis_options)[axis_options == input$y], ":",
                                      y, "<extra></extra>"),
                texttemplate = '%{y:.2s}', textposition = 'outside',
                height = 750) %>%
                layout(xaxis = list(title = "District", showticklabels = F),
                       yaxis = list(title = names(axis_options)[axis_options == input$y]),
                       title = paste(district, "vs. Comparison Districts",
                                     "<br><sup>", names(axis_options)[axis_options == input$y], input$comp_year, "</sup>"),
                       legend = list(font = list(size = 11)),
                       annotations = list(
                           x = sort(x)[1],
                           y = y_mean,
                           text = paste("Avg:", round(y_mean,1)),
                           xref = "x",
                           yref = "y",
                           showarrow = TRUE,
                           arrowhead = 7,
                           ax = 20,
                           ay = -40
                       ),
                       shapes = list(type='line', x0 = sort(x)[1], x1 = sort(x)[nrow(plotdata())], y0=y_mean, y1=y_mean, line=list(dash='dot', width=1)),
                       images = list(
                           source = base64enc::dataURI(file = "www/small_logo.png"),
                           x = 0, y = 1,
                           sizex = 0.2, sizey = 0.1
                       ))
            
        }
        
    })
    
    ### BASIC IMAGE RENDERINGS/JAVASCRIPT TO TOGGLE DISTRICT SELECTOR
    
    observeEvent(input$toggle_sidebar, {
        shinyjs::toggle(id = "Sidebar")
    })
    
    output$logo <- renderImage({
        
        list(src = "www/small_logo.png",
             contentType = 'image/png',
             alt = "")
    }, deleteFile = FALSE)
    
    output$legend <- renderImage({
        
        list(src = "www/legend_salaries.png",
             contentType = 'image/png',
             alt = "")
    }, deleteFile = FALSE)
    
    output$legend2 <- renderImage({
        
        list(src = "www/legend.png",
             contentType = 'image/png',
             alt = "")
    }, deleteFile = FALSE)
    
}