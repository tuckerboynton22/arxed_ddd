library(shiny)
library(tidyverse)
library(gt)
library(webshot)
library(shinyTree)
library(lazyeval)
library(shinyjs)
library(plotly)
library(shinyscreenshot)

options(scipen = 99999)

## Have to keep this for gtsave() to work on Shiny
webshot::install_phantomjs()

## Read in full school info data frame
school_info <- read_csv("school_info.csv") %>%
    mutate(
        district_name = case_when(
            district_name == "Seven Hills Charter Public (District)" ~ "Learning First Charter Public School (District)",
            district_name == "Massachusetts Virtual Academy at Greenfield Commonwealth Virtual District" ~ "Greater Commonwealth Virtual District",
            district_name == "Tri County Regional Vocational Technical" ~ "Tri-County Regional Vocational Technical",
            district_name == "Greater Commonwealth Virtual District" ~ "Greenfield Commonwealth Virtual District",
            district_name == "Southern Worcester County Regional Vocational School District" ~ "Southern Worcester County Regional Vocational Technical",
            TRUE ~ district_name)
    )

## Create vector of district names for dropdown menus
district_names <- school_info %>%
    filter(district_name != "State Total", district_name != "State Totals") %>%
    select(district_name) %>%
    distinct() %>%
    rename(`District` = district_name)

## Create vector of academic years for dropdown menus
years <- school_info %>%
    filter(!is.na(year)) %>%
    select(year) %>%
    arrange(desc(year)) %>%
    distinct() %>%
    rename(Year = year)

## Create vector of major stat categories for dropdown menus
stat_categories <- c(
    "Enrollment",
    "Teachers/Classes",
    "Salaries/Expenditure",
    "10th Grade MCAS",
    "SAT/AP",
    "Basic Contract",
    "Working Conditions",
    "Student Population"
)

# Define UI for application that draws a histogram
function(request){
    shinyUI(fluidPage(
        tags$head(
            tags$style(
            
            ## Import Chivo font (this was my personal choice; they may wish to use something different eventually)
            HTML("@import url('https://fonts.googleapis.com/css2?family=Chivo&display=swap');
                        
                        /* Change header text to imported font */
                        .tbl, h5, h3, h1 {
                        font-family: 'Chivo';
                        text-align: center;
                        }"),
            
            ## Format images such that they resolve aspect ratio conflicts automatically
            # Change max-width and width params to adjust width of image (relative to column width)
           "#logo img {max-width: 100%; width: auto; height: auto; display: block; margin-left: auto; margin-right: auto}",
           "#district_logo img {max-width: 100%; width: auto; height: auto}",
           "#legend2 img {max-width: 35%; width: 35%; height: auto; display: block; margin-left: auto; margin-right: auto}",
           "#legend img {max-width: 10%; width: 10%; height: auto; display: block; margin-left: auto}"

            ),
            tags$link(rel = "icon", type = "image/png", href = "small_logo.png")
        ),
        
        ## Un-comment this if you want to customize error message
        # tags$head(tags$style(".shiny-output-error{visibility: hidden}")),
        # 
        # tags$head(tags$style(".shiny-output-error:after{content: 'Please select quarterback.'; visibility: visible}")),
        
        # Application title
        titlePanel(title=div(img(src = "logo.png", height = "15%", width = "15%"), "Data-Driven Decision"),
                   windowTitle="ArxEd | D3"),
        
        
        
        ## Sidebar to choose district, year, and comparisons
        useShinyjs(),
        sidebarLayout(
            sidebarPanel(div(id = "Sidebar",
                             column(4,
                                    selectizeInput("district", label = "1. Enter your district:",
                                        choices = district_names, selected = ""),
                                    selectizeInput("comp_year", label = "2. Enter the year you wish to examine:",
                                           choices = years, selected = "")),
                             column(4,
                                    ## NOTE: auto-select checkboxes broken into two columns for aesthetic reasons
                                    tags$label("3. Choose comparison district categories to auto-select:"),
                                    column(4,
                                           checkboxGroupInput("autoselect", label = "",
                                                              choices = c("DART", "League", "County", "Municipality"))),
                                    column(8,
                                           checkboxGroupInput("autoselect2", label = "",
                                                              choices = c("Total Budget within 5%",
                                                                          "Enrollment within 5%",
                                                                          "Teacher FTE within 5%",
                                                                          "Avg Teacher Salary within 5%")))),
                             column(4,
                                    checkboxGroupInput("filters", label = "4. Pare down your results with filters
                                                       (will exclude any district that fails to meet ANY of the following criteria):",
                                                       choices = c("Total Budget","Enrollment","Teacher FTE","Avg Teacher Salary")),
                                    
                                    ## Conditional panels only show up when user adds filter for that category
                                    conditionalPanel(
                                        condition = "input.filters[0] == 'Total Budget'",
                                        sliderInput(
                                            "budget",
                                            label = "Total Budget (millions):",
                                            min = 0,
                                            max = 1600,
                                            value = c(0,1600),
                                            sep = ",",
                                            pre = "$")),
                                    conditionalPanel(
                                        condition = "input.filters[0] == 'Enrollment' || input.filters[1] == 'Enrollment'",
                                        sliderInput(
                                            "enrollment",
                                            label = "Enrollment (hundreds):",
                                            min = 0,
                                            max = 500,
                                            value = c(0,500))),
                                    conditionalPanel(
                                        condition = "input.filters[0] == 'Teacher FTE' || input.filters[1] == 'Teacher FTE' || input.filters[2] == 'Teacher FTE'",
                                        sliderInput(
                                            "fte",
                                            label = "Teacher FTE:",
                                            min = 0,
                                            max = 4500,
                                            value = c(0,4500),
                                            sep = ",")),
                                    conditionalPanel(
                                        condition = "input.filters[0] == 'Avg Teacher Salary' || input.filters[1] == 'Avg Teacher Salary' || input.filters[2] == 'Avg Teacher Salary' || input.filters[3] == 'Avg Teacher Salary'",
                                        sliderInput(
                                            "salary",
                                            label = "Avg Teacher Salary (thousands):",
                                            min = 0,
                                            max = 125,
                                            value = c(0,125),
                                            pre = "$"))),
                             selectizeInput("comp_districts",
                                            label = "View your comparison districts and add/remove any manually:",
                                            choices = district_names, multiple = TRUE),
                             actionButton("reset", label = "Reset"),
                             bookmarkButton(label = "Save Inputs")),
                         actionButton("toggle_sidebar", label = "Show/Hide District Selector"), width = 12),
            mainPanel(
                navbarPage(markdown("ArxEd | D<sup>3</sup>"),
                           tabPanel("Home",
                                    fluidRow(
                                        column(style='padding:20px', width = 12,
                                               screenshotButton(label = 'Download as Image', filename = 'ddn_dashboard'))),
                                    fluidRow(
                                        column(3,
                                               gt_output("summary_table")),
                                        column(8,
                                               h1(textOutput("district")),
                                               h3("-----"),
                                               h3("Your District vs. Average of Selected Comparisons"),
                                               h5(textOutput("comps"))),
                                        column(1,
                                               imageOutput("logo", height = "auto"))
                                    ),
                                    fluidRow(
                                        column(style='border-right: 2px solid black; border-top: 2px solid black; padding:20px ', width = 3,
                                               h3("Contract Elements"),
                                               h5("-----"),
                                               h5("Your District vs. Average of All Other Districts"),
                                               h3("  "),
                                               imageOutput("legend2", height = "50px"),
                                               h3("  "),
                                               plotlyOutput("cola_plot", height = "auto")),
                                        column(style='padding:10px', width = 9,
                                               imageOutput("legend", height = "50px"),
                                               h3("  "),
                                               plotlyOutput("total_exp_plot", height = "auto"))
                                    ),
                                    fluidRow(
                                        column(style='border-right: 2px solid black; padding:20px', width = 3,
                                               plotlyOutput("upper_left_salary_plot", height = "auto")),
                                        column(style='padding:10px', width = 3,
                                               plotlyOutput("per_pupil_exp_plot", height = "auto")),
                                        column(style='padding:10px', width = 3,
                                               plotlyOutput("average_salary_plot", height = "auto")),
                                        column(style='padding:10px', width = 3,
                                               plotlyOutput("num_students_plot", height = "auto"))
                                    ),
                                    fluidRow(
                                        column(style='border-right: 2px solid black; padding:20px', width = 3,
                                               plotlyOutput("lower_right_salary_plot", height = "auto")),
                                        column(style='border-right: padding:10px', width = 3,
                                               plotlyOutput("sat_plot", height = "auto")),
                                        column(style='border-right: padding:10px', width = 3,
                                               plotlyOutput("ap_plot", height = "auto")),
                                        column(style='padding:10px', width = 3,
                                               plotlyOutput("teacher_fte_plot", height = "auto"))
                                    ),
                                    fluidRow(
                                        column(style='border-right: 2px solid black; padding:20px', width = 3,
                                               plotlyOutput("yearly_salary_plot", height = "auto")),
                                        column(style='padding:10px', width = 6,
                                               plotlyOutput("mcas_plot", height = "auto")),
                                        column(style='padding:10px', width = 3,
                                               plotlyOutput("student_demo_plot", height = "auto"))
                                    )
                                    ),
                           tabPanel("Overview",
                                    sidebarPanel(
                                        radioButtons("individuals", label = "Show individual comparison districts?", choices = c("No","Yes")),
                                        downloadButton("download_table", label = "Download Image"),
                                        downloadButton("download_csv", label = "Download CSV"),
                                        width = 2),
                                    mainPanel(gt_output("comparisons_table"), width = 10)),
                           tabPanel("Plots",
                                    fluidRow(
                                        column(style='padding:20px', width = 12,
                                               screenshotButton(label = 'Download as Image', filename = 'ddn_dashboard'))),
                                    sidebarPanel(
                                        selectizeInput("plot_type", label = "Plot type", choices = c("Bar","Scatter","Pie")),
                                        ## X-Axis option only available for scatter plots
                                        conditionalPanel(
                                            conditionalPanel(
                                                condition = "input.plot_type == 'Scatter'",
                                                selectizeInput("x_cat", label = "X-Axis Category", choices = stat_categories)),
                                            condition = "input.plot_type == 'Scatter'",
                                            selectizeInput("x", label = "X-Axis", choices = NULL)),
                                        selectizeInput("y_cat", label = "Y-Axis Category", choices = stat_categories),
                                        selectizeInput("y", label = "Y-Axis", choices = NULL),
                                        width = 2),
                                    mainPanel(plotlyOutput("userplot"), width = 10))),
                width = 12)
        )
))}
