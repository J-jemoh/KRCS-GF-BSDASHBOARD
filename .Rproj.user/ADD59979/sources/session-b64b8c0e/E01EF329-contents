source("dependencies.R")

ui=dashboardPage(
    header = dashboardHeader(
        title = dashboardBrand(
            title = "KRCS-GF DASHBORD",
            color = "primary",
            href = "#",
            image = "https://adminlte.io/themes/v3/dist/img/AdminLTELogo.png"
        )
    ),
    sidebar = dashboardSidebar(skin = "dark",status = "danger",
        sidebarMenu(
            id="ayp",sidebarHeader(strong("ANALYTICS")),
            menuItem(strong('AYP Analytics'),tabName = "ayp",icon = icon("bar-chart")),
            menuItem(strong('FSW analytics'),tabName = "fsw",icon = icon("tachometer")),
            menuItem(strong('MSM analytics'),tabName = "msm",icon = icon("tachometer")),
            menuItem(strong('PWID analytics'),tabName = "pwid",icon = icon("tachometer")),
            menuItem(strong('TG analytics'),tabName = "tg",icon = icon("tachometer")),
            menuItem(strong('TCS analytics'),tabName = "tcs",icon = icon("tachometer"))
        )
        
    ),
    body = bs4DashBody(
        tabItems(
            tabItem("ayp",
                    fluidRow(width=12,
                             bs4ValueBox(health_education,"Provided Health Education",icon = icon("person"),width=3,color="success"),
                             bs4ValueBox(srh_info,"Provided SRH Information",icon = icon("map-marker"),width=3,color="danger"),
                             bs4ValueBox(tested_for_hiv,"Tested forHIV",icon = icon("building"),width=3,color="info"),
                             bs4ValueBox(screened_sti,"Screened for STI",icon = icon("building"),width=3,color="secondary")
                             ),
                    fluidRow(
                        bs4Card(
                            title = strong("Aggregation based on Age and Gender"),status = "fuchsia",icon = icon("person"),
                            DTOutput("age_group")
                        ),
                        bs4Card(
                            title = strong("Count of outreach by region and gender"),status = "fuchsia",icon = icon("person"),
                            DTOutput("gender_county")
                        )
                    ),
                    fluidRow(
                        bs4Card(
                            title = strong("chart of outreach based on Region and Age Group"),status = "fuchsia",icon = icon("person"),
                            plotlyOutput("age1_barchartt")
                        ),
                        bs4Card(
                            title = strong("chart of outreach by region and gender"),status = "fuchsia",icon = icon("person"),
                            plotlyOutput("gender1_county1")
                        )
                    ),
                    fluidRow(
                        bs4Card(width=12,
                                title = strong("EBIS indicator summary(Completed sessions)"),status = "fuchsia",icon = icon("person"),
                                selectInput(
                                    inputId = "EBIRegionInput",
                                    label = "Filter by Region",
                                    choices = c("All", unique(ebi_HBCF$region)),
                                    multiple = FALSE,
                                    selectize = TRUE,
                                ),
                                uiOutput("EBI_individual")
                        )
                    ),
                    fluidRow(
                        bs4Card(width=12,
                            title = strong("Individual indicator summary"),status = "fuchsia",icon = icon("person"),
                            selectInput(
                                inputId = "aypRegionInput",
                                label = "Filter by Region",
                                choices = c("All", unique(AYP$region)),
                                multiple = FALSE,
                                selectize = TRUE,
                            ),
                            uiOutput("ayp_individual")
                        )
                    )
                    ),
            tabItem("fsw",
                    tabsetPanel(
                        tabPanel("2023-ANALYTICS",
                                 fluidRow(width=12,
                                          bs4ValueBox(total_fsw,"Total enrolled",icon = icon("person"),width=3,color="info"),
                                          bs4ValueBox(total_counties,"No of counties",icon = icon("map-marker"),width=3,color="danger"),
                                          bs4ValueBox(sr_number,"No of SR",icon = icon("building"),width=3,color="success"),
                                          bs4ValueBox(total_pees,"No of PE",icon = icon("building"),width=3,color="secondary"),
                                          
                                 ),
                                 fluidRow(
                                     
                                     bs4Card(
                                         title = strong("HIV Status at Enrollment"),status = "fuchsia",icon = icon("person"),
                                         selectInput(
                                             inputId = "hivRegionInput",
                                             label = "Filter by Region",
                                             choices = c("All", unique(fsw_consolidated$region)),
                                             multiple = FALSE,
                                             selectize = TRUE,
                                         ),
                                         dataTableOutput("enrol_status")
                                     ),
                                     bs4Card(
                                         title = strong("HIV Chart Status at Enrollment"),status = "fuchsia",icon = icon("person"),
                                         selectInput(
                                             inputId = "hivRegionInput",
                                             label = "Filter by Region",
                                             choices = c("All", unique(fsw_consolidated$region)),
                                             multiple = FALSE,
                                             selectize = TRUE,
                                         ),
                                         plotlyOutput("enrol_status_chart")
                                     )
                                 ),
                                 fluidRow(
                                     bs4Card(width=12,
                                             title = strong("KEY INDICATOR PROGRESS SUMMARY"),status = "fuchsia",icon = icon("person"),
                                             selectInput(
                                                 inputId = "indicatorRegionInput",
                                                 label = "Filter by Region",
                                                 choices = c("All", unique(fsw_consolidated$region)),
                                                 multiple = FALSE,
                                                 selectize = TRUE,
                                             ),
                                             uiOutput("fsw_indicators")
                                     )
                                 ),
                                 fluidRow(
                                     bs4Card(width=12,
                                             title = strong("Defined Package INDICATOR PROGRESS SUMMARY"),status = "fuchsia",icon = icon("person"),
                                             selectInput(
                                                 inputId = "definedRegionInput",
                                                 label = "Filter by Region",
                                                 choices = c("All", unique(fsw_consolidated$region)),
                                                 multiple = FALSE,
                                                 selectize = TRUE,
                                             ),
                                             uiOutput("fsw_defined")
                                     )
                                 )
                                 ),
                        tabPanel("2024-ANALYTICS")
                    )
                   
            ),
            tabItem("msm",
                    tabsetPanel(
                        tabPanel("2023-ANALYTICS",
                                 fluidRow(width=12,
                                          bs4ValueBox(total_msm,"Total enrolled",icon = icon("person"),width=3,color="info"),
                                          bs4ValueBox(total_counties_msm,"No of counties",icon = icon("map-marker"),width=3,color="danger"),
                                          bs4ValueBox(sr_number_msm,"No of SR",icon = icon("building"),width=3,color="success"),
                                          bs4ValueBox(total_pees_msm,"No of PE",icon = icon("building"),width=3,color="secondary"),
                                          
                                 ),
                                 fluidRow(
                                     
                                     bs4Card(
                                         title = strong("HIV Status at Enrollment"),status = "fuchsia",icon = icon("person"),
                                         selectInput(
                                             inputId = "msmhivRegionInput",
                                             label = "Filter by Region",
                                             choices = c("All", unique(msm_consolidated$region)),
                                             multiple = FALSE,
                                             selectize = TRUE,
                                         ),
                                         dataTableOutput("enrol_status_msm")
                                     ),
                                     bs4Card(
                                         title = strong("HIV Chart Status at Enrollment"),status = "fuchsia",icon = icon("person"),
                                         selectInput(
                                             inputId = "msmhivRegionInput",
                                             label = "Filter by Region",
                                             choices = c("All", unique(msm_consolidated$region)),
                                             multiple = FALSE,
                                             selectize = TRUE,
                                         ),
                                         plotlyOutput("enrol_status_chart_msm")
                                     )
                                 ),
                                 fluidRow(
                                     bs4Card(width=12,
                                             title = strong("MSM KEY INDICATOR PROGRESS SUMMARY"),status = "fuchsia",icon = icon("person"),
                                             selectInput(
                                                 inputId = "indicator_msm_RegionInput",
                                                 label = "Filter by Region",
                                                 choices = c("All", unique(msm_consolidated$region)),
                                                 multiple = FALSE,
                                                 selectize = TRUE,
                                             ),
                                             uiOutput("msm_indicators")
                                     )
                                 ),
                                 fluidRow(
                                     bs4Card(width=12,
                                             title = strong("Defined Package INDICATOR PROGRESS SUMMARY"),status = "fuchsia",icon = icon("person"),
                                             selectInput(
                                                 inputId = "defined_msm_RegionInput",
                                                 label = "Filter by Region",
                                                 choices = c("All", unique(msm_consolidated$region)),
                                                 multiple = FALSE,
                                                 selectize = TRUE,
                                             ),
                                             uiOutput("msm_defined")
                                     )
                                 )
                        ),
                        tabPanel("2024-ANALYTICS")
                    )
                    
            ),
            tabItem("pwid",
                    tabsetPanel(
                        tabPanel("2023-ANALYTICS",
                                 fluidRow(width=12,
                                          bs4ValueBox(total_pwid,"Total enrolled",icon = icon("person"),width=3,color="info"),
                                          bs4ValueBox(total_counties_pwid,"No of counties",icon = icon("map-marker"),width=3,color="danger"),
                                          bs4ValueBox(sr_number_pwid,"No of SR",icon = icon("building"),width=3,color="success"),
                                          bs4ValueBox(total_pees_pwid,"No of PE",icon = icon("building"),width=3,color="secondary"),
                                          
                                 ),
                                 fluidRow(
                                     
                                     bs4Card(
                                         title = strong("HIV Status at Enrollment"),status = "fuchsia",icon = icon("person"),
                                         selectInput(
                                             inputId = "PWIDRegionInput",
                                             label = "Filter by Region",
                                             choices = c("All", unique(pwid_consolidated$region)),
                                             multiple = FALSE,
                                             selectize = TRUE,
                                         ),
                                         dataTableOutput("enrol_status_pwid")
                                     ),
                                     bs4Card(
                                         title = strong("HIV Chart Status at Enrollment"),status = "fuchsia",icon = icon("person"),
                                         selectInput(
                                             inputId = "PWIDRegionInput",
                                             label = "Filter by Region",
                                             choices = c("All", unique(pwid_consolidated$region)),
                                             multiple = FALSE,
                                             selectize = TRUE,
                                         ),
                                         plotlyOutput("enrol_status_chart_pwid")
                                     )
                                 ),
                                 fluidRow(
                                     bs4Card(width=12,
                                             title = strong("MSM KEY INDICATOR PROGRESS SUMMARY"),status = "fuchsia",icon = icon("person"),
                                             selectInput(
                                                 inputId = "PWIDIndicatorRegionInput",
                                                 label = "Filter by Region",
                                                 choices = c("All", unique(pwid_consolidated$region)),
                                                 multiple = FALSE,
                                                 selectize = TRUE,
                                             ),
                                             uiOutput("pwid_indicators")
                                     )
                                 ),
                                 fluidRow(
                                     bs4Card(width=12,
                                             title = strong("Defined Package INDICATOR PROGRESS SUMMARY"),status = "fuchsia",icon = icon("person"),
                                             selectInput(
                                                 inputId = "pwiddefinedRegionInput",
                                                 label = "Filter by Region",
                                                 choices = c("All", unique(pwid_consolidated$region)),
                                                 multiple = FALSE,
                                                 selectize = TRUE,
                                             ),
                                             uiOutput("pwid_defined")
                                     )
                                 )
                        ),
                        tabPanel("2024-ANALYTICS")
                    )
                    
            ),
            tabItem("tg",
                    tabsetPanel(
                        tabPanel("2023-ANALYTICS",
                                 fluidRow(width=12,
                                          bs4ValueBox(total_tg,"Total enrolled",icon = icon("person"),width=3,color="info"),
                                          bs4ValueBox(total_counties_tg,"No of counties",icon = icon("map-marker"),width=3,color="danger"),
                                          bs4ValueBox(sr_number_tg,"No of SR",icon = icon("building"),width=3,color="success"),
                                          bs4ValueBox(total_pees_tg,"No of PE",icon = icon("building"),width=3,color="secondary"),
                                          
                                 ),
                                 fluidRow(
                                     
                                     bs4Card(
                                         title = strong("HIV Status at Enrollment"),status = "fuchsia",icon = icon("person"),
                                         selectInput(
                                             inputId = "TGRegionInput",
                                             label = "Filter by Region",
                                             choices = c("All", unique(tg_consolidated$region)),
                                             multiple = FALSE,
                                             selectize = TRUE,
                                         ),
                                         dataTableOutput("enrol_status_tg")
                                     ),
                                     bs4Card(
                                         title = strong("HIV Chart Status at Enrollment"),status = "fuchsia",icon = icon("person"),
                                         selectInput(
                                             inputId = "TGRegionInput",
                                             label = "Filter by Region",
                                             choices = c("All", unique(tg_consolidated$region)),
                                             multiple = FALSE,
                                             selectize = TRUE,
                                         ),
                                         plotlyOutput("enrol_status_chart_tg")
                                     )
                                 ),
                                 fluidRow(
                                     bs4Card(width=12,
                                             title = strong("TG KEY INDICATOR PROGRESS SUMMARY"),status = "fuchsia",icon = icon("person"),
                                             selectInput(
                                                 inputId = "TGIndicatorRegionInput",
                                                 label = "Filter by Region",
                                                 choices = c("All", unique(tg_consolidated$region)),
                                                 multiple = FALSE,
                                                 selectize = TRUE,
                                             ),
                                             uiOutput("tg_indicators")
                                     )
                                 ),
                                 fluidRow(
                                     bs4Card(width=12,
                                             title = strong("Defined Package INDICATOR PROGRESS SUMMARY"),status = "fuchsia",icon = icon("person"),
                                             selectInput(
                                                 inputId = "tgdefinedRegionInput",
                                                 label = "Filter by Region",
                                                 choices = c("All", unique(tg_consolidated$region)),
                                                 multiple = FALSE,
                                                 selectize = TRUE,
                                             ),
                                             uiOutput("tg_defined")
                                     )
                                 )
                        ),
                        tabPanel("2024-ANALYTICS")
                    )
                    
            )
        )
        # lapply(getAdminLTEColors(), function(color) {
        #     box(status = color)
        # })
    ),
    controlbar = dashboardControlbar(),
    title = "DashboardPage"
)