library(shiny)
library(tidyverse)
library(leaflet)
library(highcharter)
library(shinyWidgets)
library(htmltools)
library(DT)
library(shinyjs)
library(V8)
library(lubridate)



source("global.R")



# DEFINE BODY ----

body <- mainPanel(width = 12,
                  position = "center",
                  fluidRow(
                    column(
                        12,
                        align = "left",
                        tags$a(
                            href="https://www.sorensonimpact.com/maps",
                            target = "_blank",
                            img(src = "./images/logo.png", class = "logoImg1")
                            )
                    )  
                  ),
                  br(),
                  br(),
                  fluidRow(
                      column(
                          12,
                          align = "center",
                          img(src = "./images/header.png", class = "headerImg")
                          
                      )
                  ),
                  fluidRow(
                      column(10, 
                             offset = 1,
                             align = "left", 
                             fluidRow(
                                 h1("The Impact of Covid-19 on Higher Education"),
                                 fluidRow(br()),
                                 fluidRow(
                                     column(
                                         6,
                                         "The pandemic will affect different institutions and students unequally. With your help, we can do something about it.",
                                         fluidRow(br()),
                                         "This dashboard is a part of the MAPS Project, where we Model, Analyze, Prototype, and Share innovative data and solutions to challenges in higher education. We invite your collaboration. Find our project on ",
                                         tags$a(
                                             href="https://github.com/Sorenson-Impact/MAPS-Dashboard", 
                                             target = "_blank",
                                             "Github"),
                                         "and add to our analysis. Reach out with ideas on data and topics you want to see addressed. We will be updating this dashboard regularly."
                                     ),
                                     column(
                                         6,
                                         "As institutions grapple with the challenges brought by Covid-19, data on student preferences, state policies, and institutional reactions are more important than ever to help drive decisions that are student centric.",
                                         fluidRow(br()),
                                         "This crisis raises new questions as we grapple with an uncertain future, but colleges and universities have an opportunity to help create a new system of higher education that is more equitable and improves outcomes for all students. "
                                     )
                                 )
                             ),
                      ),
                      
                  ),
                  
                  
                  br(),
                  tabsetPanel(
                      
                      type = "tabs",
                      
                      
                      # STATE TAB ---------------------------------------------------------------
                      
                      
                      tabPanel(
                          title = h4("STATE IMPACT"), 
                          
                          # State Spending----
                          
                          br(),
                          br(),
                          
                          fluidRow(
                              column(
                                  4,
                                  align = "center",
                                  img(src = "./images/state.png", class = "stateImg")
                                  
                              ), 
                              column(
                                  8,
                                  align = 'left',
                                  div(class = "lvl2_title",
                                      "Higher Education Budget by State"),
                                  fluidRow(br()),
                                  fluidRow(
                                      column(
                                          6,
                                          "Most state governments are facing falling projected revenues and are grappling with extensive budget reductions for FY2021. Institutions of higher education (IHEs) in many states will face significant budget reductions even as they confront new and evolving challenges on how to deliver education in a safe and effective manner."
                                      ),
                                      column(
                                          6,
                                          "The impact of these budget cuts will be felt by public universities and colleges across the country. The average public institution of higher education receives 33% of their revenue from state appropriations, grants, and tuition subsidies. "
                                          
                                      )
                                  ),
                                  # Map -----
                                  fluidRow(
                                      leafletOutput("st", height = 500)
                                  )
                                  
                              )
                          ),
                          
                          br(),
                          fluidRow(
                              column(
                                  12,
                                  uiOutput("select_state")
                              )
                              
                          ),
                          fluidRow(
                              column(
                                  12, 
                                  uiOutput("state_selected")
                              )
                          ),
                          
                          br(),
                          
                          fluidRow(
                              column(12,
                                     fluidRow(
                                         column(
                                             6,
                                             #State budget area ----
                                             
                                             highchartOutput("line", height = 200),
                                             
                                             # State budget numbers ----
                                             
                                             fluidRow(
                                                 column(
                                                     12,
                                                     align = "center",
                                                     uiOutput("state_spending_text_post", height = 100),
                                                     h4("Est. State Budget in 2021 Post-Covid-19"),
                                                     uiOutput("state_spending_text_diff", height = 100),
                                                     h4("Est. % Difference in Budget Post-Covid-19")
                                                 )
                                                 
                                                 
                                             ),
                                             
                                             # Budget text ----
                                             
                                             uiOutput("spending_caveats"),
                                             fluidRow(br()),
                                             
                                             # Download button ----
                                             fluidRow(
                                                 column(
                                                     12, 
                                                     
                                                     align = "left",
                                                     downloadBttn(
                                                         outputId = "sp_download_btn",
                                                         label = "DOWNLOAD DATA",
                                                         style = "fill",
                                                         color = "danger"
                                                     )
                                                     
                                                 )
                                             )
                                             
                                             
                                         ),
                                         column(6,
                                                # certainty bar ----
                                                uiOutput("title_confidence"),
                                                highchartOutput("cer_bar", height = 60),
                                                # certainty key ----
                                                h4("Key for Confidence"),
                                                div(class = "uncertain",
                                                    "UNCONFIDENT"
                                                ),
                                                fluidRow(
                                                    column(
                                                        12,
                                                        "We have data about statewide budget shortfalls, but no proposals."
                                                    )
                                                ),
                                                fluidRow(br()),
                                                div(class = "lesscertain",
                                                    "LOW CONFIDENCE"
                                                ),
                                                fluidRow(
                                                    column(
                                                        12,
                                                        "We have some data about statewide budget cuts, but not about how it will apply specifically to higher education."
                                                    )
                                                ),
                                                fluidRow(br()),
                                                div(class = "morecertain",
                                                    "MODERATE CONFIDENCE"
                                                ),
                                                fluidRow(
                                                    column(
                                                        12,
                                                        "We have data about a budget that's been proposed and the way it will affect higher education specifically, but that it still has a moderate level of potential to be altered by the state legislature or governor."
                                                    )
                                                ),
                                                fluidRow(br()),
                                                div(class = "almostcertain",
                                                    "HIGH CONFIDENCE"
                                                ),
                                                fluidRow(
                                                    column(
                                                        12,
                                                        "We have data that an education budget act has been passed and is now only subject to future changes in legislation."
                                                    )
                                                )
                                                
                                                
                                                
                                         ),
                                         
                                         
                                         
                                         
                                         fluidRow()),
                                     
                              )
                          ),
                          
                          br(),
                          br(),
                          # Methodology ----
                          
                          fluidRow(
                              column(
                                  12,
                                  div(class = "lvl3_title",
                                      "Methodology")
                              )
                          ),
                          
                          fluidRow(
                              column(
                                  4,
                                  "Data collection for FY2006-20 began with",
                                  tags$a(
                                      href="https://shef.sheeo.org/", 
                                      target = "_blank",
                                      "SHEEO SHEF reports"),
                                  "and Grapevine tables. These tables included breakdowns of higher education funding by state tax appropriation, state non-tax appropriation, and a number of other categories.",
                                  fluidRow(br()),
                                  "The two state appropriation categories were summed for each state and each year to develop the cumulative state funding amounts seen in FY2006-20. The pre-Covid-19 imputations for 2021 were calculated using a linear regression for each state’s FY2013-20 data (FY13 was chosen as the start point for its status as the historical low in state funding for higher education), which was extrapolated one year out to develop an estimation of what FY21 funding would have been if Covid-19 had not disrupted state finances. This calculation relies on the assumption that trends in each state would have continued into FY21 for each state where information to the contrary was not available*. "
                              ),
                              
                              column(
                                  4,
                                  "Estimates on the potential changes in state higher education budgets due to the impact of Covid-19 on falling revenues were taken from local and national news sources. These percentage changes** were used to calculate the estimated post-Covid-19 projected spend. Based on the reliability of these numbers (i.e. whether they were projected budget shortfalls for states as a whole, which could affect higher ed budgets in a number of ways, all the way to whether they were legislature-passed bills that included specific changes for higher ed, which are then almost certain to remain unaffected), a confidence score was assigned to each value: unconfident, low confidence, moderate confidence, or high confidence."
                              ),
                              column(
                                  4,
                                  "*AK had information to the contrary. Between FY19-20, the state claimed a cut of $25mm, which came out to about a cut of $40mm according to the SHEEO data. Using this same ratio, the cut of $25mm the state had already been planning between FY20-21 comes out to approximately $40mm again. **When one potential number for percentage change was presented, it was used as the sole estimation. When a range was presented, they were averaged to determine the number factored into the post-Covid-19 projected spend."
                              )
                          ),
                          br(),
                          
                      ),
                      
                      
                      # INSTITUTION TAB ---------------------------------------------------------
                      
                      
                      tabPanel(
                          title  = h4("INSTITUTION IMPACT"),
                          br(),
                          br(),
                          
                          fluidRow(
                              column(
                                  6,
                                  img(src = "./images/institution.png", class = "institutionImg")
                              ),
                              column(
                                  6,
                                  div(class = "lvl2_title",
                                      " Institution Mergers and Closures"),
                                  fluidRow(br()),
                                  fluidRow(
                                      column(
                                          6,
                                          "Even before Covid-19 began to spread, there was a growing number of institutions making the difficult decision to either close or merge with a more stable instiution. Often this was in response to financial pressure caused by dwindling demand. Regardless of the reasons, the end result was that more than half a million students have been displaced over the",
                                          tags$a(
                                              href="https://www.chronicle.com/interactives/20190404-ForProfit", 
                                              target = "_blank",
                                              "last several years.")
                                      ),
                                      column(
                                          6,
                                          "Covid-19 will likely exacerbate this trend, deepening the college closure crisis for students and communities. We are tracking past public and non-profit institutional closures using NCES IPEDs data and current closures through public announcements. We plan to add more data on how institutions are grappling with the crisis."
                                      )
                                  ),
                                  fluidRow(br()),
                                  # mergers and closures area ----
                                  highchartOutput("merger_closures_us", height = "550px"),
                                  
                                  
                              )
                          ),
                          
                          # mergers and closures number ----
                          br(),
                          fluidRow(
                              align = "center",
                              uiOutput("mg_cl_summary")
                          ),
                          
                          br(),
                          
                          #merger and closures select ----
                          
                          fluidRow(
                              align = "center",
                              column(6,
                                     selectInput("mg_cl_yr_select",
                                                 label = h4("SELECT YEAR"),
                                                 choices = as.character(unique(mg_cl$filter_yr)),
                                                 selected = "2020")),
                              column(
                                  6,
                                  selectInput("mg_cl_status_select",
                                              label = h4("SELECT STATUS"),
                                              choices = as.character(unique(mg_cl$final_status)),
                                              selected = "Closed")
                              )
                          ),
                          
                          br(),
                          
                          
                          
                          # mergers and closures table-----
                          br(),
                         
                          br(),
                          
                          fluidRow(
                              column(
                                  10,
                                  offset = 1,
                                  DTOutput("mg_cl_df")
                              )
                          ),
                          
                          br(),
                          br(),
                          
                          # prioritization measures ----
                          fluidRow(
                              column(
                                  8, 
                                  div(class = "lvl2_title",
                                      "Prioritization Measures")
                              )
                          ),
                          br(),
                          
                          fluidRow(
                              column(
                                  4,
                                  "Even before Covid-19, student enrollment in higher education has declined over the past ten years. Some IHEs have been forced to make short-term or long-term changes due to shrinking student enrollment, declining financial security, the possibility of losing accreditation, or some combination of the three. When implementing a prioritization plan to maintain operations, strained institutions must balance their financial needs and mission with student success and educational outcomes in order to ensure long-term institutional viability and continued educational success. ",
                                
                                  fluidRow(br()),
                                  "When considering this balance, an institution could either",
                                  fluidRow(br()),
                                  fluidRow(
                                      column(
                                          12, 
                                          tags$b("1. Prioritize the institution’s success over student success ")
                                      )
                                  ), 
                                  fluidRow(
                                      column(
                                          12, 
                                          tags$b("2. Prioritize student success over institutional success, or ")
                                      )
                                  ), 
                                  fluidRow(
                                      column(
                                          12, 
                                          tags$b("3. Find a way to prioritize both the needs of the students and the institution. ")  
                                      )
                                  ), 
                                  fluidRow(br()),
                                  
                                  "Balancing the needs of the institution, students, and community is vital to preserving the integrity and longevity of institutions. "
                                  
                              ),
                              column(
                                  4,
                                  "On the one hand, if an institution cuts academic programs in order to reduce costs,  it runs the risk of compromising the quality of their academic programs and enrollment could fall further. On the other hand, if an institution does not cut costs, it could be deemed financially unfit to carry out its mission, lose its accreditation, and ultimately close its doors. ",
                                  fluidRow(br()),
                                  "The last option represents the implementation of a prioritization model that downsizes an institution while preserving student outcomes. In this way, prioritization models can be like the “Goldie Locks” tale of higher education. Instead of", tags$i("downsizing"),", an institution should focus on ", tags$i("rightsizing."),
                                  fluidRow(br()),
                                  "We collected data from public announcements in order to build a database of all the strategies adopted by IHEs leading up to and after Covid-19. The most common short-term strategies we found were implementing hiring freezes, raise and promotion freezes, and salary and benefit reductions. The most common long-term strategies include furloughing staff and faculty, and the reduction or elimination of academic program."
                                  
                                 
                              ),
                              
                              
                              # prtz bar years  ----
                              column(
                                  4,
                                  align = "center",
                                  h4("Prioritization Measures Over Time", align = "center"),
                                  fluidRow(br()),
                                  highchartOutput("prtz_yr")
                                  
                       
                              ),
                             
                          ),
                          
                          br(),
                          br(),
                          
                          fluidRow(
                              # prtz pie charts ----
                              column(
                                  5,
                                  align = "center",
                                  h4("Prioritization measures adopted in 2020"),
                                  radioButtons("prtz_measures", label = NULL,
                                               inline = T,
                                               choices = c("Short Term Measures", "Long Term Measures")),

                                  highchartOutput("prtz_pie")
                                  
                              ),
                              
                              
                              # prtz map ----
                              column(
                                  7, 
                                  align = "center",
                                  h4("IHEs that have prioritized"),
                                  selectInput("prtz_yr", " ", choices = c(2019, 2020), selected = 2020),
                                  
                                  leafletOutput("prtz_map")
                              )
                              
                              
                          ),
                          
                          br(),
                          
                          #prtz df ----
                          fluidRow(
                              column(8, 
                                     offset=2, 
                                     DTOutput("prtz_df"))
                              
                          ),
                          
                          br(),
                          br(),
                      ),
                      
                      
                      
                      # STUDENT TAB -------------------------------------------------------------
                      
                      
                      tabPanel(
                          title = h4("STUDENT IMPACT"),
                          br(),
                          br(),
                          
                          fluidRow(
                              column(
                                  5,
                                  align = "center",
                                  img(src = "./images/student1.png", class = "studentImg_1")
                                  
                              ),
                              column(
                                  7,
                                  div(class = "lvl2_title",
                                      "National Surveys of Students"),
                                  fluidRow(br()),
                                  fluidRow(
                                      column(
                                          6,
                                          "When the majority of colleges and universities went online in the middle of the Spring 2020 semester, there was a wide range of possibilities that IHEs were considering for Summer 2020 and Fall 2020. As these plans solidify, many students are debating what they will do in Fall 2020.",
                                          fluidRow(br()),
                                          "We have collected the results of more than 30 surveys deployed to students nationally, and over 20 surveys deployed at specific IHEs.  These surveys reveal a student population whose lives have been upended and who are not yet sure if they will return in the fall."
                                          
                                      ),
                                      column(
                                          6,
                                          " They have been impacted financially and are concerned about their health if they return, but are also suffering from losing their peer networks. Students who are BIPOC, women, from low-income families, and with disabilities are particularly impacted. Across the country, students are weighing the complexities of their new reality as they assess the value of higher education during a pandemic. ",
                                          fluidRow(br()),
                                          "You can explore the results of some of the most robust surveys we sourced by toggling through student populations to the right. A comprehensive list of all of the national and institution-level (grouped by state) surveys are downloadable below. "
                                          
                                      )
                                  ),
                                  fluidRow(br()),
                                  radioGroupButtons(
                                      inputId = "st_pop",
                                      label= "SEE RESULTS BY STUDENT POPULATIONS",
                                      status = "myClass",
                                      choices = unique(surveys_comp$student_population), 
                                      size = "lg",
                                      direction = "horizontal",
                                      justified = F,
                                      individual = T)
                                  
                              )
                              
                          ),
                          
                          
                          
                          
                          
                          # Survey result title ----
                          fluidRow(
                              column(
                                  12,
                                  div(class = "lvl3_title",
                                      "Survey Results For:")
                              )
                          ),
                          
                          fluidRow(
                              column(
                                  12,
                                  align = "left",
                                  uiOutput("survey_title")
                                  
                                  
                                  
                              )
                          ),
                          br(),
                          
                          fluidRow(
                              column(
                                  6,
                                  uiOutput("survey_text"),
                                  fluidRow(br())
                                  
                                  
                              ),
                              column(
                                  6,
                                  align = "center",
                                  img(src = "./images/student2.png", class = "studentImg_2")
                              )
                          ),
                          
                          br(),
                          
                          
                          # All Surveys ---- 
                          fluidRow(
                              column(
                                  4,
                                  div(class = "lvl4_title",
                                      "View all surveys"),
                                  "Click on the toggle buttons below to view and download the list of surveys, both national and state. ", 
                                  fluidRow(br())
                                  
                              )
                          ),
                          
                          br(),
                          fluidRow(
                              column(
                                  6,
                                  align = "center",
                                  prettySwitch("natnl_click", label = tags$b("Click for all national surveys"),
                                               fill = T),
                                  conditionalPanel(
                                      condition = "input.natnl_click",
                                      DTOutput("natnl_dt"))
                                  
                              ),
                              column(
                                  6,
                                  align = "center",
                                  prettySwitch("state_click", label = tags$b("Click for all state specific surveys"),
                                               fill = T),
                                  conditionalPanel(
                                      condition = "input.state_click",
                                      DTOutput("state_dt"))
                              )
                          ),
                          
                          
                          br(),
                          
                          
                          
                      )
                  ),
                  
                  
                  
                  
                  # Back to top ----
                  
                  fluidRow(
                      column(
                          width = 12,
                          align = 'center',
                          actionBttn("toTop", "Back to Top",
                                     style = "material-flat",
                                     
                                     icon=icon("angle-double-up")),
                      )
                  ),
                  br(),
                  br(),
                  
                  # Footer ----
                  
                  fluidRow(
                      column(
                          6,
                          div(class = "footer",
                              "MAPS Project supported by the Bill & Melinda Gates Foundation"),
                          div(
                              class = "footer",
                              "Data last updated July 25, 2020"
                              
                          )
                      ),
                      column(
                          6, 
                          align = "right",
                          img(src = "./images/logo.png", class = "logoImg")
                          
                      )
                  ),
                  
                  
                  # UI Tests ----
                  
                  fluidRow(
                      DTOutput("df"),
                      uiOutput("txt")
                  ),
                  br(),
                  br()
                  
)

# DEFINE UI -----------------------------------------------------------

ui <- fluidPage(theme = "maps_dash_theme.css",
                useShinyjs(),
                extendShinyjs(text = "shinyjs.toTop = function() {window.scrollTo(0, 0)}"),
                titlePanel(
                    windowTitle = "MAPS Project",
                    title = ""
                ),
                div(body, class= "fullpage")
)


# DEFINE SERVER -------------------------------------------------------
server <- function(input, output) {
    
    
    clicked_state <- reactive({
        if(is.null(input$st_shape_click$id)) {"Utah"} else {input$st_shape_click$id}
    })
    
    
    # State select -----
    
    output$select_state <- renderUI({
        selectInput("state_drop", label = h4("Select State"),
                    choices = as.character(unique(spending$state)), 
                    selected = clicked_state())
    })
    
    # State name ----
    
    
    output$state_selected <- renderUI({
        tags$div(class= "lvl3_title",
                 paste(input$state_drop, "State Higher Education Budget"))
    })
    
    #Map ----
    
    
    output$st <- renderLeaflet({
        pal <- colorNumeric(c("#3D3D3D","#F4F4F4"), domain = spending_shp$covid_percent_change)
        
        popup_label <- paste( "<b>","State:","</b>", spending_shp$state, "</br>",
                              "<b>","Estimated HE State Budget Post-Covid-19: ","</b>","$",prettyNum(spending_shp$post_covid_2021_projected_spend, big.mark = ","), "</br>",
                              "<b>","Estimated % change in HE State Budget Post-Covid-19:","</b>", spending_shp$covid_percent_change, "%","</br>",
                              "<b>","Confidence: ","</b>",spending_shp$certainty_level)
        
        spending_shp %>% 
            leaflet(options = leafletOptions(zoomControl = FALSE,
                                             minZoom = 4.13, maxZoom = 4.15,
                                             dragging = T)) %>% 
            addPolygons(fillColor =  ~pal(covid_percent_change),
                        fillOpacity = 1,
                        color = "white", 
                        weight = 2,
                        opacity = 1,
                        popup = popup_label,
                        layerId = ~state) %>% 
            addLegend("bottomleft", pal = pal, values = ~spending_shp$covid_percent_change, opacity = 1, title = paste("% Change in HE State Budget", "</br>", "Post-Covid-19"))
        
    })
    
    # Leaflet proxy ----
    
    observe({
        leafletProxy("st", data = spending_shp %>% dplyr::filter(state %in% input$state_drop)) %>%
            clearGroup("clear") %>%
            addPolygons(weight = 2,
                        color = "black",
                        opacity = 1,
                        fillOpacity = 0,
                        group = "clear")
    })
    
    
    
    
    # State Spending Area ----
    
    spending_reactive<- reactive({
        spending %>% 
            filter(state %in% input$state_drop)
        
    })
    
    output$line <- renderHighchart({
        color <- spending_reactive()$color
        
        plot_df <-  spending_reactive() %>%
            select(contains("x2"), x2021 = post_covid_2021_projected_spend) %>%
            select(-x2021_pre_covid_imputation, -x24) %>%
            pivot_longer(everything(),names_to = "year", values_to = "st_spending") %>%
            mutate(year = str_remove(year, "x")) %>%
            mutate(year = str_remove(year, "x"),
                   year = year(as.Date(year, format = "%Y")))%>%
            mutate(seg_color = case_when(year == 2021 ~ color,
                                         year == 2020 ~ color,
                                         TRUE ~ "#848c1f"))
        
        highchart() %>%
            hc_add_series(data = plot_df, type = "coloredarea", hcaes(x =year, y = st_spending,
                                                                      segmentColor = seg_color), name = "") %>%
            hc_legend(enabled = F) %>%
            #hc_colors("#848c1f") %>%
            hc_xAxis(title= list(text = "State Higher Education Budget from 2006 to 2021")) %>% 
            hc_plotOptions(series = list(marker = list(fillColor = "rgba(0, 0, 0, 0)",
                                                       lineColor = "#848c1f",
                                                       lineWidth = 2,
                                                       radius = 4)))
        
        
    })
    
    # Certainty bar ----
    
    output$title_confidence <-  renderUI({
        h4(paste("Confidence level of the estimated budget difference: ", spending_reactive()$certainty_level))
    })
    
    
    
    output$cer_bar <- renderHighchart({
        
        highchart() %>%
            hc_add_series(data = spending_reactive(), hcaes(state, 4-certainty_level_n)  ,type = "column",name = "filler",
                          spacingBottom = 0,
                          color = "#F0F0F0"
            ) %>%
            hc_add_series(data = spending_reactive() , hcaes(state, certainty_level_n)  ,type = "column",name = "certainty",
                          color = spending_reactive()$color) %>%
            hc_plotOptions(column = list(stacking = "normal", borderWidth = 0),
                           series = list(enableMouseTracking = F)
            )%>%
            hc_yAxis(max =4, gridLineWidth = 0, visible = F) %>%
            hc_xAxis(visible = F,gridLineWidth = 0) %>%
            hc_chart(inverted = TRUE) %>%
            hc_legend(enabled = F)
        
    })
    
    
    
    # State Spending text ----
    
    output$state_spending_text_post <- renderUI({
    
        
        div(
            class = "big_number",
            paste(si_scale_big_dollar(spending_reactive() %>% pull(post_covid_2021_projected_spend)), sep = "")
        )
    })
    
    output$state_spending_text_diff <- renderUI({
        
        val <- spending_reactive() %>% pull(covid_percent_change)
        class <- if (val>0) {"big_number_pos"} else {"big_number_neg"}
        
        div(class = class,
            paste(prettyNum(val , big.mark = ","), "%", sep = ""))
    })
    
    
    output$spending_caveats <- renderUI({
        
        HTML(paste("As of:", spending_reactive()$date_information_collected, "<br>",
                   spending_reactive() %>% pull(notes), "<br>",
                   "Source: ", tags$a(href = spending_reactive() %>% pull(source), "Click here",target="_blank")
        ))
        
    })
    
    
    # download spending data ----
    
    output$sp_download_btn <- downloadHandler(
        filename = function(){
            paste("spending_data_", Sys.Date(), ".csv", sep = "")
        },
        content = function(con){
            write.csv(spending %>% select(-c(state_abbr, state_code, certainty_level_n, color)), con)
        }
    )
    
    # INSTITUTION TAB ----
    # M & C area ----
    
    output$merger_closures_us <- renderHighchart({
        df <- mg_cl %>%
            group_by(filter_yr, final_status) %>%
            summarise(count = n()) %>%
            pivot_wider(names_from = final_status, values_from = count, id_cols = c(filter_yr, final_status)) %>%
            filter(filter_yr>2000)
        
        highchart() %>%
            hc_add_series(data = df, type = "area", hcaes(x = filter_yr, y = `Closed`), name = "Closed",
                          color = "#4c8567",
                          marker = list(fillColor = "rgba(0, 0, 0, 0)",
                                        lineColor = "#4c8567",
                                        lineWidth = 2,
                                        radius = 4,
                                        symbol = "circle")) %>%
            hc_add_series(data = df, type = "area", hcaes(x = filter_yr, y = `Merged`), name = "Merged", color = "#014357",
                          marker = list(fillColor = "rgba(0, 0, 0, 0)",
                                        lineColor = "#014357",
                                        lineWidth = 2,
                                        radius = 4,
                                        symbol = "circle")) %>%
            hc_xAxis(title= list(text = "PUBLIC AND NON-PROFIT IHE CLOSURES AND MERGERS 2006 TO 2020")) %>%
            hc_add_event_point(event = "click")
    })
    
    
    
    # m and c summary ----
    
    output$mg_cl_summary <- renderUI({
        
        val <- mg_cl %>% filter(filter_yr == input$mg_cl_yr_select) %>% filter(final_status == input$mg_cl_status_select) %>% count() %>% pull(n)
        
        tags$div(
            class = "lvl3_title",
            paste("Number of Public and Non Profit IHE ", input$mg_cl_status_select, "in", input$mg_cl_yr_select, ": ", val)
        )
        
    })
    
    
    # M & C data table ----

    output$mg_cl_df <- renderDT({
        
        df <-  if (input$mg_cl_status_select == "Closed") {mg_cl %>% filter(filter_yr == input$mg_cl_yr_select) %>% filter(final_status == input$mg_cl_status_select) %>% arrange(state_name) %>%
                select(`Year` = filter_yr, `Status` = final_status, `State` = state_name, `IHE` = institution_name_google)} else
                    if (input$mg_cl_status_select == "Merged") {mg_cl %>% filter(filter_yr == input$mg_cl_yr_select) %>% filter(final_status == input$mg_cl_status_select) %>% arrange(state_name) %>%
                            select(`Year` = filter_yr, `Status` = final_status, state = state_name, `IHE` = institution_name_google,
                                   `Merged Into` = name_for_merged_into_schools)}
        
    
        
        datatable(
           
            df,
            rownames = F,
            autoHideNavigation = T  )
        
        
    })
    
    # prtz pie charts ---- 
    
    output$prtz_pie <- renderHighchart({
        
        var <- if (input$prtz_measures == "Short Term Measures") {"Short Term"} else if (input$prtz_measures == "Long Term Measures") {"Long Term"}
        
        df <- prtz %>% 
            filter(year_of_prioritization_announcement == 2020) %>% 
            
            group_by(duration_of_prtz,prioritization_method) %>% 
            count() %>% 
            arrange(desc(n)) %>% 
            filter(duration_of_prtz==var)
        
        
        highchart() %>% 
            hc_add_series(data = df, type = "pie", hcaes(x = prioritization_method, y = n),
                          tooltip = list(pointFormat = "No. of IHEs: {point.n}")) %>% 
            hc_plotOptions(pie = list(innerSize = "50%")) %>% 
            hc_colors(c("#014357", "#2491f5", "#848c1f", "#4c8567", "#9ECFD8", "#05a0a8", "#ffc612", "#ff8431", "#ff5f89"))
    })
    
 
    
    
    # prtz bar years ----
    
    output$prtz_yr <- renderHighchart({
        df <- prtz%>% 
            distinct(unitid, school, state_abbreviation, year_of_prioritization_announcement, year_of_prioritization_implementation, duration_of_prtz) %>% 
            group_by(year_of_prioritization_announcement, duration_of_prtz) %>% 
            count()
        
        highchart() %>% 
            hc_add_series(data = df, type = "column", hcaes(x = year_of_prioritization_announcement, y = n, group = duration_of_prtz), 
                          tooltip = list(pointFormat = "No. of IHEs: {point.n}")) %>% 
            hc_plotOptions(column = list(stacking = "normal")) %>% 
            hc_legend(enabled = T) %>% 
            hc_colors(c("#2491f5", "#848c1f"))
        
    })
    
    #prtz map----
    
    output$prtz_map <- renderLeaflet({
        
        yr <- input$prtz_yr
        
        prtz_map_reactive <- reactive({prtz_map %>% 
            filter(year_of_prioritization_announcement == yr) }) 
        
        popup_label <- paste( "No. of institutions: ", prtz_map_reactive()$n)
        pal <- colorNumeric(c("#F4F4F4", "#3D3D3D"), domain = prtz_map$n, na.color = "#9ECFD8")
        
        prtz_map_reactive() %>% 
            leaflet(options = leafletOptions(zoomControl = FALSE,
                                             minZoom = 4.13, maxZoom = 4.15,
                                             dragging = T)) %>% 
            addPolygons(fillColor =  ~pal(n),
                        fillOpacity = 1,
                        color = "white", 
                        weight = 2,
                        opacity = 1,
                        popup = popup_label) %>% 
            addLegend("bottomright", pal = pal, values = ~prtz_map$n, opacity = 1, title = "")
    })
    
    
    #prtz df----
    output$prtz_df <- renderDT({
        
        df <- prtz %>% 
            mutate(state_abbreviation = as.factor(state_abbreviation),
                   prioritization_method= as.factor(prioritization_method),
                   type_of_prtz = as.factor(type_of_prtz),
                   duration_of_prtz = as.factor(duration_of_prtz)) %>% 
            select(`Year Prioritization Announced` = year_of_prioritization_announcement,
                   `Name of Institution` = school,
                   `State` = state_abbreviation,
                   `Prioritization Method` = prioritization_method,
                   `Type of Prioritization` = type_of_prtz,
                   `Duration` = duration_of_prtz,
                   `Source` = source) %>% 
            arrange(`State`)
        
        
        
        
        datatable(
            df,
            rownames = F,
            autoHideNavigation = T,
            filter = "top",
            options = list(
                autoWidth = TRUE, 
                scrollX = TRUE, 
                columnDefs= list(
                    list(width = '1000px', targets = c(6)),
                    list(width = '300px', targets = c(1,3))
                ),
                scroller = TRUE
                )
            )
    })
    
    
    #STUDENT TAB ----
    # Survey title ----
    
    output$survey_title <- renderUI({
        HTML(paste(tags$div(class = "lvl4_title", input$st_pop)))
    })
    
    
    
    # Survey text ----
    
    output$survey_text<- renderUI({
        grouped_df <- surveys_comp %>%
            filter(student_population == input$st_pop) %>%
            group_by(high_level_overview)
        
        
        df_lst <- lst()
        df_lst <- group_split(grouped_df)
     
        
        
        title <- lst()
        all_out<- lst()
        
        for (i in seq_len(length(df_lst))){
            df <- tibble()
            df <- df_lst[[i]]
            title [i]<- unique(df$high_level_overview)
            output <- lst()
            x = 1
            for (j in seq_len(nrow(df))){
                
                output[x] <- paste("<u>","Specific survey data: ","</u>", "</br>",
                                   df$specific_survey_data[j],"</br>",
                                   "Survey Name:", df$survey_name[j],"</br>",
                                   "Source:", df$source[j],"</br>",
                                   "Link:", df$link_to_source, "</br>","</br>"
                                   
                )
                x  = x+1
                
            }
            all_out[i] <- paste(output,collapse = "")
        }
        
        tags$div(fluidRow(
            class= "row_scroll",
            column(
                12,
                tags$b("HIGH LEVEL OVERVIEW"),
                fluidRow(br()),
                HTML(paste("+", title,"</br>", "</br>")),
                fluidRow(br()),
                tags$b("SCROLL DOWN FOR MORE DETAILS"),
                fluidRow(br()),
                HTML(paste("<b>","+", title,"</b>","</br>",all_out))
            )
            
        ))
        
        
        
        
    })
    
    # All Surveys ----
    
    
    output$natnl_dt <- renderDT({
        
        datatable(
            surveys_natnl %>%
                select(survey_name, dates_administered, company, link_for_more_information),
            extensions = 'Buttons',
            options = list(autoWidth = TRUE,scrollX = TRUE,
                           columnDefs = list(list(width = '400px', targets = c(1))),
                           scroller = TRUE,
                           dom = 'Bfrtip',
                           buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
            ))
        
    })
    
    
    output$state_dt <-  renderDT({
        
        datatable(
            surveys_state %>%
                select(state, survey_or_article_title, dates_administered, institution_location, surveyed_population, link_for_more_information),
            extensions = 'Buttons',
            options = list(autoWidth = TRUE,scrollX = TRUE,
                           columnDefs = list(list(width = '500px', targets = c(2,5))),
                           scroller = TRUE,
                           dom = 'Bfrtip',
                           buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
            ))
        
    })
    
    #back to top  ----
    
    observeEvent(input$toTop, {
        js$toTop();
    })
    
    
    #Testing-----
    
    output$txt <- renderUI({})  
    output$df <- renderDT({})
    
    
}


# Run the app -------------------------------------------------------------
shinyApp(ui, server)
