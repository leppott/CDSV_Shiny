#
# This is the user-interface definition of ContDataSumViz Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(shinyalert)
library(shinythemes)
library(shinydashboard)
library(ggplot2)
library(ggthemes)
library(DT)
library(plotly)
#library(shinycustomloader)
library(shinycssloaders)

# Define UI for application
options(spinner.color.background="#ffffff",spinner.size=1)
shinyUI(fluidPage(
  
  theme = 'styles.css',
  useShinyjs(),
  
  # main panel for variable selection
  mainPanel(width = 12,
            
            css_tabPanels <- '.nav-tabs>li>a{
                                   color: cornflowerblue;
                                   font-size: 18px;
                                   font-weight: bold;
                                   }',
            tags$head(tags$style(HTML(css_tabPanels))),
            
            tags$style("#big-heading {font-size:15px;color:black;font-style:bold;display:block; }"),
            tags$style(HTML(".shiny-notification{
                            position:fixed;
                            top:calc(50%);
                            left:calc(50%);}")),
            
            # spacing
            fluidRow(p()),
            
            # top controls  
            fluidRow(
              
              column(width = 12,
                     actionButton('reset all', label = img(src = "ContDataSumViz_banner_updated.png", width = '100%'), width = '100%')
              )
              ),
              
            
            fluidRow(p(),
                     
                     tabsetPanel(
                       
                       tabPanel("Upload Data", 
                                
                                fluidPage(
                                  
                                  fluidRow(
                                    
                                    column(width = 12,
                                           
                                           sidebarLayout(
                                             sidebarPanel(width=3,
                                                          fileInput("uploaded_data_file",label=h4(id="big-heading","Upload your data"),multiple=FALSE,
                                                                    accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
                                                          hr(),
                                                          actionButton(inputId="uploadId", label= "Use this file",style="color:cornflowerblue;background-color:black;font-weight:bold"),
                                                          hr(),
                                                          uiOutput("siteType"),
                                                          hr(),
                                                          uiOutput("manage"),
                                                          hr(),
                                                          uiOutput("select"), 
                                                          hr(),
                                                          uiOutput("display_button")
                                                          
                                                          
                                                          
                                                          
                                             ),
                                             mainPanel(width=9,
                                                       uiOutput("display_raw_ts")
                                               ) # mainPanel end
                                             
                                           ) # sidebarLayout end
                                           
                                    ), #column close
                                    
                                    column(width = 12,
                                             tableOutput("contents")
                                             
                                         
                                           
                                    ), #column close
                                    
                                    
                                  ) #fluidRow close
                                )  #fluidPage close
                       ),  # tabPanel end 
                       
                       
                       tabPanel("Data Exploration", 
                                
                                fluidPage(
                                  
                                  fluidRow(
                                    
                                    tabsetPanel( id="tabset",
                                                 tags$head(tags$style(HTML(".radio-inline {margin-right: 40px;}"))),
                                                 tabPanel("All parameters", value="all_parameters_tab",br(),
                                                          tabsetPanel(id="all_parameters_subtabs",
                                                                      tabPanel("Summary tables", value="tab_summary_tables",br(),                     
                                                          br(),
                                                          column(width = 12,
                                                                 sidebarLayout(
                                                                   sidebarPanel(width=3,
                                                                                hr(),
                                                                                uiOutput("summary_table_input_1"),
                                                                                hr(),
                                                                                uiOutput("summary_table_input_2"),
                                                                                hr(),
                                                                                uiOutput("summary_table_input_3"),
                                                                                hr(),
                                                                                uiOutput("summary_table_input_4"),
                                                                                
                                                                   ),
                                                                   mainPanel(width=9,
                                                                             column(width=9,uiOutput("display_summary_table_1")),
                                                                             column(width=9,uiOutput("display_summary_table_2"))
                                                                   ) # mainPanel end
                                                                   
                                                                 ) # sidebarLayout end
                                                                 
                                                          ), #column close
                                                          br(),
                                                                      ), # tabPanel 1 end
                                                          
                                                          tabPanel("Time series plots", value="tab_time_series",br(),
                                                                   column(width = 12,
                                                                          sidebarLayout(
                                                                            sidebarPanel(width=3,
                                                                                         uiOutput("time_series_input_1"),
                                                                                         hr(),
                                                                                         uiOutput("time_series_input_2"),
                                                                                         hr(),
                                                                                         uiOutput("time_series_input_3"),
                                                                                         hr(),
                                                                                         uiOutput("time_series_input_4"),
                                                                                         hr(),
                                                                                         fluidRow(column(width=6,
                                                                                                         uiOutput("time_series_input_5")
                                                                                                        ),
                                                                                                  column(width=6,align="right",
                                                                                                         uiOutput("time_series_input_6")
                                                                                                        )
                                                                                                  ) # fluidRow close
                                                                            ),
                                                                            mainPanel(width=9,
                                                                                      fluidRow(column(width=9,uiOutput("display_time_series"))),
                                                                                      br(),
                                                                                      br()
                                                                                      
                                                                            ) # mainPanel end
                                                                            
                                                                          ) # sidebarLayout end
                                                                          
                                                                   ), #column close
                                                                   br(),   
                                                                   column(width =12,
                                                                          uiOutput("another_time_series_UI"))
                                                                   
                                                          ), #tabPanel 2 end
                                                          
                                                          tabPanel("Time series - Annual overlays", value="tab_time_series_overlay",br(),
                                                                   column(width = 12,
                                                                          sidebarLayout(
                                                                            sidebarPanel(width=3,
                                                                                         uiOutput("time_series_overlay_input_1"),
                                                                                         hr(),
                                                                                         uiOutput("time_series_overlay_input_2"),
                                                                                         hr(),
                                                                                         uiOutput("time_series_overlay_input_3"),
                                                                                         hr(),
                                                                                         uiOutput("time_series_overlay_input_4"),
                                                                                         hr(),
                                                                                         uiOutput("time_series_overlay_input_5"),
                                                                            ),
                                                                            mainPanel(width=9,
                                                                                      fluidRow(column(width=9,uiOutput("display_time_series_overlay"))),
                                                                                      br(),
                                                                                      br()
                                                                                      
                                                                            ) # mainPanel end
                                                                            
                                                                          ) # sidebarLayout end
                                                                          
                                                                   ), #column close
                                                                   
                                                          ), #tabPanel 3 end
                                                          
                                                          
                                                          tabPanel("Box plots", value="tab_box",br(),
                                                                   column(width = 12,
                                                                          sidebarLayout(
                                                                            sidebarPanel(width=3,
                                                                                         hr(),
                                                                                         uiOutput("box_input_1"),
                                                                                         hr(),
                                                                                         uiOutput("box_input_2"),
                                                                                         hr(),
                                                                                         uiOutput("box_input_3"),
                                                                                         hr(),
                                                                                         uiOutput("box_input_4"),
                                                                                         hr(),
                                                                                         uiOutput("box_input_5")
                                                                            ),
                                                                            mainPanel(width=9,
                                                                                      fluidRow(column(width=9,uiOutput("display_box_plots"))),
                                                                                      br(),
                                                                                      br(),
                                                                                      
                                                                            ) # mainPanel end
                                                                            
                                                                          ) # sidebarLayout end
                                                                          
                                                                   ), #column close
                                                                   
                                                          ), #tabPanel 4 end
                                                          
                                                          tabPanel("CDFs", value="tab_CDF",br(),
                                                                   column(width = 12,
                                                                          sidebarLayout(
                                                                            sidebarPanel(width=3,
                                                                                         hr(),
                                                                                         uiOutput("CDF_input_1"),
                                                                                         hr(),
                                                                                         uiOutput("CDF_input_2"),
                                                                                         hr(),
                                                                                         uiOutput("CDF_input_3"), 
                                                                                         hr(),
                                                                                         uiOutput("CDF_input_4"),
                                                                                         hr(),
                                                                                         uiOutput("CDF_input_5"),
                                                                                         hr(),
                                                                                         uiOutput("display_CDF_button")
                                                                            ),
                                                                            mainPanel(width=9,
                                                                                      fluidRow(column(width=9,uiOutput("display_plot_CDF"))),
                                                                                      br(),
                                                                                      br(),
                                                                                      
                                                                                      
                                                                            ) # mainPanel end
                                                                            
                                                                          ) # sidebarLayout end
                                                                          
                                                                   ), #column close
                                                                   br(),
                                                                   
                                                          ), #tabPanel 5 end
                                                          
                                                          tabPanel("Raster graphs", value="tab_raster",br(),
                                                                   column(width = 12,
                                                                          sidebarLayout(
                                                                            sidebarPanel(width=3,
                                                                                         hr(),
                                                                                         uiOutput("raster_input_1"),
                                                                                         hr(),
                                                                                         uiOutput("raster_input_2"),
                                                                                         hr(),
                                                                                         uiOutput("raster_input_3"),
                                                                                         hr(),
                                                                                         uiOutput("raster_input_4"),
                                                                                         hr(),
                                                                                         uiOutput("raster_input_5"),
                                                                                         hr(),
                                                                                         uiOutput("raster_input_6"),
                                                                                         
                                                                            ),
                                                                            mainPanel(width=9,
                                                                                      column(width=9,uiOutput("display_raster_graphs"))
                                                                                      
                                                                            ) # mainPanel end
                                                                            
                                                                          ) # sidebarLayout end
                                                                          
                                                                   ), #column close
                                                                   
                                                          ), #tabPanel 6 end
                                                          
                                                          tabPanel("Climate spiral", value="tab_climate",br(),
                                                                   column(width = 12,
                                                                          sidebarLayout(
                                                                            sidebarPanel(width=3,
                                                                                         hr(),
                                                                                         uiOutput("climate_input_1"),
                                                                                         hr(),
                                                                                         uiOutput("climate_input_2"),
                                                                                         
                                                                            ),
                                                                            mainPanel(width=9,
                                                                                      column(width=9,uiOutput("display_climate_spiral"))
                                                                                      
                                                                            ) # mainPanel end
                                                                            
                                                                          ) # sidebarLayout end
                                                                          
                                                                   ), #column close
                                                                   
                                                          ), #tabPanel 7 end
                                                          
                                                          ) #inner tabsetPanel end
                                                          
                                                          
                                                 ), #tabPanel end
                                                 
                                                
                                                 
                                                 tabPanel("Temperature", value="temp_tab",
                                                          tabsetPanel(id="temp_subtabs",
                                                                      tabPanel("Thermal statistics",value = "sb1",br(),
                                                                               column(width = 12,
                                                                                      sidebarLayout(
                                                                                        sidebarPanel(width=3,
                                                                                                     hr(),
                                                                                                     uiOutput("thermal_input_1"),
                                                                                                     hr(),
                                                                                                     uiOutput("thermal_input_2"),
                                                                                                     hr(),
                                                                                                     uiOutput("thermal_input_3"),
                                                                                                     hr(),
                                                                                                     uiOutput("display_run_thermal_button"),
                                                                                                     hr(),
                                                                                                     uiOutput("display_save_thermal_button"),
                                                                                        ),
                                                                                        mainPanel(width=9,
                                                                                                  column(width=12
                                                                                                         ,uiOutput("display_thermal_table_1")
                                                                                                         ,br()
                                                                                                         ,uiOutput("display_thermal_table_2")
                                                                                                         ,br()
                                                                                                         ,uiOutput("display_thermal_table_3")
                                                                                                         ,br()
                                                                                                         ,uiOutput("display_thermal_table_4")
                                                                                                         ,br()
                                                                                                         ,uiOutput("display_thermal_table_5"))
                                                                                                  
                                                                                        ) # mainPanel end
                                                                                        
                                                                                      ) # sidebarLayout end
                                                                                      
                                                                               ), #column close
                                                                               
                                                                      ),
                                                                      tabPanel("Air vs Water",value = "sb2",br(),
                                                                               column(width = 12,
                                                                               sidebarLayout(
                                                                                 sidebarPanel(width=3,
                                                                                              hr(),
                                                                                              uiOutput("air_vs_water_input_1"),
                                                                                              hr(),
                                                                                              uiOutput("air_vs_water_input_2"),
                                                                                              hr(),
                                                                                              uiOutput("air_vs_water_input_3"),
                                                                                              hr(),
                                                                                              uiOutput("air_vs_water_input_4"),
                                                                                              hr(),
                                                                                              uiOutput("display_thermal_sensitivity_button"),
                                                                                             
                                                                                 ),
                                                                                 mainPanel(width=9,
                                                                                           column(width=12
                                                                                                  ,uiOutput("display_thermal_sensitivity_plot_1")
                                                                                                  ,br()
                                                                                                  ,uiOutput("display_thermal_sensitivity_plot_2")
                                                                                                  )
                                                                                           
                                                                                 ) # mainPanel end
                                                                                 
                                                                               ) # sidebarLayout end
                                                                               ) # column close
                                                                               
                                                                      ),
                                                                      tabPanel("Growing degree days",value = "sb3",br(),
                                                                               br(),
                                                                               column(width=12,uiOutput("display_plot5"))
                                                                      ),
                                                                      tabPanel("Thermal classification",value = "sb5",br(),
                                                                               br(),
                                                                               column(width=12,uiOutput("display_plot6"))
                                                                      ))), #outer tabPanel end
                                                 
                                                 tabPanel("Hydrology", value="hydro_tab",
                                                          tabsetPanel(id="hydro_subtabs",
                                                                      tabPanel("IHA",value = "IHA_tab",br(),
                                                                               column(width = 12,
                                                                                      sidebarLayout(
                                                                                        sidebarPanel(width=3,
                                                                                                     hr(),
                                                                                                     uiOutput("IHA_input_1"),
                                                                                                     hr(),
                                                                                                     uiOutput("IHA_input_2"),
                                                                                                     hr(),
                                                                                                     uiOutput("IHA_input_3"),
                                                                                                     hr(),
                                                                                                     uiOutput("IHA_input_4"),
                                                                                                     hr(),
                                                                                                     uiOutput("display_IHA_button"),
                                                                                                     
                                                                                        ),
                                                                                        mainPanel(width=9,
                                                                                                  column(width=12
                                                                                                         ,uiOutput("display_IHA_plot_1")
                                                                                                         ,br()
                                                                                                         ,uiOutput("display_IHA_plot_2")
                                                                                                  )
                                                                                                  
                                                                                        ) # mainPanel end
                                                                                        
                                                                                      ) # sidebarLayout end
                                                                               ) # column close
                                                                      ),
                                                                     
                                                                      tabPanel("Flashiness",value = "Flashiness_tab",br(),
                                                                               br(),
                                                                               column(width=12,uiOutput("display_plot8"))
                                                                      )))  
                                                 
                                                 
                                  ) #tabsetPanel end              
                                  ) #fluidRow close
                                )  #fluidPage close
                       ),# tabPanel end 
                       
               tabPanel("Create Report",
                        fluidPage(
                                fluidRow(
                                  tabsetPanel(id="report_subtabs",
                                              tabPanel("SingleSite",value = "SingleSite_tab",br(),
                                                       br(),
                                                       column(width=12,uiOutput("display_table_single_site"))
                                              ), #tabPanel close
                                              
                                              tabPanel("MultiSites",value = "MultiSites_tab",br(),
                                                       br(),
                                                       column(width=12,uiOutput("display_table_multiple_sites"))
                                              ) #tabPanel close 
                                             ) #tabsetPanel end              
                                    ) #fluidRow end
                        ) #fluidPage end
               ) # tabPanel end 
                       
            ) #tabsetPanel close
            
            )
                     
  )
))
