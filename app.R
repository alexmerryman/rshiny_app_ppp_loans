#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(tidyverse)
library(tidyr)
library(DT)
library(ggplot2)
library(plotly)
library(devtools)
library(cpp11)
# devtools::install_version("cpp11", version = "0.1", repos = "http://cran.us.r-project.org")

ppp_data <- read.csv("ppp_latlong.csv", header = TRUE)
ppp_data <- sample_n(ppp_data, 1000)  # TODO: Remove this line when publishing

ppp_data$LoanRangeClean <- substr(ppp_data$LoanRange, 3, nchar(ppp_data$LoanRange))

ppp_data$LoanRangeAmtLow <- ifelse(ppp_data$LoanRange == "a $5-10 million", 5000000, 
                                   ifelse(ppp_data$LoanRange == "b $2-5 million", 2000000, 
                                          ifelse(ppp_data$LoanRange == "c $1-2 million", 1000000, 
                                                 ifelse(ppp_data$LoanRange == "d $350,000-1 million", 350000, 
                                                        ifelse(ppp_data$LoanRange == "e $150,000-350,000", 150000, 0)))))

ppp_data$LoanRangeAmtHigh <- ifelse(ppp_data$LoanRange == "a $5-10 million", 10000000, 
                                    ifelse(ppp_data$LoanRange == "b $2-5 million", 5000000, 
                                           ifelse(ppp_data$LoanRange == "c $1-2 million", 2000000, 
                                                  ifelse(ppp_data$LoanRange == "d $350,000-1 million", 1000000, 
                                                         ifelse(ppp_data$LoanRange == "e $150,000-350,000", 350000, 0)))))

ppp_data$Zip <- as.character(ppp_data$Zip)
ppp_data$DateApproved <- as.Date(ppp_data$DateApproved, format= "%m/%d/%y")


loan_range_order = c("$150,000-350,000", "$350,000-1 million", "$1-2 million", "$2-5 million", "$5-10 million")


ui <- fluidPage(

    titlePanel("Paycheck Protection Program (PPP) Loans"),
    tags$i(h4("As part of the CARES Act, passed in response to the COVID-19 pandemic, the US federal government provided loans for businesses to pay up to 8 weeks of payroll costs, including benefits.")),
    tags$i(h4("After a protracted legal fight, the US Department of the Treasury released details about a subset of PPP loan recipients on July 6, 2020.")),
    tags$i(h6("Raw data can be viewed and downloaded ", tags$a(href="https://www.sba.gov/funding-programs/loans/coronavirus-relief-options/paycheck-protection-program#section-header-11", "here.", .noWS = "outside"))),
    tags$hr(),
    
    sidebarLayout(
        sidebarPanel(
            # TODO: buttons for loan range amounts, etc
            h4("Filter by various conditions:"),
            br(),
            selectInput("state", "State(s):", choices = sort(unique(ppp_data$State.x)), multiple = TRUE),
            selectInput("gender", "Gender(s):", choices = sort(unique(ppp_data$Gender)), multiple = TRUE),
            selectInput("raceethnicity", "Race/Ethnicity(s):", choices = sort(unique(ppp_data$RaceEthnicity)), multiple = TRUE),
            selectInput("veteran", "Veteran status:", choices = sort(unique(ppp_data$Veteran)), multiple = TRUE),
            selectInput("nonprofit", "Non-Profit:", choices = sort(unique(ppp_data$NonProfit)), multiple = TRUE),
            selectInput("businesstype", "Business Type:", choices = sort(unique(ppp_data$BusinessType)), multiple = TRUE),
            selectInput("loanrangeclean", "Loan Amount Range:", choices = factor(unique(ppp_data$LoanRangeClean), levels = loan_range_order), multiple = TRUE),
            # conditionalPanel(condition = "output.nrows", checkboxInput("headonly", "Only use first 1000 rows"))
            
            actionButton(
                inputId = "goButton",
                label = "Submit")
            
            # TODO: Add "Reset Filters" button to clear all filters
        ),

        mainPanel(
            plotlyOutput("map_plotly"),
            
            tabsetPanel(
                # tabPanel("Scatterplot", DT::dataTableOutput("scatterplot")),
                tabPanel("Loans Approved Over Time", plotlyOutput("timeseries_plotly")),
                # tabPanel("Brushed Table", DT::dataTableOutput("table_brushed")),
                tabPanel("Search All Loan Recipients", shiny::dataTableOutput("reactive_df"))
                ),
            ),
        )
    )



server <- function(input, output) {
    
    library(shiny)
    library(dplyr)
    library(tidyverse)
    library(tidyr)
    library(DT)
    library(ggplot2)
    library(plotly)
    library(devtools)
    library(cpp11)
    # devtools::install_version("cpp11", version = "0.1", repos = "http://cran.us.r-project.org")

    ppp_data <- read.csv("ppp_latlong.csv", header = TRUE)
    ppp_data <- sample_n(ppp_data, 1000)  # TODO: Remove this line when publishing

    ppp_data$LoanRangeClean <- substr(ppp_data$LoanRange, 3, nchar(ppp_data$LoanRange))
    
    ppp_data$LoanRangeAmtLow <- ifelse(ppp_data$LoanRange == "a $5-10 million", 5000000,
                                       ifelse(ppp_data$LoanRange == "b $2-5 million", 2000000,
                                              ifelse(ppp_data$LoanRange == "c $1-2 million", 1000000,
                                                     ifelse(ppp_data$LoanRange == "d $350,000-1 million", 350000,
                                                            ifelse(ppp_data$LoanRange == "e $150,000-350,000", 150000, 0)))))

    ppp_data$LoanRangeAmtHigh <- ifelse(ppp_data$LoanRange == "a $5-10 million", 10000000,
                                       ifelse(ppp_data$LoanRange == "b $2-5 million", 5000000,
                                              ifelse(ppp_data$LoanRange == "c $1-2 million", 2000000,
                                                     ifelse(ppp_data$LoanRange == "d $350,000-1 million", 1000000,
                                                            ifelse(ppp_data$LoanRange == "e $150,000-350,000", 350000, 0)))))

    # TODO: Create grouped table of zip code-sum(LoanRangeAmt)

    observeEvent(
        eventExpr = input$goButton,
        ignoreNULL = FALSE,
        handlerExpr = {
                
            if (length(input$state) == 0) {
                state_filter <- unique(ppp_data$State.x)
            } else {
                state_filter <- input$state
            }
                
            if (length(input$gender) == 0) {
                gender_filter <- unique(ppp_data$Gender)
            } else {
                gender_filter <- input$gender
            }
                
            if (length(input$raceethnicity) == 0) {
                raceethnicity_filter <- unique(ppp_data$RaceEthnicity)
            } else {
                raceethnicity_filter <- input$raceethnicity
            }
                
            if (length(input$veteran) == 0) {
                veteran_filter <- unique(ppp_data$Veteran)
            } else {
                veteran_filter <- input$veteran
            }
            
            if (length(input$nonprofit) == 0) {
                nonprofit_filter <- unique(ppp_data$NonProfit)
            } else {
                nonprofit_filter <- input$nonprofit
            }
            
            if (length(input$businesstype) == 0) {
                businesstype_filter <- unique(ppp_data$BusinessType)
            } else {
                businesstype_filter <- input$businesstype
            }
            
            if (length(input$loanrangeclean) == 0) {
                loanrangeclean_filter <- unique(ppp_data$LoanRangeClean)
            } else {
                loanrangeclean_filter <- input$loanrangeclean
            }
            
            
            
            # TODO: Filter by LoanRange
            
            ppp_data <- 
                filter(ppp_data,
                       (ppp_data$State.x %in% state_filter) &
                           (ppp_data$Gender %in% gender_filter) &
                           (ppp_data$RaceEthnicity %in% raceethnicity_filter) &
                           (ppp_data$Veteran %in% veteran_filter) &
                           (ppp_data$NonProfit %in% nonprofit_filter) &
                           (ppp_data$BusinessType %in% businesstype_filter) &
                           (ppp_data$LoanRangeClean %in% loanrangeclean_filter)
                       )
    
        # ==========================================================================
        
        # https://plotly.com/r/scatter-plots-on-maps/
        # geo styling
        g <- list(
            scope = 'north america',
            projection = list(type = 'albers usa'),
            showland = TRUE,
            landcolor = toRGB("gray95"),
            subunitcolor = toRGB("gray85"),
            countrycolor = toRGB("gray85"),
            countrywidth = 0.5,
            subunitwidth = 0.5
        )
    
        output$map_plotly <- renderPlotly({
    
            map_fig <- plotly::plot_geo(ppp_data, lat = ~Latitude, lon = ~Longitude)
            map_fig <- map_fig %>% add_markers(
                text = ~paste(BusinessName, LoanRangeClean, sprintf("Jobs Retained: %s", JobsRetained), sprintf("%s, %s (%s)", tools::toTitleCase(tolower(City.x)), State.x, Zip), sep = "<br />"),
                color = ~LoanRangeAmtLow, symbol = I("square"), size = I(8), hoverinfo = "text"
            )
            map_fig <- map_fig %>% colorbar(title = "Min Loan Amount")
            map_fig <- map_fig %>% layout(title = "PPP Loans - Mapped", geo = g)
            
            map_fig
            })
        
    
        # TODO: Reverse color scale order
        # TODO: Keep color scale consistent, even after filtering
    
        # ==========================================================================
    
        # output$scatterplot <- renderPlot({
        #     ggplot(ppp_data, aes(LoanRangeAmtLow, JobsRetained)) + geom_point()
        # })
    
        # ==========================================================================
    
        ppp_data$ones <- 1.0
        
        # TODO: include other factors in by=list(), ex: RaceEthnicity=ppp_data$RaceEthnicity
        timeseries_df_sum_jobs <- aggregate(ppp_data$JobsRetained, by=list(Date=ppp_data$DateApproved), FUN=sum)
        timeseries_df_count_loans <- aggregate(ppp_data$ones, by=list(Date=ppp_data$DateApproved), FUN=sum)

        output$timeseries_plotly <- renderPlotly({
            
            timeseries_fig_plotly <- plot_ly(
                x = as.Date(timeseries_df_count_loans$Date, format= "%m/%d/%y"),
                y = timeseries_df_count_loans$x,
                # mode = "markers",
                type = "bar",
                text = paste(timeseries_df_count_loans$x, "PPP loan(s) approved.")
                )
            
            timeseries_fig_plotly %>% layout(title = "PPP Loans by Approval Date") #, barmode = "stack")
            # timeseries_fig_plotly <- timeseries_fig_plotly %>% add_trace(y = ~timeseries_df_count_loans$RaceEthnicity, name = "Race/Ethnicity")
            
            timeseries_fig_plotly
        })

        # ==========================================================================
    
        # brush_sel <- reactive({
        #     user_brush <- input$user_brush
        #     sel <- brushedPoints(ppp_data,
        #                          user_brush,
        #                          xvar = "LoanRangeAmtLow",
        #                          yvar = "JobsRetained")
        #     return(sel)
        # })
        # 
        # output$table_brushed <- DT::renderDataTable(DT::datatable(brush_sel))
    
        # ==========================================================================
        
        output$reactive_df <- shiny::renderDataTable(ppp_data[c("BusinessName", "Address", "City.x", "State.x", "Zip",
                                                                "BusinessType", "RaceEthnicity", "Gender", "Veteran", "NonProfit",
                                                                "LoanRangeClean", "JobsRetained", "DateApproved", "Lender")])
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
