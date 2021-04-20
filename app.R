## app.R ##
library(shiny)
library(shinydashboard)
library(slider)
library(shinyWidgets)
#####################################################################################################
### Libraries
#####################################################################################################


library(httr, warn.conflicts = FALSE)
library(devtools, warn.conflicts = FALSE)
library(gridExtra, warn.conflicts = FALSE)
library(bea.R)
library(readxl, warn.conflicts = FALSE)
library(rvest, warn.conflicts = FALSE)
library(tidyverse, warn.conflicts = FALSE)
library(stringr, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
library(rstudioapi, warn.conflicts = FALSE)
library(rjson, warn.conflicts = FALSE)
library(blsAPI)
library(ggrepel)



#####################################################################################################
### Import and tidy date
#####################################################################################################

# Logic:
# Import saved csv-file. If date < sys.date -> parse current years data, run calculations and parse.


source("getData_All.r")



#Fix from factors to doable
df_inflation_data$date <- as.Date(df_inflation_data$date)
df_inflation_data$value <- as.numeric(as.character(df_inflation_data$value))
df_inflation_data$maturity <- as.numeric(df_inflation_data$maturity)
df_inflation_data$info <- as.character(df_inflation_data$info)
df_inflation_data$des <- as.character(df_inflation_data$des)
df_inflation_data$last_update <- as.Date(df_inflation_data$last_update)

df_inflation_data$longer_name <- as.character(df_inflation_data$longer_name)
df_inflation_data$group <- as.character(df_inflation_data$group)

df_inflation_data <- arrange(df_inflation_data, date)


#Fed speak
source("getData_Fed_Speak.r")
df_fed_inflation_count$frequency_share <-as.numeric(as.character(df_fed_inflation_count$frequency_share))

df_fed_inflation_count$frequency <-as.numeric(as.character(df_fed_inflation_count$frequency))
df_fed_inflation_count$date <- as.Date(df_fed_inflation_count$date)
df_fed_inflation_count$category <-(as.character(df_fed_inflation_count$category))
df_fed_inflation_count$additional  <-(as.character(df_fed_inflation_count$additional))
df_fed_inflation_count$subject  <-(as.character(df_fed_inflation_count$subject))
df_fed_inflation_count$avg_lt_share <-as.numeric(as.character(df_fed_inflation_count$avg_lt_share))



# ------------------------------------------------------------------------------- 
# Functions
# ------------------------------------------------------------------------------- 


fill_until_current_day <- function(df) {
  
  ##Fill from latest date to today, to keep all the plots ending on the same date.
  latest_date <- max(df$last_update) ##Get latest date
  
  dates <- data.frame(
    date = seq(latest_date, Sys.Date(), by = 'days')   #create date table
  ) 
  
  df <- full_join(dates, df, by="date") #merge
  df <- arrange(df, date) 
  
  
  
  
  
  
  ###Fixa pivot wider sÃ¥ det ska funka
  
  temp <- df %>% #create temp wider table if there are multiple variables
    select(date,des,value) %>%
    pivot_wider(
      names_from = des,
      values_from = value
    ) 
  
  temp <- temp %>% #fill and recreate longer table
    fill(names(temp), .direction = "down") %>%
    pivot_longer(
      !date,
      names_to = "des",
      values_to = "value_temp"
    ) %>%
    filter(des != "NA")
  
  df <- left_join(temp, df, by=c("des", "date"), all.x = TRUE) %>% ##join with full dataset
    select(-value) %>%
    rename(value = value_temp)
  
  df <- df %>%
    group_by(des) %>%
    fill(c("group", "info", "longer_name", "maturity", "last_update"), .direction = "down")  #fill
  
  return(df)
  
}


# ------------------------------------------------------------------------------- 
# Create lists for sidebar page Line plots
# ------------------------------------------------------------------------------- 


line_plot_nominal_rates = list("UST 3m" = "yieldYear_3m",
                               "UST 2y" = "yieldYear_2y",
                               "UST 5y" = "yieldYear_5y", 
                               "UST 10y" = "yieldYear_10y",
                               "UST 30y" = "yieldYear_30y"
                               
)

line_plot_real_rates = list("TIPS 2y" = "realyieldYear_2y",
                            "TIPS 5y" = "realyieldYear_5y",
                            "TIPS 10y" = "realyieldYear_10y",
                            "TIPS 30y" = "realyieldYear_30y"
                            
)

line_plot_inflation_rates = list("Breakeven 5y" = 	"breakeven_5y",
                                 "Breakeven 10y" = "breakeven_10y",
                                 "Breakeven 5y5y" = "frw_breakeven_5y",
                                 "Breakeven 10y10y" = "frw_breakeven_10y"
)

line_plot_spreads = list("Spread 3m10y" = "yieldYear_3m10y",
                         "Spread 2y10y" = "yieldYear_2y10y",
                         "Spread 5y30y" = "yieldYear_5y30y",
                         "Spread 2y5y10y" = "yieldYear_2y5y10y"
)

line_plot_checkbox <- c(line_plot_spreads, line_plot_real_rates, line_plot_inflation_rates,line_plot_nominal_rates)




# ------------------------------------------------------------------------------- 
# Information for FAQs
# ------------------------------------------------------------------------------- 

info_page <- function(){
    fluidRow(
        h3( "Information about the app" ),
        tags$p("This app aims to give a comprehensive picture of U.S. inflation, combining the latest data available data with inflation expectations, some key inflations triggers and Fed speak in regards to inflation. The data is gathered from different sources, like BLS, BES and U.S. Treasury. Also, with text mining approach FOMC Minutes are analysed. All data is gathered and stored on Amazon S3, and is updated only the first time the app is used each day. This framework reduces unnecessary scraping and makes the app load faster."),
        br(),
        h4( "Latest Inflation Numbers (headline and core)" ),
        
          strong("Personal Consumption Expenditures (PCE)"),
           p("PCE stands for Personal Consumption Expenditures and is the value of all the goods or services purchased by U.S. residents."),
        
         strong("Consumer Price Index (CPI)"),
           p("CPI stands for Consumer Price Index and aims to measure the average change over time in the prices paid by urban consumers for a market basket of consumer goods and services."),

          strong("Headline or Core?"),
           p("Headline inflation includes all components of the index, while food and energy prices are excluded of core inflation. These components are usually volatile, and have a low correlation to the broad price trend of the economy. Due to the volatile components of food and energy prices, headline inflation is usually more volatile then core inflation. As it includes all aspects of the economy, headline inflation is closely related to shifts in the cost of living. However, if the purpose is to measure the underlying pricing trends of the economy, core inflation should be used. FOMC often referrers to both, but uses core for the underlying trend."),
        
         strong("PCE or CPI?"),
           p("PCE is the current target for FOMC. The main reason may be that the expenditure weights in the PCE can change as people substitute away from some goods and services toward others, and PCE includes more comprehensive coverage of goods and services."),
        
        br(),
        h4( "Breakeven Inflation Rates - Market Based Expectations" ),    
        
         strong("Breakeven Inflation Rates"),
           p("The breakeven inflation rate represents a measure of expected inflation derived from Treasury Constant Maturity Securities and Treasury Inflation-Indexed Constant Maturity Securities for each term. The latest value implies what market participants expect inflation to be in the next x years, on average."),
           p("Breakeven 5y shows where the market expects the inflation will be on average for the next 5 year. A breakeven 5y5y shows where the market expects the inflation will be, on average, over the 5 year period that begins in 5 years."),
        
        
        br(),
        h4( "Inflation Drivers" ),
        
          strong("Average Hourly Earnings (AHE)"),
            p("AHE is the average amount employees make per hour in the U.S. in a given month. AHE is an important indicator of labor cost inflation and of the tightness of the labor market. However, as most measures, it´s important to note if a change in earnings are due to a broad wage increase, or if the demographic aspects of the labor market has changed. 2020 Covid crisis gave an uplift to AHE, mainly due to a large drop of employment in low salary groups."),
        
          strong("Employment Cost Index (ECI)"),
            p("ECI is a quarterly series detailing the changes in the costs of labor for business in the U.S. ECI is the most comprehensive measure of labor costs, and in contrast to AHE, also includes non-cash benefits costs as healthcare. Also, it is not affected by shifts in the composition of employment between high-wage and low-wage industries, because ECI represents labor costs for the same jobs over time. FOMC usually gives more weight to ECI rather than the monthly AHE report deciding wheter wage inflation and wage costs are increasing or not.") ,
        
          strong("Employment To Population"),
            p("Employment to population ration is a measure of the number of people employed against the total working age population. Unlike the unemployment rate, Employment to Population also includes unemployed people not looking for jobs. For the similar reason, employment to population is a more accurate ration then the, more famous, participation rate which only consists of those in the labor force."),
        
        #  strong("GDP Growth"),
         #   p("Butterfly spreads are calculated from the Constant Maturity Treasury Rates and shows the curvature of the yield curve. A butterfly is a non-parallel shift of the yield curve when long and short rates shifts more (or less) then medium-term rates."),
        
          strong("Money Supply M2"),
            p("M2 is a measure of the money supply that includes cash, checking deposits and easily convertible near money. M2 is published weekly."),
        
          strong("Money Supply M2 Velocity"),
            p("The velocity of money is the frequency at which one unit of currency is used to purchase goods and services. The velocity is usually measured as a ratio of GDP to a countries money supply, M1 or M2. A high velocity has traditionally been associated with a healthy, expanding economy. M2 velocity has decreased the last 20 plus years.") ,
        
        
        br()
    )
}


#####################################################################################################
### Create User Interface
#####################################################################################################


ui <- dashboardPage(skin = "blue",
                    dashboardHeader(title = "US Inflation Monitor"),
                    ## Sidebar content
                    dashboardSidebar(
                        tags$style(
                            "#sidebarItemExpanded {
            overflow: auto;
            height: calc(100vh - 50px) !important;
        }"
                        ),
                        sidebarMenu(
                            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                            #menuItem("Curve plots", tabName = "curve_plots", icon = icon("chart-line", lib = "font-awesome")),
                          #  menuItem("Line plots", tabName = "line_plots", icon = icon("chart-line", lib = "font-awesome")),
                            menuItem( "FAQs", tabName = 'help', icon = icon('question-circle') ),
                            conditionalPanel("input.sidebarmenu === 'curve_plots'")
                            
                        )
                    ),
                    
                    
                    ## Body content
                    dashboardBody(
                        tabItems(
                            
                            # ------------------------------------------------------------------------------- 
                            # Tab Panel Dashboard
                            # ------------------------------------------------------------------------------- 
                            tabItem(tabName = "dashboard",
                                    fluidRow(
                                        shinydashboard::valueBoxOutput("initials"),
                                        
                                        box(title="Select input parameters",
                                            status="primary", solidHeader = TRUE,width=12,

                                            
                                            box(
                                                #    h4("Select period"),
                                                radioGroupButtons(
                                                    inputId = "dash_time_frame",
                                                    label = "Time frame",
                                                    #   choices = c("1 month", 
                                                    #               "3 months", "1 year", "3 years", "5 years", "10 years", "Max"),
                                                    choices = list("1 year" = paste(as.Date(Sys.Date() - years(1)), sep=""),
                                                                   "3 years" = paste(as.Date(Sys.Date() - years(3)), sep=""),
                                                                   "5 years" = paste(as.Date(Sys.Date() - years(5)), sep=""),
                                                                   "10 years" = paste(as.Date(Sys.Date() - years(10)), sep=""),
                                                                   "Max" = paste(as.Date(min(df_inflation_data$date)), sep="")),
                                                    
                                                    individual = TRUE,
                                                    checkIcon = list(
                                                        yes = tags$i(class = "fa fa-circle", 
                                                                     style = "color: light-blue"),
                                                        no = tags$i(class = "fa fa-circle-o", 
                                                                    style = "color: light-blue")),
                                                    selected = c("5 year" = paste(as.Date(Sys.Date() - years(5)), sep=""))
                                                ),width=12)
                                            
                                        ),
                                        box(
                                          width = 12, background = "light-blue",
                                          "Latest Inflation Numbers (headline and core)"
                                        ),
                                              box(plotOutput("plot_dash_pce")), #PCE
                                              box(plotOutput("plot_dash_cpi")),#cpi
                                        
                                        box(
                                          width = 12, background = "light-blue",
                                          "Inflation Breakeven Expectations Rates"
                                        ),
                                        
                                        
                                          box(plotOutput("plot_dash_5y_breakeven")), #curvature
                                          box(plotOutput("plot_dash_10y_breakeven")), #par curve
                                        
                                        
                                        box(
                                          width = 12, background = "light-blue",
                                          "Inflation Drivers"
                                        ),

                                        box(plotOutput("plot_dash_hourly_earnings")), #hourly earnings
                                        box(plotOutput("plot_dash_employment_cost")), #employment cost,
                                        box(plotOutput("plot_dash_employment_to_population")), #employment to population
                                        box(plotOutput("plot_dash_gdp")), #employment to population
                                        box(plotOutput("plot_dash_m2")), #employment to population
                                        box(plotOutput("plot_dash_m2_velocity")), #employment to population
                                        
                                        #fed
                                        box(
                                          width = 12, background = "light-blue",
                                          "Fed Speak - Text Mining FOMC Minutes"
                                        ),
                                        
                                        box(plotOutput("plot_dash_fed_inflation_count")) #fed speak
                                        
                                        
                                        
                                    )
                            ),
                            
                          
                            
                            # ------------------------------------------------------------------------------- 
                            # Tab Panel FAQs page
                            # -------------------------------------------------------------------------------                             
                            
                            tabItem( tabName = 'help',
                                     ## 3.5.1 Data sources ---------------
                                     box(title="Information and terminology",
                                         status="primary", solidHeader = TRUE,width=12,
                                         
                                         box(id = 'info_page',
                                             info_page(), width=12 )),
                            )
                            
                            
                            
                            
                        ),
                        
                        # Footer -------------------------------
                        hr(style = "border-color: #cbcbcb;"),
                        fluidRow(
                            column(9,
                                   p('All of the data used to generate this app were obtained from ', 
                                     tags$a(href = "www.bea.gov", 'Bureau of Economic Analysis', target = '_blank'),
                                     ', ',
                                     tags$a(href = "www.bls.gov", 'Bureau of Labor Statistics', target = '_blank'),
                                     ' and ',
                                     tags$a(href = "https://home.treasury.gov/", 'US Treasury', target = '_blank'),
                                     '.',
                                     style = "font-size: 85%"),
                                   
                                   p(' Disclaimer: BLS, BEA, US Treasury  cannot vouch for the data or analyses derived from these data after the data have been retrieved.',
                                     style = "font-size: 85%"),
                                   
                                  p("App created by ", tags$a(href = "https://www.cnordenlow.com", 'Christoffer Nordenlow', target = '_blank'), " in 2021", HTML("&bull;"),
                                     "Find the code on Github:", tags$a(href = "https://github.com/cnordenlow/us-inflation-monitor", tags$i(class = 'fa fa-github', style = 'color:#5000a5'), target = '_blank'), style = "font-size: 85%"),
                                   p("Have a question? Spot an error? Send an email ", tags$a(href = "mailto:christoffer.nordenlow@outlook.com", tags$i(class = 'fa fa-envelope', style = 'color:#990000'), target = '_blank'), style = "font-size: 85%"),
                                   # p(tags$em("Last updated: November 2020"), style = 'font-size:75%')
                            )
                        )
                    )
)

#####################################################################################################
### Server
#####################################################################################################


server <- function(input, output,session) {
    

    start_date = as.Date(max(df_inflation_data$date)) - 365
    end_date = as.Date(max(df_inflation_data$date))
    
    

    # ------------------------------------------------------------------------------- 
    # PCE
    # ------------------------------------------------------------------------------- 
    
    Input_dashboard_pce <- reactive({

        end_date = as.Date(Sys.Date())
        start_date = as.Date(input$dash_time_frame)

        ######################PCE
        df <- df_inflation_data %>%
          filter(group == "inflation") %>%
          filter(des %in% c("PCE", "Core-PCE")) %>%
          filter(date >= start_date) %>%
          filter(date <= end_date) %>%
          filter(!is.na(value))

      df <- fill_until_current_day(df)
        

        
    })
    
    output$plot_dash_pce <- renderPlot({
        
        df <- Input_dashboard_pce()
        ###show latest value:
        data_ends <- df %>% filter(date == max(last_update))
        
        latest_date <- max(df$last_update)
        

        
        title_text = paste("Personal Consumption Expenditures", sep="")
        subtitle_text = paste("PCE and Core-PCE (dashed: FOMC Inflation Target)", sep="")

        caption_test = "Source: BEA, own calculations."
        
        p1 <- ggplot(df, aes(x = date, y = value, group = des, color = des)) + 
          geom_line(size = 1)+
          geom_hline(yintercept = 2, linetype = "dashed")+
          theme_minimal() +
          theme(legend.position="bottom",
                legend.title = element_blank(),
                plot.caption=element_text(hjust=0),
                plot.subtitle=element_text(face="italic"),
                plot.title=element_text(size=16,face="bold"))+
          theme(panel.grid.minor = element_line(colour = "white"))+
          
          labs(x="Date",y="Percent",
               title=title_text,
               subtitle=subtitle_text,
               caption=caption_test)+ 
          geom_text_repel(
            aes(label = value), data = data_ends,
            color = "black", size = 4
          )
        
        p1
        
        
    })
    
    
    # ------------------------------------------------------------------------------- 
    # CPI
    # ------------------------------------------------------------------------------- 
    
    Input_dashboard_cpi <- reactive({
      
      end_date = as.Date(Sys.Date())
      start_date = as.Date(input$dash_time_frame)
      
      
      ######################PCE
      df <- df_inflation_data %>%
        filter(group == "inflation") %>%
        filter(des %in% c("CPI", "Core-CPI")) %>%
        filter(date >= start_date) %>%
        filter(date <= end_date) %>%
        filter(!is.na(value))
      
      df <- fill_until_current_day(df)
      

      
    })
    
    
    output$plot_dash_cpi <- renderPlot({
      
      df <- Input_dashboard_cpi()
      ###show latest value:
      data_ends <- df %>% filter(date == max(last_update))
      
      latest_date <- max(df$last_update)
      
      title_text = paste("Consumer Price Index", sep="")
      subtitle_text = paste("CPI and Core-CPI (dashed: FOMC Inflation Target), ", sep="")
      
      caption_test = "Source: BLS, own calculations."
      
      p1 <- ggplot(df, aes(x = date, y = value, group = des, color = des)) + 
        geom_line(size = 1)+
        geom_hline(yintercept = 2, linetype = "dashed")+
        theme_minimal() +
        theme(legend.position="bottom",
              legend.title = element_blank(),
              plot.caption=element_text(hjust=0),
              plot.subtitle=element_text(face="italic"),
              plot.title=element_text(size=16,face="bold"))+
        theme(panel.grid.minor = element_line(colour = "white"))+
        
        labs(x="Date",y="Percent",
             title=title_text,
             subtitle=subtitle_text,
             caption=caption_test)+ 
        geom_text_repel(
          aes(label = value), data = data_ends,
          color = "black", size = 4
        )
      
      p1
      
      
    })
    
    
    
    
    ################BREakeven inflation expectations
    
    # ------------------------------------------------------------------------------- 
    # Breakeven Inflation expectations, 5y
    # ------------------------------------------------------------------------------- 

    
    Input_dashboard_5y_breakeven <- reactive({
      
      end_date = as.Date(Sys.Date())
      start_date = as.Date(input$dash_time_frame)
      
#      start_date = end_date - 720
      
      ######################PCE
      df <- df_inflation_data %>%
        filter(group == "rates") %>%
        filter(des %in% c("breakeven", "frw_breakeven")) %>%
        filter(maturity == "5") %>%
        filter(date >= start_date) %>%
        filter(date <= end_date) %>%
        filter(!is.na(value))
      
      
      ##Fill from latest date to today, to keep all the plots ending on the same date.
      latest_date <- max(df$last_update)
      
      dates <- data.frame(
        date = seq(latest_date, Sys.Date(), by = 'days')
      ) 
      
      df <- full_join(dates, df, by="date")
      df <- arrange(df, date) %>%
        fill(names(df), .direction = "down")
      
      
    })
    
    
    output$plot_dash_5y_breakeven <- renderPlot({
      
      df <- Input_dashboard_5y_breakeven()
      ###show latest value:
      data_ends <- df %>% filter(date == max(last_update))
      
      latest_date <- max(df$last_update)

      title_text = paste("Breakeven Inflation Rate, 5y", sep="")
      subtitle_text = paste("Breakeven Inflation Rates (dashed: FOMC Inflation target)", sep="")
      
      caption_test = "Source: US Treasury, own calculations."
      
      p1 <- ggplot(df, aes(x = date, y = value, group = longer_name, color = longer_name)) + 
        geom_line(size = 1)+
        geom_hline(yintercept = 2, linetype = "dashed")+

        theme_minimal() +
        theme(legend.position="bottom",
              legend.title = element_blank(),
              plot.caption=element_text(hjust=0),
              plot.subtitle=element_text(face="italic"),
              plot.title=element_text(size=16,face="bold"))+

        theme(panel.grid.minor = element_line(colour = "white"))+
        
        labs(x="Date",y="Percent",
             title=title_text,
             subtitle=subtitle_text,
             caption=caption_test)+ 
        geom_text_repel(
          aes(label = value), data = data_ends,
          color = "black", size = 4
        )
      
      p1
      
      
    })
    
    
    
    # ------------------------------------------------------------------------------- 
    # Breakeven Inflation expectations, 10y
    # ------------------------------------------------------------------------------- 
    
    
    
    Input_dashboard_10y_breakeven <- reactive({
      
      end_date = as.Date(Sys.Date())
      start_date = as.Date(input$dash_time_frame)
      
      #      start_date = end_date - 720
      
      ######################PCE
      df <- df_inflation_data %>%
        filter(group == "rates") %>%
        filter(des %in% c("breakeven", "frw_breakeven")) %>%
        filter(maturity == "10") %>%
        filter(date >= start_date) %>%
        filter(date <= end_date) %>%
        filter(!is.na(value))
      
      ##Fill from latest date to today, to keep all the plots ending on the same date.
      latest_date <- max(df$last_update)
      
      dates <- data.frame(
        date = seq(latest_date, Sys.Date(), by = 'days')
      ) 
      
      df <- full_join(dates, df, by="date")
      df <- arrange(df, date) %>%
        fill(names(df), .direction = "down")
      
    })
    
    
    output$plot_dash_10y_breakeven <- renderPlot({
      
      df <- Input_dashboard_10y_breakeven()
      ###show latest value:
      data_ends <- df %>% filter(date == max(last_update))
      
      latest_date <- max(df$last_update)

      title_text = paste("Breakeven Inflation Rate, 10y", sep="")
      subtitle_text = paste("Breakeven Inflation Rates (dashed: FOMC Inflation target)", sep="")
      
      caption_test = "Source: US Treasury, own calculations."
      
      p1 <- ggplot(df, aes(x = date, y = value, group = longer_name, color = longer_name)) + 
        geom_line(size = 1)+
        geom_hline(yintercept = 2, linetype = "dashed")+
        # geom_point()+
        # geom_line(aes(y=date_1), linetype="dashed")+
        theme_minimal() +
        theme(legend.position="bottom",
              legend.title = element_blank(),
              plot.caption=element_text(hjust=0),
              plot.subtitle=element_text(face="italic"),
              plot.title=element_text(size=16,face="bold"))+
        
        # scale_x_continuous(breaks = scale_breaks)+
        theme(panel.grid.minor = element_line(colour = "white"))+
        
        labs(x="Date",y="Percent",
             title=title_text,
             subtitle=subtitle_text,
             caption=caption_test)+ 
        geom_text_repel(
          aes(label = value), data = data_ends,
          color = "black", size = 4
        )
      
      p1
      
      
    })
    
    
    
    
    
    
    ##Drivers
    ##
    ###################################################
    
    # ------------------------------------------------------------------------------- 
    # Average Hourly Earnings
    # ------------------------------------------------------------------------------- 
    
    
    Input_dashboard_average_earnings <- reactive({
      
      end_date = as.Date(Sys.Date())
      start_date = as.Date(input$dash_time_frame)
      
      #      start_date = end_date - 720
      
      ######################PCE
      df <- df_inflation_data %>%
        filter(group == "earnings") %>%
        filter(des %in% c("Average Hourly Earnings")) %>%
        filter(date >= start_date) %>%
        filter(date <= end_date) 

      df <- fill_until_current_day(df)
      
      
      
    })
    
    
    output$plot_dash_hourly_earnings <- renderPlot({
      
      df <- Input_dashboard_average_earnings()
      ###show latest value:
      data_ends <- df %>% filter(date == max(last_update))
      
      latest_date <- max(df$last_update)

      title_text = paste("Average Hourly Earnings", sep="")
      subtitle_text = paste("Average Hourly Earnings", sep="")
      
      caption_test = "Source: BLS, own calculations."
      
      p1 <- ggplot(df, aes(x = date, y = value, group = des, color = des)) + 
        geom_line(size = 1)+
        theme_minimal() +
        theme(legend.position="bottom",
              legend.title = element_blank(),
              plot.caption=element_text(hjust=0),
              plot.subtitle=element_text(face="italic"),
              plot.title=element_text(size=16,face="bold"))+

        theme(panel.grid.minor = element_line(colour = "white"))+
        
        labs(x="Date",y="Percent",
             title=title_text,
             subtitle=subtitle_text,
             caption=caption_test)+ 
        geom_text_repel(
          aes(label = value), data = data_ends,
          color = "black", size = 4
        )
      
      p1
      
      
    })
    
    # ------------------------------------------------------------------------------- 
    # Employment Cost Index
    # ------------------------------------------------------------------------------- 
    
    
    Input_dashboard_employment_cost <- reactive({
      
      end_date = as.Date(Sys.Date())
      start_date = as.Date(input$dash_time_frame)
      
      #      start_date = end_date - 720
      
      ######################PCE
      df <- df_inflation_data %>%
        filter(group == "earnings") %>%
        filter(des %in% c("Employment Cost Index")) %>%
        filter(date >= start_date) %>%
        filter(date <= end_date) 
      
      df <- fill_until_current_day(df)
      
      
      
    })
    
    
    output$plot_dash_employment_cost <- renderPlot({
      
      df <- Input_dashboard_employment_cost()
      ###show latest value:
      data_ends <- df %>% filter(date == max(last_update))
      
      latest_date <- max(df$last_update)

      title_text = paste("Employment Cost Index", sep="")
      subtitle_text = paste("Employment Cost Index", sep="")
      caption_test = "Source: BLS, own calculations."
      
      
      p1 <- ggplot(df, aes(x = date, y = value, group = des, color = des)) + 
        geom_line(size = 1)+
        theme_minimal() +
        theme(legend.position="bottom",
              legend.title = element_blank(),
              plot.caption=element_text(hjust=0),
              plot.subtitle=element_text(face="italic"),
              plot.title=element_text(size=16,face="bold"))+
        
        # scale_x_continuous(breaks = scale_breaks)+
        theme(panel.grid.minor = element_line(colour = "white"))+
        
        labs(x="Date",y="Percent",
             title=title_text,
             subtitle=subtitle_text,
             caption=caption_test)+ 
        geom_text_repel(
          aes(label = value), data = data_ends,
          color = "black", size = 4
        )
      
      p1
      
      
    })
    
    
    
    # ------------------------------------------------------------------------------- 
    # Employment To Population
    # ------------------------------------------------------------------------------- 
    
    
    Input_dashboard_employment_to_population <- reactive({
      
      end_date = as.Date(Sys.Date())
      start_date = as.Date(input$dash_time_frame)
      
      #      start_date = end_date - 720
      
      df <- df_inflation_data %>%
        filter(group == "employment") %>%
        filter(des %in% c("Employment-To-Population ratio")) %>%
        filter(date >= start_date) %>%
        filter(date <= end_date) 
      
      df <- fill_until_current_day(df)
      
      
      
    })
    
    
    output$plot_dash_employment_to_population <- renderPlot({
      
      df <- Input_dashboard_employment_to_population()
      ###show latest value:
      data_ends <- df %>% filter(date == max(last_update))
      
      latest_date <- max(df$last_update)
      

      title_text = paste("Employment To Population", sep="")
      subtitle_text = paste("Employment To Population", sep="")
      caption_test = "Source: BLS, own calculations."
      
      
      p1 <- ggplot(df, aes(x = date, y = value, group = des, color = des)) + 
        geom_line(size = 1)+
        #geom_hline(yintercept = 2, linetype = "dashed")+
        # geom_point()+
        # geom_line(aes(y=date_1), linetype="dashed")+
        theme_minimal() +
        theme(legend.position="bottom",
              legend.title = element_blank(),
              plot.caption=element_text(hjust=0),
              plot.subtitle=element_text(face="italic"),
              plot.title=element_text(size=16,face="bold"))+
        
        # scale_x_continuous(breaks = scale_breaks)+
        theme(panel.grid.minor = element_line(colour = "white"))+
        
        labs(x="Date",y="Percent",
             title=title_text,
             subtitle=subtitle_text,
             caption=caption_test)+ 
        geom_text_repel(
          aes(label = value), data = data_ends,
          color = "black", size = 4
        )
      
      p1
      
      
    })
    
    
    
    
    
    
    # ------------------------------------------------------------------------------- 
    # GDP
    # ------------------------------------------------------------------------------- 
    
    
    
    

    Input_dashboard_gdp <- reactive({
      
      end_date = as.Date(Sys.Date())
      start_date = as.Date(input$dash_time_frame)
      
      #      start_date = end_date - 720
      
      df <- df_inflation_data %>%
        filter(group == "GDP") %>%
        filter(des %in% c("GDP_YoY")) %>%
        filter(date >= start_date) %>%
        filter(date <= end_date) 
      
      df <- fill_until_current_day(df)
      
      
      
    })
    
    
    output$plot_dash_gdp<- renderPlot({
      
      df <- Input_dashboard_gdp()
      ###show latest value:
      data_ends <- df %>% filter(date == max(last_update))
      
      latest_date <- max(df$last_update)
      
      #      title_text = paste("Consumer Price Index", sep="")
      #     subtitle_text = paste("CPI and Core-CPI (dashed: FOMC Inflation Tartet), ", latest_date, sep="")
      
      title_text = paste("GDP", sep="")
      subtitle_text = paste("GDP", sep="")
      
      caption_test = "Source: BEA, own calculations."
      
      
      p1 <- ggplot(df, aes(x = date, y = value, group = des, color = des)) + 
        geom_line(size = 1)+
        geom_hline(yintercept = 0)+
        # geom_point()+
        # geom_line(aes(y=date_1), linetype="dashed")+
        theme_minimal() +
        theme(legend.position="bottom",
              legend.title = element_blank(),
              plot.caption=element_text(hjust=0),
              plot.subtitle=element_text(face="italic"),
              plot.title=element_text(size=16,face="bold"))+
        
        # scale_x_continuous(breaks = scale_breaks)+
        theme(panel.grid.minor = element_line(colour = "white"))+
        
        labs(x="Date",y="Percent",
             title=title_text,
             subtitle=subtitle_text,
             caption=caption_test)+ 
        geom_text_repel(
          aes(label = value), data = data_ends,
          color = "black", size = 4
        )
      
      p1
      
      
    })
    
    
    
    # ------------------------------------------------------------------------------- 
    # Money Supply M2
    # ------------------------------------------------------------------------------- 
    
    
    
    
    Input_dashboard_m2 <- reactive({
      
      end_date = as.Date(Sys.Date())
      start_date = as.Date(input$dash_time_frame)
      
      #      start_date = end_date - 720
      
      df <- df_inflation_data %>%
        filter(group == "money_supply") %>%
        filter(des %in% c("M2_YoY")) %>%
        filter(date >= start_date) %>%
        filter(date <= end_date) 
      
      df <- fill_until_current_day(df)
      
      
      
    })
    
    
    output$plot_dash_m2<- renderPlot({
      
      df <- Input_dashboard_m2()
      ###show latest value:
      data_ends <- df %>% filter(date == max(last_update))
      
      latest_date <- max(df$last_update)
      
      #      title_text = paste("Consumer Price Index", sep="")
      #     subtitle_text = paste("CPI and Core-CPI (dashed: FOMC Inflation Tartet), ", latest_date, sep="")
      
      title_text = paste("Money Supply, M2", sep="")
      subtitle_text = paste("Money Supply, M2", sep="")
      caption_test = "Source: Federal Reserve, own calculations."
      
      
      p1 <- ggplot(df, aes(x = date, y = value, group = des, color = des)) + 
        geom_line(size = 1)+
        #geom_hline(yintercept = 2, linetype = "dashed")+
        # geom_point()+
        # geom_line(aes(y=date_1), linetype="dashed")+
        theme_minimal() +
        theme(legend.position="bottom",
              legend.title = element_blank(),
              plot.caption=element_text(hjust=0),
              plot.subtitle=element_text(face="italic"),
              plot.title=element_text(size=16,face="bold"))+
        
        # scale_x_continuous(breaks = scale_breaks)+
        theme(panel.grid.minor = element_line(colour = "white"))+
        
        labs(x="Date",y="Percent",
             title=title_text,
             subtitle=subtitle_text,
             caption=caption_test)+ 
        geom_text_repel(
          aes(label = value), data = data_ends,
          color = "black", size = 4
        )
      
      p1
      
      
    })
    
    
    
    
    # ------------------------------------------------------------------------------- 
    # Money Supply M2 Velocity
    # ------------------------------------------------------------------------------- 
    
    
    
    
    Input_dashboard_m2_velocity <- reactive({
      
      end_date = as.Date(Sys.Date())
      start_date = as.Date(input$dash_time_frame)
      
     # start_date = end_date - 720
      
      ######################PCE
      df <- df_inflation_data %>%
        filter(group == "money_supply") %>%
        filter(des %in% c("M2_Velocity")) %>%
        filter(date >= start_date) %>%
        filter(date <= end_date) 
      
      
      df <- fill_until_current_day(df)
      
      

    })
    
    
    output$plot_dash_m2_velocity<- renderPlot({
      
      df <- Input_dashboard_m2_velocity()
      ###show latest value:
      data_ends <- df %>% filter(date == max(last_update))
      
      latest_date <- max(df$last_update)
      
 
      title_text = paste("M2 Velocity", sep="")
      subtitle_text = paste("M2 Velocity", sep="")
      
      caption_test = "Source: BEA, Federal Reserve, own calculations."
      
      
      p1 <- ggplot(df, aes(x = date, y = value, group = des, color = des)) + 
        geom_line(size = 1)+
        #geom_hline(yintercept = 2, linetype = "dashed")+
        # geom_point()+
        # geom_line(aes(y=date_1), linetype="dashed")+
        theme_minimal() +
        theme(legend.position="bottom",
              legend.title = element_blank(),
              plot.caption=element_text(hjust=0),
              plot.subtitle=element_text(face="italic"),
              plot.title=element_text(size=16,face="bold"))+
        
        # scale_x_continuous(breaks = scale_breaks)+
        theme(panel.grid.minor = element_line(colour = "white"))+
        
        labs(x="Date",y="Percent",
             title=title_text,
             subtitle=subtitle_text,
             caption=caption_test)+ 
        geom_text_repel(
          aes(label = value), data = data_ends,
          color = "black", size = 4
        )
      
      p1
      
      
    })
    
    
    
    
    
    Input_dashboard_fed_speak_inflation_count <- reactive({
      
      end_date = as.Date(Sys.Date())
      start_date = as.Date(input$dash_time_frame)
      
      # start_date = end_date - 720
      
      ######################PCE

        # ------------------------------------------------------------------------------- 
      # Count number of times Fed is using the word inflation.
      # ------------------------------------------------------------------------------- 
      
      df <- df_fed_inflation_count %>%
        filter(category %in% c("broad_topics")) %>%
        filter(subject %in% c("Inflation")) %>%
        filter(additional == "count_words") %>%
        arrange(date) %>%
        mutate(longer_name = "Inflation Count") %>%
        filter(date >= start_date) %>%
        filter(date <= end_date)  
      
 
      
    })
    
    
    output$plot_dash_fed_inflation_count <- renderPlot({
      
      df <- Input_dashboard_fed_speak_inflation_count()
      ###show latest value:
      #data_ends <- df %>% filter(date == max(last_update))
      
      latest_date <- max(df$date)
      
      
      title_text = paste("Fed Speak: Inflation Count", sep="")
      subtitle_text = paste("The use of the word inflation divided by number of paragraphs. \nLong term average in black", sep="")
      
      caption_text = "Source: FOMC, own calculations."
      

      p <- ggplot(data=df, aes(x=date, y=frequency_share,group = longer_name, color = longer_name)) + 
        geom_line(size=1)+
        geom_point(size=2)+
        # geom_line( aes(y=fed_funds_rate), size=1, color="light-blue") +
        
        geom_hline(yintercept=max(df$avg_lt_share), linetype="dashed", color = "black",size=1)+
        theme_light()+
        #  theme_minimal(base_size=8)+
        theme(legend.position="bottom",
              legend.title = element_blank(),
              plot.caption=element_text(hjust=0),
              plot.subtitle=element_text(face="italic"),
              plot.title=element_text(size=16,face="bold"))+
        
        labs(x="",y="Share of paragraphs",
             title=title_text,
             subtitle=subtitle_text,
             caption=caption_text)
      
      p
      
      
    })


    
}

shinyApp(ui, server)
