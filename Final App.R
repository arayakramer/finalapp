library('fpp3')
library('shiny')
library('shinydashboard')
library('ggplot2')
library('seasonal')

data('aus_production')

aus_20 <- tail(aus_production, 20)
fit <- aus_20 %>%
  model(TSLM(Gas ~trend() + season()))

ui <- dashboardPage(
  dashboardHeader(title = "Australian Oil Production"),
  dashboardSidebar(
    sidebarMenu(
    menuItem("Introduction", tabName = "intro"),
    menuItem("Simple Time Series", tabName = "simple_models"),
    menuItem("Seasonality", tabName = "seasonality"),
    menuItem("Autocorrelation", tabName = "autocorrelation"),
    menuItem("Decomposition", tabName = "decomposition"),
    menuItem("Exponential Smoothing", tabName = "smoothing"),
    menuItem("ARIMA", tabName = "arima")
  )),
  dashboardBody(
    tabItems(
      tabItem(tabName = "intro",
              h2("Exploring Australian Gas Production"),
              fluidRow(box(
                title = "Introduction",
                textOutput("intro_text"),
                solideHeader = TRUE))
      ),
      
      tabItem(tabName = "simple_models",
              h2("Total Australian Gas Production 1956-2010"),
              fluidRow(box(
                title = "Full Time Series Model",
                solidHeader = TRUE,
                plotOutput("full_ts"),
                textOutput("simple_text"))),
              fluidRow(box(
                title = "Simple Model Forecasting",
                solidHeader = TRUE,
                plotOutput("simple_model"),
                radioButtons(
                  inputId = "simple_choices",
                  label = "Choose other models you would like to view:",
                  choices = c("Naive", "Seasonal Naive", "Mean", "Drift"),
                  selected = NULL)))
              ),
      
      tabItem(tabName = "seasonality",
              h2("Quarterly Gas Production Per Year"),
              fluidRow(
                box(plotOutput("season_plot"),
                    textOutput("season_text")))
              ),
      
      tabItem(tabName = "autocorrelation",
              h2("Autocorrelation of Gas Production"),
              fluidRow(
                box(plotOutput("auto_plot"),
                    textOutput("auto_text")))
              ),
      
      tabItem(tabName = "decomposition",
              h2("Decomposition Breakdown of Gas Production"),
              fluidRow(
                box(plotOutput("decomp_plot"),
                    textOutput("decomp_text")))
              ),
      
      tabItem(tabName = "smoothing",
              h2("Exponential Smoothing Models"),
              fluidRow(
                box(radioButtons(
                  inputId = "smoothing_choices",
                  label = "Choose which exponential smoothing model to view:",
                  choices = c("Holts", "Holts/Winters"),
                  selected = "Holts"),
                  plotOutput("smoothing_plot")))
              ),
      
      tabItem(tabName = "arima",
              h2("ARIMA Model"),
              fluidRow(box(
                sliderInput(
                  inputId = "years",
                  label = "years",
                  min = 1,
                  max = 20,
                  step = 1,
                  value = 5
                ),
                plotOutput("arima_model"),
                plotOutput("arima_gg"))))
    
  )
))


server <- function(input, output) {
  
  output$intro_text <- renderText("Using the data from 'aus_production', quarterly data was used from 
                                  1956 to 2010 to record Australian gas production.")
  
  output$full_ts <- renderPlot({
    aus_production %>%
      autoplot(Gas) +
      labs(x = "Year", y = "Gas Production (Petajoules)")
  })
  
  output$simple_model <- renderPlot({
    if(input$simple_choices == "Naive") {
      aus_production %>%
        filter(!is.na(Gas)) %>%
        model(Naive = NAIVE(Gas))%>%
        forecast(h = "5 years")%>%
        autoplot(aus_production, level = NULL) +
        labs(x = "Year", y = "Gas Production (Petajoules)") +
        guides(colour = guide_legend(title = "Forecast"))
    } else if(input$simple_choices == "Seasonal Naive") {
      gas_fit <- aus_production %>%
        filter(!is.na(Gas)) %>%
        model(Seasonal_Naive = SNAIVE(Gas))
      gas_fc <- gas_fit %>%
        forecast(h = "5 years")
      gas_fc %>%
        autoplot(aus_production, level = NULL) +
        labs(x = "Year", y = "Gas Production (Petajoules)") +
        guides(colour = guide_legend(title = "Forecast"))
    } else if(input$simple_choices == "Mean") {
      gas_fit <- aus_production %>%
        filter(!is.na(Gas)) %>%
        model(Mean = MEAN(Gas))
      gas_fc <- gas_fit %>%
        forecast(h = "5 years")
      gas_fc %>%
        autoplot(aus_production, level = NULL) +
        labs(x = "Year", y = "Gas Production (Petajoules)") +
        guides(colour = guide_legend(title = "Forecast"))
    } else if (input$simple_choices == "Drift") {
      gas_fit <- aus_production %>%
        filter(!is.na(Gas)) %>%
        model(Drift = RW(Gas ~ drift()))
      gas_fc <- gas_fit %>%
        forecast(h = "5 years")
      gas_fc %>%
        autoplot(aus_production, level = NULL) +
        labs(x = "Year", y = "Gas Production (Petajoules)") +
        guides(colour = guide_legend(title = "Forecast"))
    }
  })
  
  output$simple_text <- renderText("From 1956, to 2010, gas production in Australia has been
                                     recorded quarterly. There is an obvious increasing trend 
                                     that seems to have seasonality and possibly cyclical
                                     behavior.")
  
  output$season_plot <- renderPlot({
    aus_production %>%
      gg_season(Gas) +
      labs(x = "Quarter", y = "Gas Production (Petajoules)")
  })
  
  output$season_text <- renderText("When looking at the gas production against each quarter, 
                                    a seasonal pattern starts to become more promienent as years
                                    continue. Every year, gas production seems to increase throughout the
                                    year until around quarter 3. Production seems to decrease 
                                    going into quarter 4.")
  output$auto_plot <- renderPlot({
    aus_production %>%
      ACF(Gas) %>%
      autoplot() +
      labs(x = "Lag", y = "ACF")
  })
  
  output$auto_text <- renderText("This plot analyzes how past gas production correlates
                                  to current gas production. Gas production is, again, proven 
                                  that the data has seasonality and a cyclical trend. The seasonality 
                                  is shown through the similar peaks and troughs that happen over
                                  time. A cyclical trend is shown through the consistent decrease.")
  
  output$decomp_plot <- renderPlot({
    aus_20 %>%
      model(
        classical_decomposition(Gas, type = 'multiplicative')
      ) %>%
      components() %>%
      autoplot()
  })
  
  output$decomp_text <- renderText("To remove variation throughout the years, a subset was used only from the 
                                    years 1990 to 2010. Seasonality is shown through the pattern in the seasonal graph.
                                    ")
  
  output$smoothing_plot <- renderPlot({
    if (input$smoothing_choices == "Holts") {
      aus_production %>%
        model(holt = ETS(Gas ~ error("A") + trend("A") + season("N"))) %>%
        forecast(h = "5 years") %>%
        autoplot(aus_production) +
        labs(x = "Year", y = "Gas Production (Petajoules)")
    } else if (input$smoothing_choices == "Holts/Winters") {
      aus_production %>%
        model(
          additive = ETS(Gas ~ error("A") + trend("A") + season("A")),
          multiplicative = ETS(Gas ~ error("M") + trend("A") + season("N"))) %>%
        forecast(h = "5 years") %>%
        autoplot(aus_production)
    }
  })
  
  output$arima_model <- renderPlot({
    aus_production %>%
      model(ARIMA(Gas))%>%
      forecast(h = input$years) %>%
      autoplot(aus_production)
  })
  
  output$arima_gg <- renderPlot({
    aus_production %>%
      model(ARIMA(Gas))%>%
      gg_tsresiduals()
  })
  
}


shinyApp(ui = ui, server = server)

