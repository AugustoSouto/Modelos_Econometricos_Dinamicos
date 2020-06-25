# Creado por Federico Molina para el curso de Series de Tiempo
# Profesora titular: Silvia Rodriguez.
# Ayudante: Federico Molina
# Año 2019

# Idealmente la aplicación debería:
# Tener una opción para cargar la serie de interés
# Tener una opción para generar un archivo con las predicciones de interés


# Voy a tener que modificar el tabsetpanel
# para que quede un panel para gráfico o al menos, algunos análisis por separada.

library(shiny)
library(tidyverse)

ma_inf <- ARMAtoMA(ar = c(-0.7, -0.18), lag.max=40)
ma_inf
plot(ma_inf, type="l")
acf_ma_inf<-acf(ma_inf, lag.max =30, main="Autocorrelograma")
pacf_ma_inf<-pacf(ma_inf, lag.max =30, main="Autocorrelograma parcial")

# Arreglar para que quede con una semilla fija.

# Programa para modelar Series de Tiempo
ui <- fluidPage(
   
   # Application title
   titlePanel("Series Temporales 2019 - Análisis procesos ARMA(P,Q)"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         textInput("ar1",
                     "Elegir phi 1",
                     value = "0.5" 
#                    min = -2,
#                    max = 2,
#                     value = 0.5,
#                     step = 0.01)
        ),
        textInput("ar2",
                    "Elegir phi 2",
                    value = "0"
                    # min = -2,
                    # max = 2,
                    # value = 0,
                    # step = 0.01)
        ),
        textInput("ma1",
                    "Elegir theta 1",
                    value = "0"
                    # min = -1,
                    # max = 2,
                    # value = 0,
                    # step = 0.01)
        ),
        textInput("ma2",
                    "Elegir theta 2",
                    value = "0"
                    # min = -1,
                    # max = 1,
                    # value = 0,
                    # step = 0.01)
      ),
        textInput(
          "constante",
          "Elegir constante",
          value = "0"
        ),
        sliderInput("sd",
                    "Elegir el SD",
                    min = 0.1,
                    max = 100,
                    value = 1,
                    step = 1)
      ,
        sliderInput("N",
                  "Elegir n",
                  min = 10,
                  max = 10000,
                  value = 500)
      ),
      # Show a plot of the generated distribution
      mainPanel(
          tabsetPanel(
              type = "tabs",
              id = "tabsetpanel",
              tabPanel(
                  title = "Serie Temporal",
                  plotOutput(outputId = "distPlot")
              ),
              tabPanel(
                  title = "Autocorrelograma",
                  plotOutput(outputId = "ACF")
              ),
              tabPanel(
                  title = "ARMA to MA",
                  plotOutput(outputId = "ARMAtoMA")
              ),
              tabPanel(
                  title = "Predicción",
                  plotOutput(outputId = "pred")
              ),
              tabPanel(
                  title = "Random Walk",
                  plotOutput(outputId = "RW")
              )
        
          )
      )
   )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
    ar1_fun <- function(n = 200, set.seed = 123, phi = 1, c = 0) {
        # Extenderla para el caso arma(p,q), puede ser. O creo otra función.
        n=n
        set.seed(set.seed)
        epsilon=rnorm(n)
        X=rep(0,n)
        phi=phi
        c = c
        for(t in 2:n) X[t]= c + phi*X[t-1] + epsilon[t]
        plot(X,type="l", main=paste("Simulación AR(1) con phi =",phi))
    }
    
    RW <- reactive({
        ar1_fun(n = input$N, phi = as.numeric(input$ar1), c = as.numeric(input$constante))
    })
    
    simulacion <- reactive({arima.sim(n = input$N, list( ar = c(as.numeric(input$ar1), as.numeric(input$ar2)), 
                                               ma = c(as.numeric(input$ma1), as.numeric(input$ma2))),
                            sd = input$sd) %>% xts::as.xts(.)})
    
    ma_inf <- reactive({ARMAtoMA(ar = c(as.numeric(input$ar1), as.numeric(input$ar2)), lag.max=40)})
    
    
   output$distPlot <- renderPlot({
      # bins_ar <- seq(min(x), max(x), length.out = input$bins + 1)
      # generate bins based on input$bins from ui.R
      
      # Gráfico de la serie
      #par(mfrow=c(2,1))
      autoplot(simulacion()) +
           ggtitle("Simulación modelo ARMA(p,q)") +
           theme_bw() +
           labs(x = "Tiempo", y = "Serie simulada")
      #plot(simulacion(), main = "Proceso ARMA", ylab = "simulación", xlab = "Tiempo")
      #acf(simulacion, lag.max =30, main="Autocorrelograma MA(1)")
      #pacf(simulacion, lag.max =30, main="Autocorrelograma parcial MA(1)")
      # ACF de la serie
      #bins <- seq(min(x), max(x), length.out = input$bins + 1)
      # draw the histogram with the specified number of bins
      #hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
   
   output$ACF <- renderPlot({
       par(mfrow=c(1,2))
       acf(simulacion(), lag.max = 30, main = "Autocorrelograma ARMA(p,q)")
       pacf(simulacion(),lag.max= 30,main="Función de autocorrelación parcial")
   })
   
   output$ARMAtoMA <- renderPlot({
       par(mfrow=c(1,3))
       plot(ma_inf(), type="l")
       acf(ma_inf(), lag.max =30, main="Autocorrelograma")
       pacf(ma_inf(), lag.max =30, main="Autocorrelograma parcial")
   })
   
   output$pred <- renderPlot({
       arima = forecast::auto.arima(simulacion(), approximation=FALSE,trace=FALSE)
       pred <- forecast::forecast(arima, h = 30)
       plot(pred,type="l",xlab = "Tiempo",ylab = "Simulación",lwd = 2,col = 'red',main="Predicción modelo ARIMA")
       
   })
   
   output$RW <- renderPlot({
       RW()
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

