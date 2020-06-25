rm(list=ls())
libs=c("tidyverse", "readxl", "plotly")

# instalar librerias
lib_nuevas <- libs[!(libs %in% installed.packages()[,"Package"])] 
if(length(lib_nuevas)) install.packages(lib_nuevas)

load_libs <- function(libraries = libs) for (lib in libraries)
  require(lib, character.only=T)
load_libs(libs)



data<-"C:/Users/Usuario/Documents/MEGA/MEGAsync/MaestrÌa/Modelos Econometricos Dinamicos/Ejercicios/dECON_19.xlsx"
data<-readxl::read_xlsx(data)

data<-data[complete.cases(data),]
data<-dplyr::rename(.data = data, "sectores" = "...1")
data<-tidyr::gather(data, key = fecha, value = VAB, -sectores) # en vez de -sectores se puede poner 2:ncol(data)
data<-tidyr::spread(data, key = sectores, value = VAB)
data$fecha <- stringr::str_replace_all(data$fecha, pattern = c("\\*"="", "IV" = "4" ,"III"="3", "II" = "2", "I"="1"))
data$fecha <- lubridate::dmy(paste(1, data$fecha))
data <- data[order(data$fecha),]

serie <- ts(data, start = c(2005,1), frequency = 4)

serie

serie <- log(serie)
serie_diff1 <- diff(serie, 1)
serie18 <- window(serie, start = c(2005,1), end = c(2019,1))
serie18_diff1 <- diff(serie18, 1)
test <- window(serie, start = c(2018,4), end = c(2019,1))

(serie[,-1] %>% autoplot()) %>% ggplotly()
sapply(serie, summary)

##algunas observaciones:##

#teleco es claramente integrada
#int financiera parece integrada
#impuestos es dudosa, pero parece integrada
#electricidad,gas y agua parece estacionaria
#acticidades primarias parece estacionaria
#el resto son muy dudosas

sapply(serie[,-1], function(x){plot(decompose(x))})
sapply(serie[,-1], function(x){stl(x, s.window = "periodic") %>% plot(.)}) # descomposici√≥n con stl

serie<-serie[,-1]

for(j in c(FALSE,TRUE)) {
  for(i in colnames(serie)) {
    (forecast::ggseasonplot(serie[, i], continuous = TRUE, polar = j, 
                            year.labels = TRUE, year.labels.left = TRUE, labelgap = 0.1) + 
       ggtitle(i)) %>% plot()
    # Sys.sleep(20) #20 segundos de pausa para analizar cada uno. Si se quiere, correr por separado...
    readline(prompt = "Continuar? Apretar enter")
  }
}

serie %>% diff(lag = 1) %>% autoplot() 
serie %>% diff(lag = 4) %>% autoplot()
serie %>% diff(lag = 1) %>% diff(lag = 4) %>% autoplot()
serie %>% diff(lag = 1) %>% diff(lag = 4)%>% autoplot()

acf(serie)
pacf(serie)

#PRUEBA DE DICKEY FULLER PARA CADA SERIE#
for (i in 1:dim(serie)[2]){ print(c(colnames(serie)[i],
                          forecast::ndiffs(serie[,i], test="adf")))}


for(j in colnames(serie)){
  for(especificacion in c("trend","drift", "none")) {
    df <- urca::ur.df(serie[,j], type = especificacion, selectlags = "AIC")
    print(c(toupper(j), especificacion))
    print(df@teststat)
    print(df@cval)
    print("-----------------------")
    readline(prompt = "Continuar? enter")
    # serie[,j] %>% urca::ur.df(type = especificacion, selectlags = "AIC") %>% urca::summary() %>% print()
  }
  
  
}

#resultados:

#ACTIVIDADES PRIMARIAS: de la serie se ve que no hay crec, probamos la tercer especificacion de una
#no rechazo H0, entonces hay RU
#i(1)
serie_1_est<-serie[,1] %>% diff(lag = 4)
serie_1_est %>% autoplot() #no parece haber necesidad de mas dif

#AGRIC, GANAD Y CAZA ES CASI LO MISMO, la prueba da casi identica a act primarias
#no rechazamos ho en especif sin drift, entonces hay RU
#i(1)
serie_2_est<-serie[,2] %>% diff(lag = 4)
serie_2_est %>% autoplot() #no parece haber ncesidad de otra dif


#COMERCIO, REPARAC, REST Y HOTELES: crece pero no sist, pruebo con drift
#no rechazo H0 en la especific, voy a la especif sin drift
#en la especif sin drift tampoco rechazo, hay RU
#i(2)
serie_3_est<-serie[,3] %>% diff(lag = 4) 
serie_3_est %>% autoplot() #aca no queda muy claro, pruebo otra raiz

df<-urca::ur.df(serie_3_est, type = "none", selectlags = "AIC")
print(df@teststat)
print(df@cval) #parece q

serie_3_est<-serie_3_est %>% diff(lag = 4) 
serie_3_est %>% autoplot() #ahora parece que no hay mas raices

#prueba por las dudas
df<-urca::ur.df(serie_3_est, type = "none", selectlags = "AIC")
print(df@teststat)
print(df@cval) #no hay mas raices, la variable era I(2)


#CONSTRUCCION: crece, no sist, prueba con drift primero
#no rechazo H0, voy a la prueba sin drift
#no rechazo H0 en la prueba sin dridt, hay RU

#i(1)
serie_4_est<-serie[,4] %>% diff(lag = 4) 
serie_4_est %>% autoplot() #parece estac, pero es rara, checkeo otro DF

df<-urca::ur.df(serie_4_est, type = "none", selectlags = "AIC")
print(df@teststat)
print(df@cval) #no hay mas raices, la variable era I(1)


#IMPUESTOS MENOS SUBV: crece bastante, primero pruebo con trend
#no rechazo H0--->pruebo drift solo
#no rechazo H0--->pruebo sin drift
#no rechazo H0 (al 5%, si al 10%)--->hay RU
#i(2)
serie_5_est<-serie[,5] %>% diff(lag = 4) 
serie_5_est %>% autoplot() #parece estac, pero es rara, checkeo otro DF

df<-urca::ur.df(serie_5_est, type = "none", selectlags = "AIC")
print(df@teststat)
print(df@cval) #tiene otra RU

serie_5_est<-serie_5_est %>% diff(lag = 4) 
serie_5_est %>% autoplot() #ahora si, es bien estacionaria

#un check, aunque ya se ve graficamente
df<-urca::ur.df(serie_5_est, type = "none", selectlags = "AIC")
print(df@teststat)
print(df@cval) 

#IND MANUF:crece poco, uso solo drift primero
#no rechazo H0--->pruebo sin drift
#no rechazo H0--->hay RU
#i(1)
serie_6_est<-serie[,6] %>% diff(lag = 4) 
serie_6_est %>% autoplot() #parece estacionaria

df<-urca::ur.df(serie_6_est, type = "none", selectlags = "AIC")
print(df@teststat)
print(df@cval) #es estacionaria

#OTRAS ACTIV crece poco, uso solo drift primero
#no rechazo H0---->pruebo sin drift
#no rechazo H0----> hay RU
#i(2)
serie_7_est<-serie[,7] %>% diff(lag = 4) 
serie_7_est %>% autoplot() #es rara

df<-urca::ur.df(serie_7_est, type = "none", selectlags = "AIC")
print(df@teststat)
print(df@cval) #tiene raiz

serie_7_est<-serie_7_est %>% diff(lag = 4) 
serie_7_est %>% autoplot() #ahora si, es estac

#check
df<-urca::ur.df(serie_7_est, type = "none", selectlags = "AIC")
print(df@teststat)
print(df@cval) 


#PIB: crece algo, pruebo solo drift primero
#no rechazo H0--->pruebo sin drift
#no rechazo H0--->hay RU
#i(2)
serie_8_est<-serie[,8] %>% diff(lag = 4) 
serie_8_est %>% autoplot() #es rara

df<-urca::ur.df(serie_8_est, type = "none", selectlags = "AIC")
print(df@teststat)
print(df@cval) #tiene raiz

serie_8_est<-serie_8_est %>% diff(lag = 4) 
serie_8_est %>% autoplot() #ahora si, es estac

#check
df<-urca::ur.df(serie_8_est, type = "none", selectlags = "AIC")
print(df@teststat)
print(df@cval)  #es estaci


#INT FINANCIERA: pruebo con trend, crece mucho en casi todo el periodo
#no rechazo H0--->pruebo solo con drift
#no rechazo H0--->pruebo sin drift
#no rechazo H0--->hay RU

#i(2)
serie_9_est<-serie[,9] %>% diff(lag = 4) 
serie_9_est %>% autoplot() #es rara

df<-urca::ur.df(serie_9_est, type = "none", selectlags = "AIC")
print(df@teststat)
print(df@cval) #tiene raiz

serie_9_est<-serie_9_est %>% diff(lag = 4) 
serie_9_est %>% autoplot() #es rara, pero parece estac

df<-urca::ur.df(serie_9_est, type = "none", selectlags = "AIC")
print(df@teststat)
print(df@cval) #es estac

#ELECT, GAS, AGUA: no esta muy claro, pruebo con drift inicialmente
#rechazo H0---->no hay RU
#i(0)
serie_10_est<-serie[,10] #no hace falta la dif, no hay RU
serie_10_est %>% autoplot() #es estacionaria


#TELECO Y TRANS: crece muchisimo, pruebo con trend
#no rechazo H0--->pruebo solo con drift
#no rechazo H0--->pruebo sin drift
#no rechazo H0--->hay RU

#i(2)
serie_11_est<-serie[,11] %>% diff(lag = 4) 
serie_11_est %>% autoplot() #es rara

df<-urca::ur.df(serie_11_est, type = "none", selectlags = "AIC")
print(df@teststat)
print(df@cval) 

serie_11_est<-serie_11_est %>% diff(lag = 4) 
serie_11_est %>% autoplot() #es rara

df<-urca::ur.df(serie_11_est, type = "none", selectlags = "AIC")
print(df@teststat)
print(df@cval) #es estac

serie_estac<-cbind(serie_1_est, 
                   serie_2_est,
                   serie_3_est,
                   serie_4_est,
                   serie_5_est,
                   serie_6_est,
                   serie_7_est,
                   serie_8_est, 
                   serie_9_est, 
                   serie_10_est,
                   serie_11_est)

colnames(serie_estac)<-colnames(serie)

#junto las variables i(1) para probar cointegracion
series_i_1<-serie[,c(2,4,6)]

#uso el test de traza de Johansen#
jotest=ca.jo(data.frame(series_i_1), type="trace", K=2, ecdet="const", spec="longrun")

#al parecer hay 1 sola relacion de cointegracion entre las series#
summary(jotest)

#extraigo el vector de cointegracion#
vec_coint<-jotest@V[,1] %>% as.matrix 
#creo la variable I(0) con el vector de cointegracion
error_corr<- vec_coint[1,]*serie[,2]+vec_coint[2,]*serie[,4]+vec_coint[3,]*serie[,6]+
  vec_coint[4,]
#claramente es I(0), ver el plot
error_corr %>% autoplot()


?vars::VARselect()
##CREO LA BASE PARA HACER EL VAR, PONGO TODAS LAS VARIABLES MAS EL TERMINO DE ERROR I(0)#

#VAR_data<-window(serie_estac, start=c(2008,1), end=c(2019,1))
VAR_data<-window(series_i_1, start=c(2005,1), end=c(2019,1))
error_corr<-window(error_corr, start=c(2005,1), end=c(2019,1))

one<-dynlm(serie_2_est~L(error_corr,1)+L(serie_4_est,1:2)+
           L(serie_6_est,1:2) )

two<-dynlm(serie_4_est~L(error_corr,1)+L(serie_2_est,1:2)+
            L(serie_6_est,1:2) )

three<-dynlm(serie_6_est~L(error_corr,1)+L(serie_4_est,1:2)+
             L(serie_2_est,1:2) )

vars::vec2var(jotest, r=1)

summary(one)
summary(two)
summary(three)

#ESTIMO EL VECM#
vecm<-vars::VAR(VAR_data, type = "const", exogen = quantmod::Lag(error_corr),
    ic="AIC", lag.max=2)

#FORMA MATRICIAL DEL VECM#
# Y=c+alpha_%*%Y_1+beta_%*%Y_2+delta %*% rel_LP + error

error_corr %>% is.na %>% sum