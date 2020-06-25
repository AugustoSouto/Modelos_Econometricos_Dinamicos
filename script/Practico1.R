libs=c("tidyverse", "ggplot2", "ggfortify", "readxl","gridExtra", "vars", "readxl",
       "seasonal", "lmtest", "tsoutliers", "urca", "plotly")##
# instalar librerias
lib_nuevas <- libs[!(libs %in% installed.packages()[,"Package"])] 
if(length(lib_nuevas)) install.packages(lib_nuevas)

# cargar librerias
load_libs <- function(libraries = libs) for (lib in libraries)
    require(lib, character.only=T)
load_libs(libs)
theme_set(theme_bw())
options(tibble.print_max = 50, tibble.print_min = 50, tibble.width = Inf, max.print = 50)
tso1 <- function(data, serie, cval1, cval2) {
    return(tsoutliers::tso(data[,serie], tsmethod = "auto.arima", 
                           args.tsmethod = list(ic = "aicc", seasonal = FALSE, approximation = FALSE,
                                                stepwise = FALSE), types = c("AO"), cval = cval1, 
                           discard.method = "en-masse", maxit.iloop = 200, maxit.oloop = 200, check.rank = TRUE,
                           discard.cval = cval2))
}


# Carga de datos y limpieza -----------------------------------------------


data <- readxl::read_excel(here::here("input","cuadro_133t.xls"), range = "B10:CM22")
data <- data[complete.cases(data),]
data <- dplyr::rename(.data = data, "sectores" = "...1")
data <- tidyr::gather(data, key = fecha, value = VAB, -sectores) # en vez de -sectores se puede poner 2:ncol(data)
data <- tidyr::spread(data, key = sectores, value = VAB)
data$fecha <- stringr::str_replace_all(data$fecha, pattern = c("\\*"="", "IV" = "4" ,"III"="3", "II" = "2", "I"="1"))
data$fecha <- lubridate::dmy(paste(1, data$fecha))
data <- data[order(data$fecha),]
# zoo::as.yearqtr(data$fecha, format = format("%q %Y"))
colnames(data) # Nos quedamos con [3],[4],[7],  # Falta una serie!!
serie <- ts(data[c(3,4,7, 10)], start = c(1997,1), frequency = 4)
colnames(serie) <- c("agro", "industria", "comercio", "servicios")
serie


# EDA ---------------------------------------------------------------------

# Generamos las series de interés a usar en las partes b y c
serie <- log(serie)
serie_diff1 <- diff(serie, 1)
serie18 <- window(serie, start = c(1997,1), end = c(2018,2))
serie18_diff1 <- diff(serie18, 1)
test <- window(serie_diff1, start = c(2018,3), end = c(2018,4))



(serie %>% autoplot(xlab = "años", show.legend = FALSE, ylab = "log")) %>% ggplotly()
sapply(serie, summary)
# Del análisis gráfico ninguna serie puede tener una tendencia determinística. Dado que no presentan
# crecimiento sistemático.
# Tampoco podrían ser un RW con drift por no presentar variación sistemático (crecimiento o caída)
# El test de RU con tendencia NO se puede plantear, sería incorrecto al sesgar la prueba.
# El test de RU testeando el drift si se debe plantear, dado que dependiendo el valor de una posible constesnte
# sería como crecería el proceso estocástico. Con una constante de pequeña, no se observa un crecimiento sistemático.
# Si se desea ver esto, correr el archivo app.R y simular un RW con phi1=1 y constante 0.02, por ejemplo.

sapply(serie, function(x){plot(decompose(x))})
sapply(serie, function(x){stl(x, s.window = "periodic") %>% plot(.)}) # descomposición con stl

# Esto, porque descargue la serie sin desestacionalizar. A elegir si la desestacionalizamos o descargarla
# desestacionalizada...En dicho caso usamos el archivo cuadro_133t y esta parte nos la salteamos.

for(j in c(FALSE,TRUE)) {
    for(i in colnames(serie)) {
    (forecast::ggseasonplot(serie[, i], continuous = TRUE, polar = j, 
                            year.labels = TRUE, year.labels.left = TRUE, labelgap = 0.1) + 
         ggtitle(i)) %>% plot()
    # Sys.sleep(20) #20 segundos de pausa para analizar cada uno. Si se quiere, correr por separado...
    readline(prompt = "Continuar? Apretá enter")
    }
}
for(i in colnames(serie)) {
    (forecast::ggsubseriesplot(serie[,i]) + ggtitle(i)) %>% plot()
    readline(prompt = "Continuar? Apretá enter")
}  # Se nota que están desestacionalizadas pero sigue habiendo mínimas diferencias.
# O linea por linea en vez del for...
boxplot(serie[,1]~cycle(serie[,1]))
boxplot(serie[,2]~cycle(serie[,2]))
boxplot(serie[,3]~cycle(serie[,3]))
boxplot(serie[,4]~cycle(serie[,4]))

# Diferencia regular. 
(serie_diff1 %>% autoplot(xlab = "Años", show.legend = FALSE, ylab = "Tasa de crecimiento")) %>%  plotly::ggplotly()

acf(serie)
pacf(serie)
acf(serie_diff1)
pacf(serie_diff1)

# Test de RU --------------------------------------------------------------

# Por detalles ir a:
# libro en 'libros-artículos' de Bernhard Pfaff
# https://stats.stackexchange.com/questions/24072/interpreting-rs-ur-df-dickey-fuller-unit-root-test-results
# Una forma de hacerlo
# lapply(apply(serie, MARGIN=2,FUN=ur.df,type="drift",selectlags= "AIC"), summary) 

# DF-Aumentado
for(j in colnames(serie)){
    for(especificacion in c("drift", "none")) {
        df <- urca::ur.df(serie[,j], type = especificacion, selectlags = "AIC")
        print(c(toupper(j), especificacion))
        print(df@teststat)
        print(df@cval)
        print("-----------------------")
        readline(prompt = "Continuar? Apretá enter")
        # serie[,j] %>% urca::ur.df(type = especificacion, selectlags = "AIC") %>% urca::summary() %>% print()
    }
}
# Para el caso con TREND tenemos: (no lo usamos pero para saber)
# delta y(t) = a0 + gamma * y(t-1) + a2(t) e(t) (formula Enders p. 208)
# tau:      gamma = 0                       (H0, test t)
# phi3:     gamma = a2 = 0                  (Hipótesis computesta, test F)
# phi2:     gamma = a2 = a0 = 0             (Hipótesis computesta, test F)
# Lectura:
# tau : Si NO rechazamos H0 entonces hay RU (gamma=0)
# phi3: Si NO rechazamos H0 entonces hay RU (gamma=0) y NO hay tendencia determinística (a2=0)
# Si rechazamos la nula entonces gamma=0 y a2=0 o bien gamma=0 y a2!=0 o bien gamma!=0 y a2=0
# phi2: Si NO rechazamos H0 entonces hay RU (gamma=0) y NO hay tendencia determinística (a2=0) y NO drift (a0=0)
# Si rechazamos la nula entonces 1, 2 o los 3 terminos NO son 0.


# Para el caso con DRIFT tenemos:
# delta y(t) = a0 + gamma * y(t-1) + e(t)   (formula Enders p. 208)
# tau2:     gamma = 0                       (H0, test t)
# phi1:     a0 = gamma = 0                  (Hipótesis compuesta, test F)
# Lectura:
# tau2: Si NO rechazamos H0 entonces hay RU (gamma=0)
# phi1: Si NO rechazamos H0 entonces gamma=0 y a0=0 o bien gamma=0 y a2!=0 o bien gamma!=0 y ao=0
# Si NO rechazamos la nula tenemos gamma=0 y ao=0 entonces HAY RU y NO hay drift.

# Para el caso NONE tenemos:
# delta y(t) = gamma * y(t-1) + e(t)        (formula Enders p. 208)
# tau1:     gamma = 0                       (H0, test t)
# Lectura:
# tau1: Si NO rechazamos H0 entonces HAY RU

# Resultados:

# Agro drift:
# tau2 es mayor al valor crítico de tau2, no cae en la región crítica NO se rechaza la hipótesis nula de RU.
# phi1 es menor al valor crítico de phi1 (5%), no cae en la región crítica por lo que NO se rechaza H0 de
#       tener RU y NO tener drift.
# Agro none:
# tau1 es mayor al valor crítico de tau1, no cae en la región crítica NO se rechaza H0. Existe RU

# Industria drift:
# tau2 es mayor al valor crítico de tau2, no cae en la región crítica NO se rechaza la nula de RU
# phi1 es menor al valor crítico de phi1, no cae en la región crítica NO se rechaza H0 de RU y NO tener drift.
# Industria none:
# tau1 es mayor al valor crítico de tau1, no cae en la región crítica entonces no se rechaza que hay RU.

# Comercio drift:
# tau2 es mayor al valor crítico de tau2, no cae en la región crítica NO se rechaza H0. Existe RU
# phi1 es menor al valor crítico de phi1, no cae en la región crítica No se rechaza H0 RU y NO tener drift.
# Comercio none:
# tau1 es mayor al valor crítico de tau1, no cae en la región crítica NO se rechaza H0. Hay RU

# Servicios drift:


# Verificamos con los resultados del paquete forecast que aplica el test
forecast::ndiffs(serie[, "agro"], type = "level", test = "adf")
forecast::ndiffs(serie[, "industria"], type = "level", test = "adf")
forecast::ndiffs(serie[, "comercio"], type = "level", test = "adf")
forecast::ndiffs(serie[, "servicios"], type = "level", test = "adf")
# En todos los casos nos da que es necesario realizar una diferencia regular.

# KPSS
for(j in colnames(serie)){
    for(especificacion in c("mu", "tau")) {
        print(toupper(j))
        serie[,j] %>% urca::ur.kpss(type = especificacion, lags = "short") %>% urca::summary() %>% print()
        print("-----------------------")
        readline(prompt = "Continuar? Apretá enter")
    }
}
# Análisis KPSS
forecast::ndiffs(serie[, "agro"], type = "level", test = "kpss")
forecast::ndiffs(serie[, "industria"], type = "level", test = "kpss")
forecast::ndiffs(serie[, "comercio"], type = "level", test = "kpss")
forecast::ndiffs(serie[, "servicios"], type = "level", test = "kpss")
# En todos los casos no se rechaza una diferencia regular. Resultados idénticos. OK


# Estimación del VAR hasta 2018------------------------------------------------------


# Análisis de outliers. Usamos el método de Cheng y Liu
agro0      <- tso1(serie18_diff1, "agro", cval1 = 2.5, cval2 = 3)
industria0 <- tso1(serie18_diff1, "industria", cval1 = 3, cval2 = 3)
comercio0  <- tso1(serie18_diff1, "comercio", cval1 = 3, cval2 = 3)
servicios0 <- tso1(serie18_diff1, "servicios", cval1 = 3, cval2 = 3)

agro0$outliers
industria0$outliers
comercio0$outliers
servicios0$outliers
tsoutliers::plot.tsoutliers(agro0)
tsoutliers::plot.tsoutliers(industria0)
tsoutliers::plot.tsoutliers(comercio0)
tsoutliers::plot.tsoutliers(servicios0)
# El outlier en obs inicial en comercio lo borro más adelante. Si hay un Ao en la misma fecha se borra uno.

# generamos los regresores hasta 2018-2
l             <- length(serie18_diff1[, "agro"])
agro0_ao      <- tsoutliers::outliers.effects(agro0$outliers, l)
industria0_ao <- tsoutliers::outliers.effects(industria0$outliers, l)
comercio0_ao  <- tsoutliers::outliers.effects(comercio0$outliers, l) 
servicios0_ao <- tsoutliers::outliers.effects(servicios0$outliers, l) 

colnames(agro0_ao)      <- paste("agr", colnames(agro0_ao), sep = "")
colnames(industria0_ao) <- paste("ind", colnames(industria0_ao), sep = "")
colnames(comercio0_ao)  <- paste("com", colnames(comercio0_ao), sep = "")
colnames(servicios0_ao) <- paste("com", colnames(servicios0_ao), sep = "")

xreg0   <- cbind(agro0_ao, industria0_ao, comercio0_ao, servicios0_ao) 
xreg0 # Borramos repetidos y A0 en obs inicial: 2, 3, 8
indices <- c(1,4:7)
xreg0   <- xreg0[, indices]
sum(apply(xreg0, 1, sum) > 1) 

h = 2
xreg0_pred <- matrix(0L, nrow = h, ncol = ncol(xreg0), dimnames = list(NULL, colnames(xreg0)))

# lags, según distintos criterios
p0 <- vars::VARselect(serie18_diff1, exogen = xreg0, type = "const")$selection["AIC(n)"]

# Estimamos inicialmente un var
var0 <- vars::VAR(serie18_diff1, p = p0, type = "const", exogen = xreg0)
var0

# Test
vars::stability(var0)
sum(vars::roots(var0) > 1) # Ningún valor propio es mayor a 1. El VAR es estable.
vars::normality.test(var0)$jb.mul # Se rechaza la hipótesis nula de normalidad. Problema
vars::serial.test(var0, type = "PT.adjusted")$serial # Se rechaza H0 que es no correlación. Problema
vars::arch.test(var0)$arch.mul # No se rechaza la hipótesis nula de errores esféricos.

# predicciones
pred0 <- predict(object = var0, n.ahead = 2, ci = 0.95, dumvar = xreg0_pred)
pred0 %>% plot(main = c("agro", "industria", "comercio", "servicios"))
forecast::forecast(var0, dumvar = xreg0_pred, h =2) %>% autoplot() + xlab("Año") + 
    autolayer(test, colour = FALSE, color = "red") # El rojo es la realidad.
fanchart(pred0, main = c("agro", "industria", "comercio", "servicios"))

# Estimamos un SVAR
# Impongo Cholesky. Recordar que acá importa el orden de las variables en la matriz
var0$y # El orden es agro, industria, comercio, servicios.
amat <- diag(4)
diag(amat) <- NA
amat[2, 1] <- NA
amat[3, c(1,2)] <- NA
amat[4, c(1:4)] <- NA

# Dos métodos para estimar. Aunque como nosotros ya imponemos las restricciones, es lo mismo.
# La gracia es cuando tenemos u_t = A^1*B*epsilon_t
# Pero nosotros estamos trabajando en clases con una matrix B = I. 
# Por lo cual dejando Bmat = NULL se genera automáticamente la identidad.
svar0 <- SVAR(x = var0, estmethod = "direct", Amat = amat, Bmat = NULL,
     hessian = TRUE, method="BFGS") 
# svar0 <- SVAR(x = var0, estmethod = "scoring", Amat = amat, Bmat = NULL,
#                max.iter = 100, maxls = 1000, conv.crit = 1.0e-8) 
# Los probé y dan prácticamente los mismos valores, usamos diect y listo.

# IRF
irf1 <- function(svar, impulso, acumulado = FALSE){
    return(
        vars::irf(svar, n.ahead = 8, boot = TRUE, ci = 0.95, runs = 100, seed = 123, 
                  impulse = impulso, cumulative = acumulado)
    )
}
irf_agro      <- irf1(svar0, "agro", FALSE)
irf_industria <- irf1(svar0, "industria", FALSE)
irf_comercio  <- irf1(svar0, "comercio", FALSE)
irf_servicios <- irf1(svar0, "servicios", FALSE)

irfc_agro      <- irf1(svar0, "agro", TRUE)
irfc_industria <- irf1(svar0, "industria", TRUE)
irfc_comercio  <- irf1(svar0, "comercio", TRUE)
irfc_servicios <- irf1(svar0, "servicios", TRUE)

plot(irf_agro,      main = "SVAR IRF desde agro",      sub = "95% Bootstrap CI, 100 corridas")
plot(irf_industria, main = "SVAR IRF desde industria", sub = "95% Bootstrap CI, 100 corridas")
plot(irf_comercio,  main = "SVAR IRF desde comercio",  sub = "95% Bootstrap CI, 100 corridas")
plot(irf_servicios, main = "SVAR IRF desde servicios", sub = "95% Bootstrap CI, 100 corridas")

plot(irfc_agro,      main = "SVAR IRFC desde agro",      sub = "95% Bootstrap CI, 100 corridas")
plot(irfc_industria, main = "SVAR IRFC desde industria", sub = "95% Bootstrap CI, 100 corridas")
plot(irfc_comercio,  main = "SVAR IRFC desde comercio",  sub = "95% Bootstrap CI, 100 corridas")
plot(irfc_servicios, main = "SVAR IRFC desde servicios", sub = "95% Bootstrap CI, 100 corridas")

# FEDV
fevd <- vars::fevd(svar0, n.ahead = 8)
win.graph(width=8,height=10) # Para guardar la imagen y poner en el trabajo.
plot(fevd, xlab = "horizonte", ylab = "Porcentaje", 
     main = c("FEVD para el Agro", "FEVD para Industria", "FEVD para comercio", "FED para servicios")) 
print(fevd)
# Es llamativo el bajo porcentaje que afecta el agro a la industria y comercio.

{# # No tendríamos que estimar necesariamente un SVAR, porque esa función deja grados de libertad 
# # (aunque lo podemos hacer de hecho noten que las irf dan casi mismos valores y comportamientos).
# # Se puede imponer la descomposición de cholesky en las funciones impulso respuesta de 
# # forma de tener una representación VMA mediante la descomposición de Wold. Para eso solo necesitamos estimar el
# # VAR mediante VAR() y luego usar irf fijando errores ortogonales.
# # Ver Bernard Pfaff página 39 y 40.
# # Impulsos respuesta no acumuladas y acumuladas hasta 8 periodos (2 años)
# IRF  <- vars::irf(x = var18, ortho = TRUE, boot = TRUE, seed = 123, n.ahead = 8, runs = 500)
# IRFC <- vars::irf(x = var18, ortho = TRUE, cumulative = TRUE, seed = 123, n.ahead = 8, runs = 500)
# plot(IRF)
# # Es posible especificar desde que variables se quieren las respuestas. (ie Cholesky) Pero, si solamente se 
# # fija ortho = TRUE genera los IRF para todas las variables. En donde el shock en t=0 es 0 quiere decir que por el
# # orden de las variables no hay efecto.
# # Así como están ordenadas las variables un shock del agro afecta a todos los sectores (shock en 0 positivo)
# # Un shock de industría afecta a industria y comercio
# # Un shock de comercio solo afecta a comercio.
# plot(IRFC)
# # Para la descomposición de varianza la transformación se hace internamente, notar que dan casi lo mismo...
# vars::fevd(svar18) %>% plot()
# vars::fevd(var18) %>% plot()
}


# Estimación del VAR para todo el período ---------------------------------

# Análisis de outlier. # Usamos la serie: serie_diff1

agro1      <- tso1(serie_diff1, "agro", cval1 = 2.5, cval2 = 3)
industria1 <- tso1(serie_diff1, "industria", cval1 = 3, cval2 = 3)
comercio1  <- tso1(serie_diff1, "comercio", cval1 = 3, cval2 = 3)
servicios1 <- tso1(serie_diff1, "servicios", cval1 = 3, cval2 = 3)

agro1$outliers
industria1$outliers
comercio1$outliers
servicios1$outliers
tsoutliers::plot.tsoutliers(agro1)
tsoutliers::plot.tsoutliers(industria1)
tsoutliers::plot.tsoutliers(comercio1)
tsoutliers::plot.tsoutliers(servicios1)

l1 <- length(serie_diff1[, "agro"])
agro1_ao <- tsoutliers::outliers.effects(agro1$outliers, l1)
industria1_ao <- tsoutliers::outliers.effects(industria1$outliers, l1)
# comercio1_ao  <- tsoutliers::outliers.effects(comercio1$outliers, l1) 
servicios1_ao  <- tsoutliers::outliers.effects(servicios1$outliers, l1) 

colnames(agro1_ao) <- paste("agr", colnames(agro1_ao), sep = "")
colnames(industria1_ao) <- paste("ind", colnames(industria1_ao), sep = "")
# colnames(comercio1_ao) <- paste("com", colnames(comercio1_ao), sep = "")
colnames(servicios1_ao) <- paste("com", colnames(servicios1_ao), sep = "")

xreg1  <- cbind(agro1_ao, industria1_ao, servicios1_ao) 
sum(apply(xreg1, 1, sum) > 1) 
xreg1 # Borramos repetidos: 3, 6
indices1 <- c(1:2,4:5)
xreg1  <- xreg1[, indices1]


# Estimación del VAR en forma reducida
p1 <- vars::VARselect(serie_diff1, exogen = xreg1)$selection[1] # Número de rezagos óptimos según AIC
var1 <- vars::VAR(y = serie_diff1, p = p1, type = "const", exogen = xreg1)
plot(var1)
var1

# test
vars::stability(var1)
sum(vars::roots(var1) > 1) 
vars::normality.test(var1)$jb.mul # Se rechaa H0 normalidad
vars::serial.test(var1, type = "PT.adjusted")$serial # Se rechaza H0 no correlación
vars::arch.test(var1)$arch.mul # No se rechaza H0 errores esféricos


# var estructural SVAR con restricciones v2
    # Agro solo se afecta por agro en t0
    # Comercio se afecta por comercio y agro
    # Industria se afecta por industria, comercio y agro
    # Servicios no afecta a nadie
var0$y # El orden es agro, industria, comercio, servicios.
amat <- diag(4)
diag(amat) <- NA
amat[2, 1] <- NA
amat[3, c(1,2)] <- NA
amat[4, c(1:4)] <- NA
svar1 <- SVAR(x = var1, estmethod = "scoring", Amat = amat, Bmat = NULL,
             max.iter = 100, maxls = 1000, conv.crit = 1.0e-8)
svar1
# IRF
irf1_agro      <- irf1(svar1, "agro", FALSE)
irf1_industria <- irf1(svar1, "industria", FALSE)
irf1_comercio  <- irf1(svar1, "comercio", FALSE)
irf1_servicios <- irf1(svar1, "servicios", FALSE)

irfc1_agro      <- irf1(svar1, "agro", TRUE)
irfc1_industria <- irf1(svar1, "industria", TRUE)
irfc1_comercio  <- irf1(svar1, "comercio", TRUE)
irfc1_servicios <- irf1(svar1, "servicios", TRUE)

print(irf1_agro$irf)
print(irf1_industria$irf)
print(irf1_comercio$irf)
print(irf1_servicios$irf)

plot(irf1_agro,      main = "SVAR IRF desde agro",      sub = "95% Bootstrap CI, 100 corridas")
plot(irf1_industria, main = "SVAR IRF desde industria", sub = "95% Bootstrap CI, 100 corridas")
plot(irf1_comercio,  main = "SVAR IRF desde comercio",  sub = "95% Bootstrap CI, 100 corridas")
plot(irf1_servicios, main = "SVAR IRF desde servicios", sub = "95% Bootstrap CI, 100 corridas")

plot(irfc1_agro,      main = "SVAR IRFC desde agro",      sub = "95% Bootstrap CI, 100 corridas")
plot(irfc1_industria, main = "SVAR IRFC desde industria", sub = "95% Bootstrap CI, 100 corridas")
plot(irfc1_comercio,  main = "SVAR IRFC desde comercio",  sub = "95% Bootstrap CI, 100 corridas")
plot(irfc1_servicios, main = "SVAR IRFC desde servicios", sub = "95% Bootstrap CI, 100 corridas")

# FEDV
fevd1 <- vars::fevd(svar1, n.ahead = 8)
win.graph(width=8,height=10) # Para guardar la imagen y poner en el trabajo.
plot(fevd1, xlab = "horizonte", ylab = "Porcentaje",
     main = c("FEVD para el Agro", "FEVD para Industria", "FEVD para comercio", "FED para servicios")) 
print(fevd1)


# Modificaciones en el orden de variables ---------------------------------

var0$y # El orden es agro, industria, comercio, servicios.
amat2 <- t(amat)
svar2 <- SVAR(x = var1, estmethod = "scoring", Amat = amat2, Bmat = NULL,
              max.iter = 100, maxls = 1000, conv.crit = 1.0e-8)
svar2

irf2_agro      <- irf1(svar2, "agro", FALSE)
irf2_industria <- irf1(svar2, "industria", FALSE)
irf2_comercio  <- irf1(svar2, "comercio", FALSE)
irf2_servicios <- irf1(svar2, "servicios", FALSE)

irfc2_agro      <- irf1(svar2, "agro", TRUE)
irfc2_industria <- irf1(svar2, "industria", TRUE)
irfc2_comercio  <- irf1(svar2, "comercio", TRUE)
irfc2_servicios <- irf1(svar2, "servicios", TRUE)

plot(irf2_agro,      main = "SVAR IRF desde agro",      sub = "95% Bootstrap CI, 100 corridas")
plot(irf2_industria, main = "SVAR IRF desde industria", sub = "95% Bootstrap CI, 100 corridas")
plot(irf2_comercio,  main = "SVAR IRF desde comercio",  sub = "95% Bootstrap CI, 100 corridas")
plot(irf2_servicios, main = "SVAR IRF desde servicios", sub = "95% Bootstrap CI, 100 corridas")

plot(irfc2_agro,      main = "SVAR IRFC desde agro",      sub = "95% Bootstrap CI, 100 corridas")
plot(irfc2_industria, main = "SVAR IRFC desde industria", sub = "95% Bootstrap CI, 100 corridas")
plot(irfc2_comercio,  main = "SVAR IRFC desde comercio",  sub = "95% Bootstrap CI, 100 corridas")
plot(irfc2_servicios, main = "SVAR IRFC desde servicios", sub = "95% Bootstrap CI, 100 corridas")

# FEDV
fevd2 <- vars::fevd(svar2, n.ahead = 8)
win.graph(width=8,height=10) # Para guardar la imagen y poner en el trabajo.
plot(fevd2, xlab = "horizonte", ylab = "Porcentaje",
     main = c("FEVD para el Agro", "FEVD para Industria", "FEVD para comercio", "FED para servicios")) 

print(fevd2$agro)
print(fevd1$agro)
# No se observa ninguna modificación relevante en agro

print(fevd2$industria)
print(fevd1$industria)
# Cambios importantes en comercio que en fevd2 gana mucha importancia, de 0% a 18%

print(fevd2$comercio)
print(fevd1$comercio)
# Cambios importantes en industria que en fevd2 pierde mucha importancia, de 18% a 18-20%

print(fevd2$servicios)
print(fevd1$servicios)
# Cambio en industria que pasa de 20% en fevd1 a casi 14-15% en fevd2. Pierde importancia.


# Tercer caso
amat3 <- diag(4)*0
amat3[1,1] <- NA
amat3[2, c(1:3)] <- NA
amat3[3, c(1:2)] <- NA
amat3[4, c(1:4)] <- NA


svar3 <- SVAR(x = var1, estmethod = "scoring", Amat = amat3, Bmat = NULL,
              max.iter = 100, maxls = 1000, conv.crit = 1.0e-8)
svar3

irf3_agro      <- irf1(svar3, "agro", FALSE)
irf3_industria <- irf1(svar3, "industria", FALSE)
irf3_comercio  <- irf1(svar3, "comercio", FALSE)
irf3_servicios <- irf1(svar3, "servicios", FALSE)

irfc3_agro      <- irf1(svar3, "agro", TRUE)
irfc3_industria <- irf1(svar3, "industria", TRUE)
irfc3_comercio  <- irf1(svar3, "comercio", TRUE)
irfc3_servicios <- irf1(svar3, "servicios", TRUE)

plot(irf3_agro,      main = "SVAR IRF desde agro",      sub = "95% Bootstrap CI, 100 corridas")
plot(irf3_industria, main = "SVAR IRF desde industria", sub = "95% Bootstrap CI, 100 corridas")
plot(irf3_comercio,  main = "SVAR IRF desde comercio",  sub = "95% Bootstrap CI, 100 corridas")
plot(irf3_servicios, main = "SVAR IRF desde servicios", sub = "95% Bootstrap CI, 100 corridas")

plot(irfc3_agro,      main = "SVAR IRFC desde agro",      sub = "95% Bootstrap CI, 100 corridas")
plot(irfc3_industria, main = "SVAR IRFC desde industria", sub = "95% Bootstrap CI, 100 corridas")
plot(irfc3_comercio,  main = "SVAR IRFC desde comercio",  sub = "95% Bootstrap CI, 100 corridas")
plot(irfc3_servicios, main = "SVAR IRFC desde servicios", sub = "95% Bootstrap CI, 100 corridas")

# FEDV
fevd3 <- vars::fevd(svar3, n.ahead = 8)
win.graph(width=8,height=10) # Para guardar la imagen y poner en el trabajo.
plot(fevd3, xlab = "horizonte", ylab = "Porcentaje",
     main = c("FEVD para el Agro", "FEVD para Industria", "FEVD para comercio", "FED para servicios")) 

print(fevd3$agro)
print(fevd2$agro)
print(fevd1$agro)
# Pierde importancia comercio, gana industria en torno a un 5-6%

print(fevd3$industria)
print(fevd2$industria)
print(fevd1$industria)
# Cambios drámaticos en fevd3. Comercio pasa a explicar el 95%. En fevd2 era 18% y en fevd1 menos de 1%. Agro igual

print(fevd3$comercio)
print(fevd2$comercio)
print(fevd1$comercio)
# Mismo resultado que en el caso anterior. Gana % industria y la pierde comercio en fevd3. Agro igual

print(fevd3$servicios)
print(fevd2$servicios)
print(fevd1$servicios)
# En fevd3 comercio pasa a 20% cuando en fevd1 y fevd2 estaba en 1-2%.
# En fevd industría pierde importancia respecto a fevd1 (20%) y fevd2 (15%)


# Para el caso de AGRO los resultados (en los 3 casos probados) NO dependen del orden. Servicios es similar
# Aunque muestra leves variaciones % en algunos casos
# Industria y comercio muestrasn GRANDES variaciones en el % explicado dependiendo del orden.


# Prueba de causalidad de Granger -----------------------------------------

# Correr de nuevo el var por seguridad sin outliers.
var2 <- vars::VAR(y = serie_diff1, ic = "AIC", type = "const", exogen = xreg1)
var3 <- vars::VAR(y = serie_diff1, ic = "AIC", type = "const")
# Pruebas de causalidad de dos formas:
    # Con bootstrap ó
    # Con matriz robusta a la heteroscedasticidad.

vars::causality(x = var2, cause = "agro", boot = TRUE, boot.runs=1000)
vars::causality(x = var3, cause = "agro", boot = TRUE, boot.runs=1000)
vars::causality(x = var2, cause = "agro", vcov. = vcovHC(var2))
vars::causality(x = var3, cause = "agro", vcov. = vcovHC(var3))
# mismos resultados. El agro no causa a nadie. No se rechaza H0 en todos los casos.

vars::causality(x = var2, cause = "industria", boot = TRUE, boot.runs=1000)
vars::causality(x = var3, cause = "industria", boot = TRUE, boot.runs=1000)
vars::causality(x = var2, cause = "industria", vcov. = vcovHC(var2))
vars::causality(x = var3, cause = "industria", vcov. = vcovHC(var3))
# Resultados diferentes entre las dos pruebas.
# Mismos resultados al trabajar el modelo con outliers o sin
# Industria según Granger no causa a nadie. Pero en instantánea se rechaza H0. Resultados contrapuestos
# No se puede concluir nada.

vars::causality(x = var2, cause = "comercio", boot = TRUE, boot.runs=1000)
vars::causality(x = var3, cause = "comercio", boot = TRUE, boot.runs=1000)
vars::causality(x = var2, cause = "comercio", vcov. = vcovHC(var2))
vars::causality(x = var3, cause = "comercio", vcov. = vcovHC(var3))
# Misma situación al caso previo.

vars::causality(x = var2, cause = "servicios", boot = TRUE, boot.runs=1000)
vars::causality(x = var3, cause = "servicios", boot = TRUE, boot.runs=1000)
vars::causality(x = var2, cause = "servicios", vcov. = vcovHC(var2))
vars::causality(x = var3, cause = "servicios", vcov. = vcovHC(var3))
# Si se usa el modelo con outliers NO se rechaza H0 en ningún caso. O sea no hay causalidad a la granger
# Si se usa el modelo sin outliers las pruebas dan resultados contrapuestos. No se puede concluir nada.
# Por lo tanto, la conclusión del modelo a utilizar.
