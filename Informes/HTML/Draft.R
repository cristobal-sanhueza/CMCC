
library(openair)
library(pander)
library(tidyverse)
library(dygraphs)
library(xts)
library(readxl)
library(RColorBrewer)
library(data.table)
library(gplots)
library(scales)
library(hydroGOF)
library(cvms)
library(lubridate)
library(stringr)

start_date <- '01-07-2023'
end_date <- '31-07-2023'

#MP10 MOD #

path_mp10_mod <- paste0('/Users/cristobal512/Desktop/geoaire/CMCC/Modelados/Geologger/calidad-aire-dia-hora_Campamento-PRO_', start_date, '_a_', end_date, '.csv')

MP10_Modelados <- read_csv(path_mp10_mod, col_types = cols(Fecha = col_date(format = "%d/%m/%Y")))

MP10_Modelados <- MP10_Modelados %>% 
  rename(MP10_Mod = `MP10 (µg/m³N)`)

MP10_Modelados <- MP10_Modelados %>%
  mutate(Fecha = ymd_h(paste(Fecha, Hora)))

MeanMP10byHour_Modelados <- MP10_Modelados %>% 
  select(Hora, MP10_Mod) %>% 
  group_by(Hora) %>% 
  summarise(MP10_Mod = mean(MP10_Mod, na.rm = TRUE))


#MP10 OBS #

CMCC_Observados <- read_excel("/Users/cristobal512/Desktop/geoaire/CMCC/Observados/CMCC_website/CMCC_01Julio2023-31Julio2023.xlsx")

names(CMCC_Observados) <- trimws(names(CMCC_Observados))

CMCC_Observados <- CMCC_Observados %>%
  mutate(Fecha = dmy_hm(`FECHA/HORA`),
         Hora = hour(Fecha)) %>% 
  rename(t = `TEMPERATURA AMBIENTAL (℃)`,
         ws = `WS (m/s)`,
         wd = `WD (°)`,
         MP10_Obs = `MP 10 (N) (µg/m3)`)

MeanMP10byHour_Observados <- CMCC_Observados %>% 
  select(Hora, MP10_Obs) %>% 
  group_by(Hora) %>% 
  summarise(MP10_Obs = mean(MP10_Obs, na.rm = TRUE))

MP10_Observados <- CMCC_Observados %>% 
  select(Fecha, Hora, MP10_Obs)


# MET MOD #

path_met_mod <- paste0('/Users/cristobal512/Desktop/geoaire/CMCC/Modelados/Geologger/Campamento-PRO_', start_date, '_a_', end_date, '.csv')

Met_Modelados <- read_csv(path_met_mod,
                          col_types = cols(Fecha = col_date(format = "%d/%m/%Y")),
                          locale = locale(decimal_mark = ","))

Met_Modelados <- Met_Modelados %>% 
  rename(date = Fecha,
         wd = `Direccion viento (°)`,
         ws = `Velocidad del viento (m/s)`,
         t = `Temperatura (°C)`)

Met_Modelados <- Met_Modelados %>%
  mutate(date = ymd_h(paste(date, Hora)))

# MET OBS #

Met_Observados <- CMCC_Observados %>% 
  select(date, Hora, wd, t, ws)

########################################################################################################################################################

MesDelReporte = "julio"

FechaEntrega = "Agosto 2023"

start_date <- '01-07-2023'
end_date <- '31-07-2023'

fecha_comienzo <- as.Date(start_date, format='%d-%m-%Y')

fecha_comienzo_dia <- format(fecha_comienzo, format = "%d")

fecha_termino <- as.Date(end_date, format='%d-%m-%Y')

fecha_termino_dia <- format(fecha_termino, format = "%d")

fecha_termino_mes <- format(fecha_termino, format = "%m")

fecha_termino_year <- format(fecha_termino, format = "%Y")

# Create a lookup vector for Spanish month names
spanish_months <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", 
                    "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")

# Use the month number to get the Spanish month name
fecha_termino_mes_nombre <- spanish_months[as.numeric(fecha_termino_mes)]

Mes_de_entrega = spanish_months[as.numeric(fecha_termino_mes) + 1]














































































































































































































































































