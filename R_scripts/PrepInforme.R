library(readxl)
library(dplyr)
library(lubridate)

setwd("/Users/cristobal512/Desktop/geoaire")

CMCC_Mayo_Obs <- read_excel("Observados/2023/5.Mayo/Informe.xlsx")

CMCC_Mayo_Obs <- CMCC_Mayo_Obs %>% 
  rename(DateAndTime = `FECHA/HORA`,
         MP_10 = `MP 10 (N) (µg/m3)`,
         DIR = `WD (°)`,
         VEL = `WS (m/s)`,
         TEMP = `TEMPERATURA AMBIENTAL (℃)`)


CMCC_Mayo_Obs$DateAndTime <- as.POSIXct(CMCC_Mayo_Obs$DateAndTime, format = "%d/%m/%Y %H:%M")

CMCC_Mayo_Obs <- CMCC_Mayo_Obs %>% 
  mutate(Hora = hour(DateAndTime))


MeanMP10byHour_Observados <- CMCC_Mayo_Obs %>% 
  select(Hora, MP_10) %>% 
  group_by(Hora) %>% 
  summarise(MP_10 = mean(MP_10, na.rm = TRUE))


Met_Observados <- CMCC_Mayo_Obs %>% 
  select(DateAndTime, DIR, VEL, TEMP)


###################################################################

MP10_Modelados <- list.files(path = "CMCC/Modelados/V1/2023/5.Mayo/MP10",
                             pattern = "*.xlsx",
                             full.names = TRUE) %>%
  lapply(read_excel)


for (i in 1:length(MP10_Modelados)) {
  MP10_Modelados[[i]] <- MP10_Modelados[[i]][1:24,]
}

MP10_Modelados <-  bind_rows(MP10_Modelados)

MeanMP10byHour_Modelados <- MP10_Modelados %>% 
  select(hora, VALOR) %>% 
  group_by(hora) %>% 
  summarise(VALOR = mean(VALOR, na.rm = TRUE))



Met_Modelados <- list.files(path = "CMCC/Modelados/V1/2023/5.Mayo/Meteorologia",    
                            pattern = "*.xlsx",
                            full.names = TRUE) %>% 
  lapply(read_excel) 

for (i in 1:length(Met_Modelados)) {
  Met_Modelados[[i]] <- Met_Modelados[[i]][1:24,c(1,6:8)]
}

Met_Modelados <-  bind_rows(Met_Modelados)










































































































































































































































































































































































