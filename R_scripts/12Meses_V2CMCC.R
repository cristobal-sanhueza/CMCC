library("plyr")  
library("dplyr")                                                
library("readr")  
library("readxl")
library("tidyverse")
library("xlsx")
library("lubridate")


### JUNIO 2021

Modelados <- list.files(path = "CMCC/Modelados/V2/2021/6-Junio_2021",    
                        pattern = ".xlsx",
                        full.names = TRUE) %>% 
  lapply(read_excel) %>%                                           
  bind_rows()                                               


NumberOfDaysInMonth = 30

MyLogicVector <- rep(c(T,F,F,F), each = 24, length.out = 95)

MyLogicVector_2 <- rep(MyLogicVector, times = NumberOfDaysInMonth)  

Mysequence <- seq(1:(95*NumberOfDaysInMonth)) 

MyNewIndex <- Mysequence[MyLogicVector_2]  

Junio2021 <- Modelados[MyNewIndex,]

write.xlsx(Junio2021, file = "CMCC/Modelados/V2/2021/6-Junio_2021/Combinados/Junio2021.xlsx")


### JULIO 2021

Modelados <- list.files(path = "CMCC/Modelados/V2/2021/7-Julio_2021",    
                        pattern = ".xlsx",
                        full.names = TRUE) %>% 
  lapply(read_excel) %>%                                           
  bind_rows()                                               


NumberOfDaysInMonth = 31

MyLogicVector <- rep(c(T,F,F,F), each = 24, length.out = 95)

MyLogicVector_2 <- rep(MyLogicVector, times = NumberOfDaysInMonth)  

Mysequence <- seq(1:(95*NumberOfDaysInMonth)) 

MyNewIndex <- Mysequence[MyLogicVector_2]  

Julio2021 <- Modelados[MyNewIndex,]

################################ ARREGLAR DIA 31################################

July31 <- read_excel(path = "CMCC/Modelados/V2/2021/7-Julio_2021/V2-CMCC-Pronostico-31-07-2021.xlsx")

Julio2021$VALOR[721:744] <- July31$VALOR[25:48]

Julio2021$FECHA[721:744] <- July31$FECHA[25:48]

write.xlsx(Julio2021, file = "CMCC/Modelados/V2/2021/7-Julio_2021/Combinados/Julio2021.xlsx")

################################################################################

### AGOSTO 2021

Modelados <- list.files(path = "CMCC/Modelados/V2/2021/8-Agosto_2021",    
                        pattern = ".xlsx",
                        full.names = TRUE) %>% 
  lapply(read_excel) %>%                                           
  bind_rows()                                               


NumberOfDaysInMonth = 31

MyLogicVector <- rep(c(T,F,F,F), each = 24, length.out = 95)

MyLogicVector_2 <- rep(MyLogicVector, times = NumberOfDaysInMonth)  

Mysequence <- seq(1:(95*NumberOfDaysInMonth)) 

MyNewIndex <- Mysequence[MyLogicVector_2]  

Agosto2021 <- Modelados[MyNewIndex,]

write.xlsx(Agosto2021, file = "CMCC/Modelados/V2/2021/8-Agosto_2021/Combinados/Agosto2021.xlsx")

### Septiembre 2021

Modelados <- list.files(path = "CMCC/Modelados/V2/2021/9-Septiembre_2021",    
                        pattern = ".xlsx",
                        full.names = TRUE) %>% 
  lapply(read_excel) %>%                                           
  bind_rows()                                               


NumberOfDaysInMonth = 25

MyLogicVector <- rep(c(T,F,F,F), each = 24, length.out = 95)

MyLogicVector_2 <- rep(MyLogicVector, times = NumberOfDaysInMonth)  

Mysequence <- seq(1:(95*NumberOfDaysInMonth)) 

MyNewIndex <- Mysequence[MyLogicVector_2]  

Septiembre2021 <- Modelados[MyNewIndex,]

################################ ARREGLO DIA 27 ################################

Sept27 <- read_excel(path = "CMCC/Modelados/V2/2021/9-Septiembre_2021/V2-CMCC-Pronostico-27-09-2021.xlsx")

Septiembre2021$VALOR[529:552] <- Sept27$VALOR[25:48]

Septiembre2021$FECHA[529:552] <- Sept27$FECHA[25:48]

write.xlsx(Septiembre2021, file = "CMCC/Modelados/V2/2021/9-Septiembre_2021/Combinados/Septiembre2021.xlsx")

################################################################################


### OCTUBRE 2021

Modelados <- list.files(path = "CMCC/Modelados/V2/2021/10-Octubre_2021",    
                        pattern = ".xlsx",
                        full.names = TRUE) %>% 
  lapply(read_excel) %>%                                           
  bind_rows()                                               


NumberOfDaysInMonth = 31

MyLogicVector <- rep(c(T,F,F,F), each = 24, length.out = 95)

MyLogicVector_2 <- rep(MyLogicVector, times = NumberOfDaysInMonth)  

Mysequence <- seq(1:(95*NumberOfDaysInMonth)) 

MyNewIndex <- Mysequence[MyLogicVector_2]  

Octubre2021 <- Modelados[MyNewIndex,]


################################ ARREGLAR FECHAS ###############################

Octubre2021 <- Octubre2021 %>% 
  mutate(MyNewDate = make_datetime(year = Año, month = mes, day = dia, hour = hora))

Octubre2021$FECHA[673:744] <- Octubre2021$MyNewDate[673:744]

Octubre2021 <- Octubre2021[-c(3:7)]

write.xlsx(Octubre2021, file = "CMCC/Modelados/V2/2021/10-Octubre_2021/Combinados/Octubre2021.xlsx")

################################################################################

### NOVIEMBRE 2021

Modelados <- list.files(path = "CMCC/Modelados/V2/2021/11-Noviembre_2021",    
                        pattern = ".xlsx",
                        full.names = TRUE) %>% 
  lapply(read_excel) %>%                                           
  bind_rows()                                               


FirstHalf = 17

MyLogicVector <- rep(c(T,F,F,F), each = 24, length.out = 95)

MyLogicVector_2 <- rep(MyLogicVector, times = FirstHalf)

SecondHalf = 13

MyLogicVector_3 <- rep(c(T,F,F,F), each = 24)

MyLogicVector_4 <- rep(MyLogicVector_3, times = SecondHalf)

MyNewLogicVector <- c(MyLogicVector_2, MyLogicVector_4)

Mysequence <- seq(1:(95*FirstHalf + 96*SecondHalf)) 

MyNewIndex <- Mysequence[MyNewLogicVector]  

Noviembre2021 <- Modelados[MyNewIndex,]

################################ ARREGLAR FECHAS ###############################

Noviembre2021 <- Noviembre2021 %>% 
  mutate(FECHA = make_datetime(year = Año, month = mes, day = dia, hour = hora))

Noviembre2021 <- Noviembre2021[-c(1:4)]

Noviembre2021 <- Noviembre2021[, c(2,1)]

write.xlsx(Noviembre2021, file = "CMCC/Modelados/V2/2021/11-Noviembre_2021/Combinados/Noviembre2021.xlsx")


################################################################################

### DICIEMBRE 2021

Modelados <- list.files(path = "CMCC/Modelados/V2/2021/12-Diciembre_2021",    
                        pattern = ".xlsx",
                        full.names = TRUE) %>% 
  lapply(read_excel) %>%                                           
  bind_rows()                                               


NumberOfDaysInMonth = 30

MyLogicVector <- rep(c(T,F,F,F), each = 24)

MyLogicVector_2 <- rep(MyLogicVector, times = NumberOfDaysInMonth)  

Mysequence <- seq(1:(96*NumberOfDaysInMonth)) 

MyNewIndex <- Mysequence[MyLogicVector_2]  

Diciembre2021 <- Modelados[MyNewIndex,]

################################ ARREGLAR FECHAS ###############################

Diciembre2021 <- Diciembre2021 %>% 
  mutate(FECHA = make_datetime(year = Año, month = mes, day = dia, hour = hora))

Diciembre2021 <- Diciembre2021[-c(1:4)]

Diciembre2021 <- Diciembre2021[, c(2,1)]

write.xlsx(Diciembre2021, file = "CMCC/Modelados/V2/2021/12-Diciembre_2021/Combinados/Diciembre2021.xlsx")

################################################################################

### ENERO 2022

Modelados <- list.files(path = "CMCC/Modelados/V2/2022/1-Enero_2022",    
                        pattern = ".xlsx",
                        full.names = TRUE) %>% 
  lapply(read_excel) %>%                                           
  bind_rows()                                               


NumberOfDaysInMonth = 30

MyLogicVector <- rep(c(T,F,F,F), each = 24)

MyLogicVector_2 <- rep(MyLogicVector, times = NumberOfDaysInMonth)  

Mysequence <- seq(1:(96*NumberOfDaysInMonth)) 

MyNewIndex <- Mysequence[MyLogicVector_2]  

Enero2022 <- Modelados[MyNewIndex,]

write.xlsx(Enero2022, file = "CMCC/Modelados/V2/2022/1-Enero_2022/Combinados/Enero2022.xlsx")

### FEBRERO 2022

Modelados <- list.files(path = "CMCC/Modelados/V2/2022/2-Febrero_2022",    
                        pattern = ".xlsx",
                        full.names = TRUE) %>% 
  lapply(read_excel) %>%                                           
  bind_rows()                                               


NumberOfDaysInMonth = 28

MyLogicVector <- rep(c(T,F,F,F), each = 24)

MyLogicVector_2 <- rep(MyLogicVector, times = NumberOfDaysInMonth)  

Mysequence <- seq(1:(96*NumberOfDaysInMonth)) 

MyNewIndex <- Mysequence[MyLogicVector_2]  

Febrero2022 <- Modelados[MyNewIndex,]

write.xlsx(Febrero2022, file = "CMCC/Modelados/V2/2022/2-Febrero_2022/Combinados/Febrero2022.xlsx")

### MARZO 2022

Modelados <- list.files(path = "CMCC/Modelados/V2/2022/3-Marzo_2022",    
                        pattern = ".xlsx",
                        full.names = TRUE) %>% 
  lapply(read_excel) %>%                                           
  bind_rows()                                               


NumberOfDaysInMonth = 31

MyLogicVector <- rep(c(T,F,F,F), each = 24)

MyLogicVector_2 <- rep(MyLogicVector, times = NumberOfDaysInMonth)  

Mysequence <- seq(1:(96*NumberOfDaysInMonth)) 

MyNewIndex <- Mysequence[MyLogicVector_2]  

Marzo2022 <- Modelados[MyNewIndex,]

write.xlsx(Marzo2022, file = "CMCC/Modelados/V2/2022/3-Marzo_2022/Combinados/Marzo2022.xlsx")

### ABRIL 2022

Modelados <- list.files(path = "CMCC/Modelados/V2/2022/4-Abril_2022",    
                        pattern = ".xlsx",
                        full.names = TRUE) %>% 
  lapply(read_excel) %>%                                           
  bind_rows()                                               


NumberOfDaysInMonth = 30

MyLogicVector <- rep(c(T,F,F,F), each = 24)

MyLogicVector_2 <- rep(MyLogicVector, times = NumberOfDaysInMonth)  

Mysequence <- seq(1:(96*NumberOfDaysInMonth)) 

MyNewIndex <- Mysequence[MyLogicVector_2]  

Abril2022 <- Modelados[MyNewIndex,]

write.xlsx(Abril2022, file = "CMCC/Modelados/V2/2022/4-Abril_2022/Combinados/Abril2022.xlsx")

### MAYO 2022

Modelados <- list.files(path = "CMCC/Modelados/V2/2022/5-Mayo_2022",    
                        pattern = ".xlsx",
                        full.names = TRUE) %>% 
  lapply(read_excel) %>%                                           
  bind_rows()                                               


NumberOfDaysInMonth = 31

MyLogicVector <- rep(c(T,F,F,F), each = 24)

MyLogicVector_2 <- rep(MyLogicVector, times = NumberOfDaysInMonth)  

Mysequence <- seq(1:(96*NumberOfDaysInMonth)) 

MyNewIndex <- Mysequence[MyLogicVector_2]  

Mayo2022 <- Modelados[MyNewIndex,]

write.xlsx(Mayo2022, file = "CMCC/Modelados/V2/2022/5-Mayo_2022/Combinados/Mayo2022.xlsx")



##################################################### CMCC ################################################################


TrainingData <- rbind(Junio2021, Julio2021, Agosto2021, Septiembre2021, Octubre2021, Noviembre2021, Diciembre2021,
                      Enero2022, Febrero2022, Marzo2022, Abril2022, Mayo2022)





















































































































































































































































































































































