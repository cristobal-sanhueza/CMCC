library("plyr")  
library("dplyr")                                                
library("readr")  
library("readxl")
library("tidyverse")
library("xlsx")



### DATAFRAME MODELADOS

Modelados <- list.files(path = "CMCC/Modelados/V2/2021/6-Junio_2021",    
                        pattern = ".xlsx",
                        full.names = TRUE) %>% 
  lapply(read_excel) %>%                                           
  bind_rows()                                               

View(Modelados)

NumberOfDaysInMonth = 30

MyLogicVector <- rep(c(T,F,F,F), each = 24, length.out = 95)

MyLogicVector_2 <- rep(MyLogicVector, times = NumberOfDaysInMonth)  

Mysequence <- seq(1:(95*30)) 

MyNewIndex <- Mysequence[MyLogicVector_2]  # 1,2,3,...24, 73,74,75,...96, 145,146,147,...168, 217,218,219,... ... 2182,2183,2184 (only correct indeces)

Modelados <- Modelados[MyNewIndex,]  # correct dataframe.

View(Modelados)

### -------------------------------------------------------------------------------------------------------------

write.xlsx(Modelados, file = "SIERRA_GORDA/Modelados/pronostico-alerta/sg/2022/8.Agosto/MP2.5/Combinados/Agosto2022.xlsx")


write_excel_csv(Modelados, file = "CMCC/Modelados/Mayo/MP10/Combinados/Mayo2022.csv")































































































































