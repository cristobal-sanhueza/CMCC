library(readr)
library(lubridate)
library(tidyverse)
library(tree)
library(data.table) # para usar funcion setDT()
library(xlsx) # para usar function addDataFrame()


Meteorologia <- read_csv("Observados/CrearModelos/Campamento_01-08-2023_a_31-12-2023.csv", 
                                                               col_types = cols(Fecha = col_date(format = "%d/%m/%Y")), 
                                                               locale = locale(decimal_mark = ",", grouping_mark = "."))


MP10 <- read_csv("Observados/CrearModelos/calidad-aire-dia-hora_Campamento_01-08-2023_a_31-12-2023.csv", 
                                                                     col_types = cols(Fecha = col_date(format = "%d/%m/%Y")))


MP10 <- MP10 %>% 
  mutate(DateAndTime = as.POSIXct(paste(Fecha, Hora), format = "%Y-%m-%d %H"))


Meteorologia <- Meteorologia %>% 
  mutate(DateAndTime = as.POSIXct(paste(Fecha, Hora), format = "%Y-%m-%d %H"))

MP10 <- MP10 %>% 
  select(-c("Fecha", "Hora"))

Meteorologia <- Meteorologia %>% 
  select(-c("Fecha", "Hora"))

MP10 <- MP10 %>% 
  relocate(DateAndTime)

Meteorologia <- Meteorologia %>% 
  relocate(DateAndTime)

JoinedData <- merge(Meteorologia, MP10, by = "DateAndTime")

JoinedData <- JoinedData %>% 
  drop_na()

names(JoinedData) <- c("DateAndTime", "DV", "HR", "TEMP", "VV", "MP10")

JoinedData <- JoinedData %>% 
  mutate(Hora = hour(DateAndTime))

MP10 %>% 
  ggplot(aes(x = DateAndTime, y = `MP10 (µg/m³N)`)) +
  geom_point()


### ARBOL ####

################################################################################################################

wb = xlsx::createWorkbook()

for (i in 0:23) {
  
  sheet = xlsx::createSheet(wb, paste("Hora", i))
  
  
  tree.dataframe <- tree(MP10 ~ DV + VV + TEMP + HR,
                         control = tree.control(nobs = nrow(JoinedData %>% filter(Hora == i)),
                                                mincut = 5, minsize = 10, mindev = 0),
                         data = JoinedData %>% filter(Hora == i))$frame
  
  
  setDT(tree.dataframe, keep.rownames = TRUE)
  names(tree.dataframe)[1] <- "NodeNum"
  
  tree.dataframe$NodeNum <- as.numeric(tree.dataframe$NodeNum)
  
  tree.dataframe <- tree.dataframe %>% 
    mutate(spaces = case_when(
      1 == NodeNum ~ "   ",
      2 <= NodeNum & NodeNum <= 3 ~ "      ",
      4 <= NodeNum & NodeNum <= 7 ~ "        ",
      8 <= NodeNum & NodeNum <= 15 & nchar(NodeNum) == 1 ~ "          ",
      8 <= NodeNum & NodeNum <= 15 & nchar(NodeNum) == 2 ~ "         ",
      16 <= NodeNum & NodeNum <= 31 ~ "           ",
      32 <= NodeNum & NodeNum <= 63 ~ "             ",
      64 <= NodeNum & NodeNum <= 127 & nchar(NodeNum) == 2 ~ "               ",
      64 <= NodeNum & NodeNum <= 127 & nchar(NodeNum) == 3 ~ "              ",
      128 <= NodeNum & NodeNum <= 255 ~ "                ",
      256 <= NodeNum & NodeNum <= 511 ~ "                  ",
      512 <= NodeNum & NodeNum <= 1023 & nchar(NodeNum) == 3 ~ "                    ",
      512 <= NodeNum & NodeNum <= 1023 & nchar(NodeNum) == 4 ~ "                   ",
      1024 <= NodeNum & NodeNum <= 2047 ~ "                     ",
      2048 <= NodeNum & NodeNum <= 4095 ~ "                       ",
      4096 <= NodeNum & NodeNum <= 8191 ~ "                         ",
      8192 <= NodeNum & NodeNum <= 16383 & nchar(NodeNum) == 4 ~ "                           ",
      8192 <= NodeNum & NodeNum <= 16383 & nchar(NodeNum) == 5 ~ "                          ",
      16384 <= NodeNum & NodeNum <= 32767 ~ "                            ",
      32768 <= NodeNum & NodeNum <= 65535 ~ "                              ",
      65536 <= NodeNum & NodeNum <= 131071 & nchar(NodeNum) == 5 ~ "                                ",
      65536 <= NodeNum & NodeNum <= 131071 & nchar(NodeNum) == 6 ~ "                               ",
      131072 <= NodeNum & NodeNum <= 262143 ~ "                                 ",
      262144 <= NodeNum & NodeNum <= 524287 ~ "                                   ",
      524288 <= NodeNum & NodeNum <= 1048575 & nchar(NodeNum) == 6 ~ "                                     ",
      524288 <= NodeNum & NodeNum <= 1048575 & nchar(NodeNum) == 7 ~ "                                    ",
      1048576 <= NodeNum & NodeNum <= 2097151 ~ "                                      ",
      between(NodeNum, 2^21, 2^22-1) ~ strrep(" ", 47 - nchar(NodeNum)),
      between(NodeNum, 2^22, 2^23-1) ~ strrep(" ", 49 - nchar(NodeNum)),
      between(NodeNum, 2^23, 2^24-1) ~ strrep(" ", 51 - nchar(NodeNum)),
      between(NodeNum, 2^24, 2^25-1) ~ strrep(" ", 53 - nchar(NodeNum)),
      between(NodeNum, 2^25, 2^26-1) ~ strrep(" ", 55 - nchar(NodeNum)),
      between(NodeNum, 2^26, 2^27-1) ~ strrep(" ", 57 - nchar(NodeNum)),
      between(NodeNum, 2^27, 2^28-1) ~ strrep(" ", 59 - nchar(NodeNum))
    ))
  
  tree.dataframe <- tree.dataframe %>% mutate(Index = row_number()) %>% select(Index, everything())
  
  col_types <- c("numeric", "numeric", "text", "numeric", "numeric", "numeric", "text", "text")
  
  addDataFrame(as.matrix(tree.dataframe), sheet=sheet, startColumn=1, row.names=FALSE)
  
}


################################################################################################################

xlsx::saveWorkbook(wb, file = "Arboles/NuevoModelo.xlsx")

################################################################################################################





























































































































































































































































































































































































































































