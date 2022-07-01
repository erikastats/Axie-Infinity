
# Bibliotecas -------------------------------------------------------------

library(tidyverse)
library(googlesheets4)
library(lubridate)
library(plotly)

#Code to download a new API token commented here:
gs4_auth(
  cache = ".secrets",
  email = "erikanima3dk@gmail.com"
)

# Dados GoogleSheets ------------------------------------------------------

Linksheet <- "https://docs.google.com/spreadsheets/d/1_200lNjQLTdpcnYh0ReCZBjMiePNejjbcd1CfIlXfjg/edit#gid=0"
# sheet <- "Principal"



# Importando planilha ----------------------------------------------------

comprasDF<- read_sheet(Linksheet)


# Análise exploratória ----------------------------------------------------

# Valor de compras

Compras_total <- comprasDF %>% 
  group_by(Data, Local) %>% 
  summarise(Total_compas = sum(`Valor Total`)) %>% 
  arrange(Data) 

Compras_total %>% view

# Compras por mês e Local

Compras_total %>% 
  ungroup() %>% 
  mutate(  Mes = month(Data)) %>% 
  group_by(Mes, Local) %>% 
  summarise(Quantidade = n(),
            Total_compras = sum(Total_compas)) %>% 
  view()

# Compras por mês
Compras_total %>% 
  ungroup() %>% 
  mutate(  Mes = month(Data)) %>% 
  group_by(Mes) %>% 
  summarise(Quantidade = n(),
            Total_compras = sum(Total_compas)) %>% 
  view()

# Produtos mais comprados por Local

comprasDF %>% 
  group_by(Descricao, Local) %>% 
  summarise(Quant = sum(Quantidade), 
            Valor_total = sum(`Valor Total`)) %>% 
  filter(Quant > 1) %>% 
  arrange(desc(Quant)) %>% 
  view

# Produtos mais comprados por mês

comprasDF %>% 
  mutate(Mes = Data %>% ymd_hms() %>% month()) %>% 
  group_by(Descricao, Mes) %>% 
  summarise(Quant = sum(Quantidade), 
            Valor_total = sum(`Valor Total`)) %>% 
  filter(Quant > 1) %>% 
  arrange(desc(Quant)) %>% 
  view

# Produtos mais frequentemente comprados

prodfre <- comprasDF %>% 
  mutate(Mes = Data %>% ymd_hms() %>% month()) %>% 
  group_by(Descricao, Mes) %>% 
  summarise(Freq = n(),
            Quant = sum(Quantidade),
            Valor = sum(`Valor Total`)) %>% 
  filter(Quant > 1) %>% 
  arrange(Descricao,desc(Freq),desc(Mes)) 

prodfre2 <- prodfre %>%
  ungroup() %>% 
  group_by(Descricao) %>% 
  summarise(Freq = sum(Freq),
            Valor = sum(Valor),
            Quant = sum(Quant)) %>% 
  arrange(desc(Freq))

prodfre2 %>% view

# Variação dos valores de produtos mais comprados

prodMaisComprados <- prodfre2 %>% head(10) %>% pull(Descricao)

comprasDF %>% 
  filter(Descricao %in% prodMaisComprados) %>% 
  mutate(Data = Data %>% ymd_hms()) %>% 
  # ggplot(aes(x = Data, y = `Valor unidade`,colour=factor(Descricao))) +
  # geom_line( ) 
  # facet_wrap( ~Descricao, nrow = 2) 
  hchart("line", hcaes(x = Data, y = `Valor unidade`, group = Descricao))
  # view
  plot_ly(x = ~Data, y = ~`Valor unidade`, split = ~Descricao,
          type = 'scatter', mode = 'lines+markers')
