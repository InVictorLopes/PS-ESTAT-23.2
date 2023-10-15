#INSTALACAO DOS PACOTES
if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, zoo)

#TEMAS
cores_estat <- c("#A11D21", "#003366", "#CC9900", "#663333", "#FF6600", "#CC9966", "#999966", "#006606", "#008091", "#041835", "#666666")

theme_estat <- function(...) {
  theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(colour = "black", size = 12),
      axis.title.x = ggplot2::element_text(colour = "black", size = 12),
      axis.text = ggplot2::element_text(colour = "black", size = 9.5),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      legend.position = "top",
      ...
    )
  
  return(
    list(
      theme,
      scale_fill_manual(values = cores_estat),
      scale_colour_manual(values = cores_estat)
    )
  )
}

#CARREGAMENTO DO BANCO DE DADOS
base_vendas <- read.csv("/Users/victo/Documents/PS_ESTAT_23_2/Base de dados/vendas.csv") %>%
  mutate(
    Data.Venda=as.Date(Data.Venda, format = "%m/%d/%Y"),
    Ano = lubridate::year(Data.Venda),
    Mes = lubridate::month(Data.Venda),
    AnoMes = as.yearmon(paste(Ano, Mes), "%Y %m")
  )

# Agrupar e resumir os dados por mes
faturamento_anual <- base_vendas %>%
  group_by(AnoMes, Category) %>%
  summarise(Faturamento_Ano = sum(Price)) %>%
  na.omit()

# Visualização do faturamento anual
ggplot(faturamento_anual, aes(x = AnoMes, y = Faturamento_Ano, color = Category)) +
  geom_line(stat = "identity") +
  labs(x = "Ano", y = "Faturamento Anual", title = "Faturamento Anual ao Longo do Tempo") +
  scale_color_manual(values = cores_estat) +
  scale_x_yearmon(breaks=sort(unique(faturamento_anual$AnoMes))) +
  theme_estat()
