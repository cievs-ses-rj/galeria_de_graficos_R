# Título: Gráfico de barras com porcentagens
# Autora: Denis
# Descrição: 
# Data de atualização: 20/7
# Observação: /

# Pacotes -----------------------------------------------------------------
 require(pacman); p_load(scales, tidyverse)

# Importar ----------------------------------------------------------------

base_exemplo <- tibble(
  ano = c(2016, 2017, 2018),
  variavel1 = c(762017, 825130, 865997),
  variavel2 = c(275848, 366359, 268330)
)

# Arrumar -----------------------------------------------------------------
base_m <- base_exemplo |> 
  pivot_longer(
    cols = variavel1:variavel2,
    values_to = "valores",
    names_to = "categoria"
  ) |> 
  mutate(
    proporcao = valores/sum(valores),
    .by = ano # igual ao group_by
  )


# Visualizar --------------------------------------------------------------
ggplot(base_m, aes(x = as.factor(ano), y= proporcao, fill = categoria)) +
  theme_bw() +
  geom_bar(stat="identity", width=.8, colour = 'black', position = "dodge", alpha = 0.8) +
  labs(
    title = "Título", y = 'Porcentagem', x = "Ano") +
  geom_text(aes(label = percent(proporcao)), position=position_dodge(width=0.9), vjust=-0.25, size = 5)+
  scale_fill_manual(values=c("dodgerblue2", "red3")) +
  scale_y_continuous(labels = percent)


# Exportar ----------------------------------------------------------------

# Rascunho ----------------------------------------------------------------