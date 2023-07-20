# Título: Gráfico com dois eixos verticais
# Autora: Denis
# Descrição:
# Data de atualização: 20/7
# Observação:

# Pacotes -----------------------------------------------------------------
require(pacman); p_load(janitor, reaxl, tidyverse)

# Importar ----------------------------------------------------------------
base <- tibble(ano = 1994:2016,
               variavel1 = c(86L, 91L, 98L, 107L,116L, 126L, 123L, 112L,
                             103L, 102L, 103L, 92L,77L, 59L, 43L, 29L,
                             19L, 14L, 13L, 12L,12L, 10L, 9L),
               variavel2 = c(728.364,757.467,780.423, 792.756,
                             701.685, 720.71,677.292, 761.649,
                             668.218, 679.042,974.355, 1005.035,
                             1123.09, 1055.07,1092.498, 1100.654,
                             899.767, 1018.462,1046.096, 1084.173,
                             1158.217, 802.194,276.773))

# Arrumar -----------------------------------------------------------------

base_m <- base |>
  pivot_longer(
    cols = variavel1:variavel2,
    values_to = "valores",
    names_to = "categoria"
  )

# Visualizar --------------------------------------------------------------

#versão sem 2 eixos
ggplot(base_m, aes(ano, valores, fill = categoria)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("variavel1" = "#024B79", "variavel2" = "#65B840")) +
  facet_wrap(~ categoria, ncol = 1, scales = "free_y") +
  theme(legend.position = "top") +
  labs(
    x = "Anos",
    y = "rotulos",
    fill = "")



scaleFactor <- max(base$variavel1) / max(base$variavel2)

ggplot2::ggplot(base, ggplot2::aes(x = ano,  width = .4)) +
  ggplot2::geom_col(
    ggplot2::aes(y = variavel1),
    fill = "#024B79",
    position = ggplot2::position_nudge(x = -.4)
  ) +
  ggplot2::geom_line(ggplot2::aes(y = variavel2 * scaleFactor),
                     color = "#65B840",
                     linewidth = 2) +
  ggplot2::scale_y_continuous(
    name = "Variavel 1",
    sec.axis = ggplot2::sec_axis( ~ . / scaleFactor, name = "Título do segundo eixo")
  ) +
  ggplot2::scale_x_continuous(breaks = seq(1994, 2016, 4)) +
  ggplot2::theme(
    axis.title.y.left = ggplot2::element_text(color = "#024B79"),
    axis.text.y.left = ggplot2::element_text(color = "#024B79"),
    axis.title.y.right = ggplot2::element_text(color = "#65B840"),
    axis.text.y.right = ggplot2::element_text(color = "#65B840")
  ) +
  ggplot2::labs(title = "Título",
                x = "") +
  ggplot2::theme_bw()








