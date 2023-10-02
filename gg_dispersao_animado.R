# Título: Gráfico de dispersão estilo hans Rosling
# Autora: Denis
# Descrição: 
# Data de atualização: 2/10
# Observação: 

# Pacotes -----------------------------------------------------------------
  require(pacman); p_load(janitor, readxl, tidyverse, gganimate)

# Importar ----------------------------------------------------------------

base <- expand.grid(
  ano = 2000:2020,
  municipio = c("Municipio A", "Municipio B",
                "Municipio C", "Municipio D")
)

# Arrumar -----------------------------------------------------------------
set.seed(14052021)
base_ <- base |>
  mutate(renda = runif(n(), 5000, 50000),
         expec_vida = as.integer(runif(n(), 40, 80)))

# Visualizar --------------------------------------------------------------
gg_hans <- base_ |> 
  ggplot2::ggplot(ggplot2::aes(x = renda,
                               y = expec_vida,
                               size = expec_vida,
                               color = municipio)) +
  ggplot2::geom_point(alpha = 0.7) +
  ggplot2::scale_size_continuous(range = c(5, 30)) +
  ggplot2::scale_x_log10() +
  ggplot2::labs(
    title = "Ano: {frame_time}",
    x = "Renda",
    y = "Expectativa de Vida") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(size = 20)
  )

# gganimate aqui
gg_anim <- gg_hans +
  gganimate::transition_time(ano) +
  gganimate::ease_aes('linear')

gg_anim


# Exportar ----------------------------------------------------------------


anim_save("exemplo_ggplot_animado.gif", gg_anim)