# Título: Gráfico de pirâmide etária utilizando ggplot
# Autora: Denis
# Descrição: 
# Data de atualização: 20/7
# Observação: 

# Pacotes -----------------------------------------------------------------
require(pacman)  
pacman::p_load(janitor, readxl, tidyverse)

# Importar ----------------------------------------------------------------

# Banco de dados de exemplo. Utilize o seu, altere o nome das variáveis.
bd_piramide <-  tibble(
    idade_em_anos = sample(2:99, size = 100, replace = T),
    sexo = sample(c("Feminino","Masculino"), size = 100, replace = T)
  )


bd_piramide <- bd_piramide |> 
  mutate(
    fx_etaria = cut(
      x = idade_em_anos,
      breaks = c(-Inf, 10, 20, 30, 40, 50, 60, Inf),
      right = FALSE,
      include.lowest = FALSE,
      labels = c("0-9", "10-19", "20-29", "30-39",  "40-49",  "50-59", "60 anos e+")
    ) |> as.factor()
  ) |>
  count(fx_etaria, sexo) |>
  mutate(
         percent = ifelse (sexo == 'Feminino',
                           round(100*(n / sum(n, na.rm=T)),1)*-1,
                           round(100*(n / sum(n, na.rm=T)),1))
         
         )

ggplot(data = bd_piramide) +
  aes(x = percent,
      y = fx_etaria,
      fill = sexo) +
  geom_col() +
  scale_x_continuous(labels = \(x) paste0(abs(x), "%")) +
  scale_fill_manual(values = c("green","blue")) +
  scale_y_discrete(drop = F) +
  theme_light() +
  labs(
    x = "Porcentagem",
    y = "Faixas etárias",
    fill = "",
    title = "Pirâmide Etária"
  )
  
  
  
  
# Arrumar -----------------------------------------------------------------

# Visualizar --------------------------------------------------------------
