# Título: Gráfico de ranking
# Autora: Denis
# Descrição: 
# Data de atualização: 02/10/2023
# Observação: Arrumar base antes

# Pacotes -----------------------------------------------------------------
  require(pacman); p_load(janitor, readxl, tidyverse)

# Importar ----------------------------------------------------------------



base <- sim |>
  janitor::clean_names() |> 
  dplyr::mutate(
    dt_obito = lubridate::dmy(dtobito),
    ano_obito = lubridate::year(dt_obito) |> as.character(),
    mes_obito = str_pad(paste0(month(dt_obito),lubridate::year(dt_obito)), side = "left", pad = "0", width = 6),
    mes_obito = lubridate::my(mes_obito),
    cap = factor(stringr::str_sub(causabas, 1, 1), levels = LETTERS)
  ) |>
  dplyr::filter(
    dt_obito < as.Date("2023-01-01"),
    stringr::str_sub(codmunres, 1, 2) == "33"
  )

cores <- c("#C16B44","#221147","#E3B754","#18AA12","#CC1478","#1B6097","#852A6D","#218055","#E58997","#236886",
           "#41A34F","#EEC1B3","#FAFA14","#770F1A","#125577","#E08E9F","#C71E59","#E59646","#85C09D","#AC7DA9" ,
           "#34fCA1","#D4D056")#,"#CC3075","#96C66F","#32AFFF")


# Arrumar -----------------------------------------------------------------

base_gg_ranking_t <- base |>
  dplyr::mutate(
    cap1 =
      dplyr::case_when(
        causabas >= "A00" & causabas <= "B99" ~ 1,
        causabas >= "C00" & causabas <= "D48" ~ 2,
        causabas >= "D50" & causabas <= "D89" ~ 3,
        causabas >= "E00" & causabas <= "E90" ~ 4,
        causabas >= "F00" & causabas <= "F99" ~ 5,
        causabas >= "G00" & causabas <= "G99" ~ 6,
        causabas >= "H00" & causabas <= "H59" ~ 7,
        causabas >= "H60" & causabas <= "H95" ~ 8,
        causabas >= "I00" & causabas <= "I99" ~ 9,
        causabas >= "J00" & causabas <= "J99" ~ 10,
        causabas >= "K00" & causabas <= "K93" ~ 11,
        causabas >= "L00" & causabas <= "L99" ~ 12,
        causabas >= "M00" & causabas <= "M99" ~ 13,
        causabas >= "N00" & causabas <= "N99" ~ 14,
        causabas >= "O00" & causabas <= "O99" ~ 15,
        causabas >= "P00" & causabas <= "P96" ~ 16,
        causabas >= "Q00" & causabas <= "Q99" ~ 17,
        causabas >= "R00" & causabas <= "R99" ~ 18,
        causabas >= "S00" & causabas <= "T98" ~ 19,
        causabas >= "V01" & causabas <= "Y98" ~ 20,
        causabas >= "Z00" & causabas <= "Z99" ~ 21,
        causabas >= "U04" & causabas <= "U99" ~ 22,
        TRUE ~ 99
        
      ),
    
  ) |>
  dplyr::group_by(cap1, ano_obito) |>
  dplyr::summarise(n = n()) |>
  dplyr::ungroup() |>
  
  dplyr::left_join(pop_rj, by = c("ano_obito" = "ano")) |>
  dplyr::mutate(tx = round(n.x / n.y * 1e5, 3),
                color_cap = case_when(
                  cap1 == 1 ~ "#18AA12",
                  cap1 == 2 ~ "#221147",
                  cap1 == 3 ~ "#852A6D",
                  cap1 == 4 ~ "#218055",
                  cap1 == 5 ~ "#E58997",
                  cap1 == 6 ~ "#236886",
                  cap1 == 7 ~ "#41A34F",
                  cap1 == 8 ~ "#EEC1B3",
                  cap1 == 9 ~ "#C16B44",
                  cap1 == 10 ~ "#E3B754",
                  cap1 == 11 ~ "#FAFA14",
                  cap1 == 12 ~ "#770F1A",
                  cap1 == 13 ~ "#125577",
                  cap1 == 14 ~ "#E08E9F",
                  cap1 == 15 ~ "#E59646",
                  cap1 == 16 ~ "#85C09D",
                  cap1 == 17 ~ "#C71E59",
                  cap1 == 18 ~ "#CC1478",
                  cap1 == 19 ~ "#AC7DA9",
                  cap1 == 20 ~ "#1B6097",
                )) |>
  dplyr::arrange(ano_obito, desc(tx)) |> #view()# taxa
  dplyr::mutate(
    posicao = factor(paste0(row_number(), "º"), levels = paste0(1:length(LETTERS), "º")),
    .by = ano_obito)

base_posicao_t_sec_y <- base_gg_ranking_t |>
  filter(ano_obito == "2022",
         cap1 %in% c(9,2,10,18,1,20)
  ) |>
  left_join(cid10::cid_capitulos, by = c("cap1"="capitulo")) |>
  select(posicao, cap1, ano_obito, tx, abrev, color_cap)

base_posicao_t_fir_y <- base_gg_ranking_t |>
  filter(ano_obito == "2018",
         cap1 %in% c(9,2,10,18,1,20)
  ) |>
  select(posicao, cap1, ano_obito, color_cap)

base_gg_ranking_t |>
  # filter(cap1 %in% c(9,2,10,18,1,20)) |> # Preocupar com esta linha
  ggplot2::ggplot(ggplot2::aes(
    x = ano_obito ,
    y = forcats::fct_rev(posicao),
    group = cap1,
    colour = color_cap
  ))+
  ggplot2::geom_line(linewidth = 2.4, show.legend = F) +
  ggplot2::geom_point(size = 6, show.legend = F) +
  ggplot2::geom_point(colour = "#FFFFFF", size = 3) +
  ggplot2::geom_text(
    data = base_posicao_t_sec_y,
    aes(label = str_wrap(paste0(posicao, " - ", abrev), 45), colour = color_cap),
    hjust = 0,
    nudge_x = 0.1,
    fontface = "bold",
    size = 5,
    show.legend = F
  ) +
  ggplot2::geom_text(
    data = base_posicao_t_fir_y,
    aes(label = posicao, colour = color_cap),
    hjust = 0,
    nudge_x = -.25,
    fontface = "bold",
    size = 5,
    show.legend = F
  ) +
  ggplot2::scale_colour_identity(expand = c(.01, 0)) +
  ggplot2::labs(
    x = "",
    y = "",
    title = " Ranking de óbitos no Estado do Rio de Janeiro, 2018-2022\n\n"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(
      size = 18,
      face = "bold",
      color = "#22568B",
      # family = "Ubuntu"
    ),
    plot.title.position = "plot",
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_line(linetype = 2, linewidth = 1),
    panel.grid.major.y = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(size = 14, face = "bold"),
    axis.text.y =  ggplot2::element_blank(),
    plot.margin = unit(c(0.35, 0, 0.3, 0), "cm"),
  ) +
  ggplot2::coord_cartesian(xlim = c(1, 7), clip = "off")


# Visualizar --------------------------------------------------------------
