# Título: Gráfico de pirâmide etária utilizando ggplot
# Autora: Helena/Marianna
# Descrição: 
# Data de atualização: 20/7
# Observação: 

# Pacotes -----------------------------------------------------------------
require(pacman); pacman::p_load(readxl, dplyr, ggplot2)

# Importar ----------------------------------------------------------------

# Banco de dados de exemplo. Utilize o seu, altere o nome das variáveis.
setwd("C:/Users/marianna.silva/Downloads")
dir()


PirMasc <- read_excel("Piramide Etaria -  Masculino2.xlsx")

PirFem <- read_excel("Piramide Etaria - Feminino2.xlsx")


# Arrumar -----------------------------------------------------------------


PirMasc2 <- PirMasc %>% #filtrar o que deseja do banco
  filter(`Região de Saúde (CIR)`=="33006 Metropolitana II") %>%
  t() %>% as.data.frame() %>% #t é transposição de matrizes, transformou linha em coluna
  mutate(sexo="Masculino") %>% 
  rename(pop = V1)

PirMasc2$fx_etaria<-rownames(PirMasc2)
PirMasc2<-PirMasc2[-c(1,19),]


PirFem2 <- PirFem %>%
  filter(`Região de Saúde (CIR)`=="33006 Metropolitana II") %>%
  t() %>% as.data.frame() %>%
  mutate(sexo="Feminino") %>%
  rename(pop = V1)

PirFem2$fx_etaria<-rownames(PirFem2)
PirFem2<-PirFem2[-c(1,19),]


str(PirMasc2)

PirMasc2$pop<-as.numeric(PirMasc2$pop)
PirMasc2$pop<-PirMasc2$pop*(-1)

PirEtaria<-rbind(PirMasc2,PirFem2)

PirEtaria$pop<-as.numeric(PirEtaria$pop)

PirEtaria$pop[PirEtaria$sexo=="Masculino"]<-PirEtaria$pop[PirEtaria$sexo=="Masculino"]*-1

abs_virgula <- function (x) {
  format(abs(x), big.mark = ".", decimal.mark = ",", scientific = FALSE)
}


PirEtaria <- PirEtaria %>%
  mutate(sexo = factor(sexo, levels = c("Masculino", "Feminino"))) 


# Visualizar --------------------------------------------------------------

PirEtaria %>%
  ggplot(mapping = aes(x = fx_etaria,
                       y = ifelse(sexo == "Feminino",  yes = pop, no = -pop), fill = sexo)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = abs_virgula, limits = (max(as.numeric(PirEtaria$pop)))* c(-1,1)) +
  labs(y = "População", x = "Faixa etária (em anos)") +
  scale_fill_brewer(palette = "Set1", direction = 1) +
  theme(legend.position = "bottom") +
  coord_flip()