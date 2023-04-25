# GESTION DE L'ENVIRONNEMENT ----

rm(list = ls())

library(tidyverse)
library(forcats)
library(MASS)
library(yaml)

source("R/functions.R", encoding = "UTF-8")

## jeton API ----

api_token <- yaml::read_yaml("secrets.yaml")$jeton_api

# IMPORTATION DES DONNÉES ----

# j'importe les données avec read_csv2 parce que c'est un csv avec des ;
# et que read_csv attend comme separateur des ,
df <- arrow::read_parquet(
  "individu_reg.parquet",
  col_select = c("region", "aemm", "aged", "anai", "catl", "cs1", "cs2", "cs3",
                 "couple", "na38", "naf08", "pnai12", "sexe", "surf", "tp",
                 "trans", "ur")
)

# RETRAITEMENT DES DONNÉES ----

df <- df %>%
  mutate(aged = as.numeric(aged))

df$sexe <- df$sexe %>%
  as.character() %>%
  fct_recode(Homme = "1", Femme = "2")

# STATISTIQUES DESCRIPTIVES ----

summarise(group_by(df, aged), n())

fonction_de_stat_agregee(df %>% filter(sexe == "Homme") %>% pull(aged))
fonction_de_stat_agregee(df %>% filter(sexe == "Femme") %>% pull(aged))

# stats trans par statut
df3 <- df %>%
  group_by(couple, trans) %>%
  summarise(x = n()) %>%
  group_by(couple) %>%
  mutate(y = 100 * x / sum(x))


# GAPHIQUES ----

ggplot(df) +
  geom_histogram(aes(x = 5 * floor(as.numeric(aged) / 5)), stat = "count")

p <- # part d'homme dans chaque cohort
  df %>%
  group_by(aged, sexe) %>%
  summarise(SH_sexe = n()) %>%
  group_by(aged) %>%
  mutate(SH_sexe = SH_sexe / sum(SH_sexe)) %>%
  filter(sexe == 1) %>%
  ggplot() +
  geom_bar(aes(x = aged, y = SH_sexe), stat = "identity") +
  geom_point(aes(x = aged, y = SH_sexe), stat = "identity", color = "red") +
  coord_cartesian(c(0, 100))

ggsave("p.png", p)


# MODELISATION ----

df3 <- df %>%
  dplyr::select(surf, cs1, ur, couple, aged) %>%
  dplyr::filter(surf != "Z")
df3[, 1] <- factor(df3$surf, ordered = TRUE)
df3[, "cs1"] <- factor(df3$cs1)
df3 %>%
  filter(couple == "2" & aged > 40 & aged < 60)
MASS::polr(surf ~ cs1 + factor(ur), df3)

