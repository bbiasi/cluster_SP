# LINK:  https://cran.r-project.org/web/packages/ClustGeo/vignettes/intro_ClustGeo.html

library(tidyverse)
library(ClustGeo)
library(sp)
# devtools::install_github("ipeaGIT/geobr", subdir = "r-package")
library(geobr)
library(geosphere)
require(randomForestSRC)
require(cluster)
require(reshape2)
library(ggrepel)

# Dados:

coord <- read.delim("Data/municipios_coord.txt", sep = ",") %>% 
  filter(codigo_uf == 35)

load("Data/df_final.RData")
# load("Data/df_ias2_final.Rdata")

agencia_mun <- read_csv("Data/agencia_reguladora.csv")

# balanco <- read_delim("Data/balanco_csv.csv", 
#                       ";", escape_double = FALSE,
#                       trim_ws = TRUE)

balanco <- read_csv2("Data/balanco_csv.csv")

balanco_ref <- balanco %>% 
  select(`Código do Município`,`Sigla do Prestador`, Prestador, `Natureza Jurídica`, 
         `Abrangência`, `Ano de Referência`) %>% 
  filter(`Ano de Referência` == 2019) %>% 
  select(- `Ano de Referência`)

names(balanco_ref) <- paste0(names(balanco_ref), " Ref.")

balanco_ref <- balanco_ref %>% 
  rename("Código do Município" = `Código do Município Ref.`)

balanco <- balanco %>% 
  left_join(balanco_ref)


df <- df %>% 
  mutate("Natureza jurídica - Abrangência" = 
           paste0(`Natureza jurídica`, 
                  " - ", `Abrangência`)) %>% 
  mutate(`Natureza jurídica - Abrangência` = 
           case_when(
             `Natureza jurídica - Abrangência` == "NA - NA" ~  "NA", 
             TRUE ~ `Natureza jurídica - Abrangência`
           )) %>% 
  select(-c(`Microrregião`, `Microrregião Nome`))

micro <- read_csv("Data/regionalizacao.csv") %>% 
  mutate(`Código do Município` = as.character(`Código do Município`))

estados_filter <- unique(micro$Estado)  

# micro <- micro %>% 
#   dplyr::mutate(city = tolower(`Município`)) %>% 
#   dplyr::mutate(city = stringi::stri_trans_general(str = city, id = "Latin-ASCII")) %>% 
#   dplyr::mutate(city = stringr::str_replace_all(city, "[[:punct:]]", "")) %>% 
#   dplyr::mutate(city = stringr::str_replace_all(city, "[[:space:]]", "")) %>% 
#   dplyr::select(-`Município`)

# erro
# objeto 'Município' não encontrado

df <- df %>% 
  dplyr::mutate(city = tolower(`Município`)) %>% 
  dplyr::mutate(city = stringi::stri_trans_general(str = city, id = "Latin-ASCII")) %>% 
  dplyr::mutate(city = stringr::str_replace_all(city, "[[:punct:]]", "")) %>% 
  dplyr::mutate(city = stringr::str_replace_all(city, "[[:space:]]", "")) %>% 
  left_join(micro) %>% 
  select(-city) %>% 
  # filter(Estado %in% estados_filter) %>% 
  mutate(`Bloco` = case_when(
    is.na(`Bloco`) ~ "Sem regionalização", 
    TRUE ~  `Bloco`
  )) %>% 
  mutate(micro_logical = 
           case_when(
             `Bloco` == "Sem regionalização" ~ "não", 
             `Bloco` != "Municípios sem bloco de saneamento" ~ "sim"
           ))

# Ajustando df 


df_cluster <- df %>%
  filter(estado == "São Paulo") %>%
  # filter(`Município` != "São Paulo") %>%
  select(c(1, 2,  21, 22, 26:57, 59, 62, 63,
           65:69, 71, 74, 76, 78:83, 85:91,
           163:166, 169, 171, 172, 175, 176, 182,
           161, 162, 163, 164, 168, 169, 170, 171,
           175, 176, 183, 184, 188)) %>%
  select(-c(3,4, 52)) %>%
  select(-c(6,8, 10, 12, 14, 17, 19,
            34, 36, 39, 41)) %>%
  select(-c(24,26)) %>%
  select(-c(15)) %>%
  select(-16) %>%
  select(-17)

df_cluster_filter <- df_cluster %>%
  filter(if_all(3:length(.), ~!is.na(.))) %>%
  distinct(`Código do Município`, .keep_all = TRUE)

contagem_na <- df_cluster %>%
  summarise(across(everything(), ~ sum(is.na(.))))

# Novo

# PCA

library(factoextra)

res.pca <- prcomp(df_cluster_filter[,-c(1,2)], scale = TRUE, 
                  center = TRUE)

fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 60)) +
  ggplot2::ggtitle("") +
  ggplot2::xlab(label = "Dimensões") +
  ggplot2::ylab(label = "% de variância explicada") +
  theme_classic()

53.4+9.2+6.6
summary(res.pca) # 10 PCs


df_cluster_filter2 <- res.pca$x %>% 
  as.tibble() %>% 
  select(1:10) %>% 
  mutate(`Código do Município` = df_cluster_filter$`Código do Município`)

### definindo silhouetta da divisao existente: 

df_existente <- df_cluster_filter2 %>% 
  left_join(df %>% select(`Código do Município`, `Bloco`)) %>%
  mutate(Bloco_num = as.numeric(as.factor(Bloco))) %>% 
  distinct(`Código do Município`, .keep_all = TRUE)

dist_existente_cluster <- dist(df_existente[, -c(11, 12, 13)])

aux <-silhouette(df_existente$Bloco_num, dist_existente_cluster)


sil_existente <- summary(aux)[["avg.width"]] # media da silhueta


### Criando D0 e d1

# CRIANDO D0 por Random Forest # fora

# require(randomForest) # fora
require(cluster)
require(geosphere)

# Tentando outra RF # fora
# aux <- sidClustering(df_cluster_filter2[-11]) # fora

aux <- dist(df_cluster_filter2[-11])

# 

# dist0 <- aux$dist %>%
#   as.dist()

dist0 <- aux

### Definindo melhor numero de clusters usando D0 

fviz_nbclust(as.matrix(dist0),
             method = "silhouette", hcut) +
  ggplot2::ggtitle("") +
  ggplot2::xlab(label = "Número de Grupos K") +
  ggplot2::ylab(label = "Largura Média de Silhueta") +
  theme_classic()


# distancia geografica 

coord_dist <- coord %>% # sede do municipio
  mutate(codigo_ibge = substr(codigo_ibge, 1, 6)) %>% 
  filter(codigo_ibge %in% df_cluster_filter$`Código do Município`) %>% 
  select(longitude, latitude) %>% 
  as.matrix()

dist <- distm(coord_dist, coord_dist, 
              fun = distGeo)

dist <- as.dist(dist)

# vamos usar 4 a 7 grupos 

# P/ n ter pouco = homogeneidade
# P/ n ter mto = heterogeneidade

K <- c(4, 5, 6, 7)

range.alpha <- seq(0, 1, 0.05) #mudei de 0 a 0.5 para de 0 a 1

# criando os grupos: 

sil <- NULL

for (i in 1:4){
  
  for(j in 1:11){

tree <- hclustgeo(dist0 , dist, alpha = range.alpha[j]) # de 0 ate 0.5

grupos <- cutree(tree,K[i])

aux <- silhouette(grupos, dist = dist) 

sil <- rbind(sil, cbind(summary(aux)[["avg.width"]], K[i], range.alpha[j]))

}

}

# Antigo
# silhouette_todos <- sil %>% as.data.frame() %>% 
#   filter(V1 > sil_existente)
# # Melhor -> K = 0.5 // alpha = 0.1
# tree <- hclustgeo(dist0 , dist, alpha= range.alpha[7])
# grupos <- cutree(tree,K[1])

# Novo
# Teste
# iris.scaled <- scale(iris[, -5])
# km.res <- eclust(iris.scaled, "kmeans", k = 3,
#                  nstart = 25, graph = FALSE)
# silx <- silhouette(km.res$cluster, dist(iris.scaled))
# summary(silx)[["avg.width"]]
# fviz_silhouette(silx)

silhouette_todos <- sil %>% as.data.frame() %>% 
  filter(V1 > sil_existente) 

dist1 <- dist 
cr <- choicealpha(dist0, dist1, range.alpha,
                  4, graph = FALSE)
plot(cr)

alfas <- data.frame(cr$Q)
alfas2 <- alfas %>% 
  tibble::remove_rownames() %>% 
  dplyr::mutate(alpha = range.alpha)

alfas2 %>% 
  reshape2::melt(id.vars = "alpha") %>% 
  dplyr::rename("Alfa" = "alpha",
                "Pseudo-inércia" = "variable",
                "Valor" = "value") %>% 
  ggplot2::ggplot(aes(x = Alfa, y = Valor, color = `Pseudo-inércia`)) +
  lemon::geom_pointline() +
  # annotate("segment", x = 0.15, xend = 0.05, y = .8, yend = .65,
  #          colour = "grey", size = .5, arrow = arrow()) +
  ggplot2::ylab("Q") +
  ggplot2::xlab("α") +
  scale_color_brewer(palette = "Paired") +
  scale_y_continuous(labels = scales::comma_format(accuracy = 0.01,
                                                   decimal.mark = ",")) +
  scale_x_continuous(labels = scales::comma_format(accuracy = 0.01,
                                                   decimal.mark = ",")) +
  theme_classic() +
  theme(legend.title = element_blank(),
        legend.position = c(0.8, 0.5),
        legend.background = element_rect(size = 0.5,
                                         linetype = "solid", 
                                         colour = "grey"))
alfas2

# perda de homogeneidade das variáveis pertinentes a D0
1-alfas2[2,1]/max(alfas2$Q0)

# ganho de homogeneidade geográfica D1
1-alfas2[2,2]/max(alfas2$Q1)


#
#tree <- hclustgeo(dist0 , dist, alpha = range.alpha[2]) # alfa = 0.05
tree <- hclustgeo(dist0 , dist, alpha = range.alpha[8]) # alfa = 0.35


# Dendrograma
# install.packages("ggdendro")
library("ggdendro")
ggdendrogram(tree)

# install.packages("dendextend")
library("dendextend")
tree2 <- tree %>% as.dendrogram %>% 
  set("branches_k_color", k = K[1]) %>% set("branches_lwd", 1.2) %>%
  set("labels_colors") %>% set("labels_cex", c(.9,1.2)) %>% 
  set("leaves_pch", 19)
ggd1 <- as.ggdend(tree2)
ggplot(ggd1)


grupos <- cutree(tree, K[1]) # k=4

# municipio cluster: 

library(geobr)

clusters_mun <- read_municipality(code_muni= "SP", year=2007) %>%
  mutate(code_muni = substr(code_muni, 1, 6)) %>% 
  filter(code_muni %in% df_cluster_filter$`Código do Município`) %>% 
  mutate(cluster = grupos) %>% 
  select(code_muni, cluster) %>% 
  as.data.frame() %>% 
  select(-geom)


muni_sp <- read_municipality(code_muni= "SP", year=2007) %>%
  mutate(code_muni = substr(code_muni, 1, 6)) %>% 
  left_join(clusters_mun) 

## plot: 

# Remove plot axis

no_axis <- theme(axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank())

muni_sp %>% 
  mutate(cluster = as.factor(cluster)) %>% 
  ggplot() +
  geom_sf(aes(fill = cluster),
          size=.15, show.legend = FALSE) +
  labs(subtitle="States", size=8) +
  theme_minimal() +
  no_axis

