# AGRUPAMENTOS MUNICIPAIS FRENTE AO NOVO MARCO LEGAL DO SANEAMENTO: UMA ANALISE 
# UTILIZANDO INFORMACOES SOBRE AS COMPONENTES AGUA E ESGOTO, E COORDENADAS 
# GEOGRAFICAS 

# 2022


# Dev: BIT Analytics

# Tarssio Barreto
# Icaro Bernardes
# Brenner Silva


# Clinte: IAS - Instituto Agua e Saneamento
# Coordenacao: Eduardo Caetano



# Instrucoes basicas - Read me ----

# 1- Para melhor entendimento do presente script recomenda-se a leitura previa de:
# ClustGeo: an R package for hierarchical clustering with spatial constraints
# https://arxiv.org/pdf/1707.03897.pdf

# 2- O codigo esta em linguagem R e com Encoding UTF-8.

# 3- Antes de "rodar" o codigo, certifique da instalacao e carregamento dos dados.

# 4- Caso nao tenha algum dos pacotes instalados, recomenda-se a instalacao via
# CRAN ou repositorio GitHub

# 5- Nao compete a BIT Analytics assegurar a manutencao do codigo desenvolvido e
# dos pacotes utilizados, bem como futuras aplicacoes desenvolvidas por terceiros.


# Pacotes ----
{
  if(!require("tidyverse")) install.packages("tidyverse"); library(tidyverse)
  if(!require("ClustGeo")) install.packages("ClustGeo"); library(ClustGeo)
  if(!require("sp")) install.packages("sp"); library(sp)
  if(!require("geosphere")) install.packages("geosphere"); library(geosphere)
  if(!require("cluster")) install.packages("cluster"); library(cluster)
  if(!require("reshape2")) install.packages("reshape2"); library(reshape2)
  if(!require("lemon")) install.packages("lemon"); library(lemon)
  if(!require("factoextra")) install.packages("factoextra"); library(factoextra)
  if(!require("scales")) install.packages("scales"); library(scales)
  if(!require("caret")) install.packages("caret"); library(caret)
  if(!require("ggsn")) install.packages("ggsn"); library(ggsn)
  if(!require("patchwork")) install.packages("patchwork"); library(patchwork)
  if(!require("wesanderson")) install.packages("wesanderson"); library(wesanderson)
}

# Pacote a ser obtido diretamente via repositorio de GitHub
# devtools::install_github("ipeaGIT/geobr", subdir = "r-package")
library(geobr)


# Dados ----

# save(coord, df, agencia_mun, balanco, micro, file = "Cluster/agrp_sp_ias.RData")

load("Cluster/agrp_sp_ias.RData")

# Tratanento e ajustes ----

balanco_ref <- balanco %>% 
  dplyr::select(`Código do Município`,`Sigla do Prestador`, Prestador,
                `Natureza Jurídica`, `Abrangência`, `Ano de Referência`) %>% 
  dplyr::filter(`Ano de Referência` == 2019) %>% 
  dplyr::select(-`Ano de Referência`)

names(balanco_ref) <- paste0(names(balanco_ref), " Ref.")

balanco_ref <- balanco_ref %>% 
  dplyr::rename("Código do Município" = `Código do Município Ref.`)

balanco <- balanco %>% 
  dplyr::left_join(balanco_ref)

df <- df %>% 
  dplyr::mutate("Natureza jurídica - Abrangência" = 
                  paste0(`Natureza jurídica`, 
                         " - ", `Abrangência`)) %>% 
  dplyr::mutate(`Natureza jurídica - Abrangência` = 
                  case_when(
                    `Natureza jurídica - Abrangência` == "NA - NA" ~  "NA", 
                    TRUE ~ `Natureza jurídica - Abrangência`
                  )) %>% 
  dplyr::select(-c(`Microrregião`, `Microrregião Nome`)) %>% 
  dplyr::mutate(city = tolower(`Município`)) %>% 
  dplyr::mutate(city = stringi::stri_trans_general(str = city, 
                                                   id = "Latin-ASCII")) %>% 
  dplyr::mutate(city = stringr::str_replace_all(city, "[[:punct:]]", "")) %>% 
  dplyr::mutate(city = stringr::str_replace_all(city, "[[:space:]]", "")) %>% 
  dplyr::left_join(micro) %>% 
  dplyr::select(-city) %>% 
  dplyr::mutate(`Bloco` = case_when(
    is.na(`Bloco`) ~ "Sem regionalização", 
    TRUE ~  `Bloco`
  )) %>% 
  dplyr::mutate(micro_logical = 
                  case_when(
                    `Bloco` == "Sem regionalização" ~ "não", 
                    `Bloco` != "Municípios sem bloco de saneamento" ~ "sim"
                  ))

# Selecionando as variaveis de interesse
df_cluster <- df %>%
  dplyr::filter(estado == "São Paulo") %>%
  dplyr::select(c(1, 2,  21, 22, 26:57, 59, 62, 63, 65:69, 71, 74, 76, 
                  78:83, 85:91, 163:166, 169, 171, 172, 175, 176, 182,
                  161, 162, 163, 164, 168, 169, 170, 171, 175, 176, 183,
                  184, 188)) %>%
  dplyr::select(-c(3,  # GE017 | ~43% NA
                   4,  # GE018 | ~43% NA
                   52) # ES025A | Ano anterior e 100% NA
  ) %>%
  dplyr::select(-c(6,  # AG001A | Ano anterior
                   8,  # AG002A | Ano anterior
                   10, # AG003A | Ano anterior
                   12, # AG004A | Ano anterior
                   14, # AG005A | Ano anterior
                   #17, # AG008 | Volume de agua micromedido # ~3,5% NA
                   #19, # AG011 | Volume de agua faturado    # ~3,5% NA
                   34, # AG024 | Volume de servico | % NA > 10%
                   36, # AG028 | % NA > 10%
                   39, # ES002A | Ano anterior
                   41) # ES003A | Ano anterior
  ) %>%
  dplyr::select(-c(`AG021A - Quantidade de ligações totais de água no ano anterior ao de referência.`, # Ano anterior
                   `AG022A - Quantidade de economias residenciais ativas de água micromedidas no ano anterior ao de referência.`) # Ano anterior
  ) %>%
  dplyr::select(-c(`AG013A - Quantidade de economias residenciais ativas de água no ano anterior ao de referência.`) # Ano anterior
  ) %>%
  dplyr::select(-`AG014A - Quantidade de economias ativas de água micromedidas no ano anterior ao de referência.`    # AG014A | Ano anterior
  ) %>%
  dplyr::select(-`AG017 - Volume de água bruta exportado`)  # AG017 | Ano anterior # Quantidade de valores zeros muito maior
  #dplyr::select(-`ES028 - Consumo total de energia elétrica nos sistemas de esgotos`)

df_cluster_filter <- df_cluster %>%
  dplyr::filter(if_all(3:length(.), ~!is.na(.))) %>%
  dplyr::distinct(`Código do Município`, .keep_all = TRUE)

contagem_na <- df_cluster %>%
  dplyr::summarise(across(everything(), ~ sum(is.na(.))))

# PCA ----
res.pca <- prcomp(df_cluster_filter[,-c(1,2)], scale = TRUE, 
                  center = TRUE)

fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 60)) +
  ggplot2::ggtitle("") +
  ggplot2::xlab(label = "Dimensões") +
  ggplot2::ylab(label = "% de variância explicada") +
  theme_classic()

ggsave("Cluster/pca.png", width = 20, height = 16, units = "cm")

summary(res.pca) # 10 PCs

df_cluster_filter2 <- res.pca$x %>% 
  tibble::as.tibble() %>% 
  dplyr::select(1:10) %>% 
  dplyr::mutate(`Código do Município` = df_cluster_filter$`Código do Município`)

# Silhueta ----
df_existente <- df_cluster_filter2 %>% 
  dplyr::left_join(df %>% select(`Código do Município`, `Bloco`)) %>%
  dplyr::mutate(Bloco_num = as.numeric(as.factor(Bloco))) %>% 
  dplyr::distinct(`Código do Município`, .keep_all = TRUE)

dist_existente_cluster <- dist(df_existente[, -c(11, 12, 13)])

aux <- cluster::silhouette(df_existente$Bloco_num, dist_existente_cluster)

sil_existente <- summary(aux)[["avg.width"]] # media da silhueta


# Criando D0 e D1 ----
aux <- dist(df_cluster_filter2[-11])
dist0 <- aux

# Definindo melhor numero de clusters usando D0 
fviz_nbclust(as.matrix(dist0),
             method = "silhouette", pam) +
  ggplot2::ggtitle("") +
  ggplot2::xlab(label = "Número de Grupos K") +
  ggplot2::ylab(label = "Largura Média de Silhueta") +
  theme_classic()

ggsave("Cluster/estimativa_grupos.png", width = 20, height = 20, units = "cm")

# Distancia geografica 
coord_dist <- coord %>% # sede do municipio
  dplyr::mutate(codigo_ibge = substr(codigo_ibge, 1, 6)) %>% 
  dplyr::filter(codigo_ibge %in% df_cluster_filter$`Código do Município`) %>% 
  dplyr::select(longitude, latitude) %>% 
  as.matrix()

dist <- geosphere::distm(coord_dist, coord_dist, 
                         fun = distGeo)

dist <- as.dist(dist)

# vamos usar de k=4 a k=7
K <- c(4, 5, 6, 7)
range_alpha <- seq(0, 1, 0.05) # valores de alpha

# Criando os grupos
sil <- NULL

for (i in 1:4){
  
  for(j in 1:11){
    
    tree <- hclustgeo(dist0 , dist, alpha = range_alpha[j])
    grupos <- cutree(tree,K[i])
    aux <- silhouette(grupos, dist = dist) 
    sil <- rbind(sil, cbind(summary(aux)[["avg.width"]], K[i], range_alpha[j]))
    
  }
}

silhouette_todos <- sil %>% as.data.frame() %>% 
  filter(V1 > sil_existente) # filtrando valores com baixo valor de indice de silhueta

### 7 Grupos
dist1 <- dist 

cr <- choicealpha(dist0, dist1, range_alpha,
                  7, graph = FALSE)
plot(cr)

alfas <- data.frame(cr$Q)
alfas2 <- alfas %>% 
  tibble::remove_rownames() %>% 
  dplyr::mutate(alpha = range_alpha)

alfas2 %>% 
  reshape2::melt(id.vars = "alpha") %>% 
  dplyr::rename("Alfa" = "alpha",
                "Pseudo-inércia" = "variable",
                "Valor" = "value") %>% 
  ggplot2::ggplot(aes(x = Alfa, y = Valor, color = `Pseudo-inércia`)) +
  lemon::geom_pointline() +
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

ggsave("Cluster/alphaQ.png", width = 20, height = 15, units = "cm")

# Perda de homogeneidade
alfas3 <- alfas2 %>% 
  dplyr::mutate(Q0H = 1-(Q0/max(alfas2$Q0)),
                Q1H = 1-(Q1/max(alfas2$Q1)),
                DQH = abs(Q0H+Q1H))

# Clustering ----
tree <- hclustgeo(dist0 , dist, alpha = range_alpha[8]) # alfa = 0.35

grupos <- cutree(tree, K[4]) # k=7

# Obtendo shapes ----
# Atencao
# Esta estapa, normalmente, demanda internet.

clusters_mun <- geobr::read_municipality(code_muni = "SP", year = 2007) %>%
  dplyr::mutate(code_muni = substr(code_muni, 1, 6)) %>% 
  dplyr::filter(code_muni %in% df_cluster_filter$`Código do Município`) %>% 
  dplyr::mutate(cluster = grupos) %>% 
  dplyr::select(code_muni, cluster) %>% 
  as.data.frame() %>% 
  dplyr::select(-geom) %>% 
  dplyr::mutate(cluster = glue::glue("C{cluster}"))

muni_sp <- geobr::read_municipality(code_muni = "SP", year = 2007) %>%
  dplyr::mutate(code_muni = substr(code_muni, 1, 6)) %>% 
  dplyr::left_join(clusters_mun) 

# Plot Mapa ----
muni_sp <- muni_sp %>% 
  dplyr::mutate(cluster = factor(cluster, 
                                 levels = c("C1", "C2", "C3", 
                                            "C4", "C5", "C6", 
                                            "C7")))

mapa1 <- muni_sp %>% 
  ggplot2::ggplot() +
  ggplot2::geom_sf(aes(fill = cluster),
                   size = .05, show.legend = T, color = "white") +
  labs(subtitle = "Estado de São Paulo - Brasil", size = 8) +
  scale_fill_manual(values = wes_palette("GrandBudapest2", 7,
                                         type = "continuous"),
                    na.value = "#7F919C",
                    labels = c("C1", "C2", "C3",
                               "C4", "C5", "C6",
                               "C7", "Sem informações")) +
  ggplot2::xlab("") + ggplot2::ylab("") +
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        linetype = "solid"),
        plot.background = element_rect(fill = "white")) +
  north(muni_sp) +
  scalebar(muni_sp, dist = 100, dist_unit = "km", st.size = 3,
           transform = TRUE, model = "WGS84")
mapa1

ggsave("Cluster/mapa1.png", width = 20, height = 20, units = "cm")

# KNN ----

# Ajustando os data frames
knn <- coord %>% 
  dplyr::select(codigo_ibge, latitude, longitude) %>% 
  dplyr::rename(`code_muni` = "codigo_ibge") %>% 
  dplyr::mutate(code_muni = substr(code_muni, 1, 6)) %>% 
  dplyr::left_join(muni_sp, by = "code_muni") %>% 
  dplyr::select(code_muni, latitude, longitude, cluster)

knn_mod <- knn %>% 
  dplyr::filter(code_muni != 355030) %>% # removendo a cidade de São Paulo por conter apenas 1 amostra
  dplyr::select(-code_muni) %>% 
  droplevels()

levels(knn_mod$cluster)

# Removendo NA para criacao do modelo
knn_mod_sNA <- knn_mod %>% 
  dplyr::filter(!is.na(cluster))

{
  set.seed(1)
  index <- caret::createDataPartition(knn_mod_sNA$cluster, 
                                      p = 0.7, list = FALSE) 
  train <- knn_mod_sNA[index, ]
  test  <- knn_mod_sNA[-index, ]
  
  ctrl  <- caret::trainControl(method  = "cv", 
                               number  = 10)
  
  model_knn <- caret::train(cluster ~ .,
                            data   = train,
                            method = "knn",
                            preProcess = c("scale", "center"),
                            trControl  = ctrl,
                            tuneLength = 20)
  }
# modelo
model_knn
plot(model_knn)

plot_knn <- data.frame(k = model_knn$results$k,
                       `Acurácia` = model_knn$results$Accuracy,
                       Kappa = model_knn$results$Kappa)

plot_knn %>% 
  reshape2::melt(id.vars = "k") %>% 
  ggplot2::ggplot(aes(x = k, y = value)) +
  geom_line(aes(color = variable)) +
  geom_point(color = "white", size = 1.5, stroke = 2.2) +
  geom_point(aes(color = variable), size = 1.5) +
  ggplot2::ylab("Valores") +
  ggplot2::xlab("K") +
  scale_color_brewer(palette = "Paired") +
  facet_wrap(~variable, nrow = 2, scales = "free") +
  theme_classic() +
  theme(legend.position = "none")

ggsave("Cluster/knn.png", width = 20, height = 15, units = "cm")

# Modelo k = 7
{
  set.seed(1)
  grid <- expand.grid(k = 7) # set k
  index <- caret::createDataPartition(knn_mod_sNA$cluster, 
                                      p = 0.7, list = FALSE) 
  train <- knn_mod_sNA[index, ]
  test  <- knn_mod_sNA[-index, ]
  
  ctrl  <- caret::trainControl(method  = "cv", 
                               number  = 10)
  
  model_knnkx <- caret::train(cluster ~ .,
                              data   = train,
                              method = "knn",
                              preProcess = c("scale", "center"),
                              trControl  = ctrl,
                              tuneGrid = grid)
}
# modelo
model_knnkx

# Predict
final_model_knnkx <- data.frame(Agrupado = test$cluster,
                                predict(model_knnkx, 
                                        newdata = test, type = "prob"))
colx <- length(final_model_knnkx)
final_model_knnkx <- final_model_knnkx %>% 
  tibble::rowid_to_column() %>% 
  tidyr::gather(Predict_Cluster, Probabilidade, 3:colx) %>% # Verificar qtd/clusters
  dplyr::group_by(rowid) %>% 
  dplyr::slice(which.max(Probabilidade)) %>% 
  dplyr::mutate(Predict_Cluster = factor(Predict_Cluster, 
                                         levels = c("C1", "C2", "C3", 
                                                    "C4", "C5", "C6")))

cm_knn_kX <- caret::confusionMatrix(final_model_knnkx$Predict_Cluster,
                                    test$cluster)
cm_knn_kX

# Preechendo Municipios Faltantes ---

novo_df <- knn %>% 
  dplyr::filter(is.na(cluster))

novo_dfs <- novo_df %>% 
  dplyr::select(-code_muni)

# Preenchendo NA
{
  set.seed(1)
  novo_dfs_na <- data.frame(predict(model_knnkx, 
                                    newdata = novo_dfs, type = "prob"))
}

novo_dfs_na <- novo_dfs_na %>% 
  tibble::rowid_to_column() %>% 
  tidyr::gather(Predict_Cluster, Probabilidade, 2:4) %>% 
  dplyr::group_by(rowid) %>% 
  dplyr::slice(which.max(Probabilidade))

novo_dfs_na <- novo_dfs_na %>% 
  dplyr::mutate(Predict_Cluster = factor(Predict_Cluster,
                                         levels = c("C1", "C2", "C3", 
                                                    "C4", "C5", 
                                                    "C6"))) %>% 
  as.data.frame() %>% 
  dplyr::mutate(code_muni = novo_df$code_muni) %>% 
  dplyr::select(code_muni, Predict_Cluster)

# Merge com predict ----
mapa_sp <- knn %>% 
  dplyr::left_join(novo_dfs_na, by = "code_muni") %>% 
  dplyr::mutate(Cluster = ifelse(is.na(cluster), Predict_Cluster, cluster)) %>% 
  dplyr::select(code_muni, Cluster)

# Atencao
# Esta estapa, normalmente, demanda internet.

clusters_mun_completo <- geobr::read_municipality(code_muni = "SP", year = 2007) %>%
  dplyr::mutate(code_muni = substr(code_muni, 1, 6)) %>% 
  dplyr::left_join(mapa_sp, by = "code_muni") %>% 
  dplyr::select(code_muni, Cluster) %>% 
  as.data.frame() %>% 
  dplyr::select(-geom)

muni_sp2 <- geobr::read_municipality(code_muni = "SP", year = 2007) %>%
  dplyr::mutate(code_muni = substr(code_muni, 1, 6)) %>% 
  dplyr::left_join(clusters_mun_completo) %>% 
  dplyr::mutate(Cluster = glue::glue("C{Cluster}"))

# Plot Mapa ----
muni_sp2 <- muni_sp2 %>% 
  dplyr::mutate(Cluster = factor(Cluster,
                                 levels = c("C1", "C2", "C3", "C4",
                                            "C5", "C6", "C7")))

mapa2 <- muni_sp2 %>% 
  ggplot2::ggplot() +
  ggplot2::geom_sf(aes(fill = Cluster),
                   size = .05, show.legend = T, color = "white") +
  labs(subtitle = "Estado de São Paulo - Brasil", size = 8) +
  scale_fill_manual(values = wes_palette("GrandBudapest2", 7,
                                         type = "continuous"),
                    na.value = "#7F919C",
                    labels = c("C1", "C2", "C3",
                               "C4", "C5", "C6",
                               "C7")) +
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        linetype = "solid"),
        plot.background = element_rect(fill = "white")) +
  ggplot2::xlab("") + ggplot2::ylab("") + 
  north(muni_sp) +
  scalebar(muni_sp, dist = 100, dist_unit = "km", st.size = 3,
           transform = TRUE, model = "WGS84")
mapa2

ggsave("Cluster/mapa2.png", width = 20, height = 20, units = "cm")

mapa1 / mapa2

ggsave("Cluster/mapa12.png", width = 20, height = 20, units = "cm")
