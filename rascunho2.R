df_cluster <- df %>% 
  filter(estado == "São Paulo") %>% 
  select(c(1, 2,  21, 22, 26:57, 59, 62, 63, 
           65:69, 71, 74, 76, 78:83, 85:91, 
           163:166, 169, 171, 172, 175, 176, 182, 
           161, 162, 163, 164, 168, 169, 170, 171, 
           175, 176, 183, 184, 188))
#

nomes1 <- names(df_cluster)

teste <- df_cluster %>% 
  dplyr::distinct(`Código do Município`, .keep_all = TRUE) %>% 
  summarize_all(funs(sum(is.na(.)) / length(.))) %>% 
  t %>% as.data.frame() %>%  tibble::rownames_to_column() %>% 
  dplyr::arrange(V1) %>% 
  dplyr::top_n(17)


df_cluster <- df_cluster %>% 
  select(-c(3,4, 52)) %>% 
  select(-c(6,8, 10, 12, 14, 17, 19, 
            34, 36, 39, 41)) %>% 
  select(-c(24,26)) %>% 
  select(-c(15)) %>% 
  select(-16) %>% 
  select(-17)
nomes2 <- names(df_cluster)

df_cluster_filter <- df_cluster %>% 
  filter(if_all(3:length(.), ~!is.na(.))) %>%
  distinct(`Código do Município`, .keep_all = TRUE) 

contagem_na <- df_cluster %>%
  summarise(across(everything(), ~ sum(is.na(.))))

l1 <- match(nomes1, nomes2)
l2 <- is.na(l1)
nomes1[l2]
nrow(df_cluster)

# NA
# gg_miss_var(df_cluster)
NA_c_df <- as.data.frame(colMeans(is.na(df_cluster)))
n_NA_c_df <- ncol(df_cluster)


n_c_df <- ncol(df_cluster)
nn <- n_NA_c_df-n_c_df

NA_c_df <- NA_c_df %>% 
  tibble::rownames_to_column() %>% 
  dplyr::arrange(2) %>% 
  dplyr::top_n(nn)


