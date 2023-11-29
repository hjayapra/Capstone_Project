### pca 

library(tidyverse)

file_name <- ("https://raw.githubusercontent.com/jyurko/cs_1950_brain_proj/main/KLU_APC2_Master_2021_07_07.csv?token=GHSAT0AAAAAABQQ7BMCKZKV7HDO5MEW2KUQYSDBPXQ")

df <- readr::read_csv(file_name,col_names = TRUE)

gmd_regional_df <- df %>% select(Vault_Scan_ID, Vault_UID, MR_Scan_Date, starts_with("GMD_"))

gmd_regional_wf <- gmd_regional_df %>% 
  select(starts_with("Vault"), MR_Scan_Date, starts_with("GMD_regional"))

my_separate <- function(the_data, num_us)
{
  names_keep <- the_data %>% select(-name) %>% names()
  
  the_data %>% 
    tidyr::separate(name,
                    c("result_type", "regional_word",
                      sprintf("name_%02d", 1:(num_us-1))),
                    sep = "_") %>% 
    rename(!!!c("side_name" = sprintf("name_%02d", num_us-1))) %>% 
    tidyr::unite("region_name",
                 sprintf("name_%02d", 1:(num_us-2)),
                 sep = "_") %>% 
    select(all_of(c(names_keep,
                    "result_type", "region_name", "side_name")))
}


gmd_regional_lf <- gmd_regional_wf %>% 
  tibble::rowid_to_column() %>% 
  pivot_longer(cols = starts_with("GMD_regional")) %>% 
  mutate(num_underscores = stringr::str_count(name, pattern = "_")) %>% 
  group_by(num_underscores) %>% 
  tidyr::nest() %>% 
  mutate(ready_data = purrr::map2(data, num_underscores, my_separate)) %>% 
  select(num_underscores, ready_data) %>% 
  tidyr::unnest(ready_data) %>% 
  ungroup()

gmd_person_summary <- gmd_regional_lf %>% 
  group_by(result_type, Vault_UID, region_name, side_name) %>% 
  summarise(num_rows = n(),
            num_scans = n_distinct(Vault_Scan_ID),
            num_dates = n_distinct(MR_Scan_Date),
            num_missing = sum(is.na(value)),
            avg_value = mean(value, na.rm = TRUE),
            min_value = min(value, na.rm = TRUE),
            max_value = max(value, na.rm = TRUE),
            .groups = 'drop')

gmd_avg_person_wf <- gmd_person_summary %>% 
  select(Vault_UID, avg_value, result_type, region_name, side_name) %>% 
  tidyr::unite(result_region_name,
               c("result_type", "region_name", "side_name"),
               sep = "-") %>% 
  pivot_wider(id_cols = "Vault_UID", names_from = "result_region_name", values_from = "avg_value")

gmd_avg_person_wf %>% 
  select(-Vault_UID) %>% 
  drop_na() %>% 
  as.data.frame() %>% 
  cor() %>% 
  corrplot::corrplot(method = 'square', tl.pos = 'n')

### execute PCA

ready_wf <- gmd_avg_person_wf %>% 
  select(-Vault_UID) %>% 
  drop_na()

ready_wf %>% dim()

gmd_pca <- prcomp(ready_wf, scale. = TRUE)

library(factoextra)

factoextra::fviz_screeplot(gmd_pca, ncp = 15)

### extract out the contributions
(factoextra::get_pca(gmd_pca))$contrib %>% as.data.frame() %>% 
  tibble::rownames_to_column() %>% 
  tibble::as_tibble() %>% 
  pivot_longer(!c("rowname")) %>% 
  mutate(pc_id = as.numeric(stringr::str_extract(name, "\\d+"))) %>% 
  ggplot(mapping = aes(x = as.factor(pc_id), y = rowname)) +
  geom_tile(mapping = aes(fill = value > 100 * (1 / length(gmd_pca$center)),
                          group = interaction(pc_id, rowname)),
            color = 'black') +
  scale_fill_manual("Variable actively contributes to PC?",
                    values = c("TRUE" = "darkred",
                               "FALSE" = "grey70")) +
  labs(y = '', x = 'PC') +
  theme_bw() +
  theme(legend.position = "top")

### focus on the first 7 PCs
(factoextra::get_pca(gmd_pca))$contrib %>% as.data.frame() %>% 
  tibble::rownames_to_column() %>% 
  tibble::as_tibble() %>% 
  pivot_longer(!c("rowname")) %>% 
  mutate(pc_id = as.numeric(stringr::str_extract(name, "\\d+"))) %>% 
  filter(pc_id < 8) %>% 
  ggplot(mapping = aes(x = as.factor(pc_id), y = rowname)) +
  geom_tile(mapping = aes(fill = value > 100 * (1 / length(gmd_pca$center)),
                          group = interaction(pc_id, rowname)),
            color = 'black') +
  scale_fill_manual("Variable actively contributes to PC?",
                    values = c("TRUE" = "darkred",
                               "FALSE" = "grey70")) +
  labs(y = '', x = 'PC') +
  theme_bw() +
  theme(legend.position = "top")

### separate region name
(factoextra::get_pca(gmd_pca))$contrib %>% as.data.frame() %>% 
  tibble::rownames_to_column() %>% 
  tibble::as_tibble() %>% 
  pivot_longer(!c("rowname")) %>% 
  mutate(pc_id = as.numeric(stringr::str_extract(name, "\\d+"))) %>% 
  filter(pc_id < 8) %>% 
  tidyr::separate(rowname,
                  c("output_name", "region_name", "symmetry_name"),
                  sep = "-",
                  remove = FALSE) %>% 
  ggplot(mapping = aes(x = as.factor(pc_id), y = region_name)) +
  geom_tile(mapping = aes(fill = value > 100 * (1 / length(gmd_pca$center)),
                          group = interaction(pc_id, rowname)),
            color = 'black') +
  facet_wrap(~symmetry_name) +
  scale_fill_manual("Variable actively contributes to PC?",
                    values = c("TRUE" = "darkred",
                               "FALSE" = "grey70")) +
  labs(y = '', x = 'PC') +
  theme_bw() +
  theme(legend.position = "top")

### alternative is to cluster the regions 

### corrplot clusters original variables through a correlation based
### distance metric
gmd_avg_person_wf %>% 
  select(-Vault_UID) %>% 
  drop_na() %>% 
  as.data.frame() %>% 
  cor() %>% 
  corrplot::corrplot(method = 'square', tl.pos = 'n',
                     order = 'hclust', hclust.method = 'ward.D2')

### calculate the correlation based distance metric
cor_matrix <- ready_wf %>% as.data.frame() %>% cor()

cor_dist <- as.dist((1 - cor_matrix)/2)

hclust_regions <- hclust(cor_dist, method = 'ward.D2')

plot(hclust_regions)

macro_groupings <- cutree(hclust_regions, k = 3)

sub_groupings <- cutree(hclust_regions, k = 5)

macro_groupings

### identify the clustered regions
region_clusters <- tibble::tibble(
  region_name = names(macro_groupings),
  macro_group = as.vector(macro_groupings),
  sub_group = as.vector(sub_groupings)
)

region_clusters

### separate the region names
region_clusters %>% 
  tidyr::separate(region_name,
                  c("output_name", "region", "symmetry_name"),
                  sep = '-')

### bring in ggseg
library(ggseg)

?geom_brain

plot(dk)

plot(aseg)

region_clusters %>% 
  tidyr::separate(region_name,
                  c("output_name", "region", "symmetry_name"),
                  sep = '-') %>% 
  distinct(region) %>% 
  pull()

### available regions in the dk atlas
dk %>% glimpse()

dk$data$region

dk$data %>% tibble::as_tibble() %>% 
  count(side, hemi)

### the full name analogous to ours
dk$data %>% tibble::as_tibble() %>% 
  tidyr::unite(our_name_style,
               c("region", "hemi"),
               sep = "-",
               remove = FALSE) %>% 
  select(hemi, side, region, label, our_name_style)

dk$data %>% tibble::as_tibble() %>% 
  tidyr::unite(our_name_style,
               c("region", "hemi"),
               sep = "-",
               remove = FALSE) %>% 
  select(hemi, side, region, label, our_name_style) %>% 
  distinct(region) %>% 
  pull()


### install ggsegHO
# remotes::install_github("LCBC-UiO/ggsegHO")

library(ggsegHO)

plot(hoCort) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 6)) +
  guides(fill = guide_legend(ncol = 3))

hoCort %>% glimpse()

### regions in the ggsegHO
hoCort$data$region

### cluster the people by the PCs

pc_scores <- gmd_pca$x %>% as.data.frame() %>% select(1:7)

pc_scores %>% 
  tibble::rowid_to_column() %>% 
  pivot_longer(starts_with("PC")) %>% 
  ggplot(mapping = aes(x = value)) +
  geom_histogram(bins = 13) +
  facet_wrap(~name,scales = 'free') +
  theme_bw()

pc_scores %>% 
  tibble::rowid_to_column() %>% 
  pivot_longer(starts_with("PC")) %>% 
  ggplot(mapping = aes(x = name, y = value)) +
  geom_boxplot() +
  theme_bw()

### cluster observations by PC scores
hclust_rows <- hclust(dist(pc_scores %>% as.data.frame()), method = 'ward.D2')

plot(hclust_rows)

pc_scores %>% 
  mutate(main_group = cutree(hclust_rows, k = 2),
         sub_group = cutree(hclust_rows, k = 4)) %>% 
  ggplot(mapping = aes(x = PC1, y = PC2)) +
  geom_point(mapping = aes(color = as.factor(sub_group),
                           shape = as.factor(main_group)),
             size = 3) +
  ggthemes::scale_color_calc('Sub group') +
  scale_shape_discrete('Main group') +
  theme_bw()

pc_scores %>% 
  tibble::rowid_to_column() %>% 
  mutate(main_group = cutree(hclust_rows, k = 2),
         sub_group = cutree(hclust_rows, k = 4)) %>% 
  ggplot(mapping = aes(x = PC1, y = PC2)) +
  geom_point(mapping = aes(color = as.factor(sub_group),
                           shape = as.factor(main_group)),
             size = 3) +
  geom_text(mapping = aes(label = rowid),
            size = 9, check_overlap = TRUE) +
  ggthemes::scale_color_calc('Sub group') +
  scale_shape_discrete('Main group') +
  theme_bw()

gmd_avg_person_wf %>% glimpse()

gmd_avg_person_wf

name_maps <- readxl::read_excel('Naming_convention.xlsx')
my_data <-name_maps %>% left_join(factoextra::get_pca(gmd_pca)$contrib %>% as.data.frame() %>% 
                          tibble::rownames_to_column() %>% 
                          tibble::as_tibble(), by= character())


my_data %>%
  group_by(rowname) %>%
  ggplot() +
  geom_brain(atlas = dk, 
             position = position_brain(hemi ~ side),
             aes(fill = values)) 
