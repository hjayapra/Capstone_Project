library(tidyverse)

file_name <- ("https://raw.githubusercontent.com/jyurko/cs_1950_brain_proj/main/KLU_APC2_Master_2021_07_07.csv?token=GHSAT0AAAAAABQQ7BMDMIVSX4QH62DGKKSOYSQJ4XQ")

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

ready_wf %>% names()

gmd_pca <- prcomp(ready_wf, scale. = TRUE)

library(factoextra)

factoextra::get_eigenvalue(gmd_pca) %>% head(20)

factoextra::fviz_screeplot(gmd_pca, ncp = 15)

factoextra::fviz_contrib(gmd_pca, choice = 'var', axis = 1)

factoextra::fviz_pca_biplot(gmd_pca)

factoextra::fviz_pca_biplot(gmd_pca, axes = c(2, 3))

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

### read in the name maps
name_maps <- readxl::read_excel("clustering/naming_convention_for_ggseg.xlsx",
                                col_names = TRUE)

name_maps %>% glimpse()

(factoextra::get_pca(gmd_pca))$contrib %>% as.data.frame() %>% 
  tibble::rownames_to_column() %>% 
  tibble::as_tibble() %>% 
  pivot_longer(!c("rowname")) %>% 
  left_join(name_maps,
            by = c("rowname" = "Gmd_avg_wf"))

### just check the regions with the ggseg names
(factoextra::get_pca(gmd_pca))$contrib %>% as.data.frame() %>% 
  tibble::rownames_to_column() %>% 
  tibble::as_tibble() %>% 
  pivot_longer(!c("rowname")) %>% 
  left_join(name_maps,
            by = c("rowname" = "Gmd_avg_wf")) %>% 
  filter(!is.na(ggsegHO))

### now merge with the brain atlas
library(ggseg)

library(ggsegHO)

hoCort$data$region

hoCort %>% glimpse()

plot(hoCort) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 6)) +
  guides(fill = guide_legend(ncol = 3))

### show all regions active with PC1
(factoextra::get_pca(gmd_pca))$contrib %>% as.data.frame() %>% 
  tibble::rownames_to_column() %>% 
  tibble::as_tibble() %>% 
  pivot_longer(!c("rowname")) %>% 
  left_join(name_maps,
            by = c("rowname" = "Gmd_avg_wf")) %>% 
  mutate(pc_id = as.numeric(stringr::str_extract(name, "\\d+"))) %>% 
  filter(pc_id == 1) %>% 
  mutate(active = ifelse(value > 100 * (1 / length(gmd_pca$center)),
                         "ACTIVE",
                         "NOT-ACTIVE"))

### put the active/inactive legend in the plot
(factoextra::get_pca(gmd_pca))$contrib %>% as.data.frame() %>% 
  tibble::rownames_to_column() %>% 
  tibble::as_tibble() %>% 
  pivot_longer(!c("rowname")) %>% 
  left_join(name_maps,
            by = c("rowname" = "Gmd_avg_wf")) %>% 
  mutate(pc_id = as.numeric(stringr::str_extract(name, "\\d+"))) %>% 
  mutate(pc_word = sprintf("PC%02d", pc_id)) %>% 
  mutate(active = ifelse(value > 100 * (1 / length(gmd_pca$center)),
                         "YES",
                         "NO")) %>% 
  filter(pc_id == 1) %>%
  rename(region = ggsegHO) %>% 
  group_by(pc_word) %>%
  ggplot() +
  geom_brain(atlas = hoCort, 
             position = position_brain(hemi ~ side),
             aes(fill = active)) +
  facet_wrap(~pc_word) +
  scale_fill_brewer("Region actively contributes to PC?",
                    palette = 'Set1',
                    direction = -1,
                    na.value = 'grey80') +
  theme_bw() +
  theme(legend.position = 'top',
        axis.text = element_blank())

### can use more than just PC1 though!
(factoextra::get_pca(gmd_pca))$contrib %>% as.data.frame() %>% 
  tibble::rownames_to_column() %>% 
  tibble::as_tibble() %>% 
  pivot_longer(!c("rowname")) %>% 
  left_join(name_maps,
            by = c("rowname" = "Gmd_avg_wf")) %>% 
  mutate(pc_id = as.numeric(stringr::str_extract(name, "\\d+"))) %>% 
  mutate(pc_word = sprintf("PC%02d", pc_id)) %>% 
  mutate(active = ifelse(value > 100 * (1 / length(gmd_pca$center)),
                         "YES",
                         "NO")) %>% 
  filter(pc_id < 5) %>%
  rename(region = ggsegHO) %>% 
  group_by(pc_word) %>%
  ggplot() +
  geom_brain(atlas = hoCort, 
             position = position_brain(hemi ~ side),
             aes(fill = active)) +
  facet_wrap(~pc_word) +
  scale_fill_brewer("Region actively contributes to PC?",
                    palette = 'Set1',
                    direction = -1,
                    na.value = 'grey80') +
  theme_bw() +
  theme(legend.position = 'top',
        axis.text = element_blank())

### look at more pcs
(factoextra::get_pca(gmd_pca))$contrib %>% as.data.frame() %>% 
  tibble::rownames_to_column() %>% 
  tibble::as_tibble() %>% 
  pivot_longer(!c("rowname")) %>% 
  left_join(name_maps,
            by = c("rowname" = "Gmd_avg_wf")) %>% 
  mutate(pc_id = as.numeric(stringr::str_extract(name, "\\d+"))) %>% 
  mutate(pc_word = sprintf("PC%02d", pc_id)) %>% 
  mutate(active = ifelse(value > 100 * (1 / length(gmd_pca$center)),
                         "YES",
                         "NO")) %>% 
  filter(pc_id < 10) %>%
  rename(region = ggsegHO) %>% 
  group_by(pc_word) %>%
  ggplot() +
  geom_brain(atlas = hoCort, 
             position = position_brain(hemi ~ side),
             aes(fill = active)) +
  facet_wrap(~pc_word) +
  scale_fill_brewer("Region actively contributes to PC?",
                    palette = 'Set1',
                    direction = -1,
                    na.value = 'grey80') +
  theme_bw() +
  theme(legend.position = 'top',
        axis.text = element_blank())

### cluster the regions together based on correlation distance

cor_matrix <- ready_wf %>% as.data.frame() %>% cor()

cor_dist <- as.dist((1 - cor_matrix)/2)

hclust_regions <- hclust(cor_dist, method = 'ward.D2')

plot(hclust_regions)

macro_groupings <- cutree(hclust_regions, k = 3)

sub_groupings <- cutree(hclust_regions, k = 5)

### identify the clustered regions
region_clusters <- tibble::tibble(
  region_name = names(macro_groupings),
  macro_group = as.vector(macro_groupings),
  sub_group = as.vector(sub_groupings)
)

### plot the macro regions on an atlas
region_clusters %>% 
  left_join(name_maps,
            by = c("region_name" = "Gmd_avg_wf")) %>% 
  rename(region = ggsegHO) %>% 
  ggplot() +
  geom_brain(atlas = hoCort, 
             position = position_brain(hemi ~ side),
             aes(fill = as.factor(macro_group))) +
  ggthemes::scale_fill_colorblind("Macro cluster group",
                                  na.value = 'grey') +
  theme_bw() +
  theme(legend.position = 'top',
        axis.text = element_blank())

### plot the sub clusters
region_clusters %>% 
  left_join(name_maps,
            by = c("region_name" = "Gmd_avg_wf")) %>% 
  rename(region = ggsegHO) %>% 
  ggplot() +
  geom_brain(atlas = hoCort, 
             position = position_brain(hemi ~ side),
             aes(fill = as.factor(sub_group))) +
  ggthemes::scale_fill_calc("Macro cluster group",
                            na.value = 'grey') +
  theme_bw() +
  theme(legend.position = 'top',
        axis.text = element_blank())


