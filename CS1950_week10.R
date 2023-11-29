### start figuring the predictive modeling

library(tidyverse)

file_name <- ("https://raw.githubusercontent.com/jyurko/cs_1950_brain_proj/main/KLU_APC2_Master_2021_07_07.csv?token=GHSAT0AAAAAABQQ7BMDMIVSX4QH62DGKKSOYSQJ4XQ")

df <- readr::read_csv(file_name,col_names = TRUE)

### what are the inputs????

df %>% 
  select(Vault_UID, Vault_Scan_ID,
         MR_Scan_Date, GPNID_VisitNum, PET_Scan_Date,
         APOE_CODE,
         Age_CurrentVisit,
         Sex, Race,
         Education)

### PIB will be the output and GMD, FDG will be inputs

### so to start use the person averaged GMD, the person averaged FDG
### to predict the person averaged PIB

### get the GMD regional values per scan per person
gmd_regional_df <- df %>% select(Vault_Scan_ID, Vault_UID, MR_Scan_Date, starts_with("GMD_"))

gmd_regional_wf <- gmd_regional_df %>% 
  select(starts_with("Vault"), MR_Scan_Date, starts_with("GMD_regional"))

### helper function
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

### GMD long format values
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

### calcualte the GMD summarized values per region per person
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

### focus first on the AVERAGE values per region per person
gmd_avg_person_wf <- gmd_person_summary %>% 
  select(Vault_UID, avg_value, result_type, region_name, side_name) %>% 
  tidyr::unite(result_region_name,
               c("result_type", "region_name", "side_name"),
               sep = "-") %>% 
  pivot_wider(id_cols = "Vault_UID", 
              names_from = "result_region_name", 
              values_from = "avg_value")

gmd_avg_person_wf %>% names()

### redo this for the FDG
fdg_regional_df <- df %>% select(Vault_Scan_ID, Vault_UID, PET_Scan_Date, starts_with("FDG_"))

fdg_regional_df %>% dim()

fdg_regional_wf <- fdg_regional_df %>% 
  select(starts_with("Vault"), PET_Scan_Date, starts_with("FDG_regional"))

fdg_regional_wf %>% dim()

fdg_regional_lf <- fdg_regional_wf %>% 
  tibble::rowid_to_column() %>% 
  pivot_longer(cols = starts_with("FDG_regional")) %>% 
  mutate(num_underscores = stringr::str_count(name, pattern = "_")) %>% 
  group_by(num_underscores) %>% 
  tidyr::nest() %>% 
  mutate(ready_data = purrr::map2(data, num_underscores, my_separate)) %>% 
  select(num_underscores, ready_data) %>% 
  tidyr::unnest(ready_data) %>% 
  ungroup()

fdg_regional_lf %>% head()

### calcualte the FDG summarized values per region per person
fdg_person_summary <- fdg_regional_lf %>% 
  group_by(result_type, Vault_UID, region_name, side_name) %>% 
  summarise(num_rows = n(),
            num_scans = n_distinct(Vault_Scan_ID),
            num_dates = n_distinct(PET_Scan_Date),
            num_missing = sum(is.na(value)),
            avg_value = mean(value, na.rm = TRUE),
            min_value = min(value, na.rm = TRUE),
            max_value = max(value, na.rm = TRUE),
            .groups = 'drop')

### focus first on the AVERAGE values per region per person
fdg_avg_person_wf <- fdg_person_summary %>% 
  select(Vault_UID, avg_value, result_type, region_name, side_name) %>% 
  tidyr::unite(result_region_name,
               c("result_type", "region_name", "side_name"),
               sep = "-") %>% 
  pivot_wider(id_cols = "Vault_UID", 
              names_from = "result_region_name", 
              values_from = "avg_value")

fdg_avg_person_wf %>% names()

### redo with PIB
pib_regional_df <- df %>% select(Vault_Scan_ID, Vault_UID, PET_Scan_Date, starts_with("PiB_"))

pib_regional_df %>% dim()

pib_regional_wf <- pib_regional_df %>% 
  select(starts_with("Vault"), PET_Scan_Date, starts_with("PiB_regional"))

pib_regional_wf %>% dim()

pib_regional_lf <- pib_regional_wf %>% 
  tibble::rowid_to_column() %>% 
  pivot_longer(cols = starts_with("PiB_regional")) %>% 
  mutate(num_underscores = stringr::str_count(name, pattern = "_")) %>% 
  group_by(num_underscores) %>% 
  tidyr::nest() %>% 
  mutate(ready_data = purrr::map2(data, num_underscores, my_separate)) %>% 
  select(num_underscores, ready_data) %>% 
  tidyr::unnest(ready_data) %>% 
  ungroup()

pib_regional_lf %>% head()

### calcualte the PIB summarized values per region per person
pib_person_summary <- pib_regional_lf %>% 
  group_by(result_type, Vault_UID, region_name, side_name) %>% 
  summarise(num_rows = n(),
            num_scans = n_distinct(Vault_Scan_ID),
            num_dates = n_distinct(PET_Scan_Date),
            num_missing = sum(is.na(value)),
            avg_value = mean(value, na.rm = TRUE),
            min_value = min(value, na.rm = TRUE),
            max_value = max(value, na.rm = TRUE),
            .groups = 'drop')

### focus first on the AVERAGE values per region per person
pib_avg_person_wf <- pib_person_summary %>% 
  select(Vault_UID, avg_value, result_type, region_name, side_name) %>% 
  tidyr::unite(result_region_name,
               c("result_type", "region_name", "side_name"),
               sep = "-") %>% 
  pivot_wider(id_cols = "Vault_UID", 
              names_from = "result_region_name", 
              values_from = "avg_value")

pib_avg_person_wf %>% names()

### we need to join the summarized averaged values per region per person
person_all_df <- pib_avg_person_wf %>% 
  left_join(gmd_avg_person_wf,
            by = 'Vault_UID') %>% 
  left_join(fdg_avg_person_wf,
            by = 'Vault_UID')

person_all_df %>% 
  names()

library(visdat)

visdat::vis_miss(person_all_df) +
  theme_bw()

### calculate the total number of missing items per person
person_all_df %>% 
  pivot_longer(!c("Vault_UID")) %>% 
  group_by(Vault_UID) %>% 
  summarise(num_rows = n(),
            num_missing_values = sum(is.na(value)),
            .groups = 'drop') %>% 
  arrange(desc(num_missing_values))

### for simplicity just keep complete cases
person_ready <- person_all_df %>% 
  drop_na()

person_ready %>% visdat::vis_miss() +
  theme_bw()

person_ready %>% nrow()

person_ready %>% ncol()

### check the correlatioin structure 
person_ready %>% 
  select(starts_with("GMD")) %>% 
  as.data.frame() %>% 
  cor() %>% 
  corrplot::corrplot(method = 'square', type = 'upper',
                     order = 'hclust', hclust.method = 'ward.D2')

person_ready %>% 
  select(starts_with("FDG")) %>% 
  as.data.frame() %>% 
  cor() %>% 
  corrplot::corrplot(method = 'square', type = 'upper',
                     order = 'hclust', hclust.method = 'ward.D2')

person_ready %>% 
  select(starts_with("PiB")) %>% 
  as.data.frame() %>% 
  cor() %>% 
  corrplot::corrplot(method = 'square', type = 'upper',
                     order = 'hclust', hclust.method = 'ward.D2')

### correlation plot of everything
person_ready %>% 
  select(-Vault_UID) %>% 
  as.data.frame() %>% 
  cor() %>% 
  corrplot::corrplot(method = 'square', type = 'upper', tl.pos = 'n')

### save this data 
person_ready %>% 
  readr::write_csv("predictive/person_ready.csv",
                   col_names = TRUE)

# PCA multi ouput 
my_data <- ("https://raw.githubusercontent.com/jyurko/cs_1950_brain_proj/main/predictive/person_ready.csv?token=GHSAT0AAAAAABQQ7BMDDRQ5QZLNRFTHWZQ2YSQLEYQ")
new_df <- readr::read_csv(my_data,col_names = TRUE)

new_df %>% glimpse()
dim(new_df)
names(new_df)

new_wf <- new_df %>% 
  select(-Vault_UID) %>% 
  drop_na()
new_wf %>% dim()

# separate data into FDG and GMD 

# change format of data to have column names: FDG, GMD, PiB
Pib_average <-new_df %>% 
  select(starts_with("PiB")) %>% as.matrix()

Pib_average %>% dim() %>% as.matrix()

Gmd_average <-new_df %>% 
  select(starts_with("GMD")) %>% as.matrix()

Gmd_average %>% dim()

Fdg_average <-new_df %>% 
  select(starts_with("FDG")) %>% as.matrix()

Fdg_average %>% dim()

### pca on the multi input 
# princomp() or prcomp() 
fdg_gmd_pca <- prcomp(~Fdg_average+Gmd_average, scale. = TRUE)

# pca on output 
pib_pca <- prcomp(Pib_average, scale = TRUE)

library(factoextra)

factoextra::fviz_screeplot(fdg_gmd_pca)

factoextra::get_eigenvalue(fdg_gmd_pca) %>% head(20)

factoextra::fviz_screeplot(fdg_gmd_pca, ncp = 15)

factoextra::fviz_contrib(fdg_gmd_pca, choice = 'var', axis = 1)

factoextra::fviz_pca_biplot(fdg_gmd_pca)

# fdg_gmd_pca information 

# 85 pc 
summary(fdg_gmd_pca)

# extract contributions 

(factoextra::get_pca(fdg_gmd_pca))$contrib %>% as.data.frame() %>% 
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


# Visualize Fdg_gmd_pca 
### read in the name maps
name_maps <- readxl::read_excel("Naming_convention.xlsx",
                                col_names = TRUE)

name_maps %>% glimpse()

# how to do the join to visualize?
(factoextra::get_pca(fdg_gmd_pca))$contrib %>% as.data.frame() %>% 
  tibble::rownames_to_column() %>% 
  tibble::as_tibble() %>% 
  pivot_longer(!c("rowname")) %>% 
  left_join(name_maps,
            by = c("rowname" = "new_wf"))


### pca on the two groups on output/input (PiB and FDG)
Pib_fdg_pca <- prcomp(~Pib_average+Fdg_average, scale = TRUE)
factoextra::fviz_screeplot(Pib_fdg_pca)

factoextra::get_eigenvalue(Pib_fdg_pca) %>% head(20)

factoextra::fviz_screeplot(Pib_fdg_pca, ncp = 15)

factoextra::fviz_contrib(Pib_fdg_pca, choice = 'var', axis = 1)

factoextra::fviz_pca_biplot(Pib_fdg_pca)


# extract contributions 

(factoextra::get_pca(Pib_fdg_pca))$contrib %>% as.data.frame() %>% 
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

### fit models to predict each output derived PC score based on the
### input derived PC scores

# data (pred_average_person)
# Split into PiB, FDG, and GMD 

# conduct PCA on training dataset (FDG and GMD)
# fdg_gmd_pca 

# percent explained variance
expl.var <- round(fdg_gmd_pca$sdev^2/sum(fdg_gmd_pca$sdev^2)*100) 

# prediction of PCs for validation dataset
# should validation set be piB_pc?
pred <- predict(fdg_gmd_pca, newdata = Pib_average)

###Plot result

COLOR <- c(2:4)
PCH <- c(1,16)

# principal components to plot
pc <- c(1,2) 

png("pca_pred.png", units="in", width=5, height=4, res=200)
op <- par(mar=c(4,4,1,1), ps=10)
plot(pca$x[,pc], col=COLOR[fdg_gmd_pca$center], cex=PCH[1], 
     xlab=paste0("PC ", pc[1], " (", expl.var[pc[1]], "%)"), 
     ylab=paste0("PC ", pc[2], " (", expl.var[pc[2]], "%)")
)
#points(pred[,pc], col=COLOR[pib_pca$center], pc=PCH[2])
#legend("topright", legend=levels(new_df$grouping), fill = COLOR, border=COLOR)
#legend("topleft", legend=c("training data", "validation data"), col=1, pch=PCH)
#par(op)
#dev.off()

### tune the number of PCs for the output side
### tune the number of PCs for each input side