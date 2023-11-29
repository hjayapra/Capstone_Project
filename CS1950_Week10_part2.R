library(tidyverse)

# PCA multi ouput 
my_data <- ("https://raw.githubusercontent.com/jyurko/cs_1950_brain_proj/main/predictive/person_ready.csv?token=GHSAT0AAAAAABQQ7BMDDRQ5QZLNRFTHWZQ2YSQLEYQ")
new_df <- readr::read_csv(my_data,col_names = TRUE)

new_df %>% glimpse()
dim(new_df)
names(new_df)

new_b <- new_df %>% 
  select(-Vault_UID) %>% 
  drop_na()
new_b %>% dim()


pib<- new_b %>% select(starts_with("PiB"))
fdg <- new_b %>% select(starts_with("FDG"))
gmd <- new_b %>% select(starts_with("GMD"))

# Create output features 
pca_pib <- prcomp(pib, scale. = TRUE)
factoextra::get_eigenvalue(pca_pib) %>% head()

# Create input features 
pca_fdg <- prcomp(fdg, scale. = TRUE)
factoextra::get_eigenvalue(pca_fdg) %>% head()

pca_gmd <- prcomp(gmd, scale. = TRUE)
factoextra::get_eigenvalue(pca_gmd) %>% head()

# Organize pc scores 

output_scores <- pca_pib$x %>% as.data.frame() %>% 
  purrr::set_names(sprintf("pib_%02d", 1:ncol(pca_pib$x))) %>% 
  select(1:5)
output_scores %>% head()

fdg_scores <- pca_fdg$x %>% as.data.frame() %>% 
  purrr::set_names(sprintf("fdg_%02d", 1:ncol(pca_fdg$x))) %>% 
  select(1:5)
fdg_scores %>% head()

gmd_scores <- pca_gmd$x %>% as.data.frame() %>% 
  purrr::set_names(sprintf("gmd_%02d", 1:ncol(pca_gmd$x))) %>% 
  select(1:5)
gmd_scores %>% head()

input_scores <- bind_cols(fdg_scores, gmd_scores)


all_scores <- bind_cols(output_scores, input_scores)

all_scores %>% 
  tibble::rowid_to_column() %>% 
  pivot_longer(starts_with("pib"), 
               names_to = "output_name", 
               values_to = "output_value") %>%
  pivot_longer(!c("rowid", "output_name", "output_value")) %>%
  ggplot(mapping = aes(x = value, y = output_value)) + 
  geom_point() + 
  facet_grid(output_name ~ name, scales = "free")

train_ready <- all_scores %>% scale() %>% 
  as.data.frame() %>% tibble::as_tibble() 

# model output PC1 

train_01 <- train_ready %>% select(pib_01, starts_with("fdg"), starts_with("gmd"))

mod01_a <- lm(pib_01 ~ ., data = train_01)

mod01_b <- lm(pib_01 ~ .^2, data = train_01)

coefplot::coefplot(mod01_b)

train_01_input_vars <- train_01 %>% select(-pib_01) %>% 
  names()

rsm_formula <- paste(sprintf('%s ~ .^2+', 'pib_01'),
                     paste(sprintf('I(%s^2)', train_01_input_vars), collapse = "+"),
                     collapse = '+')

mod01_c <- lm(as.formula(rsm_formula), data = train_01)

coefplot::coefplot(mod01_c)
