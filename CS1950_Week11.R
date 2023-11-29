library(tidyverse)

# PCA multi ouput 
my_data <- ("https://raw.githubusercontent.com/jyurko/cs_1950_brain_proj/main/predictive/person_ready.csv?token=GHSAT0AAAAAABQQ7BMCEYABCRJ4QJYUXOKYYSZTT4Q")
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

####Pib02

train_02 <- train_ready %>% select(pib_02, starts_with("fdg"), starts_with("gmd"))
mod02_a <- lm(pib_02 ~ ., data = train_02)

mod02_b <- lm(pib_02 ~ .^2, data = train_02)
coefplot::coefplot(mod02_b)

train_02_input_vars <- train_02 %>% select(-pib_02) %>% 
  names()

rsm_formula <- paste(sprintf('%s ~ .^2+', 'pib_02'),
                     paste(sprintf('I(%s^2)', train_02_input_vars), collapse = "+"),
                     collapse = '+')

mod02_c <- lm(as.formula(rsm_formula), data = train_02)

coefplot::coefplot(mod02_c)


### Pib03

train_03 <- train_ready %>% select(pib_03, starts_with("fdg"), starts_with("gmd"))
mod03_a <- lm(pib_03 ~ ., data = train_03)

mod03_b <- lm(pib_03 ~ .^2, data = train_03)
coefplot::coefplot(mod03_b)

train_03_input_vars <- train_03 %>% select(-pib_03) %>% 
  names()

rsm_formula <- paste(sprintf('%s ~ .^2+', 'pib_03'),
                     paste(sprintf('I(%s^2)', train_03_input_vars), collapse = "+"),
                     collapse = '+')

mod03_c <- lm(as.formula(rsm_formula), data = train_03)

coefplot::coefplot(mod03_c)

###Pib04

train_04 <- train_ready %>% select(pib_04, starts_with("fdg"), starts_with("gmd"))
mod04_a <- lm(pib_04 ~ ., data = train_04)

mod04_b <- lm(pib_04 ~ .^2, data = train_04)
coefplot::coefplot(mod04_b)

train_04_input_vars <- train_04 %>% select(-pib_04) %>% 
  names()

rsm_formula <- paste(sprintf('%s ~ .^2+', 'pib_04'),
                     paste(sprintf('I(%s^2)', train_04_input_vars), collapse = "+"),
                     collapse = '+')

mod04_c <- lm(as.formula(rsm_formula), data = train_04)

coefplot::coefplot(mod04_c)

### Pib05 

train_05 <- train_ready %>% select(pib_05, starts_with("fdg"), starts_with("gmd"))
mod05_a <- lm(pib_05 ~ ., data = train_05)

mod05_b <- lm(pib_05 ~ .^2, data = train_05)
coefplot::coefplot(mod05_b)

train_05_input_vars <- train_05 %>% select(-pib_05) %>% 
  names()

rsm_formula <- paste(sprintf('%s ~ .^2+', 'pib_05'),
                     paste(sprintf('I(%s^2)', train_05_input_vars), collapse = "+"),
                     collapse = '+')

mod05_c <- lm(as.formula(rsm_formula), data = train_05)

coefplot::coefplot(mod05_c)

#####Performance metrics: AIC/BIC 

# Best model using BIC : mod01_a 
AIC_mod01 <- AIC(mod01_a, mod01_b, mod01_c)
# AIC --> mod01_c
BIC_mod01 <- BIC(mod01_a, mod01_b, mod01_c)
# BIC --> mod01_a 


# mod02_a is the best model 
AIC_mod02 <- AIC(mod02_a, mod02_b, mod02_c)
# AIC --> mod02_a
BIC_mod02 <- BIC(mod02_a, mod02_b, mod02_c)
# BIC --> mod02_a 

# mod03_a is the best model 
AIC_mod03 <- AIC(mod03_a, mod03_b, mod03_c)
# AIC -->  mod03_a 
BIC_mod03 <- BIC(mod03_a, mod03_b, mod03_c)
# BIC --> mod03_a 

# mod04_a is the best model 
AIC_mod04 <- AIC(mod04_a, mod04_b, mod04_c)
# AIC --> mod04_a 
BIC_mod04 <- BIC(mod04_a, mod04_b, mod04_c)
# BIC --> mod04_a 


# Best model using BIC is Mod05_a 
AIC_mod05 <- AIC(mod05_a, mod05_b, mod05_c)
# AIC --> mod05_c
BIC_mod05 <- BIC(mod05_a, mod05_b, mod05_c)
# BIC --> mod05_a

### Predict a PC 

predict_sing_pc <- function(mod,Xtest)
{
  pred_test <- predict(mod, newdata = Xtest)
  
  pred_test 
}

predict_all_pc <- function(modList, Xtest)
{
  pred_list <- purrr::map(modList, predict_sing_pc, 
                          Xtest = Xtest)
  pred_df <- pred_list %>% purrr::map_dfc(function(ll){
     list(v = ll)
  })
  names(pred_df)<- sprintf("pib_%02d", 
                           1:length(modList))
  pred_df 
}

## predict training set 

train_input_only <- train_ready %>% select(starts_with("fdg"), starts_with("gmd"))

pred_trainscore <- predict_all_pc(list(mod01_a, mod02_a, mod03_a, mod04_a, mod05_a), 
               Xtest = train_input_only)

#### new Reconstruction function 
pib_reconstruct <- function(predscores, pca_obj)
{
  # extract the scores
  z_mat <- as.matrix(predscores)
  
  # extract the loadings matrix
  phi_mat <- pca_obj$rotation[, 1:ncol(predscores), drop = FALSE]
  
  # reconstruct the standardized variables
  xhat <- z_mat %*% t(phi_mat)
  
  # rescale the reconstructed variables
  xhat <- scale(xhat, center = FALSE, scale = 1/pca_obj$scale)
  
  # add back the means
  scale(xhat, center = -pca_obj$center, scale = FALSE)
}

### Reconstruct on 5 PCs 

pred_train_recon <- pib_reconstruct(pred_trainscore, pca_pib) %>% 
  as.data.frame() %>% tibble::as_tibble()

train_performance <- pred_train_recon %>% tibble::rowid_to_column() %>% 
  pivot_longer(!c("rowid"), 
               values_to = "recon_value") %>%
  left_join(pib %>% tibble::rowid_to_column() %>% 
              pivot_longer(!c("rowid"), 
                           values_to = "observed_value"),
            by = c("rowid", "name"))


train_performance %>% ggplot(mapping = aes(x = abs(observed_value - recon_value), y = name)) +
  geom_boxplot()


train_performance %>% ggplot(mapping = aes(x = observed_value, y = recon_value)) + geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red") + facet_wrap(~name, scales = "free") +
  theme(strip.background = element_blank(), 
        strip.text = element_blank(), 
        axis.text = element_blank())

### Make predictions with PC1 and PC5 RSM 

pred_trainscore_rsm <- predict_all_pc(list(mod01_c, mod02_a, mod03_a, mod04_a, mod05_c), 
                                  Xtest = train_input_only)

pred_train_recon_rsm <- pib_reconstruct(pred_trainscore_rsm, pca_pib)



pred_train_recon_rsm <- pred_train_recon_rsm %>% 
  as.data.frame() %>% tibble::as_tibble()

train_performance_rsm <- pred_train_recon_rsm %>% tibble::rowid_to_column() %>% 
  pivot_longer(!c("rowid"), 
               values_to = "recon_value") %>%
  left_join(pib %>% tibble::rowid_to_column() %>% 
              pivot_longer(!c("rowid"), 
                           values_to = "observed_value"),
            by = c("rowid", "name"))


train_performance_rsm %>% ggplot(mapping = aes(x = abs(observed_value - recon_value), y = name)) +
  geom_boxplot()


train_performance_rsm %>% ggplot(mapping = aes(x = observed_value, y = recon_value)) + geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red") + facet_wrap(~name, scales = "free") +
  theme(strip.background = element_blank(), 
        strip.text = element_blank(), 
        axis.text = element_blank())

unique_region_names <- train_performance_rsm %>% distinct(name) %>%
  pull()

train_performance %>% mutate(model_names = "all_lm") %>% 
  bind_rows(train_performance_rsm %>% mutate(model_names = "with_rsm")) %>%
  filter(name %in% unique_region_names[1:4]) %>% ggplot(mapping = aes(x = observed_value, y = recon_value)) + geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red") + 
  facet_grid(name~model_names, scales = "free")
##################################################

# resampling - linear model (2, 3, 4 )
#            - mod1_c, mod5_c
# Create a function 

wrapper_reconstruct <- function(numPC, ){
  # input parameter (# of PCs to use , list(formulas, formula 2, ....))
  
  
  
  # Predict Pcs iterate over all PCs 
  # for(int i = 0, i < #PCs, i++)
  #{
            
 # }
  
}



my_reconstruct <- function(nComp, pca_obj)
{
  # extract the scores
  z_mat <- pca_obj$x[, 1:nComp, drop=FALSE]
  
  # extract the loadings matrix
  phi_mat <- pca_obj$rotation[, 1:nComp, drop = FALSE]
  
  # reconstruct the standardized variables
  xhat <- z_mat %*% t(phi_mat)
  
  # rescale the reconstructed variables
  xhat <- scale(xhat, center = FALSE, scale = 1/pca_obj$scale)
  
  # add back the means
  scale(xhat, center = -pca_obj$center, scale = FALSE)
}

# mod01_a, mod02_a, mod03_a, mod_04a, mod05_a 
# How do I reconstruct PiB output given all of the pred of output model 

my_reconstruct(1, pca_pib) %>% 
  as.data.frame() %>% tibble::rownames_to_column() %>% 
  pivot_longer(!c("rowname"), values_to = "reconstructed_value") %>% 
  left_join(output_scores %>% 
              tibble::rownames_to_column() %>% 
              pivot_longer(!c("rowname"), values_to = "observed_value"),
            by = c("name", "rowname")) %>% 
  ggplot(mapping = aes(x = observed_value, y = reconstructed_value)) +
  geom_point(color = "black") +
  geom_abline(slope = 1, intercept = 0,
              color = "red", linetype = "dashed") +
  #facet_wrap(~name, scales = "free") +
  theme_bw()











