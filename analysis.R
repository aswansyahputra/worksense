# Aktifkan paket ----------------------------------------------------------

library(readxl)
library(janitor)
library(tidyverse)
library(scales)
library(hrbrthemes)
library(skimr)
library(FactoMineR)
library(agricolae)
library(factoextra)

# Impor data --------------------------------------------------------------

peas_raw <-
  read_excel(
    path = "peaimpba.xls", 
    .name_repair = make_clean_names
  )

# Inspeksi data -----------------------------------------------------------

glimpse(peas_raw)
skim(peas_raw)

# Transformasi data -------------------------------------------------------

peas <- 
  peas_raw %>% 
  mutate(
    product = factor(product),
    order = factor(order),
    replicat = factor(replicat),
    assessor = factor(assessor),
    variety = factor(variety)
  )

glimpse(peas)
skim_without_charts(peas)

# Statistik dasar ---------------------------------------------------------

peas_stats <- 
  peas %>% 
  group_by(product) %>% 
  summarise(
    across(pea_od:skin_vis, mean, na.rm = TRUE)
  )

# Visualisasi dasar -------------------------------------------------------

ggplot(data = peas, aes(skin_vis)) +
  geom_histogram(fill = "steelblue") +
  labs(
    title = "Distribusi skor Skin Visual",
    y = "Frekuensi",
    x = "Skor"
  ) +
  theme_ipsum(grid = "Y")

peas_stats %>% 
  ggplot(aes(crisp, fct_reorder(product, crisp))) +
  geom_col(fill = "darkslategray4") +
  geom_text(aes(label = number(crisp, accuracy = 0.01)), family = "Arial Narrow", nudge_x = 0.3) +
  labs(
    title = "Kerenyahan",
    x = "Rerata skor",
    y = "Produk"
  ) +
  theme_ipsum(grid = "X", ticks = TRUE)

# Statistik inferensial ---------------------------------------------------

crisp_model <-
  aov(
    crisp ~ product + assessor + replicat + order + assessor:replicat + assessor:product + replicat:product,
    data = peas
  )

anova(crisp_model)
summary.lm(crisp_model)

crisp_model_sum <-
  AovSum(
    crisp ~ product + assessor + replicat + order + assessor:replicat + assessor:product + replicat:product,
    data = peas
  )

crisp_model_sum$Ftest
crisp_model_sum$Ttest

# Post-hoc ----------------------------------------------------------------

crisp_posthoc <-
  duncan.test(crisp_model, trt = "product", console = TRUE)

plot(crisp_posthoc, horiz = TRUE)

# Analisis faktor ---------------------------------------------------------

peas_pca <- 
  peas_stats %>% 
  column_to_rownames("product") %>% 
  PCA(graph = FALSE)

fviz_eig(peas_pca, choice = "variance", addlabels = TRUE)
fviz_eig(peas_pca, choice = "eigenvalue", addlabels = TRUE)

fviz_pca_ind(peas_pca)
fviz_pca_var(peas_pca)
fviz_pca_biplot(peas_pca)
