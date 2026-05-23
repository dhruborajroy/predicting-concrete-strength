# =========================================================
# ADVANCED PCA BIPLOT WITH ELLIPSES
# Publication Quality Figure
# =========================================================

# =========================================================
# 1. INSTALL REQUIRED PACKAGES
# =========================================================

install.packages("ggplot2")
install.packages("ggrepel")
install.packages("factoextra")
install.packages("FactoMineR")
install.packages("ggforce")

# =========================================================
# 2. LOAD LIBRARIES
# =========================================================

library(ggplot2)
library(ggrepel)
library(factoextra)
library(FactoMineR)
library(ggforce)

# =========================================================
# 3. CREATE EXAMPLE DATASET
# =========================================================

plant_data <- data.frame(
  
  Plant = paste0("P",1:20),
  
  Treatment = c(
    
    rep("Control",5),
    rep("Fertilizer",5),
    rep("Organic",5),
    rep("Biochar",5)
    
  ),
  
  Height = c(
    35,40,38,37,39,
    45,48,50,47,49,
    42,44,46,43,45,
    52,55,58,54,57
  ),
  
  LeafArea = c(
    120,135,128,125,130,
    150,160,170,158,165,
    145,148,152,149,151,
    175,180,190,185,188
  ),
  
  Chlorophyll = c(
    42,46,44,43,45,
    49,50,52,51,53,
    47,48,49,48,50,
    54,56,58,57,59
  ),
  
  RootLength = c(
    15,18,17,16,17,
    21,22,24,23,25,
    20,21,22,21,23,
    25,26,28,27,29
  )
)

# =========================================================
# 4. SELECT NUMERIC VARIABLES
# =========================================================

X <- plant_data[,3:6]

# =========================================================
# 5. STANDARDIZE DATA
# =========================================================

X_scaled <- scale(X)

# =========================================================
# 6. PERFORM PCA
# =========================================================

pca_model <- prcomp(
  X_scaled,
  
  center = TRUE,
  
  scale. = TRUE
)

# =========================================================
# 7. PCA SCORES
# =========================================================

scores <- as.data.frame(
  pca_model$x
)

scores$Treatment <- plant_data$Treatment

scores$Plant <- plant_data$Plant

# =========================================================
# 8. PCA LOADINGS
# =========================================================

loadings <- as.data.frame(
  pca_model$rotation
)

loadings$Variable <- rownames(loadings)

# =========================================================
# 9. EXPLAINED VARIANCE
# =========================================================

variance <- round(
  100 * summary(pca_model)$importance[2,1:2],
  1
)

# =========================================================
# 10. PUBLICATION QUALITY PCA BIPLOT
# =========================================================

ggplot(
  
  scores,
  
  aes(
    x = PC1,
    y = PC2,
    color = Treatment,
    fill = Treatment
  )
  
) +
  
  # ---------------------------------------------------------
# FILLED CONFIDENCE ELLIPSES
# ---------------------------------------------------------

stat_ellipse(
  
  geom = "polygon",
  
  alpha = 0.15,
  
  level = 0.95,
  
  linewidth = 1
) +
  
  # ---------------------------------------------------------
# ELLIPSE BORDERS
# ---------------------------------------------------------

stat_ellipse(
  
  geom = "path",
  
  linewidth = 1
) +
  
  # ---------------------------------------------------------
# SAMPLE POINTS
# ---------------------------------------------------------

geom_point(
  
  aes(shape = Treatment),
  
  size = 4,
  
  alpha = 0.9
) +
  
  # ---------------------------------------------------------
# VARIABLE ARROWS
# ---------------------------------------------------------

geom_segment(
  
  data = loadings,
  
  aes(
    x = 0,
    y = 0,
    
    xend = PC1 * 4,
    yend = PC2 * 4
  ),
  
  inherit.aes = FALSE,
  
  arrow = arrow(
    length = unit(0.3,"cm")
  ),
  
  color = "black",
  
  linewidth = 1
) +
  
  # ---------------------------------------------------------
# VARIABLE LABELS
# ---------------------------------------------------------

geom_text_repel(
  
  data = loadings,
  
  aes(
    x = PC1 * 4,
    y = PC2 * 4,
    label = Variable
  ),
  
  inherit.aes = FALSE,
  
  size = 6,
  
  color = "black",
  
  fontface = "bold"
) +
  
  # ---------------------------------------------------------
# REFERENCE LINES
# ---------------------------------------------------------

geom_hline(
  
  yintercept = 0,
  
  linetype = "dashed",
  
  color = "black"
) +
  
  geom_vline(
    
    xintercept = 0,
    
    linetype = "dashed",
    
    color = "black"
  ) +
  
  # ---------------------------------------------------------
# AXIS LABELS
# ---------------------------------------------------------

xlab(
  
  paste0(
    "PC1 (",
    variance[1],
    "%)"
  )
  
) +
  
  ylab(
    
    paste0(
      "PC2 (",
      variance[2],
      "%)"
    )
    
  ) +
  
  # ---------------------------------------------------------
# TITLE
# ---------------------------------------------------------

ggtitle(
  
  "Publication Quality PCA Biplot"
  
) +
  
  # ---------------------------------------------------------
# THEME
# ---------------------------------------------------------

theme_bw(base_size = 18) +
  
  theme(
    
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 24
    ),
    
    axis.title = element_text(
      face = "bold"
    ),
    
    legend.title = element_text(
      face = "bold"
    ),
    
    legend.position = "right"
  )