
# EDA ---------------------------------------------------------------------

library(knitr)
library(tidyverse)
library(ggplot2)
library(corrplot)
library(gridExtra)
library(caret)
library(pROC)
library(ggpubr)
library(leaps)
library(glmnet)
library(doParallel)
setwd("C:/Users/Fan/Box Sync/stat-215-a/lab4")
path <- "image_data"
image1 <- read.table(paste0(path, '/image1.txt'), header = F)
image2 <- read.table(paste0(path, '/image2.txt'), header = F)
image3 <- read.table(paste0(path, '/image3.txt'), header = F)

# Add informative column names.
collabs <- c('y','x','label','NDAI','SD','CORR','DF','CF','BF','AF','AN')
names(image1) <- collabs
names(image2) <- collabs
names(image3) <- collabs

image1$image <- 'Image 1'
image2$image <- 'Image 2'
image3$image <- 'Image 3'
all <- rbind(image1, image2, image3)
all$label <- factor(all$label, levels=c(-1, 0, 1), labels=c("cloudy", "unlabeled", "clear"))
all_bin <- all %>% filter(label != 'unlabeled')

# map expert label
map_label <- ggplot(all) + 
  geom_point(aes(x = x, y = y, color = factor(label))) +
  facet_grid(.~image) +
  scale_colour_brewer(palette = "Paired", name = 'Expert Label') +
  theme_bw()
ggsave("map_label.pdf", width = 10, height = 3)

# correlation plot
rad <- all_bin %>% dplyr::select(c('DF','CF','BF','AF','AN', 'NDAI', 'SD', 'CORR', 'label'))
rad$label <- rad$label == 'cloudy'
corr <- cor(rad)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
pdf(file = "plot_cor.pdf")
plot_cor <- corrplot(corr, col=col(200),  
         type = "upper", order = "hclust",
         addCoef.col = "black", number.cex=0.75,
         tl.col = "black", tl.srt = 45)
dev.off()

# map features
ndai <- ggplot(all) +
  geom_point(aes(x = x, y = y, color = NDAI)) +
  facet_grid(.~image) +
  ggtitle("Map of Normalized Difference Angular Index (NDAI)") +
  theme_bw()
corr <- ggplot(all) +
  geom_point(aes(x = x, y = y, color = CORR)) +
  facet_grid(.~image) +
  ggtitle("Map of MISR Images Correlation (CORR)") +
  theme_bw()
sd <- ggplot(all) +
  geom_point(aes(x = x, y = y, color = SD)) +
  facet_grid(.~image) +
  ggtitle("Map of Nadir Pixel Values SD (SD)") +
  theme_bw()
map_var <- grid.arrange(ndai, corr, sd, nrow = 3)
ggsave("map_var.pdf", plot = map_var, width = 10, height = 8)

# density plot
an <- ggplot(all_bin) + 
  geom_density(aes(x = AN, group = factor(label), fill = factor(label)), 
               alpha = 0.5) +
  facet_grid(. ~ image) +
  scale_fill_discrete(name = "Expert label") +
  theme_bw()
af <- ggplot(all_bin) + 
  geom_density(aes(x = AF, group = factor(label), fill = factor(label)), 
               alpha = 0.5) +
  facet_grid(. ~ image) +
  scale_fill_discrete(name = "Expert label") +
  theme_bw()
bf <- ggplot(all_bin) + 
  geom_density(aes(x = BF, group = factor(label), fill = factor(label)), 
               alpha = 0.5) +
  facet_grid(. ~ image) +
  scale_fill_discrete(name = "Expert label") +
  theme_bw()
cf <- ggplot(all_bin) + 
  geom_density(aes(x = CF, group = factor(label), fill = factor(label)), 
               alpha = 0.5) +
  facet_grid(. ~ image) +
  scale_fill_discrete(name = "Expert label") +
  theme_bw()
df <- ggplot(all_bin) + 
  geom_density(aes(x = DF, group = factor(label), fill = factor(label)), 
               alpha = 0.5) +
  facet_grid(. ~ image) +
  scale_fill_discrete(name = "Expert label") +
  theme_bw()
ndai <- ggplot(all_bin) + 
  geom_density(aes(x = NDAI, group = factor(label), fill = factor(label)), 
               alpha = 0.5) +
  facet_grid(. ~ image) +
  scale_fill_discrete(name = "Expert label") +
  theme_bw()
sd <- ggplot(all_bin) + 
  geom_density(aes(x = SD, group = factor(label), fill = factor(label)), 
               alpha = 0.5) +
  facet_grid(. ~ image) +
  scale_fill_discrete(name = "Expert label") +
  theme_bw()
corr <- ggplot(all_bin) + 
  geom_density(aes(x = CORR, group = factor(label), fill = factor(label)), 
               alpha = 0.5) +
  facet_grid(. ~ image) +
  scale_fill_discrete(name = "Expert label") +
  theme_bw()
plot_density <- ggarrange(an, af, bf, cf, df, ndai, corr, sd, ncol = 2, nrow = 4, 
          common.legend = TRUE, legend="bottom")
ggsave("plot_density.pdf", plot = plot_density, width = 10, height = 10)


# Feature Selection -------------------------------------------------------

all_bin$label <- all_bin$label == 'cloudy'
ncore <- 4
registerDoParallel(ncore)
X <- as.matrix(all_bin[c('NDAI', 'SD', 'CORR','DF', 'CF', 'BF', 'AF','AN')])
cv <- cv.glmnet(X, as.factor(all_bin$label), family = "binomial", type.measure = "auc", nfold = 10, paralle = TRUE, alpha = 1)
pdf(file = "cv_glmnet.pdf", width = 8, height = 3)
plot(cv)
dev.off()
