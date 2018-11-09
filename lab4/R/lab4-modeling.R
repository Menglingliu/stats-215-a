setwd("C:/Users/Nicholas/Dropbox/2018 Fall/Git/stat-215-a/lab4")
rm(list = ls())

library("tidyverse")
library("ggthemes")
library("gridExtra")
library("cowplot")
library("SuperLearner")
library("ROCR")
library("snow")
library("parallel")
library("ck37r")
library("caret")
library("ROCR")
library("ggpubr")

# initialize the number of cells and folds
xbreak <- 10
ybreak <- 10
V <- 5

# Get the data for three images
path <- "data"
image1 <- read.table(paste0('data/', 'image1.txt'), header = F)
image2 <- read.table(paste0('data/', 'image2.txt'), header = F)
image3 <- read.table(paste0('data/', 'image3.txt'), header = F)

# Add informative column names.
collabs <- c('y','x','label','NDAI','SD','CORR','DF','CF','BF','AF','AN')
names(image1) <- collabs
names(image2) <- collabs
names(image3) <- collabs
summary(image1)
summary(image2)
summary(image3)

# use image1 and image2 as learning set and image 3 as the holdout test set
image1$image <- 1
image2$image <- 2
learning <- rbind(image1, image2)

image3$image <- 3
test <- image3
test_no0 <- test %>%
  filter(label != 0) %>%
  mutate(label = (label + 1)/2)

# form a grid 
learning <- learning %>%
  mutate(xgrid = cut(x, breaks = xbreak, labels = FALSE)) %>%
  mutate(ygrid = cut(y, breaks = ybreak, labels = FALSE)) %>%
  mutate(cell = as.factor(100*xgrid + ygrid)) %>%
  filter(label != 0) %>%
  mutate(label = (label + 1)/2)

# create xgboost learners with different tuning parameters
# focus on tuning shrinkage: shrinkage
shrinkage_seq <- c(0.1, 0.2, 0.3)
xgboost_l <- create.Learner(base_learner = "SL.xgboost",
                            tune = list(shrinkage = shrinkage_seq))

# create ranger learners with different tuning parameters
# focus on tuning number of variables to split at each node: mtry
mtry_seq <- c(1, 2, 3)
ranger_l <- create.Learner(base_learner = "SL.ranger",
                           tune = list(mtry = mtry_seq))

# run parallelized SuperLearner
cluster <- makeCluster(5)
clusterEvalQ(cluster, library(SuperLearner))
clusterExport(cluster, xgboost_l$names)
clusterExport(cluster, ranger_l$names)
clusterSetRNGStream(cluster, 1)
set.seed(1)
sl_fit <- snowSuperLearner(Y = learning$label,
                           X = learning[, c("NDAI", "SD", "CORR", "DF")],
                           newX = test[, c("NDAI", "SD", "CORR", "DF")],
                           verbose = TRUE,
                           cluster = cluster,
                           id = learning$cell,
                           family = binomial(),
                           method = "method.AUC",
                           cvControl = list(V = V),
                           SL.library = c("SL.glmnet",
                                          xgboost_l$names,
                                          ranger_l$names))

# save output and load
save.image("SL.Rdata")
load("SL.Rdata")

# check risk
sl_fit

# extracting predictions on hold out set
sl_pred <- sl_fit$SL.predict
glmnet_pred <- sl_fit$library.predict[, "SL.glmnet_All"]
xgboost_pred <- sl_fit$library.predict[, "SL.xgboost_1_All"]
ranger_pred <- sl_fit$library.predict[, "SL.ranger_1_All"]

# generate dataframes for plotting the ROC curves and finding thresholds
chosen <- c("SL.glmnet", "SL.xgboost_1", "SL.ranger_1")
chosen_index <- which(sl_fit$SL.library$library$predAlgorithm %in% chosen)

df <- data.frame(x = NULL, y = NULL, learner = NULL)
cutoff <- data.frame(cut = NULL, fpr = NULL, tpr = NULL, learner = NULL)
for (j in 1:3){
  i <- chosen_index[j]
  preds <- sl_fit$Z[, i]
  pred <- ROCR::prediction(preds, learning$label)
  perf1 <- ROCR::performance(pred, "sens", "spec")
  dframe <- data.frame(x = 1 - slot(perf1, "x.values")[[1]],
                       y = slot(perf1, "y.values")[[1]],
                       learner = chosen[j])
  df <- rbind(df, dframe)
  
  perf2 <- ROCR::performance(pred, "tpr", "fpr")
  cutoffs <- data.frame(cut = perf2@alpha.values[[1]], 
                        fpr = perf2@x.values[[1]], 
                        tpr = perf2@y.values[[1]],
                        learner = chosen[j])
  cutoff <- rbind(cutoff, cutoffs)
}
df$learner <- as.factor(df$learner)
cutoff$learner <- as.factor(cutoff$learner)

# plot ROC curves
ROC <- ggplot(data = df) + 
  geom_line(aes(x = x,
                y = y,
                color = learner),
            size = 1) + 
  geom_vline(xintercept = 0.05) +
  theme_gdocs() +
  scale_color_gdocs(name = "Classifier",
                    labels = c("glmnet", "xgboost", "ranger")) +
  scale_x_continuous(breaks = c(0.03, 0.05, 0.07)) +
  labs(title ="ROC curve",
       x = "1 - Specificity (false positives)", 
       y = "Sensitivity (true positives)")
ggsave("cv_ROC.pdf")

ROCzoom <- ggplot(data = df) + 
  geom_line(aes(x = x,
                y = y,
                color = learner),
            size = 1) + 
  geom_vline(xintercept = 0.05) +
  theme_gdocs() +
  scale_color_gdocs(name = "Classifier",
                    labels = c("glmnet", "xgboost", "ranger")) +
  scale_x_continuous(breaks = c(0.03, 0.05, 0.07)) +
  labs(title ="Zoomed-in ROC curve",
       x = "1 - Specificity (false positives)", 
       y = "Sensitivity (true positives)") +
  coord_cartesian(xlim = c(0.02, 0.08), 
                  ylim = c(0.75, 1.00))
ggsave("cv_ROC_zoom.pdf")

# compute thresholds
res_cutoffs <- cutoff %>%
  filter(fpr < 0.05) %>%
  group_by(learner) %>%
  dplyr::slice(which.max(tpr)) %>%
  ungroup()
  
# generate visual plots for 3 classfiers
test$glmnet_lab <- 2*(ranger_pred > as.numeric(res_cutoffs[1, "cut"])) - 1
test$xgboost_lab <- 2*(xgboost_pred > as.numeric(res_cutoffs[2, "cut"])) - 1
test$ranger_lab <- 2*(ranger_pred > as.numeric(res_cutoffs[3, "cut"])) - 1

i3_lab <- ggplot(test) + 
  geom_point(aes(x = x, y = y, color = factor(label))) +
  theme_gdocs() +
  scale_color_gdocs(name = "Label") +
  labs(title = "expert labels for image 3")
ggsave("i3_label_pred3.pdf")
i3_glmnet_pred <- ggplot(test) + 
  geom_point(aes(x = x, y = y, color = factor(glmnet_lab))) +
  theme_gdocs() +
  scale_color_gdocs(name = "Label") +
  labs(title = "glmnet labels for image 3")
ggsave("i3_glmnet_pred3.pdf")
i3_xgboost_pred <- ggplot(test) + 
  geom_point(aes(x = x, y = y, color = factor(xgboost_lab))) +
  theme_gdocs() +
  scale_color_gdocs(name = "Label") +
  labs(title = "xgboost labels for image 3")
ggsave("i3_xgboost_pred3.pdf")
i3_ranger_pred <- ggplot(test) + 
  geom_point(aes(x = x, y = y, color = factor(ranger_lab))) +
  theme_gdocs() +
  scale_color_gdocs(name = "Label") +
  labs(title = "ranger labels for image 3")
ggsave("i3_ranger_pred3.pdf")


# produce confusion matrix information
test_no0 <- test %>%
  select(one_of(c("label", "glmnet_lab", "ranger_lab", "xgboost_lab" ))) %>%
  filter(label != 0)

confusionMatrix(as.factor(test_no0$label),
                as.factor(test_no0$glmnet_lab))

confusionMatrix(as.factor(test_no0$label),
                as.factor(test_no0$ranger_lab))

confusionMatrix(as.factor(test_no0$label),
                as.factor(test_no0$xgboost_lab))

# ROC plots using ROCR
test$xgboost_pred <- xgboost_pred
test_no0 <- test %>%
  select(one_of(c("label", "xgboost_pred"))) %>%
  filter(label != 0)
preds <- test_no0$xgboost_pred
pred <- ROCR::prediction(preds, test_no0$label)
perf1 <- ROCR::performance(pred, measure="tpr", x.measure="fpr")
perf1df <- data.frame(FPR = slot(perf1, "x.values")[[1]],
                      TPR = slot(perf1, "y.values")[[1]])
perf2 <- ROCR::performance(pred, measure="prec", x.measure="rec")
perf2df <- data.frame(Recall = slot(perf2, "x.values")[[1]],
                      Precision = slot(perf2, "y.values")[[1]])
perf3 <- ROCR::performance(pred, measure="sens", x.measure="spec")
perf3df <- data.frame(Specificity = slot(perf3, "x.values")[[1]],
                      Sensitivity = slot(perf3, "y.values")[[1]])
perf4 <- ROCR::performance(pred, measure="lift", x.measure="rpp")
perf4df <- data.frame(RPP = slot(perf4, "x.values")[[1]],
                      Lift = slot(perf4, "y.values")[[1]])

xgboostROC <- ggplot(data = perf1df) + 
  geom_line(aes(x = FPR,
                y = TPR),
            size = 1) + 
  theme_gdocs() +
  labs(title = "ROC curve for xgboost")
ggsave("xgboostroc.pdf")
xgboostRecall <- ggplot(data = perf2df) + 
  geom_line(aes(x = Recall,
                y = Precision),
            size = 1) + 
  theme_gdocs() +
  labs(title = "Precision-Recall curve for xgboost")
ggsave("xgboostprerec.pdf")
xgboostSens <- ggplot(data = perf3df) + 
  geom_line(aes(x = Specificity,
                y = Sensitivity),
            size = 1) + 
  theme_gdocs() +
  labs(title = "Sens-Spec curve for xgboost")
ggsave("xgboostsensspec.pdf")
xgboostLift <- ggplot(data = perf4df) + 
  geom_line(aes(x = RPP,
                y = Lift),
            size = 1) + 
  theme_gdocs() +
  labs(title = "Lift chart for xgboost")
ggsave("xgboostlift.pdf")

# subset learning
sub_learn <- learning %>%
  select(one_of(c('y','x','label','NDAI','SD','CORR',
                  'DF','CF','BF','AF','AN', 'image'))) %>%
  mutate(label = 2*label - 1) %>%
  filter(label != 0)

# subset the data with x>300 and y<35
all_bin_rc <- test %>%
  filter(x>300, y<35) %>%
  select(one_of(c('y','x','label','NDAI','SD','CORR',
                    'DF','CF','BF','AF','AN', 'image')))%>%
  bind_rows(sub_learn) %>%
  filter(label != 0) 
# density plot
an_rc <- ggplot(all_bin_rc) + 
  geom_density(aes(x = AN, group = factor(label), fill = factor(label)), 
               alpha = 0.5) +
  facet_grid(. ~ image) +
  scale_fill_discrete(name = "Expert label") +
  theme_bw()
af_rc <- ggplot(all_bin_rc) + 
  geom_density(aes(x = AF, group = factor(label), fill = factor(label)), 
               alpha = 0.5) +
  facet_grid(. ~ image) +
  scale_fill_discrete(name = "Expert label") +
  theme_bw()
bf_rc <- ggplot(all_bin_rc) + 
  geom_density(aes(x = BF, group = factor(label), fill = factor(label)), 
               alpha = 0.5) +
  facet_grid(. ~ image) +
  scale_fill_discrete(name = "Expert label") +
  theme_bw()
cf_rc <- ggplot(all_bin_rc) + 
  geom_density(aes(x = CF, group = factor(label), fill = factor(label)), 
               alpha = 0.5) +
  facet_grid(. ~ image) +
  scale_fill_discrete(name = "Expert label") +
  theme_bw()
df_rc <- ggplot(all_bin_rc) + 
  geom_density(aes(x = DF, group = factor(label), fill = factor(label)), 
               alpha = 0.5) +
  facet_grid(. ~ image) +
  scale_fill_discrete(name = "Expert label") +
  theme_bw()
ndai_rc <- ggplot(all_bin_rc) + 
  geom_density(aes(x = NDAI, group = factor(label), fill = factor(label)), 
               alpha = 0.5) +
  facet_grid(. ~ image) +
  scale_fill_discrete(name = "Expert label") +
  theme_bw()
sd_rc <- ggplot(all_bin_rc) + 
  geom_density(aes(x = SD, group = factor(label), fill = factor(label)), 
               alpha = 0.5) +
  facet_grid(. ~ image) +
  scale_fill_discrete(name = "Expert label") +
  theme_bw()
corr_rc <- ggplot(all_bin_rc) + 
  geom_density(aes(x = CORR, group = factor(label), fill = factor(label)), 
               alpha = 0.5) +
  facet_grid(. ~ image) +
  scale_fill_discrete(name = "Expert label") +
  theme_bw()

plot_density_rc <- ggarrange(an_rc, af_rc, bf_rc, cf_rc, df_rc, 
                             ndai_rc, corr_rc, sd_rc, ncol = 2, nrow = 4, 
                             common.legend = TRUE, legend="bottom")
ggsave("plot_density_rc.pdf", plot = plot_density_rc, width = 10, height = 10)
plot_density_rc
