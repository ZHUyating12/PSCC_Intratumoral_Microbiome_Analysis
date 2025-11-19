#LN
#Bac
library(randomForest)
library(dplyr)
ln_info<-readRDS("/home/zyt/16s_JiangXi/model_R/data/clinical info/ln_group_info.rds")
ln_bac<-readRDS("/home/zyt/16s_JiangXi/model_R/data/fungi_bac/LN_group_all_bac.rds")
bac <- ln_bac
rownames(bac) <- bac$Taxonomy
bac$Taxonomy <- NULL

bac_t <- as.data.frame(t(bac))
bac_t$sample <- rownames(bac_t)
df <- ln_info %>%
  left_join(bac_t, by = "sample")
df$group <- factor(df$group)
df_model <- df %>% select(-sample)

set.seed(123)
rf <- randomForest(group ~ ., 
                   data = df_model,
                   importance = TRUE,
                   ntree = 1000) 
imp <- importance(rf)
imp_order <- imp[order(imp[,"MeanDecreaseAccuracy"], decreasing = TRUE), ]
top <- rownames(imp_order)[1:8]
bac_sub<-df[,c("sample","group",top)]
saveRDS(bac_sub,"/home/zyt/16s_JiangXi/model_R/LN_positive_negative/data/bacteria.rds")
