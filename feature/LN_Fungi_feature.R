#Fungi
library(randomForest)
library(dplyr)
ln_info<-readRDS("/home/zyt/16s_JiangXi/model_R/data/clinical info/ln_group_info.rds")
ln_fungi<-readRDS("/home/zyt/16s_JiangXi/model_R/data/fungi_bac/LN_group_all_fungi.rds")

fungi <- ln_fungi
rownames(fungi) <- fungi$Taxonomy
fungi$Taxonomy <- NULL

fungi_t <- as.data.frame(t(fungi))
fungi_t$sample <- rownames(fungi_t)
df <- ln_info %>%
  left_join(fungi_t, by = "sample")
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
fungi_sub<-df[,c("sample","group",top)]
saveRDS(fungi_sub,"/home/zyt/16s_JiangXi/model_R/LN_positive_negative/data/fungi.rds")
