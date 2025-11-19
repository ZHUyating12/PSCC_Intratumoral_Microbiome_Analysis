#T1_advanced_Fungi
library(randomForest)
library(dplyr)
t1_advanced_info<-readRDS("/home/zyt/16s_JiangXi/model_R/data/clinical info/t1_advanced.rds")
t1_advanced_info<-t1_advanced_info[,c("sample","group")]
t1_advanced_fungi<-readRDS("/home/zyt/16s_JiangXi/model_R/data/fungi_bac/t1_advanced_group_all_fungi.rds")

fungi <- t1_advanced_fungi
rownames(fungi) <- fungi$Taxonomy
fungi$Taxonomy <- NULL

fungi_t <- as.data.frame(t(fungi))
fungi_t$sample <- rownames(fungi_t)
df <- t1_advanced_info %>%
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
top <- rownames(imp_order)[1:15]
fungi_sub<-df[,c("sample","group",top)]
saveRDS(fungi_sub,"/home/zyt/16s_JiangXi/model_R/T1_T234/data/fungi.rds")