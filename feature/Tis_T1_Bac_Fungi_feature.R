#Tis_T1_Bac_Fungi
library(randomForest)
library(dplyr)
tis_t1_info<-readRDS("/home/zyt/16s_JiangXi/model_R/data/clinical info/tis_t1_info.rds")
colnames(tis_t1_info)[3]<-"group"
tis_t1_info<-tis_t1_info[,c("sample","group")]
tis_t1_bac_fungi<-readRDS("/home/zyt/16s_JiangXi/model_R/data/fungi_bac/tis_t1_group_all_bac_fungi_merge.rds")

bac_fungi <- tis_t1_bac_fungi
rownames(bac_fungi) <- bac_fungi$Taxonomy
bac_fungi$Taxonomy <- NULL

bac_fungi_t <- as.data.frame(t(bac_fungi))
bac_fungi_t$sample <- rownames(bac_fungi_t)
df <- tis_t1_info %>%
  left_join(bac_fungi_t, by = "sample")
df$group <- factor(df$group)
df_model <- df %>% select(-sample)

set.seed(123)
rf <- randomForest(group ~ ., 
                   data = df_model,
                   importance = TRUE,
                   ntree = 1000) 
imp <- importance(rf)
imp_order <- imp[order(imp[,"MeanDecreaseAccuracy"], decreasing = TRUE), ]
top <- rownames(imp_order)[1:22]
bac_fungi_sub<-df[,c("sample","group",top)]
saveRDS(bac_fungi_sub,"/home/zyt/16s_JiangXi/model_R/Tis_T1_model/fungi_bacteria/data_model.rds")
