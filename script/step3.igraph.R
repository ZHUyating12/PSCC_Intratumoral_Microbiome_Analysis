#!/TJPROJ5/META_ASS/meta/sunhongtao/script/miniforge3/envs/R/bin/Rscript
#setwd("E:\\personal_demand\\nature-network\\X101SC24040447-Z01-F002\\HPVneg/")
rm(list=ls())
require(optparse)
option_list <- list(
  make_option(c("--module"), type = "character",help = "module.xls"),
  make_option(c("--top5phylum"), type = "character",help = "top5phylum.xls"),
  make_option(c("--tax_des"), type = "character",help = "tax_description.xls"),
  make_option(c("--cor_filter"), type = "character",help = "cor_filter.csv"),
  make_option(c("--output"), type = "character",help = "./")
)
opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)


require(dplyr)
library(igraph)
library(RColorBrewer)
require(magrittr)

#定义10个颜色
color.scheme <- c("#9cc4b4","#cc6454","#7c5474","#5c6494","#fcccbc","#ecacbc","#fcd424", "#B3DE69","#BEBADA","#FFFFB3" )
#处理传参
# module <- read.table("module.xls",header = T) %>% setNames(c("taxonomy","module")) %>% 
#   mutate(module=factor(module,level=unique(module)))
# top5phylum <- read.table("top5phylum.xls",header = F) %>% setNames("topphylum")
# all_tax <- read.table("tax_description.xls",sep = "\t",header = T)
# edge <- read.csv("cor_filter.csv",header = T) %>% 
#   select(.,all_of(c("Source","Target","coefficient","inter_type","connection_type"))) %>% 
#   mutate(Source_label = Source)

#处理传参
module <- read.table(opt$module,header = T) %>% setNames(c("taxonomy","module")) %>% 
  mutate(module=factor(module,level=unique(module)))
top5phylum <- read.table(opt$top5phylum,header = F) %>% setNames("topphylum")
all_tax <- read.table(opt$tax_des,sep = "\t",header = T)
edge <- read.csv(opt$cor_filter,header = T) %>% 
  select(.,all_of(c("Source","Target","coefficient","inter_type","connection_type"))) %>% 
  mutate(Source_label = Source)

setwd(opt$output)


#========================处理module========================
#筛选出top5+top5的门的module
module_top <- left_join(module,all_tax,by=c("taxonomy"="Taxonomy")) %>% 
  subset(.,p%in%top5phylum$topphylum)

#给module赋予颜色,最多给10个module赋予颜色，10之外的module赋予灰色
if (length(unique(as.character(module_top$module))) <= 10) {
    color <- color.scheme[1:length(unique(module_top$module))]
}else{
    color <- c(color.scheme, rep("grey", length(unique(as.character(module_top$module))) - 10))
}

module_top.stat <- table(module_top$module) %>%
  as.data.frame() %>%
  setNames(c("module","freq")) %>%
  arrange(desc(freq)) %>%
  mutate(color=color) %>%
  select(-freq)

#物种赋予颜色，top5之外的设置为灰色
module_color <- left_join(module,all_tax,by=c("taxonomy"="Taxonomy")) %>% 
  left_join(.,module_top.stat,by="module") %>% 
  mutate(color = ifelse(is.na(color), "grey", color)) %>% 
  mutate(topphylum=ifelse(p%in%top5phylum$topphylum,TRUE,FALSE)) %>% 
  mutate(final.color=ifelse(topphylum,color,"grey"))
write.table(module_color,"module_color.xls",sep = "\t",row.names = F,quote = F)
##========================处理edge========================
#把不具有相关性的物种设置为自连
#额外的节点
extraNodes <- all_tax$Taxonomy[!all_tax$Taxonomy %in% as.character(module$taxonomy)]
if ((length(extraNodes) == 0)){
  edges <- edge
}else{
#将额外的节点设置为selfLink
selfEdges <- cbind.data.frame(Source = extraNodes, 
                              Target = extraNodes,
                              coefficient = NA,
                              inter_type = NA,
                              connection_type = "selfLink",
                              Source_label = extraNodes,
                              stringsAsFactors=F) 
#将额外的节点与边文件连接
edges <- bind_rows(edge, selfEdges)
}

##开始绘制网络图
net <- graph_from_data_frame(d=edges, vertices=all_tax, directed=F)
#将nodes中不存在定义为灰色，存在的即为module定义的颜色
V(net)$module.color <- 
  sapply(names(V(net)),
         function(x){
           if(x %in% module_color$taxonomy){
             module_color$final.color[which(module_color$taxonomy == x)]
           }else "gray"
         })

#如果节点类型是 "bacteria"，则颜色为 "white"；否则，颜色与节点的 module.color 属性相对应。
#即实现 空心圆为细菌，实心为真菌
V(net)$type.color <-
  sapply(1:length(V(net)),
         function(i){
           if(V(net)$k[i] == "k__Bacteria") "white" else V(net)$module.color[i]
         })
V(net)$label <- all_tax$Taxonomy


#设置边的属性，如果存在，即为源节点的颜色，如果是缺失或者不存在，则为灰色
E(net)$source.module.color  <- 
  sapply(E(net)$Source_label,
         function(x) {
           if(is.na(x) | !x %in% module_color$taxonomy) "gray" else module_color$final.color[which(module_color$taxonomy == x)]
         })

#取消self links
net.simp <- net - E(net)[E(net)$connection_type=="selfLink"]  #remove self links, only keep the nodes


# Fig5a:
set.seed(12345)
pdf("network_label.pdf", width = 4 , height = 4) 
#储存的图片大小（对应圆的面积）跟node个数成正比，所以长宽与node个数的平方根成正比
par(mar=c(0.1,0.1,0.1,0.1)) 

plot(net.simp, 
     vertex.label=V(net)$label, 
     vertex.size = 4,
     vertex.label.color = "black",
     vertex.label.cex=0.2,
     vertex.color = V(net)$type.color,
     vertex.frame.color= V(net)$module.color,
     edge.size = 1, 
     edge.color = E(net)$source.module.color, 
     layout=layout_with_fr(net)
)

dev.off()
set.seed(12345)
pdf("network.pdf", width = 4 , height = 4) 
#储存的图片大小（对应圆的面积）跟node个数成正比，所以长宽与node个数的平方根成正比
par(mar=c(0.1,0.1,0.1,0.1))

plot(net.simp, vertex.label=NA,
     vertex.size = 4,
     #     vertex.label.color = "black",
     #     vertex.label.cex=0.2,
     vertex.color = V(net)$type.color,
     vertex.frame.color= V(net)$module.color,
     edge.size = 1,
     edge.color = E(net)$source.module.color,
     layout=layout_with_fr(net)
)

dev.off()
