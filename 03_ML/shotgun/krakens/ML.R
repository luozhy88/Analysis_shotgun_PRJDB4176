
library(phyloseq)
library(dplyr)
library(tibble)
library(ggplot2)
source("../function/function.R")




# load("../01_statistical_analysis/Distribution.RData")
shotgun_phyloseq <- readRDS("../input_raw/shotgun_phyloseq.rds")
meta_all<-read.csv("../input_raw/meta_ERP012177.csv") %>% data.frame()
meta_all$Group<-meta_all$config
# meta_all$Age<-meta_all$Age_at_collection
rownames(meta_all)<-meta_all$Run

sample_names(shotgun_phyloseq) =gsub(".bracken.mpa.report.*","",sample.names(shotgun_phyloseq))
sample_data(shotgun_phyloseq)<-meta_all

shotgun_phyloseq_core <- microbiome::core(shotgun_phyloseq, detection = 5, prevalence = .2)


## male

# balanced_sample.male.train
# balanced_sample.male.test

# # 从 phyloseq 对象中筛选出特定的样本
# my_samples <- c(balanced_sample.male.train$Run,balanced_sample.male.test$Run) # 要筛选的样本名称
# shotgun_phyloseq_core_subset <- subset_samples(shotgun_phyloseq_core, Run %in% my_samples)
shotgun_phyloseq_core_subset=shotgun_phyloseq_core
# 查看新的 phyloseq 对象
shotgun_phyloseq_core_subset
shotgun_phyloseq_core_subset.tss.level <- microbiome::aggregate_taxa(shotgun_phyloseq_core_subset,level = "Genus") %>%microbiome::transform(transform = "compositional")
OTU_table<-otu_table(shotgun_phyloseq_core_subset.tss.level) %>% t() %>% data.frame()
meta_phy=sample_data(shotgun_phyloseq_core_subset) %>% data.frame()  %>% dplyr::select(Group)
OTU_table_Group<-merge(meta_phy,OTU_table,by=0 ) %>% column_to_rownames("Row.names")


### ML
# install.packages('DALEX')
# install.packages('devtools')
# install_github('ModelOriented/forester')
library(devtools)
library(DALEX)
library(forester)
library(dplyr)
library(tibble)

source("../function/function.R")

project_name="ERP012177_"
### classfy
# iris5 <- iris %>% dplyr::filter(Species != "virginica")
output2 <- forester::train(data = OTU_table_Group,
                           y = 'Group',
                           bayes_iter = 0,
                           engine = c('ranger', 'xgboost', 'decision_tree'),
                           advanced_preprocessing = TRUE,
                           verbose = TRUE,
                           random_evals = 3)


output2$score_test


# exp_list <- forester::explain(models = output2$best_models[[1]],
#                               test_data = output2$test_data,
#                               y = output2$y)

best_model_name=output2$score_test$engine[1]
# exp <- exp_list[[1]]
# p1 <- DALEX::model_parts(exp)
# plot(p1)


draw_feature_importance_plot=draw_feature_importance2(best_models=output2$models_list,test_data=output2$test_data,y=output2$y)
draw_Roc_plot=draw_roc_plot(best_models=output2$models_list,test_data=output2$test_data,observed=output2$test_observed)
draw_Confusion_matrix=forester::draw_confusion_matrix(best_models=output2$models_list,test_data=output2$test_data,observed=output2$test_observed)
# forester::draw_radar_plot(score_frame=output2$score_test,type="classification")

pdf(glue::glue(project_name,"best.model_draw_feature_importance_plot.pdf"))
draw_feature_importance_plot
dev.off() 
pdf(glue::glue(project_name,"best.model_draw_Roc_plot.pdf"))
draw_Roc_plot
dev.off() 
pdf(glue::glue(project_name,"best.draw_Confusion_matrix.pdf"))
draw_Confusion_matrix
dev.off() 


saveRDS(output2,glue::glue(project_name,"best.model_",best_model_name,".rds"))
write.csv(output2$score_test,glue::glue(project_name,"different.models.scores.csv"),row.names = F)