info_by_cats<-lapply(split(df, f = list(df$l2_name,
					df$seller_category)), function(x){
  cat_name<-levels(x$l2_name)[1]
  df_t_means<-apply(x[,c(4,6,7)], 2, mean) %>% 
    as.data.frame() %>% 
    t()
  df_t_sds<-apply(x[,c(4,6,7)], 2, sd) %>% 
    as.data.frame() %>% 
    t()
  df_t_means %<>% 
    cbind(df_t_sds)
  rownames(df_t_means)<-c(cat_name)
  df_t_means %>% t
}) %>% as.data.frame() %>% t %>%  as.data.frame() 

info_by_cats$l2_name<-gsub(x = rownames(info_by_cats),pattern = '[.]',replacement = ' ')

colnames(info_by_cats)<-c('mean_cnt_avg_ads','mean_avg_arpu','mean_recency',
                          'std_cnt_avg_ads','std_avg_arpu','std_recency','l2_name')

