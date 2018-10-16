# for example I have 4 H2OModels
list(logit_fit,dt_fit,rf_fit,xgb_fit) %>% 
  # map a function to each element in the list
  map(function(x) x %>% h2o.performance(valid=T) %>% 
        # from all these 'paths' in the object
        .@metrics %>% .$thresholds_and_metric_scores %>% 
        # extracting true positive rate and false positive rate
        .[c('tpr','fpr')] %>% 
        # add (0,0) and (1,1) for the start and end point of ROC curve
        add_row(tpr=0,fpr=0,.before=T) %>% 
        add_row(tpr=0,fpr=0,.before=F)) %>% 
  # add a column of model name for future grouping in ggplot2
  map2(c('Logistic Regression','Decision Tree','Random Forest','Gradient Boosting'),
       function(x,y) x %>% add_column(model=y)) %>% 
  # reduce four data.frame to one
  reduce(rbind) %>% 
  # plot fpr and tpr, map model to color as grouping
  ggplot(aes(fpr,tpr,col=model))+
  geom_line()+
  geom_segment(aes(x=0,y=0,xend = 1, yend = 1),linetype = 2,col='grey')+
  xlab('False Positive Rate')+
  ylab('True Positive Rate')+
  ggtitle('ROC Curve for Four Models')