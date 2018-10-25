#truth: integer vector with truth labels, values range from 0 to n - 1 classes
# prob_matrix: predicted probs: column 1 => label 0, column 2 => label 1 and so on
Multiclasslogloss = function(truth, pred_prob_matrix, eps = 1e-15){
  
  if(is.double(truth)=="FALSE"){
    
    truth=as.numeric(truth)-1 
    
  }
  
  
  if(max(truth) >= ncol(pred_prob_matrix) || min(truth) < 0){
    stop(cat('True labels should range from 0 to', ncol(pred_prob_matrix) - 1, '\n'))
  }
  
  
  
  
  
  
  pred_prob_matrix[pred_prob_matrix > 1 - eps] = 1 - eps
  pred_prob_matrix[pred_prob_matrix< eps] = eps
  pred_prob_matrix = t(apply(pred_prob_matrix, 1, function(r)r/sum(r)))
  truth_matrix = matrix(0, nrow = nrow(pred_prob_matrix), ncol = ncol(pred_prob_matrix))
  truth_matrix[matrix(c(1:nrow(pred_prob_matrix), truth + 1), ncol = 2)] = 1
  -sum(truth_matrix * log(pred_prob_matrix))/nrow(pred_prob_matrix)
}

