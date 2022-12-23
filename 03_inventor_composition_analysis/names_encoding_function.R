###################################
# function for one-hot-encoding a #
# sequence of names at the level  #
# of characters. The result is a  #
# 3D-Tensor of encoded names      #
###################################

encode_chars <- function(names, seq_max, char_dict, n_chars){
  
  N <- length(names)
  END_idx <- which(char_dict == "END")
  
  # Create 3D-Tensor with shape (No. of samples, maximum name length, number of characters):
  tmp <- array(rep(0, times = (N * n_chars * seq_max)), 
               dim = c(N, seq_max, n_chars)
  ) 
  
  # iterate over all names
  for(i in 1:N){
    name <- names[[i]]
    
    # truncate at seq_max:
    if(nchar(name) > seq_max){name <- substr(name, 1, seq_max)}
    
    # encode characters:
    for (char in 1:nchar(name)) {
      idx_pos <- which(char_dict == substr(name, char, char))
      tmp[i, char, idx_pos] <- 1
    }
    
    # padding:
    if(nchar(name) < seq_max){
      tmp[i, seq(nchar(name)+1, seq_max), END_idx] <- 1
    }
  }
  
  return(tmp)
}
