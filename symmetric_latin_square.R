####symmetric latin square of order 2n,  
symmetric.latin.square <- function(n)
{
  ###start with an (n-1) * (n-1) matrix
  temp_addition_table <- matrix(NA, nrow = 2*n-1, ncol = 2*n-1)  #A[i,j] = A[i,1] + A[1,j] module (2n-1) (i>1, j>1)
  temp_addition_table[1,] <- 0:(2*n-2) #initialization
  temp_addition_table[,1] <- 0:(2*n-2)
  for(i in 2:(2*n-1))
    for(j in 2:(2*n-1))
    {
      temp_addition_table[i,j] <- (temp_addition_table[i,1] + temp_addition_table[1,j]) %% (2*n-1)
    }
  
  #move the diagonal entries of temp_addition_table 
  temp_diag <- diag(temp_addition_table)
  
  #latin square matrix
  temp_latin_square <- matrix(NA, nrow = 2*n, ncol = 2*n)
  
  temp_latin_square[1:(2*n-1), 1:(2*n-1)] <- temp_addition_table
  
  #work on last column
  for(i in 1:(2*n-1) )
  {
    temp_latin_square[i,2*n] <- ifelse(temp_diag[i] == 0, 2*n-1, temp_diag[i])
  }
  
  #work on last row
  for(j in 1:(2*n-1))
  {
    temp_latin_square[2*n, j] <- ifelse(temp_diag[j] == 0, 2*n-1, temp_diag[j])
  }
  
  #deal with the upper left to bottom right
  diag(temp_latin_square) <- 0
  
  #deal with the upper right to bottom left
  for(k in 1:(2*n))
  {
    temp_latin_square[k, 2*n+1 -k] <- 2*n-1
  }
  
  return(temp_latin_square)
}

