# broken stick
broken_stick = function(P) {
  sequence = 1:P
  divided = 1/sequence
  seq_sums = sapply(sequence,FUN=function(x){
    subset = divided[x:P]
    subset_sum = sum(subset)
    i = subset_sum/P
    return(i)
  })
  return(seq_sums)
}

broken_stick(52)