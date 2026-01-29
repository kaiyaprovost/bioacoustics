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

broken_stick(20)

my_pc_importance = c(0.9935,0.00607,0.00043)

plot(broken_stick(3),my_pc_importance,xlim=c(0,1),ylim=c(0,1))
abline(0,1)
