library(stringr)

data = read.csv("train.csv")

upc_vec = data$Upc
head(upc_vec)
length(upc_vec)

apply(as.matrix(upc_vec),1, str_length)

upc_len = rep(NA, length(upc_vec))
for(i in 1:length(upc_vec))
{
    upc_len[i] = str_length(upc_vec[i])
}

upc_len_num = rep(0, 12)
for(j in 1:12)
{
  for(i in 1:length(upc_vec))
  {
    if(is.na(str_length(upc_vec[i])))
    {}
    else
    if(str_length(upc_vec[i]) == j)
    {
      upc_len_num[j] = upc_len_num[j] + 1
    }
  }
}
