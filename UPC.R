library(stringr)
library(stringi)

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

upc_man = rep(NA, length(upc_vec))
upc_fam =  rep(NA, length(upc_vec))
upc_val =  rep(NA, length(upc_vec))
upc_cou = rep(NA, length(upc_vec))
for(k in 1:length(upc_vec))
{
  if(is.na(upc_vec[k]))
  {}
  else
  if(str_length(upc_vec[k]) == 10)
  {
    upc_man[k] = stri_sub(upc_vec[k],1,5)
    upc_fam[k] = stri_sub(upc_vec[k],6,8)
    upc_val[k] = stri_sub(upc_vec[k],9,10)
  }
  else
  if(str_length(upc_vec[k]) == 11)
    {
      upc_man[k] = stri_sub(upc_vec[k],2,6)
      upc_fam[k] = stri_sub(upc_vec[k],7,9)
      upc_val[k] = stri_sub(upc_vec[k],10,11)
    }
  else
  if(str_length(upc_vec[k]) == 12)
    {
      upc_man[k] = stri_sub(upc_vec[k],2,6)
      upc_fam[k] = stri_sub(upc_vec[k],7,9)
      upc_val[k] = stri_sub(upc_vec[k],10,11)
      upc_cou[k] = stri_sub(upc_vec[k],12,12)
    }
}

data_cleaned_upc = data
data_cleaned_upc$manuf_cod = upc_man
data_cleaned_upc$fam_cod = upc_fam
data_cleaned_upc$val_cod = upc_val
data_cleaned_upc$coup = upc_cou

