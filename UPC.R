#looking at UPC

library(stringr)
library(stringi)

#reading in the training data
data = read.csv("train.csv")

#extracting the Upc column
upc_vec = data$Upc
head(upc_vec)
length(upc_vec)

apply(as.matrix(upc_vec),1, str_length)

#initialising length of UPC vector with NAs
upc_len = rep(NA, length(upc_vec))

#length vector
for(i in 1:length(upc_vec))
{
    upc_len[i] = str_length(upc_vec[i])
}

#finding the number of UPC codes for each of the lengths
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

#initialising vectors for manufacturer code, family code, value code, and coupon
upc_man = rep(NA, length(upc_vec))
upc_fam =  rep(NA, length(upc_vec))
upc_val =  rep(NA, length(upc_vec))
upc_cou = rep(NA, length(upc_vec))

#filling in the vectors for manufacturer code, family code, value code and coupon
#depending on the length of the UPC code
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

#adding in the manufacturer code, family code, value code, and coupon columns to the data
data_cleaned_upc = data
data_cleaned_upc$manuf_cod = upc_man
data_cleaned_upc$fam_cod = upc_fam
data_cleaned_upc$val_cod = upc_val
data_cleaned_upc$coup = upc_cou

