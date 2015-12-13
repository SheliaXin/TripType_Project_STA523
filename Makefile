all: writeup.html

writeup.html: writeup.Rmd newData_train_without_F.RData newData_test_without_F.RData submission.csv submission_r.csv
	Rscript -e "library(rmarkdown);render('writeup.Rmd')"
     
newData_train_without_F.RData: datacleaning.R
	R --no-save < datacleaning.R

newData_test_without_F.RData: datacleaning.R
	R --no-save < datacleaning.R

submission.csv: SVM_Models.R
	R --no-save < SVM_Models.R


submission_r.csv: SVM_Models.R
	R --no-save < SVM_Models.R      

clean:
	rm -f writeup.html

.PHONY: all clean
