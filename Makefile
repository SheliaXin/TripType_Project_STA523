all: 
     
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


