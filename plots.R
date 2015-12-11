library(dplyr)

test = read.csv("test.csv")
train = read.csv("train.csv")

visit_grouped = group_by(test, VisitNumber)
size_trip = summarise(visit_grouped, n_items = sum(ScanCount))

weekday = test[,c("VisitNumber", "Weekday")] %>%
          unique()
weekday_joined = left_join(x = size_trip, y = weekday, 
	by = "VisitNumber")

# what the person can change, min and max of basket 
min = 20
max = 400

# proportion that are within a certain size
big_baskets = filter(weekday_joined, n_items > min & n_items < max)
grouped = group_by(weekday_joined, Weekday)
grouped_big = group_by(big_baskets, Weekday)

total_df = summarise(grouped, total = n())
big_df = summarise(grouped_big, big = n())
joined = left_join(total_df, big_df, by = "Weekday")


#proportion of basket size, which changes based on min and max
percent = mutate(joined, prop = big/total)

percent = arrange(percent, Weekday)
arranged = rbind(percent[2,], percent[6,], percent[7,],
	percent[5,], percent[1,], percent[3,], percent[4,])

# plot proportion, which can change 
barplot(arranged$prop, names.arg = c("Mon", "Tues", "Wed",
	"Thurs", "Fri", "Sat", "Sun"))
