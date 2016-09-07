# start mongod instance in adimin command prompt

library(rmongodb) # load libraries
library(plyr)
library(dplyr)


mongo <- mongo.create(host = 'localhost') # connect to mongod

mongo.is.connected(mongo) # check connection

mongo.get.databases(mongo) # check contents of db
mongo.get.database.collections(mongo, 'FF2')

mongo.count(mongo, "FF.kelp_classifications")
mongo.find.one(mongo, "FF2.kelp_subjects") # display first entry



#the below code does not work, 

##### convert .bson to a data.frame (more workable for reprojection work) #####

#this takes time, 20+  minutes on my laptop

subjects = data.frame(StringAsFactors = FALSE)
DBNS = "FF2.kelp_subjects"

cursor3 = mongo.find(mongo, DBNS)

i = 1

while (mongo.cursor.next(cursor3)) {
  tmp3 = mongo.bson.to.list(mongo.cursor.value(cursor3))
  tmp.df = as.data.frame(t(unlist(tmp3)), stringAsFactors = F)
  subjects = rbind.fill(subjects, tmp.df)
}


