

library(readxl)
library(dplyr)
#install.packages("stringdist")
#install.packages("dendextend")
library(stringdist)
library(dendextend)

#Reading the excel file 

df <- read_excel("28072023_exported study area GIS VOA data_NC.xlsx", sheet = 2)

# Creating the string similarity function

jaccard_similarity <- function(str1, str2) {
  set1 <- strsplit(str1, "\\s+")[[1]]
  set2 <- strsplit(str2, "\\s+")[[1]]
  intersect_size <- length(intersect(set1, set2))
  union_size <- length(union(set1, set2))
  return(intersect_size / union_size)
}

contains_only_numeric <- function(string) {
  num <- grepl("^[0-9&/-]+$", string)
  num_alphabets <- gregexpr("[A-Za-z]", string, ignore.case = TRUE)
  num_matches <- sum(unlist(lapply(num_alphabets, function(x) length(x[x > 0]))))
  return(num_matches == 1 | num)
}

#Introducing the new column

df$Address_Code <- NA

#Categorizing the rows based on same address
  
for (i in 1:nrow(df)) {
  if (contains_only_numeric(df$Number_or_[i])) {
    df$Address_Code[i] <- "Numeric"
  }
  if (is.na(df$Address_Code[i])) {
      df$Address_Code[i] <- i
    
    for (z in (i + 1):nrow(df)) {
      if(df$Postcode[z]==df$Postcode[i]){
      if (jaccard_similarity(df$Number_or_[z], df$Number_or_[i]) >= 0.1) {
        df$Address_Code[z] <- df$Address_Code[i]
      }
      }
    }

  print(i)
}
}

#Evaluating the count of different market segments

count_data_1 <- df %>%
  group_by(`ZEG 1`,Address_Code) %>%
  summarise(count=n(), cp_sapce = sum(CP_Spaces))


count_data <- count_data_1 %>%
  group_by(`ZEG 1`) %>%
  summarise(`Number of offices(%)` = sum(ifelse(Address_Code != "Numeric" & count>1,count, 0))*100/sum(count),`Number of non-offices(%)`= sum(ifelse(Address_Code=="Numeric"| count==1,count,0))*100/sum(count),`Number of offices(Sum CP_Spaces)` = sum(ifelse(Address_Code != "Numeric" & count>1,cp_sapce, 0)),`Number of non-offices(Sum CP_Spaces)`= sum(ifelse(Address_Code=="Numeric"| count==1,cp_sapce,0)))


#Making the key csv files

x <- df[,c("Address_Code","Number_or_","ZEG 1","CP_Spaces","Postcode")]

write.csv(count_data, file = "output_differemt_business_segment_counts.csv", row.names = FALSE)

write.csv(x, file = "output_key.csv", row.names = FALSE)

write.csv(count_data_1, file = "output_Check_data.csv", row.names = FALSE)

#Counting the distinct addresses

distinct_count <- df %>%
  summarise(Name_Count = n_distinct(Address_Code))

print(distinct_count)


  
# just for Explanation 
#a<- "PT 5TH FLR RHS 50/60"

#b<-"PT 3RD FLR FRONT RHS 50/60"

#set1 <- strsplit(a, "\\s+")[[1]]
#set2 <- strsplit(b, "\\s+")[[1]]

#intersect_size <- length(intersect(set1, set2))
#union_size <- length(union(set1, set2))

