library(dplyr)
library(ggplot2)
library(forcats)

merged_common_words <- read.csv("Magdalena Jeczeń_merged_common_words.csv", sep = ",")
merged_message <- read.csv("Magdalena Jeczeń_merged_message.csv", sep = ",")
merged_time <- read.csv("Magdalena Jeczeń_merged_time.csv", sep = ",")

merged_common_words <- na.omit(merged_common_words)
##### most used words only me ######
mine_most_used_words <- merged_common_words %>% 
  group_by(word) %>% 
  summarise(my_count = sum(my_count)) %>% 
  arrange(desc(my_count))

##### most used words me vs other_person#####
example_file_name <- "Łukasz Grabarski"
most_used_words <- merged_common_words %>% 
  filter(file == example_file_name) %>% 
  group_by(word) %>% 
  summarise(my_count = sum(my_count), other_count = sum(other_count)) %>% 
  arrange(desc(my_count))
## bad words - if anyone is interested :-)
merged_common_words %>% 
  group_by(word) %>% 
  summarise(my_count = sum(my_count)) %>% 
  filter(word == "chuj" | word == "kurwa" | word == "pierdole" | word == "fuck" | word == "jprdl" | word == "jebać" | word == "jebac" | word == "gówno" | word == "gowno") %>% 
  arrange(desc(my_count))

##### participation in groups #####
merged_message %>% 
  filter(participants > 2) %>% 
  group_by(file) %>% 
  mutate(participants_grouped = participants) %>% 
  summarise(p_of_my_mess = (sum(my_messages)/sum(all_messages))*100, participants, participants_grouped) %>% 
  head(20)

merged_message_to_work <- merged_message
merged_message_to_work <- merged_message_to_work %>% 
  filter(participants != 1) %>% 
  mutate(participants_grouped = participants) 
  

merged_message_to_work$participants <- as.numeric(merged_message_to_work$participants)
merged_message_to_work$participants <- as.integer(merged_message_to_work$participants)
typeof(merged_message_to_work$participants_grouped)
merged_message_to_work[merged_message_to_work$participants_grouped == 2, 'participants_grouped'] <- 2
merged_message_to_work[merged_message_to_work$participants_grouped >2 & merged_message_to_work$participants_grouped <=5, 'participants_grouped'] <- 3
merged_message_to_work[merged_message_to_work$participants_grouped >5 & merged_message_to_work$participants_grouped <=10, 'participants_grouped'] <- 6
merged_message_to_work[merged_message_to_work$participants_grouped >10 & merged_message_to_work$participants_grouped <=20, 'participants_grouped'] <- 10
merged_message_to_work[merged_message_to_work$participants_grouped >20 , 'participants_grouped'] <- 20

merged_message_to_work$participants_grouped <- as.character(merged_message_to_work$participants_grouped)
typeof(merged_message_to_work$participants_grouped)

merged_message_to_work[merged_message_to_work$participants_grouped == "2", 'participants_grouped'] <- "2"
merged_message_to_work[merged_message_to_work$participants_grouped == "3", 'participants_grouped'] <- "[3,5]"
merged_message_to_work[merged_message_to_work$participants_grouped == "6", 'participants_grouped'] <- "[6,10]"
merged_message_to_work[merged_message_to_work$participants_grouped == "10", 'participants_grouped'] <- "[11,20]"
merged_message_to_work[merged_message_to_work$participants_grouped == "20", 'participants_grouped'] <- ">20"

##### PLOT1 #####
p1 <- merged_message_to_work %>% 
  group_by(participants_grouped) %>% 
  summarise(p_of_my_mess = (sum(my_messages)/sum(all_messages))*100) %>% 
  ggplot(aes(x = fct_relevel(participants_grouped, c("2","[3,5]","[6,10]","[11,20]",">20")), y = p_of_my_mess) )+
  geom_col(fill = "skyblue") + 
  theme_minimal() + 
  labs(title = "Udział procentowy moich wiadomości w konwersacjach zależnie od liczby osób w grupie",
       x = "Liczba osób",
       y = "% moich wiadomości") + 
 theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

  
p1


##### person with most messages overall #####
merged_message %>% 
  filter(participants == 2) %>% 
  group_by(file) %>% 
  summarise(n_of_messages = sum(all_messages)) %>% 
  arrange(desc(n_of_messages))

merged_message %>% 
  filter(participants == 2) %>% 
  group_by(file) %>% 
  summarise(precent_of_my_messages = (sum(my_messages)/sum(all_messages))*100, my_messages = sum(my_messages)) %>% 
  arrange(desc(my_messages))

##### write word to find what position in ranking it holds #####
example_word <- "kupa"

most_used_words %>% 
  mutate(rank = row_number()) %>% 
  filter(word == example_word) 

##### time fixing
merged_time_to_work <- merged_time 

merged_time_to_work$date <- strtrim(merged_time_to_work$date, 19) # deleting miliseconds

merged_time_to_work <- merged_time_to_work %>% 
  mutate(date_only = as.Date(date)) %>% 
  mutate(time_only = date) %>% 
  mutate(hour_only = date) %>% 
  mutate(year_only = date) %>% 
  mutate(month_only = date) %>% 
  mutate(day_only = date)

merged_time_to_work$time_only <- format(as.POSIXct(merged_time_to_work$time_only), 
                                        format = "%H:%M:%S")
merged_time_to_work$hour_only <- format(as.POSIXct(merged_time_to_work$hour_only),
                                        format = "%H")
merged_time_to_work$year_only <- format(as.POSIXct(merged_time_to_work$year_only),
                                        format = "%Y")
merged_time_to_work$month_only <- format(as.POSIXct(merged_time_to_work$month_only),
                                        format = "%m")
merged_time_to_work$day_only <- format(as.POSIXct(merged_time_to_work$day_only),
                                        format = "%d")
     
merged_time_to_work$year_only <- as.numeric(merged_time_to_work$year_only) 
merged_time_to_work$year_only <- as.integer(merged_time_to_work$year_only) 
typeof(merged_time_to_work$year_only)

merged_time_to_work$month_only <- as.numeric(merged_time_to_work$month_only) 
merged_time_to_work$month_only <- as.integer(merged_time_to_work$month_only) 
typeof(merged_time_to_work$year_only)

merged_time_to_work$day_only <- as.numeric(merged_time_to_work$day_only) 
merged_time_to_work$day_only <- as.integer(merged_time_to_work$day_only) 
typeof(merged_time_to_work$day_only)

merged_time_to_work$hour_only <- as.numeric(merged_time_to_work$hour_only) 
merged_time_to_work$hour_only <- as.integer(merged_time_to_work$hour_only) 
typeof(merged_time_to_work$hour_only)



merged_time_to_work %>% 
  group_by(date_only) %>% 
  summarise(count_messages = n()) %>% 
  arrange(desc(count_messages))

merged_time_to_work %>% 
  group_by(date_only, hour_only) %>% 
  summarise(n_of_messages = n()) %>% 
  arrange(hour_only)


merged_time_to_work %>% 
  group_by(hour_only) %>% 
  summarise(n_of_messages = n()) %>% 
  arrange(hour_only)


###### PLOT2: Liczba wiadomości zależnie od h
example_year_from <- 2021
example_year_to <- 2022
example_month_from <- 07
example_month_to <- 09


p_2 <- merged_time_to_work %>% 
  filter(year_only >= example_year_from) %>% 
  filter(year_only <= example_year_to) %>% 
  filter(month_only >= example_month_from) %>% 
  filter(month_only <= example_month_to) %>% 
  group_by(hour_only) %>% 
  summarise(n_of_messages = n()) %>% 
  arrange(hour_only) %>% 
  ggplot(aes(x = hour_only, y = n_of_messages)) +
  geom_col(fill = "lightblue") + 
  theme_minimal() +
  scale_x_continuous(breaks = 0:23) + 
  labs(title = "Liczba wiadomości zależnie od h",
       x = "Godzina",
       y = "Liczba wiadomości") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
p_2

### tutaj density plot ale moim zdaniem źle wygląda
p_3 <- merged_time_to_work %>% 
  filter(year_only >= example_year_from) %>% 
  filter(year_only <= example_year_to) %>% 
  filter(month_only >= example_month_from) %>% 
  filter(month_only <= example_month_to) %>% 
  group_by(hour_only) %>%
  ggplot(aes(x = hour_only)) + 
  geom_density(fill = "pink") + 
  theme_minimal()
p_3


##### respond_time - choose conversation #####
example_Name_Surname <- "Łukasz Grabarski"
merged_message %>% 
  filter(mean_my_res_time > 0) %>% 
  filter(file == example_Name_Surname) %>% 
  mutate(my_time_in_seconds = mean_my_res_time%/%1000) %>% 
  mutate(other_time_in_seconds = mean_other_res_time%/%1000) %>% 
  select(file, my_time_in_seconds, other_time_in_seconds)


#### ??????? co tutaj dudes..
## same minute??

## same hour
merged_message %>% 
  filter(mean_my_res_time > 0) %>% 
  filter(mean_my_res_time%/%36000000 < 1 & mean_other_res_time%/%36000000 < 1) %>% 
  mutate(my_time_in_seconds = mean_my_res_time%/%1000) %>% 
  mutate(other_time_in_seconds = mean_other_res_time%/%1000) %>% 
  head

## same day

