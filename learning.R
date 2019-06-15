state = c("CA","WA","OR","AZ")
first = c("Jim","Mick","Paul","Ron")
df1 <- data.frame(first, state, stringsAsFactors = FALSE)
state = c("CA","WA")
newstate = c("TX", "LA")
first =c("Jim","Mick")
df2 <- data.frame(first, state, newstate, stringsAsFactors = FALSE)
df1
df2
indx <- match(df1$first, df2$first, nomatch = 0)
df1$state[indx != 0] <- df2$newstate[indx]
df1
library(data.table)
DT1 <- as.data.table(df1)
DT2 <- as.data.table(df2)


setkey(DT1, first, state)
setkey(DT2, first, state)
DT2[DT1]
DT1[DT2]
DT2[DT1, nomatch=0]
DT1[DT2, nomatch=0]

name = c("Braund, Mr. Owen Harris",
         "Cumings, Mrs. John Bradley (Florence Briggs Thayer)")

grepl("\\(.*?\\)", name)

library(dplyr)

demo <- tribble(
  ~a, ~b,
  "bar_1",20,
  "bar_2",30,
  "bar_3",40
)m      
demo

library(ggplot2)
ggplot(data=demo)+geom_bar(aes(x=a,y=b),stat = "identity")

ggplot(data=diamonds)+geom_bar(aes(x=cut,y=..count..,group=1))

ggplot(data=diamonds)+stat_summary(aes(x=cut,y=depth),
                                   fun.ymin = min,
                                   fun.ymax=max,
                                   fun.y=median)

###Plot 1
ggplot(data = diamonds) +
  geom_pointrange(
    mapping = aes(x = cut, y = depth),
    stat = "summary",
    fun.ymin = min
    ,
    fun.ymax = max,
    fun.y = median
  )

###Plot 2
ggplot(data=diamonds) +
  stat_summary(aes(x=cut,y=depth
  ),
  fun.ymin = min,
  fun.ymax = max,
  fun.y = median)


ggplot(data = diamonds) +
  geom_pointrange(
    mapping = aes(x = cut, y = depth),
    stat = "summary"
  )
#No summary function supplied, defaulting to `mean_se()

a <- select(iris, -Petal.Length, everything())
b <- select(iris, Petal.Width, -Petal.Length, everything())
select(iris, Species, everything())
names(iris)


library(nycflights13)
not_canceled <- flights %>%
  filter(!is.na(dep_delay), !is.na(arr_delay))

#Method 1
not_canceled %>%
  group_by(tailnum) %>%
  summarise(n = sum(distance))
#Method 2
not_canceled %>%
  group_by(tailnum) %>%
  tally(distance)
#Method 1 and Method 2 are same


not_canceled %>%
  group_by(dest) %>%
  summarise(n = length(dest))#same as summarise(n=n())
#

not_canceled %>%
  group_by(dest) %>%
  summarise(n = n())

canceled_delayed <-
  flights %>%
  mutate(canceled = (is.na(arr_delay) | is.na(dep_delay))) %>%
  group_by(year, month, day) %>%
  summarise(prop_canceled = mean(canceled),
            avg_dep_delay = mean(dep_delay, na.rm = TRUE))

ggplot(canceled_delayed, aes(x = avg_dep_delay, prop_canceled)) +
  geom_point() +
  geom_smooth()


lagged_delays <- flights %>%
  arrange(origin, year, month, day, dep_time) %>%
  group_by(origin) %>%
  mutate(dep_delay_lag = lag(dep_delay)) %>%
  filter(!is.na(dep_delay), !is.na(dep_delay_lag))

a <- flights %>%
  arrange(tailnum, year, month, day) %>%
  group_by(tailnum) %>%
  mutate(delay_gt1hr = dep_delay > 60) %>%
  mutate(before_delay = cumsum(delay_gt1hr)) %>%
  filter(before_delay < 1) %>%
  count(sort = TRUE)

library(tidyverse)
library(viridis)
library(forcats)
library(dplyr)
library(tidyr)

##EDA
names(diamonds)
diamonds %>%
  mutate(id = row_number()) %>%
  select(x, y, z, id) %>%
  gather(variable, value, x:z)  %>%
  ggplot(aes(x = value)) +
  geom_density() +
  geom_rug() +
  facet_grid(variable ~ .)


ggplot(data=diamonds,aes(x=price))+
  geom_histogram(binwidth = 10,center=0)
 

ggplot(data=diamonds,aes(x=price))+
  geom_histogram(binwidth = 1,center=0)+
  geom_bar()

diamonds %>%
  filter(carat>=0.99 & carat <=1) %>%
  count(carat)

ggplot(diamonds) +
  geom_histogram(mapping = aes(x = price)) +
  coord_cartesian(xlim = c(100, 5000), ylim = c(0, 3000))

ggplot(diamonds) +
  geom_histogram(mapping = aes(x = price),bins=30) +
  xlim(100, 5000)+ylim(0, 3000)


library(nycflights13)
##8.5

flights %>%
mutate(cancelled=is.na(dep_time),
       sched_hour = sched_dep_time %/% 100,
       sched_min = sched_dep_time %% 100,
       sched_dep_time = sched_hour + sched_min / 60) %>%
  ggplot(aes(x=cancelled,y=sched_dep_time)) +
  geom_boxplot()

names(diamonds)
##carat is related to price
diamonds %>%
  ggplot(aes(x=carat,y=price))+
  geom_point()

#
diamonds %>%
  ggplot(aes(x=carat,y=price))+
  geom_boxplot(mapping = aes(group = cut_width(carat, 0.1)))

diamonds %>%
  ggplot(aes(x=cut,y=price))+
  geom_boxplot()


diamonds %>%
  ggplot(aes(x=color,y=price))+
  geom_boxplot()

diamonds %>%
  ggplot(aes(x=clarity,y=price))+
  geom_boxplot()

library(ggstance)
ggplot(data = mpg) +
  geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy)) +
  coord_flip()

ggplot(data = mpg) +
  geom_boxploth(mapping = aes(y = reorder(class, hwy, FUN = median), x = hwy))

library(lvplot)
ggplot(diamonds,aes(x=cut,y=price))+
geom_lv()


library(viridis)

##Calculate the proportion
diamonds %>%
  count(color, cut) %>%
  group_by(color) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(mapping = aes(x = color, y = cut)) +
  geom_tile(mapping = aes(fill = prop)) +
  scale_fill_viridis(limits = c(0, 1)) 


diamonds %>%
  count(color, cut) %>%
  group_by(cut) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(mapping = aes(x = color, y = cut)) +
  geom_tile(mapping = aes(fill = prop)) +
  scale_fill_viridis(limits = c(0, 1)) 


flights %>%
  group_by(dest,month) %>%
  mutate(avg_delay=mean(dep_delay,na.rm = T)) %>%
  ggplot(aes(x=factor(month),y=dest))+
  geom_tile(aes(fill=avg_delay))

flights %>%
  group_by(month, dest) %>%
  summarise(dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  group_by(dest) %>%
  #only filter the destination have 12 months, otherwise will be filtered. 
  filter(n() == 12) %>%
  ungroup() %>%
  mutate(dest = reorder(dest, dep_delay)) %>%
  ggplot(aes(x = factor(month), y = dest, fill = dep_delazy)) +
  geom_tile()+
  scale_fill_viridis()

#
library(hexbin)
ggplot(data=diamonds)+
  geom_bin2d(aes(x=carat,y=price))


library(hexbin)
ggplot(data=diamonds)+
  geom_hex(aes(x=carat,y=price))

#
read_csv2("a;b\n1;3")
read_csv("a;b\n1;3")

read_csv("a,b\n1,2,3\n4,5,6")
read_csv("a,b,c\n1,2\n1,2,3,4")
read_csv("a,b\n\"1")
read_csv("a,b\n1,2\na,b")
read_csv("a;b\n1;3")


d1 <- "January 1, 2010"
d2 <- "2015-Mar-07"
d3 <- "06-Jun-2017"
d4 <- c("August 19 (2015)", "July 1 (2015)")
d5 <- "12/30/14" # Dec 30, 2014
t1 <- "1705"
t2 <- "11:15:10.12 PM"

parse_date(d1,"%B %d, %Y")
parse_date(d2,"%Y-%b-%d")
parse_date(d3,"%d-%b-%Y")
parse_date(d4,"%B %d (%Y)")
parse_date(d5,"%m/%d/%y")
parse_time(t1, "%H%M")
parse_time(t2,"%H:%M:%OS %p")

library(tidyverse)

table2
##pull the case number from the data
case <- table2 %>% 
  filter(type=="cases") %>% 
  arrange(country,year)

##pull the population number from the data
pop <- table2 %>% 
  filter(type=="population") %>% 
  arrange(country,year)

data <- merge(case,pop,by = c("country","year"),all = T)
data <- data %>%
  mutate(rate=count.x/count.y) %>%
  select(country,year,rate)
data <- merge(table2,data,by = c("country","year"))

##
##calculate the rate
rate <- case %>%
  mutate(pop = pop$count,
         rate = count/pop) %>%
  select(country, year,rate) %>%
  mutate(type="Rate") %>%
  rename(count=rate)

#put them back to the table 2 
bind_rows(table2,rate) %>%
  arrange(country,year,type)


##Table 4a and table 4b

table4a
table4b
table4c <- tibble(country=table4a$country,
                      "1999"=table4a$`1999`/table4b$`1999`,
                      "2000"=table4a$`2000`/table4b$`2000`)

table2 %>% filter(type=="cases") %>%
  ggplot(aes(x=year,y=count))+
  geom_line(aes(group=country),colour="blue")+
  geom_point(aes(color=country))


##Chapter 9

stocks <- tibble(
  year   = c(2015, 2015, 2016, 2016),
  half  = c(   1,    2,     1,    2),
  return = c(1.88, 0.59, 0.92, 0.17)
)

stock_long <-stocks %>%
  spread(year, return) %>%
  gather(`2015`:`2016`, key = "year", value = "return")

table4a %>%
  gather(`1999`, `2000`, key = "year", value = "cases")

people <- tribble(
  ~name,             ~key,    ~value,
  #-----------------|--------|------
  "Phillip Woods",   "age",       45,
  "Phillip Woods",   "height",   186,
  "Phillip Woods",   "age",       50,
  "Jessica Cordero", "age",       37,
  "Jessica Cordero", "height",   156
)

spread(people, key, value)

#Spreading the data frame fails because there are two rows with 
#“age” for “Phillip Woods”. If we added another column with an indicator
#for the number observation it is, the code will work.

people <- tribble(
  ~name,             ~key,    ~value, ~obs,
  #-----------------|--------|------|------
  "Phillip Woods",   "age",       45, 1,
  "Phillip Woods",   "height",   186, 1,
  "Phillip Woods",   "age",       50, 2,
  "Jessica Cordero", "age",       37, 1,
  "Jessica Cordero", "height",   156, 1
)

preg <- tribble(
  ~pregnant, ~male, ~female,
  "yes",     NA,    10,
  "no",      20,    12
)

preg_tidy2 <- preg %>%
  gather(male, female, key = "sex", value = "count", na.rm = TRUE)
preg_tidy2



preg_tidy3 <- preg_tidy2 %>%
  mutate(female = sex == "female",
         pregnant = pregnant == "yes") %>%
  select(female, pregnant, count)
preg_tidy3


tibble(x = c("a,b,c", "d,e,f,g", "h,i,j")) %>%
  separate(x, c("one", "two", "three"))

tibble(x = c("a,b,c", "d,e", "f,g,i")) %>%
  separate(x, c("one", "two", "three"))

tibble(x = c("a,b,c", "d,e,f,g", "h,i,j")) %>%
  separate(x, c("one", "two", "three"), extra = "drop")

tibble(x = c("a,b,c", "d,e,f,g", "h,i,j")) %>%
  separate(x, c("one", "two", "three"), extra = "merge")

tibble(x = c("a,b,c", "d,e", "f,g,i")) %>%
  separate(x, c("one", "two", "three"), fill = "right")

tibble(x = c("X1", "X2", "Y1", "Y2")) %>%
  separate(x, c("variable", "into"), sep = c(1))

#The function extract() uses a regular expression to specify groups in character 
#vector and split that single character vector into multiple columns. 
#This is more flexible than separate() because it does not require a common 
#separator or specific column positions.

df_wide <- data.frame(
  ID = rep(1:2, each=10),
  JN = rep(1:2, each=5),
  Frame = rep(1:5, 4),
  System = rep(1:2, 10),
  RKX = rep(1:10+rnorm(10,mean=1,sd=0.5),2),
  RKY = rep(1:10+rnorm(10,mean=1,sd=0.5),2),
  RKZ = rep(1:10+rnorm(10,mean=1,sd=0.5), 2),
  LHeX = rep(1:10-rnorm(10,mean=1,sd=0.5),2),
  LHeY = rep(1:10-rnorm(10,mean=1,sd=0.5),2),
  LHeZ = rep(1:10-rnorm(10,mean=1,sd=0.5),2))

df_wide %>%
  gather(keys, values, -ID, -JN, -Frame, -System) %>%
  extract(keys, c("Joint", "Coord"), "(.*)(X|Y|Z)$") %>%
  spread(Coord, values)

who1 <- who %>%
  gather(new_sp_m014:newrel_f65, key = "key", value = "cases", na.rm = TRUE)
head(who2)

who2 <- who1 %>%
  mutate(key = stringr::str_replace(key, "newrel", "new_rel"))


who3 <- who2 %>%
  separate(key, c("new", "type", "sexage"), sep = "_")
who4 <- who3 %>%
  select(-new, -iso2, -iso3)

who5 <- who4 %>%
  separate(sexage, c("sex", "age"), sep = 1)
head(who5)



select(who3, country, iso2, iso3) %>%
  distinct() %>%
  group_by(country) %>%
  filter(n() > 1)

who5 %>%
  group_by(country, year, sex) %>%
  filter(year > 1995) %>%
  summarise(cases = sum(cases)) %>%
  unite(country_sex, country, sex, remove = FALSE) %>%
  ggplot(aes(x = year, y = cases, group = country_sex, colour = sex)) +
  geom_line()

##Joins
flights %>%
  anti_join(planes, by = "tailnum") %>%
  count(carrier, sort = TRUE)

planes_gt100 <-
  filter(flights) %>%
  group_by(tailnum) %>%
  count() %>%
  filter(n > 100)

head(planes_gt100)

flights %>%
  semi_join(planes_gt100, by = "tailnum")

fueleconomy::vehicles %>%
  semi_join(fueleconomy::common, by = c("make", "model"))

flights %>%
  group_by(year, month, day) %>%
  summarise(total_24 = sum(dep_delay, na.rm = TRUE)+ sum(arr_delay, na.rm = TRUE)) %>%
  mutate(total_48 = total_24 + lag(total_24),
         test=lag(total_24)) %>%
  arrange(desc(total_48))

multi_carrier_planes <-
  flights %>%
  filter(!is.na(tailnum)) %>%
  count(tailnum, carrier) %>%
  count(tailnum) %>%
  filter(nn > 1)
#because we count twice so there is nn not n
multi_carrier_planes
#There are 17 airplanes in this dataset that have had more than one carrier.

#

library(forcats)
names(gss_cat)
library(ggplot2)
library(dplyr)
gss_cat %>% ggplot(aes(rincome))+
  geom_bar()+
  theme(axis.text.x = element_text(angle = 90))+
  theme_bw()+
  coord_flip()

gss_cat %>%
  count(partyid) %>%
  arrange(-n) %>%
  head(1)

gss_cat %>%
  count(relig) %>%
  arrange(-n) %>%
  head(1)

summary(gss_cat)
class(gss_cat$rincome)

gss_cat %>% ggplot(aes(relig))+geom_bar()

gss_cat %>% ggplot(aes(partyid))+geom_bar()
library(readr)
parse_integer(c("1000", "$1,000", "10.00"))

library(stringr)
str_view(stringr::words, "^y", match =TRUE)
str_view(stringr::words, "x$", match = TRUE)
str_view(stringr::words, "^...$", match = TRUE)
str_view(stringr::words, "^.......", match = TRUE)


str_view(stringr::words, "^[aeiou]", match = TRUE)

str_view(stringr::words, "^[^aeiou]+$", match=TRUE)


str_view(stringr::words, "^ed$|[^e]ed$", match = TRUE)

str_view(stringr::words, "i(ng|se)$", match = TRUE)


str_view(stringr::words, "i(ng|se)$", match = TRUE)

#This regex finds all words starting with three consonants.
str_view(words, "^[^aeiou]{3}")

str_view(words, "[aeiou]{3,}")

str_view(words, "([aeiou][^aeiou]){2,}")


# one regex
words[str_detect(words, "^x|x$")]
#> [1] "box" "sex" "six" "tax"
# split regex into parts
start_with_x <- str_detect(words, "^x")
end_with_x <- str_detect(words, "x$")
words[start_with_x | end_with_x]
#> [1] "box" "sex" "six" "tax"

# one regex
words[str_detect(words, "^x|x$")]
#> [1] "box" "sex" "six" "tax"
# split regex into parts
start_with_x <- str_detect(words, "^x")
end_with_x <- str_detect(words, "x$")
words[start_with_x | end_with_x]
#> [1] "box" "sex" "six" "tax"

library(purrr)
pattern <-
  cross(rerun(5, c("a", "e", "i", "o", "u")),
          .filter = function(...) {
            x <- as.character(unlist(list(...)))
            length(x) != length(unique(x))
          }) %>%
  map_chr(~ str_c(unlist(.x), collapse = ".*")) %>%
  str_c(collapse = "|")


str_subset("aseiouds", pattern)
#> [1] "aseiouds"

str_subset(words, pattern)
#> character(0)

words[str_detect(words, "a") &
        str_detect(words, "e") &
        str_detect(words, "i") &
        str_detect(words, "o") &
        str_detect(words, "u")]


label <- tibble(
  displ = Inf,
  hwy = Inf,
  class = unique(mpg$class),
  label = stringr::str_c("Label for ", class)
)

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_text(aes(label = label), data = label, vjust = "top", hjust = "right",
            size = 3) +
  facet_wrap(~ class)

label <- tibble(
  displ = Inf,
  hwy = Inf,
  class = "2seater",
  label = "Increasing engine size is \nrelated to decreasing fuel economy."
)

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_text(aes(label = label), data = label, vjust = "top", hjust = "right",
            size = 2) +
  facet_wrap(~ class)


label <- tibble(
  displ = Inf,
  hwy = Inf,
  label = "Increasing engine size is \nrelated to decreasing fuel economy."
)

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_text(aes(label = label), data = label, vjust = "top", hjust = "right",
            size = 2) +
  facet_wrap(~ class)




df <- tibble(
  x = rnorm(10000),
  y = rnorm(10000)
)
ggplot(df, aes(x, y)) +
  geom_hex() +
  scale_colour_gradient(low = "white", high = "red") +
  coord_fixed()


ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = class)) +
  geom_smooth(se = FALSE) +
  scale_x_continuous("Engine displacement (L)") +
  scale_y_continuous("Highway fuel economy (mpg)") +
  scale_colour_discrete("Car type")


presidential %>%
  mutate(id = 33 + row_number(),
         name_id = stringr::str_c(name, " (", id, ")"),
         name_id = factor(name_id, levels = name_id)) %>%
  ggplot(aes(start, name_id, colour = party)) +
  geom_point() +
  geom_segment(aes(xend = end, yend = name_id)) +
  scale_colour_manual(values = c(Republican = "red", Democratic = "blue")) +
  scale_y_discrete(NULL) +
  scale_x_date(NULL, breaks = years, date_labels = "'%y") +
  theme(panel.grid.minor = element_blank())

library(nycflights13)
library(lubridate)
make_datetime_100 <- function(year, month, day, time) {
  make_datetime(year, month, day, time %/% 100, time %% 100)
}

flights_dt <- flights %>%
  filter(!is.na(dep_time), !is.na(arr_time)) %>%
  mutate(
    dep_time = make_datetime_100(year, month, day, dep_time),
    arr_time = make_datetime_100(year, month, day, arr_time),
    sched_dep_time = make_datetime_100(year, month, day, sched_dep_time),
    sched_arr_time = make_datetime_100(year, month, day, sched_arr_time)
  ) %>%
  select(origin, dest, ends_with("delay"), ends_with("time"))


sched_dep <- flights_dt %>%
  mutate(minute = minute(sched_dep_time)) %>%
  group_by(minute) %>%
  summarise(
    avg_delay = mean(arr_delay, na.rm = TRUE),
    n = n())

#How does the distribution of flight times within a day change over the course of the year?
flights_dt %>%
  mutate(time = hour(dep_time) * 100 + minute(dep_time),
         mon = as.factor(month
                         (dep_time))) %>%
  ggplot(aes(x = time, group = mon, colour = mon)) +
  geom_freqpoly(binwidth = 100)+
  theme_bw()

##Normalized
flights_dt %>%
  mutate(time = hour(dep_time) * 100 + minute(dep_time),
         mon = as.factor(month
                         (dep_time))) %>%
  ggplot(aes(x = time, y = ..density.., group = mon, colour = mon)) +
  geom_freqpoly(binwidth = 100)+
  theme_bw()

##dep_delay is in minutes, so we times 60 to give seconds. 
flights_dt %>%
  mutate(dep_time_ = sched_dep_time + dep_delay * 60) %>%
  filter(dep_time_ != dep_time) %>%
  select(dep_time_, dep_time, sched_dep_time, dep_delay)


#Compare air_time with the duration between the departure and arrival. Explain your findings.
flights_dt %>%
  mutate(flight_duration = as.numeric(arr_time - dep_time),
         air_time_mins = air_time,
         diff = flight_duration - air_time_mins) %>%
  select(origin, dest, flight_duration, air_time_mins, diff)



#How does the average delay time change over the course of a day? 
#Should you use dep_time or sched_dep_time? Why?
  
#Use sched_dep_time because that is the relevant metric for someone scheduling 
#a flight. Also, using dep_time will always bias delays to later in the day 
#since delays will push flights later.

flights_dt %>%
  mutate(sched_dep_hour = hour(sched_dep_time)) %>%
  group_by(sched_dep_hour) %>%
  summarise(dep_delay = mean(dep_delay)) %>%
  ggplot(aes(y = dep_delay, x = sched_dep_hour)) +
  geom_point() +
  geom_smooth()


#On what day of the week should you leave if you want to minimize the 
#chance of a delay?
  
#Sunday has the lowest average departure delay time and the lowest average 
#arrival delay time.

#5
flights_dt %>%
  mutate(dow = wday(sched_dep_time)) %>%
  group_by(dow) %>%
  summarise(dep_delay = mean(dep_delay),
            arr_delay = mean(arr_delay, na.rm = TRUE))

#6 What makes the distribution of diamonds$carat and flights$sched_dep_time similar?

ggplot(diamonds, aes(x = carat)) +
  geom_density()

ggplot(diamonds, aes(x = carat %% 1 * 100)) +
  geom_histogram(binwidth = 1)


#Confirm my hypothesis that the early departures of flights in minutes 20-30 and 
#50-60 are caused by scheduled flights that leave early. Hint: create a binary
#variable that tells you whether or not a flight was delayed.

flights_dt %>%
  mutate(early = dep_delay < 0,
         minute = minute(sched_dep_time)) %>%
  group_by(minute) %>%
  summarise(early = mean(early)) %>%
  ggplot(aes(x = minute, y = early)) +
  geom_point()


#Grouped in 10 minutes level
flights_dt %>%
  mutate(early = dep_delay < 0,
         minute = minute(sched_dep_time) %% 10) %>%
  group_by(minute) %>%
  summarise(early = mean(early)) %>%
  ggplot(aes(x = minute, y = early)) +
  geom_point()

##Function
greet <- function(time = lubridate::now()) {
  hr <- lubridate::hour(time)
  # I don't know what to do about times after midnight,
  # are they evening or morning?
  if (hr < 12) {
    print("good morning")
  } else if (hr < 17) {
    print("good afternoon")
  } else {
    print("good evening")
  }
}

greet()

greet(ymd_h("2017-01-08:05"))
#> [1] "good morning"
greet(ymd_h("2017-01-08:13"))
#> [1] "good afternoon"
greet(ymd_h("2017-01-08:20"))


switch(1, "apple", "banana", "cantaloupe")
#> [1] "apple"
switch(2, "apple", "banana", "cantaloupe")
#> [1] "banana"
#If you use a non-integer number for the first argument of switch(), 
#it will ignore the non-integer part.


switch(1.2, "apple", "banana", "cantaloupe")
#> [1] "apple"
switch(2.8, "apple", "banana", "cantaloupe")




flights %>%
  mutate(date = make_date(year, month, day),
         wday = wday(date, label = TRUE)) %>%
  group_by(wday) %>%
  summarise(dist_mean =  mean(distance),
            dist_median = median(distance)) %>%
  ggplot(aes(y = dist_mean, x = wday)) +
  geom_point()


flights %>%
  mutate(date = make_date(year, month, day),
         wday = wday(date, label = TRUE)) %>%
  group_by(wday) %>%
  summarise(dist_mean =  mean(distance),
            dist_median = median(distance)) %>%
  ggplot(aes(y = dist_mean, x = wday)) +
  geom_point()


flights %>%
  mutate(date = make_date(year, month, day),
         wday = wday(date, label = TRUE)) %>%
  group_by(wday, hour) %>%
  summarise(dist_mean =  mean(distance),
            dist_median = median(distance)) %>%
  ggplot(aes(y = dist_mean, x = hour, colour = wday)) +
  geom_point() +
  geom_line()


