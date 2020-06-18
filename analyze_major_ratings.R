library(dplyr)
library(dbplyr)
library(pool)
library(lubridate)
library(ggplot2)

print("are you connected to Pulse Secure or in the office?")

source("./sources/db_connection.R")

dbpool <- pool::dbPool(odbc::odbc(),
                       dsn = db_connection$connect_name,
                       uid = db_connection$user_name,
                       pwd = db_connection$password
)


icis_permit <- dplyr::tbl(dbpool, dbplyr::ident_q('idea_icis.icis_permit')) %>%
  select(EXTERNAL_PERMIT_NMBR, VERSION_NMBR, PERMIT_TYPE_CODE, ISSUE_DATE,
         MAJOR_MINOR_STATUS_FLAG, MAJOR_RATING_NMBR, 
         TOTAL_DESIGN_FLOW_NMBR, ACTUAL_AVERAGE_FLOW_NMBR) %>%
  mutate(MAJOR_MINOR_STATUS_FLAG = 
           if_else(is.na(MAJOR_MINOR_STATUS_FLAG), "N", MAJOR_MINOR_STATUS_FLAG)) %>%
  collect()

icis_permit_mrat <- icis_permit %>%
  filter(VERSION_NMBR == 0) %>%
  mutate(state = substr(EXTERNAL_PERMIT_NMBR, 1, 2)) 


ggplot(data = icis_permit_mrat %>% 
         filter(!is.na(MAJOR_RATING_NMBR)),
       aes(x = MAJOR_RATING_NMBR )) +
  geom_histogram(binwidth = 5) +
  theme_minimal()

ggplot(data = icis_permit_mrat %>% 
         filter(!is.na(MAJOR_RATING_NMBR), MAJOR_RATING_NMBR > 0),
       aes(x = MAJOR_RATING_NMBR )) +
  geom_histogram(binwidth = 5, boundary = 0) +
  theme_minimal()


ggplot(data = icis_permit_mrat %>% 
         filter(!is.na(MAJOR_RATING_NMBR), MAJOR_RATING_NMBR > 0, MAJOR_RATING_NMBR <200),
       aes(x = MAJOR_RATING_NMBR )) +
  geom_histogram(binwidth = 5, boundary = 0, closed = "left") +
  geom_vline(xintercept = 80, linetype = 2) +
  theme_minimal()

ggplot(data = icis_permit_mrat %>% 
         filter(!is.na(MAJOR_RATING_NMBR), MAJOR_RATING_NMBR > 0, MAJOR_RATING_NMBR <200),
       aes(x = MAJOR_RATING_NMBR )) +
  geom_histogram(binwidth = 1, boundary = 0, closed = "left") +
  geom_vline(xintercept = 80, linetype = 2) +
  theme_minimal()

test1 <- icis_permit_mrat %>% 
  filter(MAJOR_RATING_NMBR >0, !is.na(MAJOR_RATING_NMBR))

test2 <- icis_permit_mrat %>% 
  filter(MAJOR_RATING_NMBR >0, !is.na(MAJOR_RATING_NMBR)) %>%
  group_by(state) %>%
  mutate(count = n())

unique(test2$state)

count_by_state <- test2 %>%
  group_by(state) %>%
  count(sort = TRUE)

iowa <- test2 %>%
  filter(state == "IA") %>%
  group_by(MAJOR_RATING_NMBR) %>%
  count()


ggplot(data = test2 %>% 
         filter(!(state == "IA"), count > 29, MAJOR_RATING_NMBR <100),
       aes(x = MAJOR_RATING_NMBR)) +
  geom_histogram(binwidth = 5, boundary = 0, closed = "left") +
  geom_vline(xintercept = 80, linetype = 2) +
  theme_minimal() +
  facet_wrap(~ state)
  
ggplot(data = test2 %>% 
        filter(state == "TX", MAJOR_RATING_NMBR <200),
      aes(x = MAJOR_RATING_NMBR)) +
  geom_histogram(binwidth = 5, boundary = 0, closed = "left") +
  geom_vline(xintercept = 80, linetype = 2) +
  theme_minimal() 


percent_reporting <- icis_permit_mrat %>%
  filter(PERMIT_TYPE_CODE == "NPD") %>%
  mutate(mrat_reported = !is.na(MAJOR_RATING_NMBR)) %>%
  group_by(state) %>%
  summarise(total = n(), 
            mrat_reported = sum(mrat_reported),
            percent_reported = mrat_reported/total) %>%
  ungroup() %>%
  arrange(desc(percent_reported))
  

