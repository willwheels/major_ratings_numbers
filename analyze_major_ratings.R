library(dplyr)
library(dbplyr)
library(pool)
library(lubridate)
library(ggplot2)

# print("are you connected to Pulse Secure or in the office?")
# 
# source("./sources/db_connection.R")
# 
# dbpool <- pool::dbPool(odbc::odbc(),
#                        dsn = db_connection$connect_name,
#                        uid = db_connection$user_name,
#                        pwd = db_connection$password
# )
# 
# 
# icis_permit <- dplyr::tbl(dbpool, dbplyr::ident_q('idea_icis.icis_permit')) %>%
#   select(EXTERNAL_PERMIT_NMBR, VERSION_NMBR, PERMIT_TYPE_CODE, ISSUE_DATE,
#          MAJOR_MINOR_STATUS_FLAG, MAJOR_RATING_NMBR,
#          TOTAL_DESIGN_FLOW_NMBR, ACTUAL_AVERAGE_FLOW_NMBR) %>%
#   mutate(MAJOR_MINOR_STATUS_FLAG =
#            if_else(is.na(MAJOR_MINOR_STATUS_FLAG), "N", MAJOR_MINOR_STATUS_FLAG)) %>%
#   collect()

#save(icis_permit, file = "icis_permit_internal.Rda")

load("icis_permit_internal.Rda")

# test_for_aaron <- icis_permit %>%
#   group_by(EXTERNAL_PERMIT_NMBR) %>%
#   filter(n() > 1) %>%
#   summarize(num_status = n_distinct(MAJOR_MINOR_STATUS_FLAG))

#get most recent major rating

icis_permit_mrat <- icis_permit %>%
  filter(!is.na(MAJOR_RATING_NMBR)) %>%
  group_by(EXTERNAL_PERMIT_NMBR) %>%
  arrange(EXTERNAL_PERMIT_NMBR, VERSION_NMBR) %>%
  slice(1) %>%
  mutate(state = substr(EXTERNAL_PERMIT_NMBR, 1, 2)) 

length(unique(icis_permit$EXTERNAL_PERMIT_NMBR))

ggplot(data = icis_permit_mrat %>% 
         filter(!is.na(MAJOR_RATING_NMBR)),
       aes(x = MAJOR_RATING_NMBR )) +
  geom_histogram(binwidth = 5) +
  theme_minimal()

ggplot(data = icis_permit_mrat %>% 
         filter(!is.na(MAJOR_RATING_NMBR), MAJOR_RATING_NMBR > 0),
       aes(x = MAJOR_RATING_NMBR )) +
  geom_histogram(binwidth = 5) +
  theme_minimal()


ggplot(data = icis_permit_mrat %>% 
         filter(!is.na(MAJOR_RATING_NMBR), MAJOR_RATING_NMBR > 0, MAJOR_RATING_NMBR <200),
       aes(x = MAJOR_RATING_NMBR )) +
  geom_histogram(binwidth = 5) +
  geom_vline(xintercept = 80, linetype = 2) +
  theme_minimal()


count_by_state <- icis_permit_mrat %>%
  group_by(state) %>%
  count(sort = TRUE)

iowa <- icis_permit_mrat %>%
  filter(state == "IA") %>%
  group_by(MAJOR_RATING_NMBR) %>%
  count()


ggplot(data = icis_permit_mrat %>% filter(state == "KY", MAJOR_RATING_NMBR > 0, MAJOR_RATING_NMBR <100),
       aes(x = MAJOR_RATING_NMBR)) +
  geom_histogram(binwidth = 5) +
  geom_vline(xintercept = 80, linetype = 2) +
  theme_minimal() 

  