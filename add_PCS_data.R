library(tidyverse)



load(here::here("data", "major_rating_nums", "R_data_files", "icis_permit_internal.Rda"))

pcs_mrats <- read_csv(here::here("data", "major_rating_nums", "other_data_files", "PCS archive 3 27 17.csv")) %>%
  janitor::clean_names()

length(unique(pcs_mrats$permit_num_npid)) # verify that only one row per NPDES ID


icis_permit_mrat <- icis_permit %>%
  left_join(pcs_mrats, by = c("EXTERNAL_PERMIT_NMBR" = "permit_num_npid")) %>%
  group_by(EXTERNAL_PERMIT_NMBR) %>%
  arrange(EXTERNAL_PERMIT_NMBR, VERSION_NMBR) %>%
  slice(1) 

icis_permit_mrat1 <- icis_permit_mrat %>%
  filter(!is.na(MAJOR_RATING_NMBR))

icis_permit_mrat2 <- icis_permit_mrat %>%
  mutate(major_rating_num_combined = if_else(!is.na(MAJOR_RATING_NMBR),
                                             MAJOR_RATING_NMBR,
                                             maj_rating_cd_mrat
                                             )) %>%
  filter(!is.na(major_rating_num_combined ))


ggplot(data = icis_permit_mrat %>% 
         filter(!is.na(MAJOR_RATING_NMBR), MAJOR_RATING_NMBR > 0, MAJOR_RATING_NMBR <200),
       aes(x = MAJOR_RATING_NMBR )) +
  geom_histogram(binwidth = 5) +
  geom_vline(xintercept = 80, linetype = 2) +
  theme_minimal()

ggplot(data = icis_permit_mrat2 %>% 
         filter(!is.na(major_rating_num_combined), major_rating_num_combined > 0, major_rating_num_combined <200),
       aes(x = major_rating_num_combined )) +
  geom_histogram(binwidth = 5) +
  geom_vline(xintercept = 80, linetype = 2) +
  theme_minimal()
                         