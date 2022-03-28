## Notes from collaboration on setting up GitHub:
## Testing going into the cars.R file and then
## push it back to the collaborated repo.
library(tidyverse)
library(jsonlite)
library(lubridate)

vehicles <- fromJSON("https://phl.carto.com/api/v2/sql?q=SELECT%20*%20FROM%20public_cases_fc%20WHERE%20service_name%20=%20%27Abandoned%20Vehicle%27")

vehicles <- as.data.frame(vehicles$rows) 

glimpse(vehicles)

vehicles %>%
  mutate(req_date = as.Date(requested_datetime),
         req_year= year(req_date),
         req_month = month(req_date),
         req_date_con = paste(req_year, req_month, sep="_"),
         issue_duration = as.Date(updated_datetime) - req_date,
         resolved = ifelse(status_notes == "Issue Resolved", 1, 0 )) %>%
  group_by(req_month, req_year, status) %>%
  summarize(mean_duration = mean(issue_duration, na.rm = TRUE),
            num_requests = n()) %>%
  ggplot()+
  geom_bar(aes(x = req_month, y = num_requests, fill = status), 
           stat = "identity",
           position = "dodge")+
  facet_wrap(~req_year)
  

vehicles %>%
  mutate(req_date = as.Date(requested_datetime),
         req_year= year(req_date),
         req_month = month(req_date),
         req_date_con = paste(req_year, req_month, sep="_"),
         issue_duration = as.Date(updated_datetime) - req_date,
         resolved = ifelse(status_notes == "Issue Resolved", 1, 0 )) %>%
  group_by(req_month, req_year, status) %>%
  summarize(mean_duration = mean(issue_duration, na.rm = TRUE),
            num_requests = n()) %>%
  filter(status == "Closed") %>%
  ggplot()+
  geom_bar(aes(x = req_month, y = mean_duration), 
           stat = "identity",
           position = "dodge")+
  facet_wrap(~req_year)

vehicles %>%
  mutate(req_date = as.Date(requested_datetime),
         req_year= year(req_date),
         req_month = month(req_date),
         req_date_con = paste(req_year, req_month, sep="_"),
         issue_duration = as.Date(updated_datetime) - req_date,
         duration_class = case_when(issue_duration <= 30 ~ "<30 days",
                                    issue_duration > 30 & issue_duration < 60 ~ "31-60 days",
                                    issue_duration >= 60 ~ ">60 days"),
         resolved = ifelse(status_notes == "Issue Resolved", 1, 0 )) %>%
  filter(status == "Closed" & resolved == 1) %>%
  group_by(req_month, req_year, duration_class) %>%
  tally() %>%
  ggplot()+
  geom_bar(aes(x = req_month, y = n, fill = duration_class), 
           stat = "identity",
           position = "dodge")+
  facet_wrap(~req_year)+
  ggtitle("Closed, resolved cases by origin date")


vehicles %>% 
  mutate(req_date = as.Date(requested_datetime),
         req_year= year(req_date), 
         resolved = ifelse(status_notes == "Issue Resolved", 1, 0 )) %>% 
  group_by(req_year, status, status_notes) %>% 
  tally() %>%
  ggplot()+
  geom_bar(aes(y = n, x = req_year), stat = "identity")+
  facet_wrap(status~status_notes)+
  ggtitle("Status notes on cases by origin date year")
