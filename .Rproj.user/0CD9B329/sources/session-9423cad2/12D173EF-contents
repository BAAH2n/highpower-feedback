library(tidyverse)

categorised_feedback <- readxl::read_excel("data/Feedback_Classified.xlsx", sheet = 1) |> 
  janitor::clean_names() |> 
  mutate(user_id = as.numeric(user_id),
         date = parse_date_time(date, orders = c("mdy_HMS", "dmy_HMS", "dmy", "mdy"))) 
  

user_status <- read_csv("data/user_status.csv") |> 
  select(c(1,2,3)) |> 
  janitor::clean_names() |> 
  filter(!is.na(meta_user_id))

idea_legend <- readxl::read_excel("data/Idea_Dictionary.xlsx") |> 
  janitor::clean_names() |> 
  print()
  

joined_tb <- left_join(categorised_feedback, user_status, join_by(user_id == meta_user_id))

counting_ideas <- joined_tb |>
  filter(!str_detect(category, "5")) |> 
  select(code, date, user_id) |> 
  group_by(code) |> 
  summarise(number_of_mentions = n())|>
  left_join(x = _, idea_legend, join_by (code == code)) |> 
  print()

top_5_ideas <- counting_ideas |> 
  filter(!str_detect(code, "1.1")) |>
  arrange(desc(number_of_mentions)) |>  
  head(5) |> 
  print()

bugs_report <- counting_ideas |> 
  filter(str_detect(code, "1.1")) |> 
  arrange(desc(number_of_mentions)) |>
  print()

write_excel_csv(top_5_ideas, "output/top_5_ideas.csv", delim = ";")
write_excel_csv(bugs_report, "output/bugs_report.csv", delim = ";")
write_excel_csv(counting_ideas, "output/ideas.csv", delim = ";")

