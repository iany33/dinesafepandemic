
pacman::p_load(rio,
               here,
               Matrix,
               tidyverse,
               tsibble,
               janitor,
               lubridate)

# Load datasets

dinesafe1 <- import(here("data", "Dinesafe_2017_2021.xlsx"))
dinesafe2 <- import(here("data", "Dinesafe_2021_2022.xlsx"))

# Remove overlapping time periods

dinesafe2 <- dinesafe2 |> filter(DATE >= "2022-01-01")

# Combine datasets

dinesafe <- bind_rows(dinesafe1, dinesafe2)

remove(dinesafe1, dinesafe2)

dinesafe <- dinesafe |> janitor::clean_names()


# Explores types of premises

dinesafe |> tabyl(type) |> adorn_totals("row")

dinesafe |> select(type) |> gtsummary::tbl_summary() 

# Focus on only restaurants and food take-outs, remove those out of business

data <- dinesafe |> 
  filter(status2 != "Out of Business") |> 
  filter(type == "Restaurant" | type == "Food Take Out") |> 
  relocate(date, .before = id) 


# Explore other variable values

data |> tabyl(type)
data |> tabyl(status2)
data |> tabyl(severity)


# Create infraction counts per inspection

data <- data |> 
  mutate(infractions = case_when(is.na(severity) ~ 0, TRUE ~ 1)) |>
  mutate(inf_critical = case_when(severity == "C - Crucial" ~ 1, TRUE ~ 0)) |>
  mutate(inf_significant = case_when(severity == "S - Significant" ~ 1, TRUE ~ 0))

data <- data |> group_by(id, date) |>
  mutate(num_infractions = sum(infractions)) |>
  mutate(num_critical = sum(inf_critical)) |>
  mutate(num_significant = sum(inf_significant))


# reduce dataset to one row per establishment-date

data <- data |> group_by(id, date) |>
  slice(which.min(date))

data <- data |> mutate(month = as.factor(month(date))) |> 
  mutate(yearmonth = format_ISO8601(date, precision = "ym")) |> 
  mutate(quarter = paste0(year(date), "/0", quarter(date)))  |> 
  mutate(year_num = as.integer(date))
         

# Remove re-inspections (occurring in same month as another)

data <- data |> group_by(id, yearmonth) |> 
  arrange(id, yearmonth) |> slice(1) |> ungroup()


# Reformat as time series data - default week starts on Monday

timeseries <- data |> group_by(date) |>
  summarize(infractions = sum(num_infractions),
            inf_critical = sum(num_critical),
            inf_significant = sum(num_significant),
            pass = sum(status2 == "Pass", na.rm=T),
            inspections = n())

timeseries <- timeseries |> mutate(week = tsibble::yearweek(date))


# Convert to weekly given inspections not conducted every day

timeseries <- timeseries |> group_by(week) |> 
  summarize(infractions = sum(infractions),
            inf_critical = sum(inf_critical),
            inf_significant = sum(inf_significant),
            pass = sum(pass),
            inspections = sum(inspections),
            infraction_rate = infractions / inspections,
            critical_rate = inf_critical / inspections,
            significant_rate = inf_significant / inspections,
            pass_rate = pass / inspections)

covid_start <- yearweek("2020-03-23")

# Descriptive summary

timeseries |> rstatix::get_summary_stats(
  infractions, pass, inspections, infraction_rate, critical_rate, significant_rate, 
  pass_rate, type = "common")

sum(timeseries$infractions)
sum(timeseries$inspections)
sum(timeseries$pass)


timeseries2 <- timeseries |> filter(inspections > 3)

timeseries2 |> rstatix::get_summary_stats(
  infractions, pass, inspections, infraction_rate, critical_rate, significant_rate, 
  pass_rate, type = "common")

sum(timeseries2$infractions)
sum(timeseries2$inspections)
sum(timeseries2$pass)


timeseries2 <- timeseries2 |> 
  mutate(pandemic = as.factor(case_when(week >= covid_start ~ "Pandemic Period",
                                        TRUE ~ "Pre-Pandemic"))) 

timeseries2 |> tabyl(pandemic)

old <- options(pillar.sigfig = 6)
timeseries2 |> group_by(pandemic) |> rstatix::get_summary_stats(
    infractions, pass, inspections, infraction_rate, critical_rate, significant_rate, 
    pass_rate, type = "common")
options(old)


# Graph time series

ts_data <- tsibble(timeseries2, index = week)

ts_data <- ts_data |> fill_gaps(.full = TRUE)

ts_data |> ggplot(aes(x = week, y = critical_rate)) + 
  geom_line(colour = "#3b528b") + 
  labs(x = "Week", y = "Critical infraction rate") +
  scale_x_yearweek(date_breaks = "20 weeks") +
  ylim(0, 0.35) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  geom_vline(xintercept = as.Date("2020-03-17"), linetype = "dashed") + 
  annotate(geom = "text", label = "Pandemic", x = as.Date("2020-03-17"), 
           y = 0.25, angle = 90, vjust = 1, size = 3)

ts_data |> ggplot(aes(x = week, y = significant_rate)) + 
  geom_line(colour = "#3b528b") + 
  labs(x = "Week", y = "Significant infraction rate") +
  scale_x_yearweek(date_breaks = "20 weeks") +
  ylim(0, 1.1) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  geom_vline(xintercept = as.Date("2020-03-17"), linetype = "dashed") + 
  annotate(geom = "text", label = "Pandemic", x = as.Date("2020-03-17"), 
           y = 1, angle = 90, vjust = 1, size = 3)

p1 <- ts_data |> ggplot(aes(x = week, y = infraction_rate)) + 
  geom_point(colour = "#3b528b") + 
  geom_line(colour = "#3b528b") + 
  labs(x = "Week", y = "Infraction rate") +
  scale_x_yearweek(date_breaks = "20 weeks") +
  ylim(0.4, 2) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  geom_vline(xintercept = as.Date("2020-03-17"), linetype = "dashed") + 
  annotate(geom = "text", label = "Pandemic", x = as.Date("2020-03-17"), 
           y = 1.7, angle = 90, vjust = 1, size = 3)

p2 <- ts_data |> ggplot(aes(x = week, y = pass_rate)) + 
  geom_point(colour = "darkgreen") + 
  geom_line(colour = "darkgreen") + 
  labs(x = "Week", y = "Pass rate") +
  scale_x_yearweek(date_breaks = "20 weeks") +
  ylim(0.7, 1) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) + 
  geom_vline(xintercept = as.Date("2020-03-17"), linetype = "dashed") + 
  annotate(geom = "text", label = "Pandemic", x = as.Date("2020-03-17"), 
           y = 0.95, angle = 90, vjust = 1, size = 4)

cowplot::plot_grid(p1, p2, labels = c("A", "B"), ncol=1, nrow=2)

p <- cowplot::plot_grid(p1, p2, labels = c("A", "B"), ncol=1, nrow=2)
ggsave("Fig_1.tiff", p, device = "tiff", dpi = 600, width = 6, height = 8)

remove(p1, p2)

# Plot number of infractions and inspections together

p <- ts_data |> pivot_longer(cols = c("infractions", "inspections"), 
                                    names_to = "Outcome", values_to = "count") |> 
  mutate(Outcome = recode(Outcome, "infractions" = "Infractions", 
                          "inspections" = "Inspections")) |>
  ggplot(aes(x = week, y = count, colour = Outcome)) +
  geom_line() + 
  geom_point() +
  labs(x = "Week", y = "Number of infractions and inspections") +
  scale_x_yearweek(date_breaks = "20 weeks") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        legend.position="top", legend.box = "horizontal") +
  geom_vline(xintercept = as.Date("2020-03-17"), linetype = "dashed") + 
  annotate(geom = "text", label = "Pandemic", x = as.Date("2020-03-17"), 
           y = 600, angle = 90, vjust = 1, size = 4)

ggsave("Fig_2.tiff", p, device = "tiff", dpi = 600, width = 8, height = 4)

# Plot critical/significant infractions rates and overall pass rate 

ts_data |> pivot_longer(cols = c("critical_rate", "significant_rate", "pass_rate"), 
                        names_to = "Outcome", values_to = "count") |> 
  mutate(Outcome = recode(Outcome, "critical_rate" = "Critical infraction rate", 
                          "significant_rate" = "Significant infraction rate", 
                          "pass_rate" = "Pass rate")) |>   
  ggplot(aes(x = week, y = count, colour = Outcome)) +
  geom_line() + 
  labs(x = "Week", y = "Rate") +
  scale_x_yearweek(date_breaks = "20 weeks") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        legend.position="top", legend.box = "horizontal") +
  geom_vline(xintercept = as.Date("2020-03-17"), linetype = "dashed") + 
  annotate(geom = "text", label = "Pandemic", x = as.Date("2020-03-17"), 
           y = 0.7, angle = 90, vjust = 1, size = 4) 


# Plot number of inspections and critical/significant infraction raw counts

ts_data |> pivot_longer(cols = c("inf_critical", "inf_significant", "inspections"), 
                        names_to = "Outcome", values_to = "count") |> 
  mutate(Outcome = recode(Outcome, "inf_critical" = "Critical infractions", 
                          "inf_significant" = "Significant infractions",
                        "inspections" = "Inspections")) |>   
  ggplot(aes(x = week, y = count, colour = Outcome)) +
  geom_line() + 
  labs(x = "Week", y = "Count") +
  scale_x_yearweek(date_breaks = "20 weeks") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        legend.position="top", legend.box = "horizontal") +
  geom_vline(xintercept = as.Date("2020-03-17"), linetype = "dashed") + 
  annotate(geom = "text", label = "Pandemic", x = as.Date("2020-03-17"), 
           y = 500, angle = 90, vjust = 1, size = 4) 



