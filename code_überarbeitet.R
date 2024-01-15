# packages
library(tidyverse)
library(ggplot2)

# this code chunk formats all hourly data into long format and cleans up the data
Daten <- read_csv("Daten.csv")
Daten_longer <-
  Daten %>% rename("AT 20" = "A 20", "AT 22" = "A 22") %>% rename("Notizen" = ...163) %>%
  mutate(across(matches(".*[0-9]+.*"), as.numeric)) %>% # all columns to numeric
  mutate(across(matches(".*[0-9]+.*"), function(x) {
    ifelse(x < -50, NA, x)
  })) %>% # all negative values to NA
  pivot_longer(
    cols = matches(".*[0-9]+.*"),
    # columns to pivot
    names_to = c("bodypart", "hour"),
    # new column names, ATTENTION: bodypart contains things like clothing etc.
    names_pattern = "(.*\\D)(\\d{1,2})$",
    # pattern to separate the original column names
    values_to = "temperatur" # new column name for the values
  ) %>% mutate(hour = as.numeric(hour)) %>%
  mutate(bodypart = gsub(" ", "", bodypart)) # remove spaces in bodypart

all_bodyparts <-
  c("TDl", "TDr", "KT", "Tpl", "Tpr", "TF", "TG", "TN", "TZ") # all bodyparts
Daten_longer_only_bodypart <-
  Daten_longer %>% filter(bodypart %in% all_bodyparts) # only real bodyparts

Daten_longer_only_bodypart$`Anzahl Raynaud Attacken` <-
  factor(
    Daten_longer_only_bodypart$`Anzahl Raynaud Attacken`,
    levels = c("0", "<5", "<10", ">10", ">15", ">20")
  )
Daten_longer_only_bodypart <-
  Daten_longer_only_bodypart %>% drop_na(contains("Anzahl Raynaud"))

all_bodyparts_by_attacks <-
  Daten_longer_only_bodypart %>% filter(bodypart != "KT", Raynaud == 0)
raynaudornot <-
  Daten_longer_only_bodypart %>% filter(bodypart != "KT")


# condense bodyparts for all_bodyparts_by_attacks and raynaudornot
all_bodyparts_by_attacks <-
  mutate(
    all_bodyparts_by_attacks,
    bodypart = ifelse(bodypart == "TN" | bodypart == "TG",
                      "DG",
                      bodypart)
  )
all_bodyparts_by_attacks <-
  mutate(
    all_bodyparts_by_attacks,
    bodypart = ifelse(bodypart == "Tpl" | bodypart == "Tpr",
                      "DH",
                      bodypart)
  )
all_bodyparts_by_attacks <-
  mutate(
    all_bodyparts_by_attacks,
    bodypart = ifelse(
      bodypart == "TZ" |
        bodypart == "TF" | bodypart == "TDl" | bodypart == "TDr",
      "DF",
      bodypart
    )
  )
all_bodyparts_by_attacks <- all_bodyparts_by_attacks %>%
  mutate(
    all_bodyparts_by_attacks,
    `Anzahl Raynaud Attacken` = ifelse(
      `Anzahl Raynaud Attacken` == "0",
      "0 Raynaud Attacken",
      "mehr als 0 Raynaud Attacken"
    )
  )
raynaudornot <- raynaudornot %>%
  mutate(raynaudornot,
         bodypart = ifelse(bodypart == "TN" | bodypart == "TG",
                           "DG",
                           bodypart))
raynaudornot <- raynaudornot %>%
  mutate(raynaudornot,
         bodypart = ifelse(bodypart == "Tpl" | bodypart == "Tpr",
                           "DH",
                           bodypart))
raynaudornot <- raynaudornot %>%
  mutate(
    raynaudornot,
    bodypart = ifelse(
      bodypart == "TZ" |
        bodypart == "TF" | bodypart == "TDl" | bodypart == "TDr",
      "DF",
      bodypart
    )
  )





# for the naming in the plots
all_bodyparts_by_attacks$`Anzahl Raynaud Attacken` <-
  factor(
    all_bodyparts_by_attacks$`Anzahl Raynaud Attacken`,
    levels = c("0 Raynaud Attacken", "mehr als 0 Raynaud Attacken")
  )

raynaudornot$Raynaud <-
  ifelse(raynaudornot$Raynaud == 0,
         "Raynaud Symptom",
         "kein Raynaud Symptom")



Daten_longer_only_bodypart <- Daten_longer_only_bodypart %>%
  mutate(
    `Anzahl Raynaud Attacken` = ifelse(
      `Anzahl Raynaud Attacken` == ">15" |
        `Anzahl Raynaud Attacken` == ">20",
      ">15",
      `Anzahl Raynaud Attacken`
    )
  )



Daten_longer_only_bodypart$`Anzahl Raynaud Attacken` <-
  factor(
    Daten_longer_only_bodypart$`Anzahl Raynaud Attacken`,
    levels = c(1, 2, 3, 4, ">15"),
    labels = c("0", "1-4", "5-9", "10-15", "15+")
  )


Daten_longer_only_bodypart$bodypart <-
  factor(
    Daten_longer_only_bodypart$bodypart,
    levels = c("KT", "TF", "TZ", "TG", "TN", "Tpl", "TDl", "TDr", "Tpr")
  )

Daten_longer_only_bodypart <- # adds mean temps over the days
  Daten_longer_only_bodypart %>%
  group_by(Untersuchung, Patientennummer, hour, bodypart) %>%
  mutate(mean_Tag = mean(temperatur, na.rm = TRUE))


## PLOTS

# overview of all bodyparts by attacks
all_bodyparts_by_attacks_pl <- all_bodyparts_by_attacks %>%
  ggplot(aes(x = hour, y = temperatur)) +
  geom_point(alpha = 0.1, size = 5) +
  facet_grid(
    rows = vars(bodypart),
    cols = vars(`Anzahl Raynaud Attacken`),
    scales = "free"
  ) +
  theme_bw() +
  labs(x = "Uhrzeit", y = "Körpertemperatur") +
  geom_smooth(
    method = "loess",
    se = FALSE,
    color = "blue",
    size = 1
  )

# temps of patients with and without raynaudsyndrom
raynaudornot_pl <- raynaudornot %>%
  ggplot(aes(x = hour, y = temperatur)) +
  geom_point(alpha = 0.1, size = 5) +
  facet_grid(rows = vars(bodypart),
             cols = vars(Raynaud),
             scales = "free") +
  theme_bw() +
  labs(x = "Uhrzeit", y = "Körpertemperatur") +
  geom_smooth(
    method = "loess",
    se = FALSE,
    color = "blue",
    size = 1
  )

# big overview plot
all_bodyparts_plot <-
  Daten_longer_only_bodypart  %>% filter(Raynaud == 0, bodypart != "Tpr", bodypart != "TDr") %>%
  ggplot(aes(x = hour, y = mean_Tag)) +
  geom_line(aes(group = Patientennummer), alpha = 0.2) +
  geom_point(alpha = 0.2) +
  facet_grid(
    rows = vars(bodypart),
    cols = vars(`Anzahl Raynaud Attacken`),
    scales = "free"
  ) +
  theme_bw() +
  labs(x = "Uhrzeit", y = "Körpertemperatur") +
  geom_smooth(method = "loess", se = FALSE) +
  ggtitle("Anzahl Raynaud Attacken in den letzten 4 Wochen:")


# Verteilung der Daten
label.obs <- c("Tag 1", "Tag 2", "Tag 3", "Tag 4", "Tag 5", "Tag 6")
names(label.obs) <- c(1:6)
Daten_Verteilung <- Daten_longer_only_bodypart %>%
  ggplot(aes(x = hour)) +
  geom_bar() +
  facet_wrap( ~ Tag, labeller = labeller(Tag = label.obs), ncol = 6) +
  theme_bw() +
  ggtitle("Verteilung der Daten") +
  xlab("Uhrzeit") +
  ylab("Anzahl der Temperatur Messungen")


# Saving the plots as pdfs
ggsave(
  "all_bodyparts_plot.pdf",
  all_bodyparts_plot,
  width = 20,
  height = 20,
  units = "cm"
)
ggsave(
  "all_bodyparts_by_attacks_pl.pdf",
  all_bodyparts_by_attacks_pl,
  width = 20,
  height = 20,
  units = "cm"
)
ggsave(
  "Daten_Verteilung.pdf",
  Daten_Verteilung,
  width = 25,
  height = 10,
  units = "cm"
)
ggsave(
  "raynaudornot_pl.pdf",
  raynaudornot_pl,
  width = 20,
  height = 20,
  units = "cm"
)
