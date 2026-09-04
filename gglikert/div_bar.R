# Diverging bar plot
# https://larmarange.github.io/ggstats/articles/gglikert.html


library(uwb)
library(ggstats)

# Data
set.seed(42)
df <-
  tibble(q1 = sample(likert_levels, 150, replace = TRUE),
         q2 = sample(likert_levels, 150, replace = TRUE, prob = 5:1),
         q3 = sample(likert_levels, 150, replace = TRUE, prob = 1:5),
         q4 = sample(likert_levels, 150, replace = TRUE, prob = 1:5),
         q5 = sample(c(likert_levels, NA), 150, replace = TRUE),
         q6 = sample(likert_levels, 150, replace = TRUE, prob = c(1, 0, 1, 1, 0))) %>%
  mutate(across(.cols = everything(),
                .fns = ~ factor(.x, levels = c("Strongly disagree", "Disagree",
                                               "Neither agree nor disagree",
                                               "Agree", "Strongly agree"))))

df

levels(df$q1)

# 1. FUNKCE gglikert -------------------

# Zakladni graf
gglikert(df)

# Serazeni
gglikert(df, sort = "descending")

# Obracene poradu urovni
gglikert(df, reverse_likert = TRUE)

# Sirka pruhu
gglikert(df, width = 0.6)

# Vynchani %
gglikert(df, add_labels = FALSE)

# Prizpusobeni formatu %
gglikert(df,
         labels_size = 4,
         labels_accuracy = 0.1,
         labels_hide_below = .3,
         labels_color = "grey33")

# Pocitaji se marginalni % i se stredni kategorie? Defaultne ne.
# Default
gglikert(df,
         totals_include_center = FALSE,
         sort = "descending",
         sort_prop_include_center = F)
# vs. rucni zmena
gglikert(df,
         totals_include_center = TRUE,
         sort = "descending",
         sort_prop_include_center = T)

# Format marginalnich %
gglikert(df,
         totals_size = 4,
         totals_color = "blue",
         totals_fontface = "italic",
         totals_hjust = .20,
         add_totals = "right")

# Odstraneni marginalnich %
gglikert(df, add_totals = FALSE)

# Pojmenovani otazek
library(labelled)
df <-
  df %>%
  set_variable_labels(
    q1 = "first question",
    q2 = "second question",
    q3 = "third question with a quite long variable label")

gglikert(df)

# Zalomeni otazky
gglikert(df, y_label_wrap = 20)

# Nastaveni stredni kategorie
gglikert(df, cutoff = 0)
gglikert(df, cutoff = 1.5)
gglikert(df, cutoff = 2)


# Symetricka osa X (-100, 100)
gglikert(df, symmetric = TRUE)

# Vynechani kategorie v zobrazeni, ale ne ve vypoctu %
# Default
gglikert(df)
# vs. vynechana kategorie
gglikert(df, exclude_fill_values = "Neither agree nor disagree")
# vs. vynechana kategorie vpravo
gglikert_side(df, side_values = "Neither agree nor disagree")

# Klasicky stack plot
gglikert_stacked(df)


# 2. RUCNI ggplot s geom_likert -------------------
df %>%
  gglikert_data() %>%
  drop_na() %>%
  ggplot(aes(y = .question, fill = .answer, by = .question)) +
  geom_likert() +
  geom_likert_text() +
  scale_fill_likert() +
  scale_x_continuous(labels = label_percent_abs())


# 2. RUCNI ggplot s geom_bar -------------------
df %>%
  gglikert_data() %>%
  drop_na() %>%
  ggplot(aes(y = .question, fill = .answer, by = .question)) +
  geom_bar(position = position_likert(),
           width = 0.6) +
  geom_text(aes(label = label_percent_abs(accuracy = 1,
                                          hide_below = 0.1)(after_stat(prop))),
            stat = StatProp,
            complete = "fill",
            position = position_likert(vjust = 0.5)) +
  scale_fill_likert() +
  scale_x_continuous(labels = label_percent_abs())

