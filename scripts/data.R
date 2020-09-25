
  library(dplyr)
  library(haven)
  library(purrr)

# Data -------------------------------------------------------------------------

  load("data/nutrition.RData") # Brendan converted these from .sav into .RData

  patients <- D3_Nut_profile.tbl; rm(D3_Nut_profile.tbl)
  meals <-    Nutritionobs.tbl;   rm(Nutritionobs.tbl)

# Variable names ----
  names(patients) <- tolower(names(patients))
  names(meals)    <- tolower(names(meals))

  patients <- rename(patients,
    patient_type = pttype,
    comorbid = comobidities,
    admit_date = dateadmiss,
    meds = nummeds,
    must_screen = nutritionscreen,
    must_score = nutritionvalue,
    heart_fail = hearfailure,
    renal_fail = renalfailure,
    restrict_fluid = fluidrestriction,
    restrict_diet = dietrestriction,
    modified_diet = modifyieddiet,
    diet_ref = dieticianreferal,
    diet_plan = dieticianplan,
    salt_ref = saltref,
    wgt_admit = weightonadmiss,
    wgt_fup = weightfollowup,
    dis_date = datedischarge,
    aes = adverseevents
  )

  meals <- rename(meals,
    intake = mealintake,
    intake_4 = mealintake4,
    intake_2 = mealintake2,
    intake_3 = mealintake3,
    diet_code = dietcode,
    tray_table = traytable,
    red_tray = redtray,
    assist_reqd = assistreq,
    assist_10min = assist10min,
    assist_provd = assistprov,
    feed_reqd = feedassreq,
    feed_10min = feedin10min,
    feed_provd = feedprovide,
    interrupted = interrupted,
    reason_intrpt = reasinterupt,
    assist_post_int = assistpostinter,
    reason_low_intake = reasonlowintake,
    food_chart = foodchart,
    comment = comment,
    position_2 = position2,
    lr_intake_2 = lrmealintake2
   )

# Class ----
# Id to char
  patients$ptid <- as.character(patients$ptid)

# Convert to factor
# Find variables with 0,1 response and covert to No/Yes
  tar <- unlist(map(patients, function(x) max(x) == 1 & min(x) == 0))
  tar[is.na(tar)] <- FALSE
  patients[tar] <- map(patients[tar], factor, labels = c("No", "Yes"))

# Convert the haven_labelled variables into factors
  tar <- map_lgl(patients, is.labelled)
  patients[tar] <- map(patients[tar], as_factor)

  tar <- map_lgl(meals, is.labelled)
  meals[tar] <- map(meals[tar], as_factor)

# Get rid of the other SPSS attributes
  patients <- zap_formats(patients)
  meals    <- zap_formats(meals)

# Clean up variables ----
  levels(patients$ward) <- c("Surgical", "Medical (A)", "Medical (B)")
  levels(patients$gender) <- c("Male", "Female")
  levels(patients$patient_type) <- c("Medical", "Surgical")
  patients$must_screen <- factor(
    patients$must_screen,
    levels = levels(patients$must_screen),
    labels = c("No", "Yes", "Incomplete", "Incomplete")
  )

# Missnig weights as zeros
  patients$wgt_admit[patients$wgt_admit == 0] <- NA

# Set MUST to missing if MUST screen = 0
# with(patients, table(must_screen, must_score))
  patients$must_score[is.na(patients$must_screen)] <- NA

# Set inake to a number
  meals$intake <- as.numeric(as.character(meals$intake))
  meals$intake[is.na(meals$intake)] <- 0
  meals$intake[meals$intake == 100] <- 1


  levels(meals$diet_code)[5] <- "Medical retriction (salt, fluid,fat)"
  levels(meals$position) <- c("Off ward", "Bed, lying down", "Bed, propped up",
                              "Bed, sitting up", "Chair")

  levels(meals$tray_table) <- c("Not in reach", "Within reach")
  levels(meals$red_tray) <- c("Normal tray", "Red tray")


# New variables ----------------------------------------------------------------

# Categorize the must score
  patients$must_cat <- factor(
    patients$must_score,
    levels = levels(factor(patients$must_score)),
    labels = c("Low", "Medium", rep("High", 5))
  )

  patients$must_cat2 <- factor(
    patients$must_score,
    levels = levels(factor(patients$must_score)),
    labels = c("Low", rep("High", 6))
  )
# with(patients, table(must_score, must_cat))

# Add in at least one red-tray as a patient level variable.

  patients <- left_join(
    patients,
    filter(meals, red_tray == "red tray") %>%
      select(ptid, red_tray) %>%
      distinct(),
    by = "ptid"
  )

  patients$red_tray[is.na(patients$red_tray)] <- levels(patients$red_tray)[1]
  levels(patients$red_tray) <- c("No", "Yes" )

# We have 295 meals recorded on 63 patients. They want to predict who doesn't
# finish their meals. The main issue is that they categorize the outcome twice:
# it's whether they have 2 or more meals in a 48 hour period where they finish
# < 50% of the food on their plate.

# I want to see if we can analyze this without so much information loss.

# Data structure - mealintake/3meals/2days/patients

  meals <- select(meals, ptid, day, meal, intake, everything()) %>%
    arrange(ptid, day, meal)

  # group_by(meals, ptid) %>%
  #   summarise(n = n()) %>%
  #   ggplot(aes(x = n)) + geom_histogram() +
  #   ggtitle("Number of total meals per patient") +
  #   xlab("Number of meals") + ylab("Count")

# Some patients have more than 6 obervations (more than 2 days X 3 meals)

  meals <- full_join(
    meals,
    group_by(meals, ptid) %>% summarise(obs = n()),
    by = "ptid"
  )

# View(filter(meals, obs > 6))

  meals$meal <- factor(meals$meal, labels = c("Breakfast", "Lunch", "Tea"))

# Add a marker for meal-day
  meals$which_meal <- with(meals, interaction(meal, day))
  meals$which_meal <- factor(
    meals$which_meal,
    labels = c("Breakfast Day 1", "Lunch Day 1", "Tea Day 1",
               "Breakfast Day 2", "Lunch Day 2", "Tea Day 2")
    )

# Get the average meal completion for each patient and reorder patients based
# on that.

  meals <- full_join(
    meals,
    group_by(meals, ptid) %>%
      summarise(mean_comp = mean(intake, na.rm = TRUE)),
    by = "ptid"
  ) %>%
    mutate(ptid = reorder(ptid, intake))


# More cleaning
# There is only 1 person reorting off ward for bed position...set to missing
  meals$position[meals$position == "Off ward"] <- NA

# Convert yes/no to Yes/No

  meals$assist_reqd <- factor(meals$assist_reqd, labels = c("No", "Yes"))
  meals$feed_reqd <- factor(meals$feed_reqd, labels = c("No", "Yes"))

  meals <- droplevels(meals)

  patients <- patients %>%
    mutate(
      cfs_cat = factor(
        case_when(
          cfs %in% c("Very fit", "Well", "Managing well", "Vulnerable") ~ "Low",
          !cfs %in% c("Very fit", "Well", "Managing well", "Vulnerable") ~ "High"
        )
      )
    )




# Plots ------------------------------------------------------------------------

# Heat map of meal intakes
  # ggplot(meals, aes(y = ptid, x = which_meal,
  #                   fill = factor(mealintake))) +
  #   geom_tile() +
  #   scale_fill_viridis("Meal Intake", discrete = TRUE,
  #                      guide = guide_legend(reverse = TRUE)) +
  #   xlab("") + ylab("Patient ID") +
  #   theme(legend.position = "bottom",
  #         panel.background = element_blank(),
  #         axis.text.y = element_text(size = 6))
  #
  # ggsave("plots/intakes.png", height = 19.05, width = 33.86,
  #        units = "cm", scale = 0.7)


# Distributions of meal intakes

  # ggplot(meals, aes(x = mealintake)) + geom_histogram()
  #
  # ggplot(meals, aes(x = mealintake)) + geom_histogram() +
  #   facet_wrap(~meal, ncol = 1)

# Models -----------------------------------------------------------------------

# Bayesian
# https://kevinstadler.github.io/blog/bayesian-ordinal-regression-with-random-effects-using-brms/

# mixor
# https://www.sciencedirect.com/science/article/abs/pii/0169260796017208

  # library(mixor)
  # library(sjPlot)
  #
  # meals <- arrange(meals, ptid, day, meal)
  #
  # basic <- mixor(mealintake ~ meal + day, data = meals,
  #                id = ptid, link = "probit")
  #
  # summary(basic)

# View -------------------------------------------------------------------------

  # summarytools::view(summarytools::dfSummary(patients))
  # summarytools::view(summarytools::dfSummary(meals))

# Save data --------------------------------------------------------------------

  save(meals, patients, file = "data/data.Rdata")

# Queries ----------------------------------------------------------------------

# There is a mismatch on wards and patient_type
  # with(patients, table(ward, patient_type))

# Can match any red_tray by surgical/medical numbers to Corina's table 1 (p10)
  # with(patients, table(red_tray, patient_type))
  # with(patients, table(red_tray, ward))

# Why do many missing weights at admission? Follow-up?
# How are MUST scores done then?


# Old code ---------------------------------------------------------------------
#   library(tidyverse)
#   library(haven)
#
# # Read in the data
# # Convert labelled values to factors
#
#   nutrition <- read_spss("data/Nutritionobs_3.sav") %>%
#     mutate_if(is.labelled, as_factor)
#   names(nutrition) <- tolower(names(nutrition))
#
#   profile <- read_spss("data/D3 NutritionProfile.sav") %>%
#     arrange(StudyID) %>%
#     mutate_if(is.labelled, as_factor)
#   names(profile) <- tolower(names(profile))

