#### Set-up ####
# Clear environment
rm(list = ls())

# Set working directory
setwd("~/Library/CloudStorage/OneDrive-Nexus365/01. Oxford/Year 2/Political Economy of Inequality and Democracy/Essay")

# Load libraries
library(haven)
library(tidyverse)
library(igraph)
library(rlang)
library(glue)
library(ggplot2)
library(plm)
library(lmtest)
library(sandwich)
library(lavaan)
library(stargazer)
library(VIM)
library(MatchIt)

#### Create long-format bhps panel ####
# # Load BHPS datasets
# ba_indresp <- read_dta("bhps/ba_indresp.dta")
# bb_indresp <- read_dta("bhps/bb_indresp.dta")
# bc_indresp <- read_dta("bhps/bc_indresp.dta")
# bd_indresp <- read_dta("bhps/bd_indresp.dta")
# be_indresp <- read_dta("bhps/be_indresp.dta")
# bf_indresp <- read_dta("bhps/bf_indresp.dta")
# bg_indresp <- read_dta("bhps/bg_indresp.dta")
# bh_indresp <- read_dta("bhps/bh_indresp.dta")
# bi_indresp <- read_dta("bhps/bi_indresp.dta")
# bj_indresp <- read_dta("bhps/bj_indresp.dta")
# bk_indresp <- read_dta("bhps/bk_indresp.dta")
# bl_indresp <- read_dta("bhps/bl_indresp.dta")
# bm_indresp <- read_dta("bhps/bm_indresp.dta")
# bn_indresp <- read_dta("bhps/bn_indresp.dta")
# bo_indresp <- read_dta("bhps/bo_indresp.dta")
# bp_indresp <- read_dta("bhps/bp_indresp.dta")
# bq_indresp <- read_dta("bhps/bq_indresp.dta")
# br_indresp <- read_dta("bhps/br_indresp.dta")
# 
# # Create wave identifiers for BHPS
# waves <- c("ba", "bb", "bc", "bd", "be", "bf", "bg", "bh", "bi", "bj", "bk", 
#            "bl", "bm", "bn", "bo", "bp", "bq", "br")
# 
# # Create list of BHPS datasets
# bhps_datasets <- list(
#   ba_indresp, bb_indresp, bc_indresp, bd_indresp, be_indresp,
#   bf_indresp, bg_indresp, bh_indresp, bi_indresp, bj_indresp,
#   bk_indresp, bl_indresp, bm_indresp, bn_indresp, bo_indresp,
#   bp_indresp, bq_indresp, br_indresp)
# 
# # BHPS dataset names as strings
# bhps_strings <- c(
#   "ba_indresp", "bb_indresp", "bc_indresp", "bd_indresp", "be_indresp",
#   "bf_indresp", "bg_indresp", "bh_indresp", "bi_indresp", "bj_indresp",
#   "bk_indresp", "bl_indresp", "bm_indresp", "bn_indresp", "bo_indresp",
#   "bp_indresp", "bq_indresp", "br_indresp")
# 
# # Add wave variable to each BHPS dataset
# for (i in seq_along(bhps_datasets)) {
#   bhps_datasets[[i]]$wave <- waves[i]
#   assign(bhps_strings[i], bhps_datasets[[i]])
# }
# 
# # Define function to remove wave prefixes from variable names
# remove_wave_prefixes <- function(data, waves) {
#   pattern <- paste0("^(", paste(waves, collapse = "|"), ")_")
#   new_names <- gsub(pattern, "", names(data))
#   colnames(data) <- new_names
#   return(data)
# }
# 
# # Apply the function to each BHPS dataset
# bhps_datasets_clean <- lapply(bhps_datasets, remove_wave_prefixes, waves)
# 
# # Combine all BHPS datasets into one
# bhps <- bind_rows(bhps_datasets_clean)
# 
# write_dta(bhps, "bhps_panel.dta")

#### Identify families ####
# Load datasets
ba_egoalt <- read_dta("bhps/ba_egoalt.dta")
bb_egoalt <- read_dta("bhps/bb_egoalt.dta")
bc_egoalt <- read_dta("bhps/bc_egoalt.dta")
bd_egoalt <- read_dta("bhps/bd_egoalt.dta")
be_egoalt <- read_dta("bhps/be_egoalt.dta")
bf_egoalt <- read_dta("bhps/bf_egoalt.dta")
bg_egoalt <- read_dta("bhps/bg_egoalt.dta")
bh_egoalt <- read_dta("bhps/bh_egoalt.dta")
bi_egoalt <- read_dta("bhps/bi_egoalt.dta")
bj_egoalt <- read_dta("bhps/bj_egoalt.dta")
bk_egoalt <- read_dta("bhps/bk_egoalt.dta")
bl_egoalt <- read_dta("bhps/bl_egoalt.dta")
bm_egoalt <- read_dta("bhps/bm_egoalt.dta")
bn_egoalt <- read_dta("bhps/bn_egoalt.dta")
bo_egoalt <- read_dta("bhps/bo_egoalt.dta")
bp_egoalt <- read_dta("bhps/bp_egoalt.dta")
bq_egoalt <- read_dta("bhps/bq_egoalt.dta")
br_egoalt <- read_dta("bhps/br_egoalt.dta")

# Define function to remove wave prefixes
remove_prefix <- function(dataset, prefix) {
  colnames(dataset) <- gsub(paste0("^", prefix, "_"), "", colnames(dataset))
  return(dataset)
}

# Standardise variable names for all datasets
ba_egoalt <- remove_prefix(ba_egoalt, "ba")
bb_egoalt <- remove_prefix(bb_egoalt, "bb")
bc_egoalt <- remove_prefix(bc_egoalt, "bc")
bd_egoalt <- remove_prefix(bd_egoalt, "bd")
be_egoalt <- remove_prefix(be_egoalt, "be")
bf_egoalt <- remove_prefix(bf_egoalt, "bf")
bg_egoalt <- remove_prefix(bg_egoalt, "bg")
bh_egoalt <- remove_prefix(bh_egoalt, "bh")
bi_egoalt <- remove_prefix(bi_egoalt, "bi")
bj_egoalt <- remove_prefix(bj_egoalt, "bj")
bk_egoalt <- remove_prefix(bk_egoalt, "bk")
bl_egoalt <- remove_prefix(bl_egoalt, "bl")
bm_egoalt <- remove_prefix(bm_egoalt, "bm")
bn_egoalt <- remove_prefix(bn_egoalt, "bn")
bo_egoalt <- remove_prefix(bo_egoalt, "bo")
bp_egoalt <- remove_prefix(bp_egoalt, "bp")
bq_egoalt <- remove_prefix(bq_egoalt, "bq")
br_egoalt <- remove_prefix(br_egoalt, "br")

# Create single dataset
egoalt_bhps <- bind_rows(
  list(
    ba = ba_egoalt,
    bb = bb_egoalt,
    bc = bc_egoalt,
    bd = bd_egoalt,
    be = be_egoalt,
    bf = bf_egoalt,
    bg = bg_egoalt,
    bh = bh_egoalt,
    bi = bi_egoalt,
    bj = bj_egoalt,
    bk = bk_egoalt,
    bl = bl_egoalt,
    bm = bm_egoalt,
    bn = bn_egoalt,
    bo = bo_egoalt,
    bp = bp_egoalt,
    bq = bq_egoalt,
    br = br_egoalt),
  .id = "wave")

# Recode wave variable
egoalt_bhps <- egoalt_bhps %>%
  mutate(wave = case_when(
    wave == "ba" ~ 1,
    wave == "bb" ~ 2,
    wave == "bc" ~ 3,
    wave == "bd" ~ 4,
    wave == "be" ~ 5,
    wave == "bf" ~ 6,
    wave == "bg" ~ 7,
    wave == "bh" ~ 8,
    wave == "bi" ~ 9,
    wave == "bj" ~ 10,
    wave == "bk" ~ 11,
    wave == "bl" ~ 12,
    wave == "bm" ~ 13,
    wave == "bn" ~ 14,
    wave == "bo" ~ 15,
    wave == "bp" ~ 16,
    wave == "bq" ~ 17,
    wave == "br" ~ 18))

# Recode relationship variable
egoalt_bhps$relationship_bh <- as_factor(egoalt_bhps$relationship_bh)

# Assign child status
egoalt_bhps <- egoalt_bhps %>%
  mutate(
    child = case_when(
      relationship_bh %in% c("natural son/daughter",
                             "stepson/stepdaughter",
                             "other child") ~ "child",
      TRUE ~ NA_character_))

# Assign parent status
egoalt_bhps <- egoalt_bhps %>%
  mutate(
    parent = case_when(
      relationship_bh %in% c("natural son/daughter",
                             "stepson/stepdaughter",
                             "other child") & asex == 1 ~ "apidp is father",
      relationship_bh %in% c("natural son/daughter",
                             "stepson/stepdaughter",
                             "other child") & asex == 2 ~ "apidp is mother",
      TRUE ~ NA_character_))

# Find fathers
father_ids <- egoalt_bhps %>%
  filter(parent == "apidp is father") %>%
  pull(apidp) %>%
  unique()

# Find mothers
mother_ids <- egoalt_bhps %>%
  filter(parent == "apidp is mother") %>%
  pull(apidp) %>%
  unique()

# Find children
child_ids <- egoalt_bhps %>%
  filter(child == "child") %>%
  pull(pidp) %>%
  unique()

# Assign person variable
egoalt_bhps <- egoalt_bhps %>%
  mutate(
    n_roles = (pidp %in% father_ids) + (pidp %in% mother_ids) + (pidp %in% child_ids),
    person = case_when(
    n_roles != 1 ~ "child",
    pidp %in% father_ids ~ "father",
    pidp %in% mother_ids ~ "mother",
    pidp %in% child_ids ~ "child",
    TRUE ~ NA_character_)) %>% 
  select(-n_roles, -child, -parent)

# Keep families with at least one child
egoalt_bhps <- egoalt_bhps %>%
  filter(!is.na(person)) %>%
  group_by(hidp) %>%
  filter(any(person == "child") & any(person %in% c("mother", "father"))) %>%
  ungroup() %>%
  distinct(pidp, .keep_all = TRUE)

#### Clean panel ####
# Load panel
bhps_raw <- read_dta("bhps_panel.dta")

# Recode wave variable
bhps <- bhps_raw %>%
  mutate(wave = case_when(
    wave == "ba" ~ 1,
    wave == "bb" ~ 2,
    wave == "bc" ~ 3,
    wave == "bd" ~ 4,
    wave == "be" ~ 5,
    wave == "bf" ~ 6,
    wave == "bg" ~ 7,
    wave == "bh" ~ 8,
    wave == "bi" ~ 9,
    wave == "bj" ~ 10,
    wave == "bk" ~ 11,
    wave == "bl" ~ 12,
    wave == "bm" ~ 13,
    wave == "bn" ~ 14,
    wave == "bo" ~ 15,
    wave == "bp" ~ 16,
    wave == "bq" ~ 17,
    wave == "br" ~ 18))

# Assign person variable
relationships <- egoalt_bhps %>% 
  distinct(pidp, person)

bhps <- bhps %>%
  left_join(relationships, by = "pidp")

# Remove those with NA in the person variable
bhps <- bhps %>%
  filter(!is.na(person))

# Create a wave‐hidp household identifier
bhps <- bhps %>%
  mutate(hh_wave = paste0("w", wave, "_hh", hidp))

# Build edge list linking each pidp to its hh_wave
edges <- bhps %>%
  select(pidp, hh_wave) %>%
  distinct()

# Build bipartite graph
g <- graph_from_data_frame(edges, directed = F)

# Compute connected components
comps <- components(g)

membership_df <- tibble(
  name = names(comps$membership),
  component = as.integer(comps$membership))

# Extract household nodes and do component mapping
hh_map <- membership_df %>%
  filter(grepl("^w[0-9]+_hh", name)) %>%
  transmute(
    hh_wave = name,
    hh_across  = component)

# Add cross-wave household identifier to panel
bhps <- bhps %>%
  left_join(hh_map, by = "hh_wave")

# Move variables
bhps <- bhps %>%
  relocate(hh_across, .after = 3) %>%
  relocate(person, .after = 4)

# Subset to variables of interest
bhps <- bhps %>% 
  select(
    # Identifiers
    pidp, wave, hh_across, person, 
    # Covariates
    gor_dv, ivfio, sex, age, hiqualb_dv, jlnssec8_dv, jbnssec8_dv, paynu_dv, payu, fimngrs_dv, 
    # Employment status
    jbstat,
    # Redistribution preferences
    opsoca, opsocb, opsocc, opsocd, opsoce, opsocf, 
    # Employment expectations
    futrb, futrc, futrd, futre, jbsat4,
    # Partisanship
    vote1, vote2, vote4, vote5)

# Initialise long-format panel
bhps_long <- bhps

# Create unemployment dummy
bhps_long <- bhps_long %>% 
  mutate(unempl = case_when(
    jbstat == 3 ~ 1,
    TRUE ~ 0))

# Recode job status as factor
bhps_long$jbstat <- as_factor(bhps_long$jbstat)

# Keep families with at least one child and one parent
bhps_long <- bhps_long %>%
  group_by(hh_across) %>%
  filter(any(person == "child", na.rm = T) & any(person %in% c("mother", "father"), na.rm = T)) %>%
  ungroup()

# Recode redistribution preferences
bhps_long <- bhps_long %>% 
  mutate(redis = case_when(
    opsoce == 1 ~ 5,
    opsoce == 2 ~ 4,
    opsoce == 3 ~ 3,
    opsoce == 4 ~ 2,
    opsoce == 5 ~ 1,
    TRUE ~ NA))

# Re-code partisanship
bhps_long$vote1 <- ifelse(bhps_long$vote1 == 1, "Yes",
                     ifelse(bhps_long$vote1 == 2, "No", NA))

bhps_long$vote2 <- ifelse(bhps_long$vote2 == 1, "Yes",
                     ifelse(bhps_long$vote2 == 2, "No", 
                            ifelse(bhps_long$vote2 == -8, "Inapplicable", NA)))

bhps_long$vote4 <- ifelse(bhps_long$vote4 == 1, "Conservative",
                     ifelse(bhps_long$vote4 == 2, "Labour", 
                            ifelse(bhps_long$vote4 == 3, "Lib Dem", 
                                   ifelse(bhps_long$vote4 == -8, "Inapplicable",
                                          ifelse((bhps_long$vote4 < 0 & bhps_long$vote4 != -8), NA, "Other party")))))

bhps_long$vote5 <- ifelse(bhps_long$vote5 == -8, "Inapplicable",
                     ifelse((bhps_long$vote5 < 0 & bhps_long$vote5 != -8), NA, bhps_long$vote5))
bhps_long$vote5 <- ifelse(bhps_long$vote5 == 1, 3, 
                     ifelse(bhps_long$vote5 == 3, 1, 2))

#### Create children dataset ####
# Create children and waves subset
bhps_children <- bhps_long %>% 
  filter(person == "child",
         wave %in% c(1, 3, 5, 7, 10, 14, 17)) %>% 
  filter(age >= 15 & age <= 21) %>%
  ungroup()

# Extract unemployment of fathers in each wave
unempl_father <- bhps_long %>%
  filter(person %in% c("father"),
         wave %in% c(1, 3, 5, 7, 10, 14, 17)) %>%
  select(
    hh_across,
    wave,
    father_age = age, 
    father_hiqual = hiqualb_dv, 
    father_lastjb = jlnssec8_dv,
    father_curjb = jbnssec8_dv, 
    father_grsinc = fimngrs_dv, 
    father_pidp = pidp,
    father_unempl = unempl,
    father_redis = redis,
    father_vote1 = vote1,
    father_vote2 = vote2,
    father_vote4 = vote4,
    father_vote5 = vote5)

# Extract unemployment of mothers in each wave
unempl_mother <- bhps_long %>%
  filter(person %in% c("mother"),
         wave %in% c(1, 3, 5, 7, 10, 14, 17)) %>%
  select(
    hh_across,
    wave,
    mother_age = age, 
    mother_hiqual = hiqualb_dv, 
    mother_lastjb = jlnssec8_dv,
    mother_curjb = jbnssec8_dv, 
    mother_grsinc = fimngrs_dv, 
    mother_pidp = pidp,
    mother_unempl = unempl,
    mother_redis = redis,
    mother_vote1 = vote1,
    mother_vote2 = vote2,
    mother_vote4 = vote4,
    mother_vote5 = vote5)

# Merge with children dataset
bhps_children <- bhps_children %>%
  inner_join(unempl_father, by = c("hh_across", "wave")) %>%
  inner_join(unempl_mother, by = c("hh_across", "wave"))

# Recode waves
bhps_children <- bhps_children %>%
  mutate(wave = case_when(
    wave == 1 ~ 1,
    wave == 3 ~ 2,
    wave == 5 ~ 3,
    wave == 7 ~ 4,
    wave == 10 ~ 5,
    wave == 14 ~ 6,
    wave == 17 ~ 7))

# Keep only unique child-wave combinations
bhps_children <- bhps_children %>%
  arrange(pidp, wave) %>% 
  distinct(pidp, wave, 
           .keep_all = T) 

# Create variable based on unemployment of at least one parent
bhps_children <- bhps_children %>% 
  mutate(treat = case_when(
    father_unempl == 1 | mother_unempl == 1 ~ 1,
    TRUE ~ 0))

# Re-code occupation
bhps_children$father_jb <- ifelse(bhps_children$father_curjb == -8, 
                                  bhps_children$father_lastjb, 
                                  bhps_children$father_curjb) 

bhps_children$mother_jb <- ifelse(bhps_children$mother_curjb == -8, 
                                  bhps_children$mother_lastjb, 
                                  bhps_children$mother_curjb) 

bhps_children$father_jb <- ifelse(bhps_children$father_jb < 0, NA, bhps_children$father_jb) 

bhps_children$mother_jb <- ifelse(bhps_children$mother_jb < 0, NA, bhps_children$mother_jb) 

bhps_children$parent_highjb <- ifelse(bhps_children$father_jb <= bhps_children$mother_jb,  
                                      bhps_children$father_jb,
                                      bhps_children$mother_jb)

# Re-code education
bhps_children$father_hiqual <- ifelse(bhps_children$father_hiqual < 0, NA, bhps_children$father_hiqual) 

bhps_children$mother_hiqual <- ifelse(bhps_children$mother_hiqual < 0, NA, bhps_children$mother_hiqual) 

bhps_children$parent_highqual <- ifelse(bhps_children$father_hiqual < bhps_children$mother_hiqual,  
                                        bhps_children$father_hiqual,
                                        bhps_children$mother_hiqual)

# Re-code income as decile
bhps_children$father_grsinc <- ifelse(bhps_children$father_grsinc < 0, NA, bhps_children$father_grsinc) 

bhps_children$mother_grsinc <- ifelse(bhps_children$mother_grsinc < 0, NA, bhps_children$mother_grsinc) 

bhps_children <- bhps_children %>%
  filter(!is.na(father_grsinc)) %>%  
  mutate(father_decile = ntile(father_grsinc, 10))

bhps_children <- bhps_children %>%
  filter(!is.na(mother_grsinc)) %>%  
  mutate(mother_decile = ntile(mother_grsinc, 10))

bhps_children$dec_highinc <- ifelse(bhps_children$father_decile >= bhps_children$mother_decile,
                                       bhps_children$father_decile,
                                       bhps_children$mother_decile)

# Re-code income as percentile
bhps_children <- bhps_children %>%
  filter(!is.na(father_grsinc)) %>%  
  mutate(father_perc = ntile(father_grsinc, 100))

bhps_children <- bhps_children %>%
  filter(!is.na(mother_grsinc)) %>%  
  mutate(mother_perc = ntile(mother_grsinc, 100))

bhps_children$perc_highinc <- ifelse(bhps_children$father_perc >= bhps_children$mother_perc,
                                       bhps_children$father_perc,
                                       bhps_children$mother_perc)

# Re-code unemployment expectations
bhps_children$futre <- ifelse(bhps_children$futre < 0 | bhps_children$futre > 100, NA, bhps_children$futre)

#### SEM for socialisation pathway ####
# Prepare the data
bhps_children$parent_redis <- bhps_children$mother_redis + bhps_children$father_redis
bhps_children$parent_redis <- scale(bhps_children$parent_redis, center = T, scale = T)

bhps_children$perc_highinc_c <- scale(bhps_children$perc_highinc, center = T, scale = T)

bhps_children$mother_con <- ifelse(bhps_children$mother_vote4 == "Conservative", 1, 0)
bhps_children$father_con <- ifelse(bhps_children$father_vote4 == "Conservative", 1, 0)
bhps_children$mother_lab <- ifelse(bhps_children$mother_vote4 == "Labour", 1, 0)
bhps_children$father_lab <- ifelse(bhps_children$father_vote4 == "Labour", 1, 0)

bhps_children$fcon_inc <- bhps_children$perc_highinc_c * bhps_children$father_con
bhps_children$mcon_inc <- bhps_children$perc_highinc_c * bhps_children$mother_con
bhps_children$flab_inc <- bhps_children$perc_highinc_c * bhps_children$father_lab
bhps_children$mlab_inc <- bhps_children$perc_highinc_c * bhps_children$mother_lab

panel_spec <- '
  # Within‐person
  parent_redis ~ b0*perc_highinc_c
                + b1*father_con + b2*father_lab
                + b3*mother_con + b4*mother_lab
                + b5*fcon_inc   + b6*flab_inc
                + b7*mcon_inc   + b8*mlab_inc

  redis  ~ a1*parent_redis

  # Conditional indirect effects
  indirect_fcon := (b0 + b5) * a1
  indirect_flab := (b0 + b6) * a1
  indirect_mcon := (b0 + b7) * a1
  indirect_mlab := (b0 + b8) * a1
'

fit_panel <- sem(
  panel_spec,
  data = bhps_children,
  cluster = "pidp",
  missing = "FIML",
  estimator = "MLR",
  fixed.x = F)

summary(fit_panel, standardized = T, rsquare = T)

#### SEM for expectations pathway ####
# Prepare the data
bhps_children$perc_highinc_c <- scale(bhps_children$perc_highinc, center = T, scale = T)

bhps_children$futre <- scale(bhps_children$futre, center = T, scale = T)

panel_spec <- '
  # Within‐person
  futre ~ b0*perc_highinc_c

  redis  ~ a1*futre

  # Conditional indirect effects
  indirect_futre := b0 * a1
'

fit_panel <- sem(
  panel_spec,
  data = bhps_children,
  cluster = "pidp",
  missing = "FIML",
  estimator = "MLR",
  fixed.x = F)

summary(fit_panel, standardized = T, rsquare = T)


#### Matching ####
# Create ever unemployed variable
bhps_children <- bhps_children %>% 
  group_by(pidp) %>% 
  mutate(treat_ever = case_when(
    any(treat == 1) ~ 1,
    TRUE ~ 0)) %>% 
  ungroup()

# Table of treatment groups
table(bhps_children$treat_ever)

# Re-code covariates for matching
vars <- c("parent_highqual", "parent_highjb", "perc_highinc")

# Impute missing values based on previous waves
bhps_children <- bhps_children %>%
  group_by(hh_across) %>% 
  arrange(wave) %>% 
  fill(all_of(vars),
       .direction = "downup") %>% 
  ungroup()

# Re-code variables for hotdeck imputation
bhps_children <- bhps_children %>%
  mutate(
    across(c(parent_highqual, gor_dv), as.factor),
    perc_highinc = as.numeric(perc_highinc),
    mother_age  = as.numeric(mother_age),
    father_age  = as.numeric(father_age))

# Hotdeck impute parental highest income 
imp_vars <- c("parent_highqual", "perc_highinc", "gor_dv", "mother_age", "father_age")

bhps_children <- kNN(
  data = bhps_children,
  variable = "parent_highjb",
  k = 5,
  dist_var = imp_vars,
  imp_var = F)

# Hotdeck impute parental highest qualification 
imp_vars <- c("parent_highjb", "perc_highinc", "gor_dv", "mother_age", "father_age")

bhps_children <- kNN(
  data = bhps_children,
  variable = "parent_highqual",
  k = 5,
  dist_var = imp_vars,
  imp_var = F)

# Examine balance of covariates before matching
raw <- lm(treat_ever ~ gor_dv + parent_highqual + parent_highjb + perc_highinc + mother_age + father_age,
           data = bhps_children)

# Do matching
m.out <- matchit(
  formula = treat_ever ~ gor_dv + parent_highqual + parent_highjb + perc_highinc + mother_age + father_age,
  data = bhps_children,
  method = "nearest",
  distance = "glm",
  ratio = 3,
  replace = F)

summary(m.out)

# Extract matched data
bhps_matched <- match.data(m.out)

# Examine balance of covariates after matching
matched <- lm(treat_ever ~ gor_dv + parent_highqual + parent_highjb + perc_highinc + mother_age + father_age,
           data = bhps_matched)

# Table
stargazer(raw, matched)

#### Impact of parental unemployment on child redistribution preferences ####
# Naive regression
naive_children <- lm(redis ~ treat, 
                     data = bhps_matched)

summary(naive_children)

# Define indices
bhps_matched <- pdata.frame(bhps_matched, index = c("pidp", "wave"))

# Two-way fixed effects within regression with cluster-robust SEs
fe_children <- plm(redis ~ treat + factor(wave),
                   data  = bhps_matched,
                   model = "within")

coeftest(fe_children, vcovHC(fe_children, type = "HC1", cluster = "group"))

# First difference regression with cluster-robust SEs
fd_children <- plm(redis ~ treat, 
                   data = bhps_children, 
                   model = "fd")

coeftest(fd_children, vcovHC(fd_children, cluster = "group")) 

# System GMM
gmm_children <- pgmm(redis ~ lag(redis, 1) + treat | lag(redis, 2:6) +  lag(treat, 1:6),
                     data = bhps_matched,
                     effect = "twoways",
                     model = "twosteps",
                     transformation = "ld",
                     collapse = T)

summary(gmm_children)

# Table
stargazer(gmm_children)