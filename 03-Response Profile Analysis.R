library(dplyr)
library(tidyr)
library(purrr)
library(furrr)
library(future)
library(progressr)
library(ggplot2)
library(stringr)
library(readxl)
source("R/load_to_list.R")
source("R/pivot_responses.R")
source("R/generate_response_profiles.R")
source("R/correlate_response_profiles.R")
source("R/simulate_null_cor.R")



## Response data ----
#load response mapping
response_map <- readRDS("response_map 1.rds")
full_df <- read.csv("Metadata_MM_2025-11-12.csv")



## List of movers and stayers ----
#load("data/movers_and_stayers.Rdata") 

### Functions ##########################################################3
## Select lower triangle of correlation matrix ----
select_lower_triangle <- function(x){
  z <- lower.tri(x)
  return(x[z])
}

## Generate lower triangle correlation matrix
generate_correlation_matrix <- function(x){
  response_profile <- ifelse(xtabs(~responses_revised+cue, data = x) > 1, 1, 0)
  correlation_matrix <- cor(response_profile, method = "pearson")
    return(select_lower_triangle(correlation_matrix))
}

## Z score function

z_score <- function(correlation, null){
  return(correlation - mean(null)/sd(null))
}

## Stats function

stats_func <- function(x,y){
  fisher_x <- atanh(x)
  fisher_y <- atanh(y)
  z <- z_score(fisher_x, fisher_y)
  p <- (sum(x>y)+1)/length(y)
  mu <- mean(fisher_y)
  sigma <- sd(fisher_y)
  shapiro <- shapiro.test(fisher_y)
  return(list(p = p, z_score = z, mu = mu, sigma = sigma, shapiro_test = shapiro))
}
#############################################################################

## Structure response dataset and remove multi-word responses ----
response_cues <- full_df %>%
  left_join(select(response_map, response, revision), by = c("response")) %>%
  unique() %>%
    select(participant, cue, response, revision, condition, response_order) %>%
    mutate(participant = as.factor(participant), 
           cue = as.factor(cue), 
           condition = factor(condition, 
                              levels = c("c", "cl", "n"), 
                              labels = c("child", "classical", "noise"))) %>%
    #slice(which(!str_count(.$response, "\\w+") >1)) %>%
    #left_join(., movers_and_stayers, by = c("cue" = "word")) %>%
    mutate(responses_revised = ifelse(is.na(revision), response, revision)) %>%
  filter(!responses_revised == "")



## Create different dataframes for movers and stayers ----
#response_cues_movers <- response_cues %>% filter(type == "mover1")
#response_cues_stayers <- response_cues %>% filter(type == "stayer1")

## Split by condition and compute correlation matrix ----
#response_split_stayers <- split(response_cues_stayers, response_cues_stayers$condition)
response_split <- split(response_cues, response_cues$condition)


response_split_mats_cor <- map(response_split, function(x){
   generate_correlation_matrix(x)
})

conditions_contr <- list(ccl = c("child", "classical"), cn = c("child", "noise"), cln = c("classical", "noise"))


## Correlate cue-wise correlations ----
cor_ccl <- cor(response_split_mats_cor$child, response_split_mats_cor$classical, method = 'spearman')
cor_cn <- cor(response_split_mats_cor$child, response_split_mats_cor$noise, method = 'spearman')
cor_cln <- cor(response_split_mats_cor$classical, response_split_mats_cor$noise, method = 'spearman')


#response_split_mats_cor_movers <- map(response_split_movers, function(x){
 #  generate_correlation_matrix(x)
#})

## Correlate cue-wise correlations movers ----
#cor_ca_mover <- cor(response_split_mats_cor_movers$child, response_split_mats_cor_movers$adult, method = 'spearman')
#cor_cs_mover <- cor(response_split_mats_cor_movers$child, response_split_mats_cor_movers$short, method = 'spearman')
#cor_as_mover <- cor(response_split_mats_cor_movers$short, response_split_mats_cor_movers$adult, method = 'spearman')



# Establish "future" plan for parallel computing ----
# If Windows --> multisession
# If Linux --> multicore
# If you want serial (non-parallel processing) --> sequential
plan("multisession")


null_list <- list(ccl = list(classical = response_cues %>% filter(condition == "classical"),
                                  child = response_cues %>% filter(condition == "child")),
                  cn = list(child = response_cues %>% filter(condition == "child"),
                                  noise = response_cues %>% filter(condition == "noise")),
                  cln = list(classical = response_cues %>% filter(condition == "classical"),
                                  noise = response_cues %>% filter(condition == "noise"))
)

# Begin simulation ----

# Run in parallel over all combinations of conditions for all word types (this will take several minutes)

with_progress({
  p <- progressor(steps = 1000)
  null_repsim_cor <- map(null_list, function(x){
    
  list(furrr::future_map_dbl(
    seq_len(1000),
    function(i, data) {
    p()
      simulate_null_cor(data)
    },
    data = purrr::map(x, pivot_responses_wide),
    .options = furrr_options(seed = TRUE)
  ))})
})



save(null_repsim_cor, file = "null_repsim_corList_responses_mapped.Rdata")


true_correlations <- list(ccl = cor_ccl,
                          cn = cor_cn,
                          cln = cor_cln
                           )


## Statistical test for representation similarity analysis ----
null_true_test <- map2(
  true_correlations, null_repsim_cor, function(x,y){
    list(stats_func(x, unlist(y)))
  }
)



## Plotting true correlations and null distributions ----
l <- map2(
  null_repsim_cor, true_correlations, function(x,y){
    x_2 <- list(null = data.frame(null = unlist(x)), true = y)
  }
)


p <- imap(l, ~{
        x_null <- .x$null
        x_true <- .x$true
        ggplot(x_null, aes(x = null))+
        geom_histogram(fill = "#ffa20071", color = "black")+
        geom_vline(xintercept = x_true, color = "blue", linewidth = 2)+
        ggtitle(paste("Correlation comparison",.y))
        ggsave(paste("Figures_revised/",.y,".png",sep=""))
    }
  )
