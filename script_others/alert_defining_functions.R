# for all the functions, 
# dat -  the data frame that contains all the historical weekly # of reported cases for all diseases/syndromes 
# varname - variable name for the disease/syndrome you're trying to define the alert for
# the function returns a vector called "alert_diseasename" that contains 1 (alert) or 0 (no alert) as a value.



### Unusual increase (1.5 times the baseline)
unusual_inc_50p <- function(dat, varname){
 
  # create an alert variable where the generated alert will be saved
  alert_var <- paste0("alert_", epitrix::clean_labels(varname))
  
  # 1.5 times increase vs. the baseline
  dat %<>% arrange(province, district, year, week) %>%
    group_by(province, district) %>%
    mutate(
      # Define the baseline = 3 weeks' rolling meean of reported cases 
      p3w_avg = zoo::rollmean(!!sym(varname), k=3, fill=NA, align = "right")) %>%
    mutate(
      p3w_avg = lag(p3w_avg, 1, default = NA)
    ) %>%
    mutate(
      # Define alert
      {{alert_var}} := ifelse(!!sym(varname) > 1.5*p3w_avg, 1, 0)
    )
  
 
  return(dat[,alert_var])
  
}

### Unusual increase (doubling of cases in two consecutive weeks)
unusual_inc_doubling <- function(dat, varname){
  
  # create an alert variable where the generated alert will be saved
  alert_var <- paste0("alert_", epitrix::clean_labels(varname))
  
  # doubling of cases in two consecutive weeks
  dat %<>% arrange(province, district, year, week) %>%
    group_by(province, district) %>%
    mutate(
      {{alert_var}} := ifelse(!!sym(varname) > 2*lag(!!sym(varname)) &
                                lag(!!sym(varname)) > 2*lag(!!sym(varname), 2), 1, 0)
    )
  
  return(dat[,alert_var])
  
}

### Emergence of a cluster 
### -> For this, it will be useful if you verify if these numbers are reported from the same facility.
susp_cluster <- function(dat, varname, 
                         n_cluster # number of cases that constitutes a clutser
                         ){
  
  # create an alert variable where the generated alert will be saved
  alert_var <- paste0("alert_", epitrix::clean_labels(varname))
  
  # size of the cluter is passed as an argument (n_cluster)
  dat %<>% arrange(province, district, year, week) %>%
    group_by(province, district) %>%
    mutate(
      {{alert_var}} := ifelse(!!sym(varname) >= n_cluster, 1, 0)
    )
  
  
  return(dat[,alert_var])
  
}

### Mean + 2SD for malaria
mean_2sd <- function(dat, varname){
  
  # create an alert variable where the generated alert will be saved
  alert_var <- paste0("alert_", epitrix::clean_labels(varname))
  

  dat %<>% 
    ## First calculate mean and sd
    arrange(province, district, week, year) %>%
    group_by(province, district, week) %>%
    mutate(
      mean = lag(zoo::rollmean(!!sym(varname), k=3, fill=NA, align = "right")),
      sd = lag(zoo::rollapply(!!sym(varname), width=3, fill = NA, sd, align = "right")),
      mean_2sd = mean+2*sd
    )%>%
    ## Define an alert
    mutate(
      {{alert_var}} := ifelse(!!sym(varname) > mean_2sd, 1, 0)
    )
  
  
  return(dat[,alert_var])
  
}

### C-SUm method for malaria
csum <- function(dat, varname){
  
  # create an alert variable where the generated alert will be saved
  alert_var <- paste0("alert_", epitrix::clean_labels(varname))
  
  dat %<>% arrange(province, district, year, week) %>%
    group_by(province, district) %>%
    # Calculate the 3 weeks rolling average
    mutate(
      p3w_avg = zoo::rollmean(!!sym(varname), k=3, fill=NA, align = "right")
      
    )%>% ungroup() %>%
    # Past 3 years (this actually needs to be 5 in an ideal world) mean of 3w rolling average
    arrange(province, district, week, year) %>%
    group_by(province, district, week) %>%
    mutate(
      csum = lag(zoo::rollmean(p3w_avg, k=3, fill=NA, align = "right"))
    )%>% 
    # Define the alert
    mutate(
      {{alert_var}} := ifelse(!!sym(varname) > csum, 1, 0)
    )
  
  return(dat[,alert_var])
  
}

### Attack rate above certain threshold
attack_rate <- function(dat, varname, 
                        pop, # Population size per district (df) as an argument 
                        ar_cutoff, # Attack rate cut-off
                        gen_plot=F){
  
  # create an alert variable where the generated alert will be saved
  alert_var <- paste0("alert_", epitrix::clean_labels(varname))
  
  # join with the population data
  dat %<>% mutate(
    clean_name = epitrix::clean_labels(district)
  ) %>% left_join(pop, by = "clean_name") %>% select(-clean_name)
  
  dat %<>% 
    # Calculate the attack rate
    mutate(
      attack_rate = !!sym(varname)/population_2022 *100000
    )%>%
    # generate the alert
    mutate(
      {{alert_var}} := ifelse(attack_rate > ar_cutoff, 1, 0)
    )
  
 
  return(dat[,alert_var])
}
