#import libraries - I use pacman to load libraries as it keeps them uptodate and installs any library I am missing
#if you dont have pacman you will need to run install.packages("pacman")
library(pacman)
p_load(tidyverse, checkmate, stringr, lubridate, gdata, readxl, gmodels, naniar, openxlsx, ggpubr, janitor, skimr, REDCapR, hablar, mefa)
p_load(haven, rebus)



# APCD data ---------------------------------------------------------------


# APDC Step 1 read in and tidy files-------------------------------------------------------------

# Read in the SAS data
#this assumes all the files are in your working directory
apdc <- read_sas("apdc_sensitive.sas7bdat", NULL)
data_dict <- read_csv("apdc_formats.csv")
apdc_fac_ident <- read_csv("apdc_fac_ident.csv")%>%
  select(coded = start, chr_label = label)

#recode the variables with a number to their character versions using the data dictionary
recode_mode_sep <- data_dict %>%
  filter(DomainItemName == "Mode of Separation (Admitted Patient)") %>%
  select(DomainValueCode, DomainValueDescriptiveTerm)

recode_drg_mode_sep <- data_dict %>%
  filter(DomainItemName == "drg_separation_mode") %>%
  select(DomainValueCode, DomainValueDescriptiveTerm)

recode_ed_status <- data_dict %>%
  filter(DomainItemName == "Emergency Department Status") %>%
  select(DomainValueCode, DomainValueDescriptiveTerm)

recode_care_type <- data_dict %>%
  filter(DomainItemName == "Service Category") %>%
  select(DomainValueCode, DomainValueDescriptiveTerm)

recode_mdc <- data_dict %>%
  filter(DomainItemName == "Major Diagnostic Category") %>%
  select(DomainValueCode, DomainValueDescriptiveTerm)

recode_svg <- data_dict %>%
  filter(DomainItemName == "Service Related Groups") %>%
  select(DomainValueCode, DomainValueDescriptiveTerm)

recode_unit_admission <- data_dict %>%
  filter(DomainItemName == "Bed/Unit Type") %>%
  select(DomainValueCode, DomainValueDescriptiveTerm)

recode_hie_age <- data_dict %>%
  filter(DomainItemName == "Five Year Age Range (HIE)") %>%
  select(DomainValueCode, DomainValueDescriptiveTerm)


# recode coded variable by creating a _chr column
apdc <- apdc %>%
  mutate(mode_separation_chr = recode_mode_sep$DomainValueDescriptiveTerm[match(as.character(apdc$mode_of_separation_recode), as.character(recode_mode_sep$DomainValueCode))],
         drg_mode_separation_chr = recode_drg_mode_sep$DomainValueDescriptiveTerm[match(as.character(apdc$drg_mode_of_separation), as.character(recode_drg_mode_sep$DomainValueCode))],
         ed_status_chr = recode_ed_status$DomainValueDescriptiveTerm[match(as.character(apdc$ed_status), as.character(recode_ed_status$DomainValueCode))],
         episode_of_care_type_chr = recode_care_type$DomainValueDescriptiveTerm[match(as.character(apdc$episode_of_care_type), as.character(recode_care_type$DomainValueCode))],
         facility_identifier_chr = apdc_fac_ident$chr_label[match(as.character(apdc$Facility_identifier_recode), as.character(apdc_fac_ident$coded))],
         facility_trans_from_chr = apdc_fac_ident$chr_label[match(as.character(apdc$facility_trans_from_recode), as.character(apdc_fac_ident$coded))],
         facility_trans_to_chr = apdc_fac_ident$chr_label[match(as.character(apdc$facility_trans_to_recode), as.character(apdc_fac_ident$coded))],
         mdc_chr = recode_mdc$DomainValueDescriptiveTerm[match(as.character(apdc$MDC), as.character(recode_mdc$DomainValueCode))],
         service_group_chr = recode_svg$DomainValueDescriptiveTerm[match(as.character(apdc$srg), as.character(recode_svg$DomainValueCode))],
         unit_admission_chr = recode_unit_admission$DomainValueDescriptiveTerm[match(as.character(apdc$unit_type_on_admission), as.character(recode_unit_admission$DomainValueCode))],
         hie_age_grp_chr = recode_hie_age$DomainValueDescriptiveTerm[match(as.character(apdc$age_recode), as.character(recode_hie_age$DomainValueCode))]
  )

rm(recode_mode_sep, recode_svg, recode_mdc, recode_care_type, recode_ed_status, recode_drg_mode_sep,
   recode_unit_admission,data_dict, recode_hie_age, apdc_fac_ident)

#sort out the times to posixct

apdc <- apdc %>%
  mutate(episode_start_dtg = ymd_hms(str_c(episode_start_date, episode_start_time, sep = " ")),
         procedure_dateP = ymd(procedure_dateP)) %>%
  clean_names()


apdc <- distinct(apdc, ppn, episode_start_dtg, .keep_all = TRUE) # 4391018 - 438443 = 6575 duplicates removed
apdc <- distinct(apdc, .keep_all = TRUE) #no other duplicates

saveRDS(apdc, file = "apdc_raw.RDS")


# APCD Step 2 created the out of APDC facilities-------------------------------------------------------------
apdc_data_fac1 <- as.vector(unique(apdc_raw$facility_trans_from_chr))
apdc_data_fac2 <- as.vector(unique(apdc_raw$facility_trans_to_chr))
apdc_data_fac <- unique(c(apdc_data_fac1, apdc_data_fac2))
apdc_data_fac <- na.omit(apdc_data_fac)
apdc_fac <- as.vector(unique(apdc_raw$facility_identifier_chr))

out_of_apdc_facilities <- apdc_data_fac[!(apdc_data_fac %in% apdc_fac)]

out_of_apdc_facilities <- as.vector(out_of_apdc_facilities$hosp)


# APDC step 3 the make enctrs function-------------------------------------------------------------


make_enounters <- function(x) {
  
  
  
  
  ppn1 <- out[[x]]
  time_stamp_1 <- now()
  group_length <- max(cycle_ppns)
  
  
  print(str_c("processing ppns to inpatient/outpatent status and admission sequence for group", x, "of", group_length, "at", time_stamp_1, sep=" "))
  
  
  create_inpatient_outpatient <- function(y) {
   
    
    
    
    #write a message regarding progress
    ppn_single <- y
    total_ppn <- length(ppn1)
    loctn_of_ppn <- which(ppn_single == ppn1)
    percent_complete_full = (loctn_of_ppn / total_ppn)*100
    percent_complete = round(percent_complete_full, digits=3)
    #writeLines(str_c("processing ppn",loctn_of_ppn, "of",total_ppn, "from group",x , "at", now(), sep=" "))
    #writeLines(str_c(percent_complete, "% complete", sep=" "))
    time_stamp_1 <- now()
    
    #process the ppns
    apdc_enctr_single <- apdc_raw%>%
      filter(ppn == ppn_single) %>%
      replace_na( replace = list(episode_day_stay_los_recode = 0)) %>%
      select(ppn, episode_start_date, episode_start_dtg, episode_length_of_stay, episode_day_stay_los_recode, unit_admission_chr, mode_separation_chr, recnum, facility_identifier_chr, facility_trans_to_chr, drg_mode_separation_chr, ed_status_chr,episode_of_care_type_chr, facility_trans_from_chr) %>%
      mutate(episode_los_hrs = if_else(episode_day_stay_los_recode == 0 | is.na(episode_day_stay_los_recode), episode_length_of_stay*24, episode_day_stay_los_recode, episode_length_of_stay*24)) %>%
      mutate(episode_end_dtg = episode_start_dtg + hours(episode_los_hrs)) %>%
      mutate(episode_end_dtg = ymd_hm(str_c(as.character(as.Date(episode_end_dtg)), "23:59", sep = " "))) %>%
      mutate(episode_pd = episode_start_dtg %--% episode_end_dtg) 
    
    
    col_order <- c("ppn", "episode_start_date", "episode_start_dtg", "episode_length_of_stay", "episode_day_stay_los_recode", "unit_admission_chr",
                   "mode_separation_chr", "recnum", "facility_identifier_chr", "facility_trans_to_chr", "ed_status_chr",
                   "episode_of_care_type_chr", "facility_trans_from_chr", "episode_los_hrs", "episode_end_dtg", "episode_pd","daystay_as_inp")
    
    
    overnight <- apdc_enctr_single %>%
      filter(episode_day_stay_los_recode == 0 | 
               is.na(episode_day_stay_los_recode) | 
               (episode_day_stay_los_recode >0 & unit_admission_chr == "Lodger / Boarder Beds")) %>%
      mutate(inpatient_outpatient = "overnight")
    
    overnight_periods <- as.list(overnight$episode_pd)
    over_night_recnum <- as.vector(overnight$recnum)
    
    
    #separate out daystay procedures and non daystay
    overnight_start_mtrx <- apdc_enctr_single %>%
      expand(episode_start_dtg, episode_pd) %>%
      mutate(start_in_pd = if_else(episode_start_dtg %within% episode_pd, 1,0, NA_real_)) 
    
    
    
    overnight_expanded_start <- overnight_start_mtrx%>%
      group_by(episode_start_dtg) %>%
      summarise(start_in_pds = sum(start_in_pd)) %>%
      ungroup() %>%
      left_join(overnight_start_mtrx, by = "episode_start_dtg") %>%
      filter(start_in_pds >=2)  %>%
      filter(start_in_pd == 1) %>%
      group_by(episode_pd) %>%
      filter(episode_start_dtg == int_start(episode_pd)) %>%
      ungroup() %>%
      inner_join(apdc_enctr_single, by  = c("episode_start_dtg", "episode_pd")) %>%
      mutate(inpatient_outpatient = "episode in overnight pd") %>%
      select(-start_in_pds)
    
    overnight_end_mtrx <- apdc_enctr_single %>%
      expand(episode_end_dtg, episode_pd) %>%
      mutate(end_in_pd = if_else(episode_end_dtg %within% episode_pd, 1,0, NA_real_)) 
    
    overnight_expanded_end <- overnight_end_mtrx %>%
      group_by(episode_end_dtg) %>%
      summarise(end_in_pds = sum(end_in_pd)) %>%
      ungroup() %>%
      left_join(overnight_end_mtrx, by = "episode_end_dtg") %>%
      filter(end_in_pds >1) %>%
      filter(end_in_pd == 1) %>%
      group_by(episode_pd) %>%
      filter(episode_end_dtg == int_end(episode_pd)) %>%
      ungroup() %>%
      inner_join(apdc_enctr_single, by  = c("episode_end_dtg", "episode_pd")) %>%
      mutate(inpatient_outpatient = "episode in overnight pd")%>%
      select(-end_in_pds)
    
    overnight <- overnight %>%
      bind_rows(overnight_expanded_start, overnight_expanded_end) %>%
      arrange(inpatient_outpatient) %>%
      distinct(recnum, .keep_all = TRUE)
    
    
    
    overnight_periods <- as.list(overnight$episode_pd)
    over_night_recnum <- as.vector(overnight$recnum)
    
    day_stay_as_ip <- apdc_enctr_single %>%
      filter(!recnum %in% over_night_recnum) %>%
      filter(episode_start_dtg %within% overnight_periods) %>%
      mutate(inpatient_outpatient = "daystay_as_ip") %>%
      relocate(any_of(col_order))
    
    day_stay_as_out_pt <- apdc_enctr_single %>%
      filter(!recnum %in% over_night_recnum) %>%
      filter(!episode_start_dtg %within% overnight_periods) %>%
      mutate(inpatient_outpatient = "daystay")%>%
      relocate(any_of(col_order))
    
    
    apdc_enctr_single <- overnight %>%
      bind_rows(day_stay_as_out_pt) %>%
      bind_rows(day_stay_as_ip) 
    
    # updated code ------------------------------------------------------------
    
    
    apdc_enctr<- apdc_enctr_single %>%
      arrange(desc(episode_end_dtg)) %>%
      mutate(incriment_int = as.numeric(difftime(episode_start_dtg, lead(episode_end_dtg)), units="hours")) %>%
      mutate(overlaps = map_lgl(seq_along(episode_pd), function(x){
        #Get all Int indexes other than the current one
        y = setdiff(seq_along(episode_pd), x)
        #The interval overlaps with all other intervals
        #return(all(int_overlaps(Int[x], Int[y])))
        #The interval overlaps with any other intervals
        return(any(int_overlaps(episode_pd[x], episode_pd[y])))
      }))%>%
      mutate(overlaps_with = map(seq_along(episode_pd), function(x){
        #Get all Int indexes other than the current one
        y = (seq_along(episode_pd))
        #The interval overlaps with all other intervals
        #return(all(int_overlaps(Int[x], Int[y])))
        #The interval overlaps with any other intervals
        return(which(int_overlaps(episode_pd[x], episode_pd[y])))
      }))%>%
      unnest(overlaps_with) %>%
      group_by(episode_start_dtg) %>%
      slice_min(overlaps_with) %>%
      ungroup() %>%
      arrange(desc(episode_end_dtg)) %>%
      mutate(item_no = row_number()) %>%
      mutate(lag_dist = item_no - overlaps_with) %>%
      mutate(same_as_next = case_when(
        overlaps_with == lead(item_no) | incriment_int <12 ~ "yes",
        lead(mode_separation_chr) ==  "Transfer to other Hospital" & !(facility_trans_to_chr %in% out_of_apdc_facilities) & facility_identifier_chr == facility_trans_from_chr ~ "yes",
        lead(facility_trans_to_chr) %in% out_of_apdc_facilities & facility_trans_from_chr %in% out_of_apdc_facilities ~ "yes",# takes into account patients out and back in to NSW
        lead(mode_separation_chr) %in% c("Transfer to Palliative Care Unit / Hospice", "Transfer to Public Psychiatric Hospital") ~ "yes",
        !is.na(lead(drg_mode_separation_chr)) ~ "no",
        TRUE ~ "no")) %>%
      mutate(incriment = case_when(
        same_as_next == "yes" ~ 0,
        same_as_next == "no" ~ 1))%>%
      mutate(cum_inc = cumsum(incriment),
             enctr = (sum(incriment)+1) - lag(cum_inc)) %>%
      mutate(enctr = case_when(
        is.na(enctr) & incriment == 1 ~lead(enctr)+1,
        is.na(enctr) & incriment == 0 ~ lead(enctr),
        TRUE ~ enctr)) %>%
      #mutate(enctr = if_else(is.na(enctr), 1, enctr, enctr)) %>%
      relocate(ppn, episode_start_dtg, episode_end_dtg, episode_length_of_stay, episode_day_stay_los_recode, facility_identifier_chr, facility_trans_from_chr, facility_trans_to_chr, 
               episode_pd, mode_separation_chr,drg_mode_separation_chr, inpatient_outpatient, incriment_int, overlaps, item_no, overlaps_with, lag_dist, same_as_next, incriment, enctr)
    
    
    # the following loop runts the correction below the same number of times as their are rows. 
    #This allows multiple checks of the correction to the admission
    for (i in 1:nrow(apdc_enctr)) { 
      
      
      apdc_enctr <- apdc_enctr %>%
        mutate(enctr = map_dbl(seq_along(enctr), function(x){ #go along each row
          if (lag_dist[x] >=1) { #if there is an overlap with an item
            
            z <- overlaps_with[x] #record the item it overlaps with
            
            y <- apdc_enctr[[z,"enctr"]] # record the admisson of the overlapped item
            
            apdc_enctr[x,"enctr"] <- y #assign that admission to this row
            
          } else { 
            
            enctr[x] <- enctr[x] #if no overlap then the admisson remains the same 
            
          }
        }))
    }
    
    
    #now create sequential enctr numbers, enctr episode numbers, and enctr ids
    apdc_enctr <- apdc_enctr %>%
      arrange(enctr) %>% 
      mutate(enctr = as.numeric(factor(enctr))) %>%
      group_by(enctr) %>%
      mutate(enctr_episode = as.numeric(factor(episode_start_dtg)))%>%
      mutate(enctr_episode = if_else(is.na(enctr_episode), 1, enctr_episode, 1)) %>% #this corrects of the rare occasion that the above returns an NA
      mutate(max_episodes = max(enctr_episode)) %>%
      mutate(enctr_id = str_c(ppn, as.character(max_episodes),as.character(enctr) )) %>%
      #this final section createss time periods, admit and discharge dates
      mutate(enctr_start_date = min(episode_start_dtg),
             enctr_disch_date = max(episode_end_dtg)) %>%
      mutate(enctr_pd = enctr_start_date %--% enctr_disch_date) %>%
      mutate(days_los = as.numeric(as.duration(enctr_pd), "days")) %>%
      mutate(hours_los = as.numeric(as.duration(enctr_pd), "hours")) %>%
      ungroup()
    
  }
  
  apdc_enctr_single <- map(ppn1, create_inpatient_outpatient) 
  
  
  print(str_c("Now reducing the lists of ppns to a dataframe"))
  
  
  apdc_enctr_completed <- reduce(apdc_enctr_single, rbind.data.frame) %>%
    select(ppn, episode_end_dtg, episode_pd ,inpatient_outpatient ,enctr,recnum, enctr_episode,
           enctr_id, enctr_start_date,  enctr_disch_date,  enctr_pd ,days_los ,hours_los)
  
  
  time_stamp_2 <- now()
  time_to_process <- difftime(time_stamp_2, time_stamp_1, units = "hours")
  time_to_complete <- as.numeric(time_to_process * (group_length - x))
  time_to_complete <- round(time_to_complete, digits = 2)
  
  print(str_c("Completed group " , x , " Estimated total completion time ", time_to_complete, " hours"))
  
  apdc_enctr <<- apdc_enctr_completed
  
  
}


# APDC Step 4. Run the function ----------------------------------------

# create a vector of unique PPNs. This will allow the dataset to be divided as needed

ppns <- as.vector(unique(apdc_raw$ppn))

# specify the chunk number as the number of sets of PPNs. Suggest 500 or 1000
chunk_no=500

# split the vector by chunk number by specifying labels as FALSE

out <- split(ppns, cut(seq_along(ppns),chunk_no,labels = FALSE))

cycle_ppns <- 1:500

#run the make_admissions function
apdc_enctr_list <- map(cycle_ppns, make_encounters)

# reduce the function output to dataframe
apdc_enctr <-reduce(apdc_enctr_list, rbind.data.frame)


# APDC Step 5. Put the function output back into the apdc ----------------------

apdc_enctr <- apdc_enctr %>%
  select(ppn,  recnum, episode_end_dtg, episode_pd, enctr, enctr_start_date,
         enctr_disch_date, enctr_pd, enctr_episode, days_los, hours_los, inpatient_outpatient, enctr_id) %>%  
  replace_with_na_if(.predicate = c(is.character, is.numeric, is.factor, is.integer, is.POSIXct),condition = ~.x == "")

#save the interm file just in case
saveRDS(apdc_enctr, "apdc_enctr_only.RDS")

#join the encounters dataframe to the APDC dataset
apdc <- apdc_raw %>%
  left_join(apdc_enctr, by = c("ppn", "recnum"))


saveRDS(apdc, file = "apdc_with_enctrs.RDS")
