# assign specific periods to a data.frame containing annual information 
assgin_periods <- function(periods, df, year_var){
  
  period_interval <- diff(periods[1:2]) - 1
  periods_years <- data.frame()
  for(p in periods){
    years <- seq(p, p + period_interval)
    periods_years <- rbind(periods_years, 
                           data.frame(period = rep(p, length(years)),
                                      years = years)
    )
  }
  
  df$period <- NA
  for(p in periods){
    years <- periods_years[periods_years$period == p,]$years
    df$period <- ifelse(df[, year_var] %in% years, p, df$period)
  }
  
  return(df)
}

# calculate ethnic origin prevalence according to classes defined by kerr 
inv_preval_kerr <- function(df, countries, origins, min_period){
  
  df <- df %>% filter(period >= min_period 
                      & Ctry_code %in% countries) %>% 
    mutate(period = as.character(period))
  
  annual_total <- df %>%
    group_by(Ctry_code, period) %>% 
    summarise(total = n())
  
  tmp <- df %>%
    group_by(Ctry_code, period) %>% select(contains("prob")) %>%
    summarise_all(.funs = sum)
  
  tmp <- merge(tmp, annual_total, by = c("period", "Ctry_code"))
  tmp[, grepl("prob", names(tmp))] <- tmp[, grepl("prob", names(tmp))] / tmp$total
  
  tmp <- gather(tmp, key = "origin", value = "origin_share", -period, -total, -Ctry_code)
  tmp$origin <- gsub("prob_", "", tmp$origin)
  tmp <- tmp %>% filter(origin %in% origins) %>% rename(country = Ctry_code) %>% select(-total)

  return(tmp)
}

# 1) construct function to calculate annual stocks of immigrants from countries in the
# country_dict
annual_immigrants <- function(df,
                              destination_country = "US",
                              country_dict = COUNTRY_DICT,
                              min_year = 1980, max_year = 2004){
  
  # exclude source countries with the same origin as the
  # destination countries dominant domestic origin:
  dominant_domestic <- country_dict[country_dict$iso2 == destination_country, ]$origin
  source_countries <- country_dict[country_dict$origin != dominant_domestic, ]$iso2
  
  # filter to these source countries and time window
  df <- df %>% 
    filter(prio_year >= min_year & 
             prio_year <= max_year &
             iso2_ori %in% source_countries &
             iso2_des == destination_country
    ) %>%
    rename(iso2 = iso2_ori, destination_country = iso2_des, 
           year = prio_year, immigrants = flow)
  
  return(df)
}

# 2) counstruct function to calculate the stock of immigrant inventors
# for every ethnic origin
immigrant_origin_stocks <- function(df,
                                    destination_country = "US", periods = FALSE,
                                    country_dict = COUNTRY_DICT){
  
  # get ethnic origins and aggregate annual flows from their respective source countries
  
  if(periods != FALSE){
    df <- df %>%
      merge(country_dict, by = "iso2") %>%
      group_by(period, origin) %>%
      summarize(immigrants = sum(immigrants, na.rm = TRUE)) %>%
      group_by(origin) %>%
      mutate(immigrant_stock = cumsum(immigrants),
             destination_country = destination_country)
  }else{
    df <- df %>%
      merge(country_dict, by = "iso2") %>%
      group_by(year, origin) %>%
      summarize(immigrants = sum(immigrants, na.rm = TRUE)) %>%
      group_by(origin) %>%
      mutate(immigrant_stock = cumsum(immigrants),
             destination_country = destination_country)
  }
  return(df)
}

# 3) add patent inventor residents per year from WIPO and 
# calculate immigrant origin shares:
immigrant_origin_shares <- function(df,
                                    destination_country = "US", periods = FALSE,
                                    min_year = 1980, max_year = 2004){
  
  resident_stocks <- wipo_annual_stocks %>%
    filter(prio_year >= min_year & 
             prio_year <= max_year &
             iso_alpha2_code == destination_country) %>%
    rename(destination_country = iso_alpha2_code, year = prio_year)
  
  if(periods != FALSE){
    resident_stocks <- resident_stocks %>% 
      select(period, destination_country, residents) %>%
      group_by(period, destination_country) %>%
      summarise(residents = sum(residents, na.rm = TRUE))
    
    df <- df %>%
      left_join(resident_stocks, by = c("period", "destination_country")) %>%
      mutate(origin_share = immigrant_stock / residents)
  }else{
    resident_stocks <- resident_stocks %>% 
      select(year, destination_country, residents)
    df <- df %>%
      left_join(resident_stocks, by = c("year", "destination_country")) %>%
      mutate(origin_share = immigrant_stock / residents)
  }
  
  return(df)
}

