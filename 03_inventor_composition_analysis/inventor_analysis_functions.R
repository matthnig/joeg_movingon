#########################################################################
# Description:    Functions for analyzing ethnic origins of inventors   #
# Authors:        Matthias Niggli/CIEB UniBasel                         #
# Date:           28.12.2021                                            #
#########################################################################


#### ETHNIC ORIGIN COMPOSITION: -------------------------------------------

# global level:
origin_dist <- function(df){
        
        df <- df %>% filter(p_year >= 1980 & p_year <= 2015)
        
        annual_total <-  df %>% group_by(p_year) %>% summarise(total = n())
        
        tmp <- df %>% 
                group_by(p_year) %>% 
                select(contains("prob")) %>%
                summarise_all(.funs = sum)
        
        tmp <- merge(tmp, annual_total, by = "p_year")
        tmp[, grepl("prob", names(tmp))] <- tmp[, grepl("prob", names(tmp))] / tmp$total
        
        tmp <- gather(tmp, key = "origin", value = "share", -p_year, -total)
        tmp$origin <- gsub("prob_", "", tmp$origin)
        return(tmp)
}

# abroad and domestic inventors for given origins:
abroad_domestic_fun <- function(df, ORIGINS, COUNTRIES){
        df <- df %>% filter(origin %in% ORIGINS & p_year >= 1980 & p_year <= 2015) %>% 
                mutate(domestic  = "abroad")
        
        for(i in seq(length(ORIGINS))){
                df <- df %>% mutate(domestic = ifelse(
                        origin == ORIGINS[i] & Ctry_code == COUNTRIES[i], "domestic", domestic)
                )
        }
        
        annual_total <- df %>% group_by(origin, p_year) %>% summarise(total = n())
        df <- df %>% group_by(origin, p_year, domestic) %>% 
                summarise(count = n()) %>% merge(annual_total, by = c("origin", "p_year")) %>%
                mutate(share = count / total)
        
        return(df)
}

# calculates weighted sum for all ethnic origins at the country level
inv_comp_ctry <- function(df, country){
        
        annual_total <- filter(df, Ctry_code == country) %>%
                group_by(p_year) %>% summarise(total = n())
        
        tmp <- filter(df, Ctry_code == country) %>%
                group_by(p_year) %>% select(contains("prob")) %>%
                summarise_all(.funs = sum)
        
        tmp <- merge(tmp, annual_total, by = "p_year")
        tmp[, grepl("prob", names(tmp))] <- tmp[, grepl("prob", names(tmp))] / tmp$total
        
        tmp <- gather(tmp, key = "origin", value = "share", -p_year, -total)
        tmp$origin <- gsub("prob_", "", tmp$origin)
        tmp$country <- country
        
        return(tmp)
}

#### DOMINANT D0MESTIC ORIGIN SHARE: -------------------------------------------
# calculates the share of the dominant domestic origin in different countries.
# function requires a vector of countries and a list of their corresponding dominant
# domestic ethnic origins as an input.
dominant_domestic_comparison <- function(countries, domestic_origin, df){
        
        inv_origin_shares <- lapply(countries, function(x) inv_comp_ctry(df, x))
        names(inv_origin_shares) <- countries
        
        country_diff <- data.frame()
        
        for(i in 1:length(inv_origin_shares)){
                tmp <- filter(inv_origin_shares[[i]], origin %in% domestic_origin[[i]])
                tmp <- tmp %>% mutate(dominant_share = share) %>%
                        select(p_year, dominant_share, country) %>%
                        filter(p_year <= 2015 & p_year >= 1980)
                country_diff <- rbind(country_diff, tmp)
        }
        
        return(country_diff)
}

#### CUMULATIVE ORIGIN SHARES: -------------------------------------------
# calculates the cumulative share of a selection of ethnic backgrounds for different countries.
# function requires a vector of countries and a vector of the selected ethnic origins as an input.
non_western_comparison <- function(countries, origins, df){
        
        inv_origin_shares <- lapply(countries, function(x) inv_comp_ctry(df, x))
        names(inv_origin_shares) <- countries
        
        country_diff <- data.frame()
        
        for(i in 1:length(inv_origin_shares)){
                tmp <- filter(inv_origin_shares[[i]], origin %in% origins)
                tmp <- tmp %>% group_by(p_year) %>% summarize(share = sum(share)) %>%
                        filter(p_year <= 2015 & p_year >= 1980) %>%
                        mutate(country = names(inv_origin_shares)[i])
                country_diff <- rbind(country_diff, tmp)
        }
        
        return(country_diff)
}

#### SPECIFIC ORIGIN SHARES: -------------------------------------------
# calculates the individual share of each selected ethnic background for different countries.
# function requires a vector of countries and a vector of the selected ethnic origins as an input.
foreign_shares_fun <- function(COUNTRIES, ORIGIN, df){
        
        inv_origin_shares <- lapply(COUNTRIES, function(x) inv_comp_ctry(df, x))
        for (i in length(inv_origin_shares)) {
                inv_origin_shares[[i]]$country <- COUNTRIES[i]
        }
        inv_origin_shares <- bind_rows(inv_origin_shares)
        
        plot_data <- filter(inv_origin_shares, origin %in% ORIGIN & 
                                    p_year <= 2015 & p_year >= 1980)
        
        return(plot_data)
}

#### ORIGIN SHARES BY COUNTRY AND TECHFIELDS: -------------------------------------------
# calculates the ethnic origin composition within techfields and countries.
# function requires a vector of countires and specification if techfields should be
# grouped to major techfields.
inv_comp_techfield <- function(df, country, grouping = FALSE){
        
        if(grouping == FALSE){
                annual_total <- filter(df, Ctry_code == country) %>%
                        group_by(tech_field, p_year) %>% summarise(total = n())
                
                tmp <- filter(df, Ctry_code == country) %>%
                        group_by(tech_field, p_year) %>% select(contains("prob")) %>%
                        summarise_all(.funs = sum)
                
                tmp <- merge(tmp, annual_total, by = c("tech_field", "p_year"))
                tmp[, grepl("prob", names(tmp))] <- tmp[, grepl("prob", names(tmp))] / tmp$total
                
                tmp <- gather(tmp, key = "origin", value = "share", -p_year, -total, -tech_field)
        }else{
                annual_total <- filter(df, Ctry_code == country) %>%
                        group_by(tech_group_name, p_year) %>% summarise(total = n())
                
                tmp <- filter(df, Ctry_code == country) %>%
                        group_by(tech_group_name, p_year) %>% select(contains("prob")) %>%
                        summarise_all(.funs = sum)
                
                tmp <- merge(tmp, annual_total, by = c("tech_group_name", "p_year"))
                tmp[, grepl("prob", names(tmp))] <- tmp[, grepl("prob", names(tmp))] / tmp$total
                
                tmp <- gather(tmp, key = "origin", value = "share", -p_year, -total, -tech_group_name)  
        }
        
        tmp$origin <- gsub("prob_", "", tmp$origin)
        tmp$country <- country
        
        return(tmp)
}

# calculates the cumulative share of a selection of ethnic background for a specification of
# selected techfields and countries.
non_western_techfield <- function(df, countries, origins, techfields, 
                                  min_inventors = 30, MA_5 = FALSE){
        
        inv_origin_shares <- lapply(countries, function(x) inv_comp_techfield(df, x, grouping = FALSE))
        names(inv_origin_shares) <- countries
        
        country_diff <- data.frame()
        
        for(i in 1:length(inv_origin_shares)){
                tmp <- filter(inv_origin_shares[[i]], 
                              origin %in% origins & total >= min_inventors &
                                      tech_field %in% techfields)
                tmp <- tmp %>% group_by(p_year, tech_field) %>% summarize(share = sum(share)) %>%
                        filter(p_year <= 2015 & p_year >= 1980) %>%
                        mutate(country = names(inv_origin_shares)[i])
                country_diff <- rbind(country_diff, tmp)
        }
        
        # calculate 5year rolling average:
        if(MA_5 == TRUE){
                ma <- function(x, n = 5){stats::filter(x, rep(1 / n, n), sides = 1)}
                country_diff <- country_diff %>%
                        group_by(country, tech_field) %>%
                        arrange(p_year) %>%
                        mutate(five_y_ma_share = ma(share)) %>%
                        filter(p_year > 1984)}
        
        return(country_diff)
}

#### ORIGIN SHARES BY COUNTRY AND REGION: -------------------------------------------
# calculates the ethnic origin composition within regions and countries.
# function requires a vector of countires and specification if region should be based on code
# or on label
inv_comp_region <- function(df, country, code = FALSE){
        
        if(code == FALSE){
                annual_total <- filter(df, Ctry_code == country) %>%
                        group_by(Up_reg_label, p_year) %>% summarise(total = n())
                
                tmp <- filter(df, Ctry_code == country) %>%
                        group_by(Up_reg_label, p_year) %>% select(contains("prob")) %>%
                        summarise_all(.funs = sum)
                
                tmp <- merge(tmp, annual_total, by = c("Up_reg_label", "p_year"))
                tmp[, grepl("prob", names(tmp))] <- tmp[, grepl("prob", names(tmp))] / tmp$total
                
                tmp <- gather(tmp, key = "origin", value = "share", -p_year, -total, -Up_reg_label) %>%
                        rename(region = Up_reg_label)
        }else{
                annual_total <- filter(df, Ctry_code == country) %>%
                        group_by(Up_reg_code, p_year) %>% summarise(total = n())
                
                tmp <- filter(df, Ctry_code == country) %>%
                        group_by(Up_reg_code, p_year) %>% select(contains("prob")) %>%
                        summarise_all(.funs = sum)
                
                tmp <- merge(tmp, annual_total, by = c("Up_reg_code", "p_year"))
                tmp[, grepl("prob", names(tmp))] <- tmp[, grepl("prob", names(tmp))] / tmp$total
                
                tmp <- gather(tmp, key = "origin", value = "share", -p_year, -total, -Up_reg_code) %>%
                        rename(region = Up_reg_label)
        }
        
        tmp$origin <- gsub("prob_", "", tmp$origin)
        tmp$country <- country
        
        return(tmp)
}

# calculates the cumulative share of a selection of ethnic background for a specification of countries.
non_western_region <- function(df, countries, origins, 
                               min_inventors = 50, MA_5 = FALSE){
        
        inv_origin_shares <- lapply(countries, function(x) inv_comp_region(df, x, code = FALSE))
        names(inv_origin_shares) <- countries
        
        country_diff <- data.frame()
        
        for(i in 1:length(inv_origin_shares)){
                tmp <- filter(inv_origin_shares[[i]], 
                              origin %in% origins & total >= min_inventors)
                tmp <- tmp %>% group_by(p_year, region) %>% summarize(share = sum(share)) %>%
                        filter(p_year <= 2015 & p_year >= 1980) %>%
                        mutate(country = names(inv_origin_shares)[i])
                country_diff <- rbind(country_diff, tmp)
        }
        
        # calculate 5year rolling average:
        if(MA_5 == TRUE){
                ma <- function(x, n = 5){stats::filter(x, rep(1 / n, n), sides = 1)}
                country_diff <- country_diff %>%
                        group_by(country, region) %>%
                        arrange(p_year) %>%
                        mutate(five_y_ma_share = ma(share)) %>%
                        filter(p_year > 1984)}
        
        return(country_diff)
}

#### REGIONAL DISTRIBUTION OF (GROUPS) OF ETHNIC INVENTORS
# calculates the regional shares of foreign origins for a particular country and time window.
reg_share_ctry = function(country, domestic_origin, top_ten = FALSE,
                          origins, years = seq(2010, 2015)){
        
        if(domestic_origin != origins[1]){
                origins = origins[!origins %in% domestic_origin]
        }
        
        plot_df <- inv_dat %>% 
                filter(p_year %in% years, Ctry_code == country) %>%
                distinct(name, .keep_all = TRUE) %>%
                mutate(in_group = ifelse(origin %in% origins, 1, 0)) %>%
                filter(in_group == 1) %>%
                mutate(country_total = sum(in_group)) %>%
                select(Ctry_code, Up_reg_code, Up_reg_label, country_total)
        
        if(top_ten == TRUE){
                plot_df <- plot_df %>%
                        mutate(n_regions = length(unique(Up_reg_label))) %>%
                        group_by(Ctry_code, Up_reg_label) %>%
                        summarise(regional_total = n()) %>%
                        slice_max(order_by = regional_total, n = 10) %>%
                        mutate(set_total = sum(regional_total), n_regions = n(), 
                               regio_share = regional_total / set_total, 
                               regio_share_sqrd = regio_share^2) %>%
                        arrange(-regio_share)
        }else{
                plot_df <- plot_df %>%
                        mutate(n_regions = length(unique(Up_reg_label))) %>%
                        group_by(Ctry_code, Up_reg_label) %>%
                        summarise(regional_total = n(),
                                  country_total = mean(country_total),
                                  n_regions = mean(n_regions)) %>%
                        mutate(regio_share = regional_total / country_total,
                               regio_share_sqrd = regio_share ^2) %>%
                        arrange(-regio_share)
                }
        
        return(plot_df)
}

