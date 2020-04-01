naames\_ctd
================
Nicholas Baetge
3/30/2020

This document shows how the NAAMES high resolution (\<= 1 m) CTD data
from each cruise and combined into one data file. The source data can be
found in the NASA SeaWiFS Bio-optical Archive and Storage System
[SeaBASS](https://seabass.gsfc.nasa.gov/naames). Data were processed via
SBE Seasoft V2, the
[manual](https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=1&ved=2ahUKEwiFteyVzMXoAhXlJDQIHbzaAjkQFjAAegQIAhAB&url=https%3A%2F%2Fwww.seabird.com%2Fasset-get.download.jsa%3Fcode%3D251446&usg=AOvVaw2o8qNHvk2tuqzVvRNDhuob)
of which includes desriptors of variable names.

# Import, Wrangle, Tidy Data

Each cruise folder contains a file for each CTD cast, which consists of
sensor data for the the up and down casts (these are not separated).

Though the header desingnations within the cruise are consistent across
all casts, they are not consistent across all cruises:

  - We will work with each cruise separately
  - We’ll rename the SBE Seasoft default headers with our own
  - We’ll remove duplicated headers that represent data from the same
    sensor

The .cnv files have two comment types: “\#” and "\*". All comments
precede the actual CTD data, but the number of rows they take up is
different for each file. We’ll need to remove all comments before
wrangling, tidying, and combining data.

## Bottle File Metadata

We’ll need this data to add the appropriate station/cast numbers to the
files and make our final output
ODV-friendly.

``` r
metadata <- read_sheet("https://docs.google.com/spreadsheets/d/1zw-W1k__BeuJg1oQpQQ_XT7zWLiY3dfL-dTaQ0pzN5Q/edit#gid=1446474071", sheet = "Bottle File", skip = 1) %>% 
  select(Cruise, Station, Type, Time_Stamp, CruiseCN, SCN, CampCN) %>% 
  distinct() %>% 
  drop_na(CampCN) %>% 
  mutate(Station = ifelse(is.na(Station), 0, Station),
         Time_Stamp = ifelse(Time_Stamp == "2017-09-13T", "2017-09-13T03:13", Time_Stamp),
         Type = "C") 
```

    ## Using an auto-discovered, cached token.
    ## To suppress this message, modify your code or options to clearly consent to the use of a cached token.
    ## See gargle's "Non-interactive auth" vignette for more details:
    ## https://gargle.r-lib.org/articles/non-interactive-auth.html
    ## The googlesheets4 package is using a cached token for nicholasbaetge@gmail.com.

## AT32

First we’ll store filenames, profile numbers, and variable names.

``` r
#pass all the filenames in the cruise folder into a variable
n1.filenames <- list.files("~/naames_ctd/Input/at32",pattern = "*.cnv", full.names = T) 

#store the SBE seasoft variable names 
n1.sbe.names <- read_csv("~/naames_ctd/Input/at32/n1_headers.csv") 

#store new variable names corresponding to those of the SBE seasoft output 
n1.cols <- c("sec_elaps", "lon_e", "lat_n", "z_m", "pres_db", "temp0_c", "temp1_c", "cond1_Sm", "cond2_Sm", "beamT_perc", "beamA_per_m", "fl_mg_m3", "turb_ntu", "o2_v", "o2_umol_l", "dens0_kg_m3", "dens1_kg_m3", "sal0_psu", "sal1_psu", "soundvel_m_s", "soundvel1_m_s", "uvp", "z_m_lgrav_corr", "dens0_kg_m3_dup", "sigT1_kg_m3", "sigT0_kg_m3", "potT0_c", "potT1_c", "sal0_psu_dup", "sal1_psu_dup", "soundvel_m_s_dup", "soundvel1_m_s_dup", "o2_mg_l", "flag")

#pass all the filenames in a cruise folder into a variable, this time storing only the cruise and cast number of the file
n1.profiles <- list.files("~/naames_ctd/Input/at32",pattern = "*.cnv", full.names = F) 
n1.profile.names <- gsub('.cnv', '', n1.profiles)
```

Then we’ll pass the filenames through a function that removes comments
from each file (not pretty, but it works).

``` r
n1.master <-  lapply(n1.filenames, function(i){
  x <- read_csv(i, comment = "#", col_names = "value") %>% as_data_frame()
  write_csv(x, "~/naames_ctd/Input/at32/x.csv")
  y <- read_csv("~/naames_ctd/Input/at32/x.csv", comment = "*")
  write_csv(y, "~/naames_ctd/Input/at32/y.csv") 
  z <- read_csv("~/naames_ctd/Input/at32/y.csv") %>% 
  mutate(col = gsub(" ", ",", value),
         col = gsub(",,,,,", ",", col),
         col = gsub(",,,,", ",", col),
         col = gsub(",,,", ",", col),
         col = gsub(",,", ",", col))  %>% 
  select(col) %>% 
  separate(col, into = n1.cols, sep = ",")
})
```

    ## Warning: `as_data_frame()` is deprecated, use `as_tibble()` (but mind the new semantics).
    ## This warning is displayed once per session.

    ## Warning: 1 parsing failure.
    ## row col  expected    actual                                                      file
    ##  14  -- 1 columns 2 columns '/Users/nicholasbaetge/naames_ctd/Input/at32/at32003.cnv'

    ## Warning: 1 parsing failure.
    ## row col  expected    actual                                                      file
    ##  16  -- 1 columns 2 columns '/Users/nicholasbaetge/naames_ctd/Input/at32/at32004.cnv'

    ## Warning: 1 parsing failure.
    ## row col  expected    actual                                                      file
    ##  14  -- 1 columns 3 columns '/Users/nicholasbaetge/naames_ctd/Input/at32/at32011.cnv'

    ## Warning: 1 parsing failure.
    ## row col  expected    actual                                                      file
    ##  14  -- 1 columns 3 columns '/Users/nicholasbaetge/naames_ctd/Input/at32/at32014.cnv'

    ## Warning: 1 parsing failure.
    ## row col  expected    actual                                                      file
    ##  14  -- 1 columns 5 columns '/Users/nicholasbaetge/naames_ctd/Input/at32/at32016.cnv'

    ## Warning: 1 parsing failure.
    ## row col  expected    actual                                                      file
    ##  14  -- 1 columns 3 columns '/Users/nicholasbaetge/naames_ctd/Input/at32/at32023.cnv'

``` r
names(n1.master) <- n1.profile.names
```

Lastly, we combine all of the casts into one data frame, remove all the
duplicated columns, and average the values for each depth from the up
and down casts. We’re using the local gravity-corrected depth values
(latitude is used to calculate local gravity)

``` r
#convert to data frame
n1.combined <- bind_rows(n1.master, .id = "id") %>% 
  mutate(Cruise = "AT32",
         Cast = str_replace(id, "at32", "")) %>% 
  mutate_at(vars(Cast), as.numeric) %>% 
  select(Cruise, Cast, everything()) %>% 
  select(-c(contains("_dup"), sec_elaps, z_m)) %>% 
  mutate_at(vars(Cast, lon_e:flag), as.numeric) %>% 
  mutate(bin_depth = round(z_m_lgrav_corr)) %>% 
  select(Cruise:id, bin_depth, lon_e:flag) %>% 
  group_by(Cruise, Cast, bin_depth) %>% 
  mutate_at(vars(lon_e:flag), mean) %>% 
  distinct() %>% 
  ungroup() %>% 
  rename(CruiseCN = Cast) %>% 
  full_join(., metadata %>% filter(Cruise == "AT32")) %>% 
  select(Cruise, Station, Type, Time_Stamp, lon_e, lat_n, CruiseCN, SCN, CampCN, everything()) %>% 
  rename("yyyy-mm-ddThh:mm:ss.sss" = Time_Stamp,
         "Longitude [degrees_east]" = lon_e,
         "Latitude [degrees_north]" = lat_n) %>% 
  arrange(Cruise, Station, CruiseCN)
```

## AT34

We’ll repeat the process above for the next 3 cruises, changing just the
variable names to reflect the SBE Seasoft variable names in the data
files.

AT34 is the only cruise in which the CTD data were binned only to 0.01
m, so each one of the .cnv files for this cruise is huge. They also
contain a ton of -999\*10^-29 (NA indicator) values which we’ll
remove.

``` r
n2.filenames <- list.files("~/naames_ctd/Input/at34",pattern = "*.cnv", full.names = T) 

n2.sbe.names <- read_csv("~/naames_ctd/Input/at34/n2_headers.csv") 

n2.cols <- c("sec_elaps", "lon_e", "lat_n", "z_m", "pres_db", "temp0_c", "temp1_c","cond1_Sm", "cond2_Sm", "beamT_perc", "fl_mg_m3", "turb_ntu", "o2_v", "altim_m", "n_btls_fired", "dens0_kg_m3", "dens1_kg_m3", "sal0_psu", "sal1_psu", "soundvel_m_s", "soundvel1_m_s", "sec_elaps_dup", "o2_ml_l", "o2_perc_sat",  "surf_par", "par", "uvp", "descent_rate_m_s", "sensor_temp_diff", "z_m_lgrav_corr", "dens0_kg_m3_dup", "dens1_kg_m3_dup", "sal0_psu_dup", "sal1_psu_dup", "soundvel_m_s_dup", "soundvel1_m_s_dup", "descent_rate_m_s_dup", "sensor_temp_diff_dup", "flag")
            
n2.profiles <- list.files("~/naames_ctd/Input/at34",pattern = "*.cnv", full.names = F) 
n2.profile.names <- gsub('.cnv', '', n2.profiles)
```

``` r
n2.master <-  lapply(n2.filenames, function(i){
  x <- read_csv(i, comment = "#", col_names = "value") %>% as_data_frame()
  write_csv(x, "~/naames_ctd/Input/at34/x.csv")
  y <- read_csv("~/naames_ctd/Input/at34/x.csv", comment = "*")
  write_csv(y, "~/naames_ctd/Input/at34/y.csv") 
  z <- read_csv("~/naames_ctd/Input/at34/y.csv") %>% 
  mutate(col = gsub(" ", ",", value),
         col = gsub(",,,,,", ",", col),
         col = gsub(",,,,", ",", col),
         col = gsub(",,,", ",", col),
         col = gsub(",,", ",", col))  %>% 
  select(col) %>% 
  separate(col, into = n2.cols, sep = ",")
})
names(n2.master) <- n2.profile.names
```

``` r
n2.combined <- bind_rows(n2.master, .id = "id") %>% 
  mutate(Cruise = "AT34",
         Cast = str_replace(id, "at34", "")) %>% 
  mutate_at(vars(Cast), as.numeric) %>% 
  select(Cruise, Cast, everything()) %>% 
  select(-c(contains("_dup"), sec_elaps, z_m)) %>% 
  mutate_at(vars(Cast, lon_e:flag), as.numeric) %>%
  filter_all(all_vars(. != -9.99 * 10^-29)) %>% 
  mutate(bin_depth = round(z_m_lgrav_corr)) %>% 
  select(Cruise:id, bin_depth, lon_e:flag) %>% 
  group_by(Cruise, Cast, bin_depth) %>% 
  mutate_at(vars(lon_e:flag), mean) %>% 
  distinct() %>% 
  ungroup() %>% 
  rename(CruiseCN = Cast) %>% 
  full_join(., metadata %>% filter(Cruise == "AT34")) %>% 
  select(Cruise, Station, Type, Time_Stamp, lon_e, lat_n, CruiseCN, SCN, CampCN, everything()) %>% 
  rename("yyyy-mm-ddThh:mm:ss.sss" = Time_Stamp,
         "Longitude [degrees_east]" = lon_e,
         "Latitude [degrees_north]" = lat_n) %>% 
  arrange(Cruise, Station, CruiseCN)
```

## AT38

``` r
n3.filenames <- list.files("~/naames_ctd/Input/at38",pattern = "*.cnv", full.names = T) 

n3.sbe.names <- read_csv("~/naames_ctd/Input/at38/n3_headers.csv") 

n3.cols <- c("lon_e", "lat_n", "pres_db", "temp0_c", "temp1_c", "cond1_Sm", "cond2_Sm", "beamT_perc", "fl_mg_m3", "turb_ntu", "o2_v",  "surf_par", "par", "corr_par", "uvp", "z_m_lgrav_corr", "dens0_kg_m3", "dens1_kg_m3",  "sal0_psu", "sal1_psu", "soundvel_m_s", "soundvel1_m_s", "o2_umol_kg", "o2_perc_sat", "descent_rate_m_s", "flag")


n3.profiles <- list.files("~/naames_ctd/Input/at38",pattern = "*.cnv", full.names = F) 
n3.profile.names <- gsub('.cnv', '', n3.profiles)
```

``` r
n3.master <-  lapply(n3.filenames, function(i){
  x <- read_csv(i, comment = "#", col_names = "value") %>% as_data_frame()
  write_csv(x, "~/naames_ctd/Input/at38/x.csv")
  y <- read_csv("~/naames_ctd/Input/at38/x.csv", comment = "*")
  write_csv(y, "~/naames_ctd/Input/at38/y.csv") 
  z <- read_csv("~/naames_ctd/Input/at38/y.csv") %>% 
  mutate(col = gsub(" ", ",", value),
         col = gsub(",,,,,", ",", col),
         col = gsub(",,,,", ",", col),
         col = gsub(",,,", ",", col),
         col = gsub(",,", ",", col))  %>% 
  select(col) %>% 
  separate(col, into = n3.cols, sep = ",")
})
names(n3.master) <- n3.profile.names
```

``` r
#convert to data frame
n3.combined <- bind_rows(n3.master, .id = "id") %>% 
  mutate(Cruise = "AT38",
         Station_Cast = str_replace(id, "at38s", "")) %>% 
  separate(Station_Cast, c("Station", "Cast"), sep = "c") %>% 
  mutate(Station = gsub("-5", ".5", Station),
         Station = gsub("_5", ".5", Station),
         Station = gsub("a", "A", Station)) %>% 
  mutate_at(vars(Cast), as.numeric) %>% 
  select(Cruise, Station, Cast, everything()) %>% 
  select(-contains("_dup")) %>% 
  mutate_at(vars(Cast, lon_e:flag), as.numeric) %>%
  mutate(bin_depth = round(z_m_lgrav_corr)) %>% 
  select(Cruise:id, bin_depth, lon_e:flag) %>% 
  group_by(Cruise, Station, Cast, bin_depth) %>% 
  mutate_at(vars(lon_e:flag), mean) %>% 
  distinct() %>% 
  ungroup() %>% 
  rename(SCN = Cast) %>% 
  mutate(Station = gsub("1A", 0, Station)) %>% 
  mutate_at(vars(Station), as.numeric) %>% 
  full_join(., metadata %>% 
    filter(Cruise == "AT38") %>% 
    arrange(Station, CruiseCN)) %>% 
  select(Cruise, Station, Type, Time_Stamp, lon_e, lat_n, CruiseCN, SCN, CampCN, everything()) %>% 
  rename("yyyy-mm-ddThh:mm:ss.sss" = Time_Stamp,
         "Longitude [degrees_east]" = lon_e,
         "Latitude [degrees_north]" = lat_n) %>% 
  arrange(Cruise, Station, CruiseCN)
```

## AT39

``` r
n4.filenames <- list.files("~/naames_ctd/Input/at39",pattern = "*.cnv", full.names = T) 

n4.sbe.names <- read_csv("~/naames_ctd/Input/at39/n4_headers.csv") 

n4.cols <- c("lon_e", "lat_n", "pres_db", "temp0_c", "temp1_c", "cond1_Sm", "cond2_Sm", "beamT_perc", "fl_mg_m3", "turb_ntu", "o2_v", "uvp", "z_m_lgrav_corr",  "dens0_kg_m3", "dens1_kg_m3",  "sal0_psu", "sal1_psu",  "soundvel_m_s", "soundvel1_m_s", "o2_umol_kg", "o2_ml_l", "o2_perc_sat",  "descent_rate_m_s", "flag")


n4.profiles <- list.files("~/naames_ctd/Input/at39",pattern = "*.cnv", full.names = F) 
n4.profile.names <- gsub('.cnv', '', n4.profiles)
```

``` r
n4.master <-  lapply(n4.filenames, function(i){
  x <- read_csv(i, comment = "#", col_names = "value") %>% as_data_frame()
  write_csv(x, "~/naames_ctd/Input/at39/x.csv")
  y <- read_csv("~/naames_ctd/Input/at39/x.csv", comment = "*")
  write_csv(y, "~/naames_ctd/Input/at39/y.csv") 
  z <- read_csv("~/naames_ctd/Input/at39/y.csv") %>% 
  mutate(col = gsub(" ", ",", value),
         col = gsub(",,,,,", ",", col),
         col = gsub(",,,,", ",", col),
         col = gsub(",,,", ",", col),
         col = gsub(",,", ",", col))  %>% 
  select(col) %>% 
  separate(col, into = n4.cols, sep = ",")
})
names(n4.master) <- n4.profile.names
```

``` r
#convert to data frame
n4.combined <- bind_rows(n4.master, .id = "id") %>% 
  mutate(Cruise = "AT39",
         Station_Cast = str_replace(id, "at3906s", "")) %>% 
  separate(Station_Cast, c("Station", "Cast"), sep = "c") %>% 
  mutate(Station = gsub("-1", ".1", Station)) %>% 
  mutate_at(vars(Cast), as.numeric) %>% 
  select(Cruise, Station, Cast, everything()) %>% 
  select(-contains("_dup")) %>% 
  mutate_at(vars(Cast, lon_e:flag), as.numeric) %>%
  mutate(bin_depth = round(z_m_lgrav_corr)) %>% 
  select(Cruise:id, bin_depth, lon_e:flag) %>% 
  group_by(Cruise, Station, Cast, bin_depth) %>% 
  mutate_at(vars(lon_e:flag), mean) %>% 
  distinct() %>% 
  ungroup() %>% 
  rename(SCN = Cast) %>% 
  mutate_at(vars(Station), as.numeric) %>% 
  full_join(., metadata %>% 
   filter(Cruise == "AT39-6") %>% 
    mutate(Cruise = "AT39")) %>% 
  select(Cruise, Station, Type, Time_Stamp, lon_e, lat_n, CruiseCN, SCN, CampCN, everything()) %>% 
  rename("yyyy-mm-ddThh:mm:ss.sss" = Time_Stamp,
         "Longitude [degrees_east]" = lon_e,
         "Latitude [degrees_north]" = lat_n) %>% 
  arrange(Cruise, Station, CruiseCN)
```

    ## Joining, by = c("Cruise", "Station", "SCN")

## Save

``` r
saveRDS(n1.combined, file = "~/naames_ctd/Output/n1_ctd.rds")
saveRDS(n2.combined, file = "~/naames_ctd/Output/n2_ctd.rds")
saveRDS(n3.combined, file = "~/naames_ctd/Output/n3_ctd.rds")
saveRDS(n4.combined, file = "~/naames_ctd/Output/n4_ctd.rds")
write_csv(n1.combined, path = "~/naames_ctd/Output/n1_ctd.csv")
write_csv(n2.combined, path = "~/naames_ctd/Output/n2_ctd.csv")
write_csv(n3.combined, path = "~/naames_ctd/Output/n3_ctd.csv")
write_csv(n4.combined, path = "~/naames_ctd/Output/n4_ctd.csv")
```

# Combine Cruise Data

``` r
naames_ctd <- bind_rows(n1.combined, n2.combined) %>% 
  bind_rows(., n3.combined) %>% 
  bind_rows(., n4.combined)
```

# Derived Variables

Here we add derived variables, including:

  - sensor averages
  - oxygen unit conversions following
    [ICES](https://ocean.ices.dk/Tools/UnitConversion.aspx) conversion
    factors
      - AT32, µmol L<sup>-1</sup> to µmol kg<sup>-1</sup>
      - AT34, ml L<sup>-1</sup> to µmol kg<sup>-1</sup>
      - All cruises, µmol kg<sup>-1</sup> to µmol L<sup>-1</sup>
  - potential temperature and density using the oce package (these were
    not always calculated in SBE Seasoft)
  - o2 saturation based on Weiss 1970 (Deep Sea Res., 17, 721-735), the
    same calculation that ODV uses. The equation is as
follows:

\(O2_sat = exp {[A1 + A2 * (100/T_a) + A3 * ln(T_a/100) + A4 * ( T_a/100)] + S * [B1 + B2 * (T_a/100) + B3 * (T_a/100)2 ]}\)

where \(T_a\) = absolute temperature (temperature + 273.15), \(S\) =
salinity, and

``` r
A1 = -173.4292     
A2 = 249.6339     
A3 = 143.3483     
A4 = -21.8492
B1 = -0.033096     
B2 = 0.014259      
B3 = -0.00170
```

``` r
deriv_naames_ctd <- naames_ctd  %>%
  group_by(Cruise, Station, CruiseCN) %>%
  mutate(ave_temp_c = (temp0_c + temp1_c)/2,
         ave_cond_Sm = (cond1_Sm + cond2_Sm)/2,
         ave_dens_kg_m3 = (dens0_kg_m3 + dens1_kg_m3)/2,
         ave_sal_psu = (sal0_psu + sal1_psu)/2,
         ave_soundvel_m_s = (soundvel_m_s + soundvel1_m_s)/2,
         ave_sigT_kg_m3 = (sigT1_kg_m3 + sigT1_kg_m3)/2,
         ave_potT_c = (potT0_c + potT1_c)/2,
         deriv_potT_c = swTheta(ave_sal_psu, temperature = ave_temp_c, pressure = pres_db),
         deriv_sigT_kg_m3 = swSigmaT(ave_sal_psu, temperature = ave_temp_c, pressure = pres_db),
         deriv_o2_umol_kg = o2_umol_kg,
         deriv_o2_umol_kg = ifelse(Cruise == "AT32", o2_umol_l/1.025, deriv_o2_umol_kg),
         deriv_o2_umol_kg = ifelse(Cruise == "AT34", o2_ml_l/(0.022391 * 1.025), deriv_o2_umol_kg),
         deriv_o2_umol_l = o2_umol_l,
         deriv_o2_umol_l = ifelse(is.na(deriv_o2_umol_l), deriv_o2_umol_kg * 1.025, deriv_o2_umol_l),
         ta = ave_temp_c + 273.15,
         deriv_o2_sat_ml_l = exp( (A1 + A2 * (100/ta) + A3 * log(ta/100) + A4 * (ta/100)) + ave_sal_psu * (B1 + B2 * (ta/100) + B3 * (ta/100)^2)),
         deriv_o2_sat_umol_kg = deriv_o2_sat_ml_l/(0.022391 * 1.025),
         deriv_o2_sat_umol_l = deriv_o2_sat_umol_kg * 1.025,
         deriv_aou_umol_kg = deriv_o2_sat_umol_kg - deriv_o2_umol_kg,
         deriv_aou_umol_l = deriv_o2_sat_umol_l - deriv_o2_umol_l,
         ) %>% 
  ungroup()
```

## Save

``` r
saveRDS(deriv_naames_ctd, file = "~/naames_ctd/Output/deriv_naames_ctd.rds")
write_csv(deriv_naames_ctd, path = "~/naames_ctd/Output/deriv_naames_ctd.csv")
```
