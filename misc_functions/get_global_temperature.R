get_global_temperature = function(save = F, 
                                  file = 'data/global_temperatures.RData') {
  gtemp = readr::read_table("https://crudata.uea.ac.uk/cru/data/temperature/HadCRUT4-gl.dat")
  
  ## Drop the even rows
  gtemp = gtemp %>% drop_na()
  
  ## Add colnames
  colnames(gtemp) <- c("Year", month.abb, "Annual")
  
  # Create a long format for later; Also set year to start at 0
  gtemp_long = gtemp %>% 
    mutate(Year0 = Year-1850) %>% 
    gather(key = Month, value=Anomaly, -Year, -Annual) %>% 
    mutate(Month = factor(Month, levels = month.abb)) %>% # gam 'by' needs a factor
    arrange(Year, Month)
  
  if (save) save(gtemp, gtemp_long, file = file)
  
  list(gtemp, gtemp_long)
}
