# update present data

# up to 2002 --------------------------------------------------------
source("http://www.openintro.org/stat/data/present.R")

# match 2002: http://www.cdc.gov/nchs/data/nvsr/nvsr52/nvsr52_10.pdf
# In 2002 there were 2,057,979 male and 1,963,747 female live births

# 2003 --------------------------------------------------------------

# source: http://www.cdc.gov/nchs/data/nvsr/nvsr54/nvsr54_02.pdf
# There were 2,093,535 male live births in 2003 compared with 
# 1,996,415 female live births

present <- rbind(present, data.frame(year = 2003, boys = 2093535, girls = 1996415))

# 2004 --------------------------------------------------------------

# source: http://www.cdc.gov/nchs/data/nvsr/nvsr55/nvsr55_01.pdf
# In 2004, there were 2,104,661 male and 
# 2,007,391 female live births in the United States

present <- rbind(present, data.frame(year = 2004, boys = 2104661, girls = 2007391))

# 2005 --------------------------------------------------------------

# source: http://www.cdc.gov/nchs/data/nvsr/nvsr56/nvsr56_06.pdf
# In 2005, there were 2,118,982 male compared with 
# 2,019,367 female live births

present <- rbind(present, data.frame(year = 2005, boys = 2118982, girls = 2019367))

# 2006 --------------------------------------------------------------

# source: http://www.cdc.gov/nchs/data/nvsr/nvsr57/nvsr57_07.pdf
# In 2006, there were 2,184,237 male and 
# 2,081,318 female live births

present <- rbind(present, data.frame(year = 2006, boys = 2184237, girls = 2081318))

# 2007 --------------------------------------------------------------

# source: http://www.cdc.gov/nchs/data/nvsr/nvsr58/nvsr58_24.pdf
# In 2007, there were 2,208,071 male and 
# 2,108,162 female live births in the United States

present <- rbind(present, data.frame(year = 2007, boys = 2208071, girls = 2108162))

# 2008 --------------------------------------------------------------

# source: http://www.cdc.gov/nchs/data/nvsr/nvsr59/nvsr59_01.pdf
# Figures from Table 13

total <- 4247694
m_f_ratio <- 1048 / 1000
m_f_tot <- m_f_ratio + 1
female <- round(total / m_f_tot)
male <- total - female

present <- rbind(present, data.frame(year = 2008, boys = male, girls = female))

# 2009 --------------------------------------------------------------

# source: http://www.cdc.gov/nchs/data/nvsr/nvsr60/nvsr60_01.pdf
# Figures from Table 13

total <- 4130665
m_f_ratio <- 1048 / 1000
m_f_tot <- m_f_ratio + 1
female <- round(total / m_f_tot)
male <- total - female

present <- rbind(present, data.frame(year = 2009, boys = male, girls = female))

# 2010 --------------------------------------------------------------

# source: http://www.cdc.gov/nchs/data/nvsr/nvsr61/nvsr61_01.pdf
# Figures from Table 13

total <- 3999386
m_f_ratio <- 1048 / 1000
m_f_tot <- m_f_ratio + 1
female <- round(total / m_f_tot)
male <- total - female

present <- rbind(present, data.frame(year = 2010, boys = male, girls = female))

# 2011 --------------------------------------------------------------

# source: http://www.cdc.gov/nchs/data/nvsr/nvsr61/nvsr61_01.pdf
# Figures from Table 13

total <- 3953590
m_f_ratio <- 1049 / 1000
m_f_tot <- m_f_ratio + 1
female <- round(total / m_f_tot)
male <- total - female

present <- rbind(present, data.frame(year = 2011, boys = male, girls = female))

# 2012 --------------------------------------------------------------

# source: http://www.cdc.gov/nchs/data/nvsr/nvsr62/nvsr62_09.pdf
# Figures from Table 13

total <- 3952841
m_f_ratio <- 1047 / 1000
m_f_tot <- m_f_ratio + 1
female <- round(total / m_f_tot)
male <- total - female

present <- rbind(present, data.frame(year = 2012, boys = male, girls = female))

# 2013 --------------------------------------------------------------

# source: http://www.cdc.gov/nchs/data/nvsr/nvsr64/nvsr64_01.pdf
# Figures from Table 13

total <- 3932181
m_f_ratio <- 1049 / 1000
m_f_tot <- m_f_ratio + 1
female <- round(total / m_f_tot)
male <- total - female

present <- rbind(present, data.frame(year = 2013, boys = male, girls = female))

# tbl_df present ----------------------------------------------------

library(dplyr)
present <- tbl_df(present)

# save present ------------------------------------------------------

save(present, file = "present.RData")
