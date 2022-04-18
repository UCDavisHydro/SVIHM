library(RSVP)

# Visualization


# Precipitation -----------------------------------------------------------

# Read
precip1 <- read.table('../../SVIHM_Input_Files/Scenario_Development/precip_regressed_orig.txt',
                      header = F, col.names = c('prcp','date'))
precip2 <- read.table('../../SVIHM_Input_Files/Scenario_Development/precip_regressed.txt',
                      header = F, col.names = c('prcp','date'))
precip1$date <- as.Date(precip1$date, format='%d/%m/%Y')
precip2$date <- as.Date(precip2$date, format='%d/%m/%Y')

# Subset
precip1 <- precip1[precip1$date > '2011-9-30',]
precip2 <- precip2[precip2$date > '2011-9-30',]
precip2 <- precip2[precip2$date <= max(precip1$date),]

# View
#plot.precip(dates = precip1$date, precip = precip1$prcp, unit = 'm')

par(mfrow=c(2,1))
plot.precip.compare(precip1$date, precip1$prcp, precip2$prcp, unit = 'm', legend_names = c('Original','New'))
plot.precip.cumulative(precip1$date, precip1$prcp, precip2$prcp, unit='m', col=c('blue','red'))
