# read dataset
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#1 Have total emissions from PM2.5 decreased in the United States from 1999 to 2008?- Yes
#Using the base plotting system, make a plot showing the total PM2.5 emission from all 
#sources for each of the years 1999, 2002, 2005, and 2008.

# Aggregate by sum the total emissions by year
agg_sum <- aggregate(Emissions ~ year, data=NEI, sum)

png("plot1.png", width=480, height=480)

barplot(
  (agg_sum$Emissions)/10^6,
  names.arg=agg_sum$year,
  xlab="Year",
  ylab="PM2.5 Emissions",
  main="USA PM2.5 Emissions"
)

dev.off()

#Have total emissions from PM2.5 decreased in the Baltimore City, Maryland -yes

# Subset NEI data TO Baltimore.
baltimore_NEI <- subset(NEI, fips=="24510")
              
# Aggregate using sum the Baltimore emissions data by year
agg_Baltimore <- aggregate(Emissions ~ year, baltimore_NEI, sum)

png("plot2.png", width=480, height=480)

barplot(
  agg_Baltimore$Emissions,
  names.arg=agg_Baltimore$year,
  xlab="Year",
  ylab="PM2.5 Emissions",
  main="Baltimore PM2.5 Emissions"
)

dev.off()

#3 Of the four types of sources indicated by the 𝚝𝚢𝚙𝚎 (point, nonpoint, onroad, nonroad) variable, which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? Which have seen increases in emissions from 1999–2
#008? - Non-road, nonpoint, on-road decreased while point had increases from 1999 to 2005
png("plot3.png",width=480,height=480)

library(ggplot2)

p <- ggplot(baltimore_NEI, aes(factor(year), Emissions,fill=type)) +
  geom_bar(stat="identity") +
  theme_bw() + guides(fill=FALSE)+
  facet_grid(. ~ type, scales = "free", space="free")+ 
  labs(x="Year", y="Total PM 2.5 Emissions") + 
  labs(title="PM 2.5 Emissions of Baltimore by Source Type") 

print(p)

dev.off()

#4 Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?


# Subset coal combustion related NEI data
combustionRelated <- grepl("comb", SCC$SCC.Level.One, ignore.case=TRUE)
coalRelated <- grepl("coal", SCC$SCC.Level.Four, ignore.case=TRUE) 
coalCombustion <- (combustionRelated & coalRelated)
combustionSCC <- SCC[coalCombustion,]$SCC
combustionNEI <- NEI[NEI$SCC %in% combustionSCC,]

png("plot4.png",width=480,height=480)

library(ggplot2)

ggp <- ggplot(combustionNEI,aes(factor(year),Emissions/10^5)) +
  geom_bar(stat="identity",fill="grey",width=0.75) +
  theme_bw() +  guides(fill=FALSE) +
  labs(x="year", y="Total PM2.5 Emission",title="PM2.5 Coal Combustion Source Emissions Across US from 1999-2008")

print(ggp)

dev.off()

#How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

# Gather the subset of the NEI data which corresponds to vehicles
vehicles <- grepl("vehicle", SCC$SCC.Level.Two, ignore.case=TRUE) #grepl returns a logical vector
vehiclesSCC <- SCC[vehicles,]$SCC
vehiclesNEI <- NEI[NEI$SCC %in% vehiclesSCC,]

# Subset the vehicles NEI data to Baltimore's fip
baltimoreVehiclesNEI <- vehiclesNEI[vehiclesNEI$fips=="24510",]

png("plot5.png",width=480)


ggp <- ggplot(baltimoreVehiclesNEI,aes(factor(year),Emissions)) +
  geom_bar(stat="identity",fill="grey",width=0.75) +
  theme_bw() +  guides(fill=FALSE) +
  labs(x="year", y=expression("Total PM"[2.5]*" Emission (10^5 Tons)")) + 
  labs(title=expression("PM"[2.5]*" Motor Vehicle Source Emissions in Baltimore from 1999-2008"))

print(ggp)

dev.off()

#Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California (𝚏𝚒𝚙𝚜 == "𝟶𝟼𝟶𝟹𝟽"). 

# Gather the subset of the NEI data which corresponds to vehicles
vehicles <- grepl("vehicle", SCC$SCC.Level.Two, ignore.case=TRUE)
vehiclesSCC <- SCC[vehicles,]$SCC
vehiclesNEI <- NEI[NEI$SCC %in% vehiclesSCC,]

# Subset the vehicles NEI data by each city's fip and add city name.
vehiclesBaltimoreNEI <- vehiclesNEI[vehiclesNEI$fips=="24510",]
vehiclesBaltimoreNEI$city <- "Baltimore City"

vehiclesLANEI <- vehiclesNEI[vehiclesNEI$fips=="06037",]
vehiclesLANEI$city <- "Los Angeles County"

# Combine the two subsets with city name into one data frame
bothNEI <- rbind(vehiclesBaltimoreNEI,vehiclesLANEI)

png("plot6.png",width=480,height=480)

library(ggplot2)

ggp <- ggplot(bothNEI, aes(x=factor(year), y=Emissions, fill=city)) +
  geom_bar(aes(fill=year),stat="identity") +
  facet_grid(scales="free", space="free", city.~) +
  guides(fill=FALSE) + theme_bw() +
  labs(x="year", y=expression("Total PM"[2.5]*" Emission (Kilo-Tons)")) + 
  labs(title=expression("PM"[2.5]*" Motor Vehicle Source Emissions in Baltimore & LA, 1999-2008"))

print(ggp)

dev.off()
