data <- read.csv('base.csv')

data.with.deaths <- data[!is.na(data$Total.Deaths),]

data.with.deaths.affected <- data.with.deaths[!is.na(data.with.deaths$Total.Affected),]

# - Quantidade de desastres por país
table(as.factor(data.with.deaths.affected$Country))

# - Quantidade de desastres por continente
table(as.factor(data.with.deaths.affected$Continent))

# Quantidade de desastres do Regional Hub
regional.hubs <- c('Brazil', 'United Arab Emirates (the)', 'Rwanda', 'China')
data.regional.hubs <- data.with.deaths.affected[which(data.with.deaths.affected$Country %in% regional.hubs),]
table(data.regional.hubs$Country)

# - Quantidade de desastres ao longo da série histórica
disasters.by.country <-  as.factor(data.regional.hubs$Country)
table(disasters.by.country, data.regional.hubs$Year)['Brazil',]
plot(table(disasters.by.country, data.regional.hubs$Year))
plot(table(disasters.by.country, data.regional.hubs$Year)['China',], type='l', col=2, xlab='Ano (a partir de 1970)', ylab='Número de desastres')
lines(table(disasters.by.country, data.regional.hubs$Year)['Brazil',], type='l', col=3)
lines(table(disasters.by.country, data.regional.hubs$Year)['Rwanda',], type='l', col=4)
legend("topleft",                           
       c("China", "Brazil", "Rwanda"),
       lty = 1,
       col = 2:4)

# - Número de mortos e afetados por país
total.deaths.brazil <- sum(data.regional.hubs[which(data.regional.hubs$Country == 'Brazil'),]$Total.Deaths)

total.deaths.china <- sum(data.regional.hubs[which(data.regional.hubs$Country == 'China'),]$Total.Deaths)

total.deaths.rwanda <- sum(data.regional.hubs[which(data.regional.hubs$Country == 'Rwanda'),]$Total.Deaths)

total.deaths.regional.hubs <- c(total.deaths.brazil, total.deaths.china, total.deaths.rwanda)
total.deaths.regional.hubs

