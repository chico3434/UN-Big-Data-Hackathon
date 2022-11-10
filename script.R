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

total.affected.brazil <- sum(data.regional.hubs[which(data.regional.hubs$Country == 'Brazil'),]$Total.Affected)

total.affected.china <- sum(data.regional.hubs[which(data.regional.hubs$Country == 'China'),]$Total.Affected)

total.affected.rwanda <- sum(data.regional.hubs[which(data.regional.hubs$Country == 'Rwanda'),]$Total.Affected)

total.affected.regional.hubs <- c(total.affected.brazil, total.affected.china, total.affected.rwanda)
total.affected.regional.hubs

# - Razão número de mortos e afetados por país
total.deaths <- sum(data.with.deaths.affected$Total.Deaths)
total.affected <- sum(data.with.deaths.affected$Total.Affected)
prop.deaths.brazil <- total.deaths.brazil/total.deaths
prop.deaths.chine <- total.deaths.china/total.deaths
prop.deaths.rwanda <- total.deaths.rwanda/total.deaths

prop.affected.brazil <- total.affected.brazil/total.affected
prop.affected.chine <- total.affected.china/total.affected
prop.affected.rwanda <- total.affected.rwanda/total.affected

# - Evolução dessa razão ao longo da série histórica
deaths.china <- c()
prop.deaths.china <- c()
for(i in 1970:2021) {
  year.deaths.china <- sum(data.regional.hubs[which(data.regional.hubs$Country=='China' & data.regional.hubs$Year == i),]$Total.Deaths)
  deaths.china <- c(deaths.china, year.deaths.china)
  prop.deaths.china <- c(prop.deaths.china, year.deaths.china/sum(data.with.deaths.affected[which(data.with.deaths.affected$Year == i),]$Total.Deaths))
}

deaths.brazil <- c()
prop.deaths.brazil <- c()
for(i in 1970:2021) {
  year.deaths.brazil <- sum(data.regional.hubs[which(data.regional.hubs$Country=='Brazil' & data.regional.hubs$Year == i),]$Total.Deaths)
  deaths.brazil <- c(deaths.brazil, year.deaths.brazil)
  prop.deaths.brazil <- c(prop.deaths.brazil, year.deaths.brazil/sum(data.with.deaths.affected[which(data.with.deaths.affected$Year == i),]$Total.Deaths))
}

deaths.rwanda <- c()
prop.deaths.rwanda <- c()
for(i in 1970:2021) {
  year.deaths.rwanda <- sum(data.regional.hubs[which(data.regional.hubs$Country=='Rwanda' & data.regional.hubs$Year == i),]$Total.Deaths)
  deaths.rwanda <- c(deaths.rwanda, year.deaths.rwanda)
  prop.deaths.rwanda <- c(prop.deaths.rwanda, year.deaths.rwanda/sum(data.with.deaths.affected[which(data.with.deaths.affected$Year == i),]$Total.Deaths))
}

plot(1970:2021, deaths.china, type='l', col=2, xlab='Ano', ylab='Número de mortes')
lines(1970:2021, deaths.brazil, type='l', col=3)
lines(1970:2021, deaths.rwanda, type='l', col=4)

plot(1970:2021, prop.deaths.china, type='l', col=2, xlab='Ano', ylab='Razão de mortes')
lines(1970:2021, prop.deaths.brazil, type='l', col=3)
lines(1970:2021, prop.deaths.rwanda, type='l', col=4)
legend("topright",                           
       c("China", "Brazil", "Rwanda"),
       lty = 1,
       col = 2:4)


affected.china <- c()
prop.affected.china <- c()
for(i in 1970:2021) {
  year.affected.china <- sum(data.regional.hubs[which(data.regional.hubs$Country=='China' & data.regional.hubs$Year == i),]$Total.Affected)
  affected.china <- c(affected.china, year.affected.china)
  prop.affected.china <- c(prop.affected.china, year.affected.china/sum(data.with.deaths.affected[which(data.with.deaths.affected$Year == i),]$Total.Affected))
}

affected.brazil <- c()
prop.affected.brazil <- c()
for(i in 1970:2021) {
  year.affected.brazil <- sum(data.regional.hubs[which(data.regional.hubs$Country=='Brazil' & data.regional.hubs$Year == i),]$Total.Affected)
  deaths.brazil <- c(affected.brazil, year.affected.brazil)
  prop.affected.brazil <- c(prop.affected.brazil, year.affected.brazil/sum(data.with.deaths.affected[which(data.with.deaths.affected$Year == i),]$Total.Affected))
}

affected.rwanda <- c()
prop.affected.rwanda <- c()
for(i in 1970:2021) {
  year.affected.rwanda <- sum(data.regional.hubs[which(data.regional.hubs$Country=='Rwanda' & data.regional.hubs$Year == i),]$Total.Affected)
  affected.rwanda <- c(affected.rwanda, year.affected.rwanda)
  prop.affected.rwanda <- c(prop.affected.rwanda, year.affected.rwanda/sum(data.with.deaths.affected[which(data.with.deaths.affected$Year == i),]$Total.Affected))
}

plot(1970:2021, affected.china, type='l', col=2, xlab='Ano', ylab='Número de afetados')
lines(1970:2021, affected.brazil, type='l', col=3)
lines(1970:2021, affected.rwanda, type='l', col=4)

plot(1970:2021, prop.affected.china, type='l', col=2, xlab='Ano', ylab='Razão de afetados')
lines(1970:2021, prop.affected.brazil, type='l', col=3)
lines(1970:2021, prop.affected.rwanda, type='l', col=4)
legend("topright",                           
       c("China", "Brazil", "Rwanda"),
       lty = 1,
       col = 2:4)


