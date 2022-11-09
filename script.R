data.with.deaths <- data[!is.na(data$Total.Deaths),]

data.with.deaths.affected <- data.with.deaths[!is.na(data.with.deaths$Total.Affected),]

brazil.with.deaths.affected <- data.with.deaths.affected[which(data.with.deaths.affected$Country == 'Brazil'),]
  