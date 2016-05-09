library(jsonlite)
library(data.table)
library(ggplot2)

## Get yours here: https://secure.meetup.com/meetup_api/key/
key<-"172121794c1ea6a101d4a416281f70"

group<-"dublinr"

## To check all avaliable fields go here: 
## https://secure.meetup.com/meetup_api/console/?path=/:urlname/events
fields<-"name,status,id,yes_rsvp_count,waitlist_count,time"

events_url <- paste0("https://api.meetup.com/",group,"/events?key=",key,"&sign=true&photo-host=public&page=2000&status=upcoming,past&only=",fields)

dt <- as.data.table(fromJSON(events_url))

dt[,wants_to_go_count:=yes_rsvp_count+waitlist_count]
dt[,meetup_date:=as.factor(lubridate::date(as.POSIXct(time/1000, origin = "1970-01-01",tz = "GMT")))]
dt[,meetup_year:=as.factor(lubridate::year(as.POSIXct(time/1000, origin = "1970-01-01",tz = "GMT")))]
setkey(dt,meetup_date)

ggplot(dt, aes(x = meetup_date, y = wants_to_go_count)) +
  geom_ribbon(aes(color = meetup_year, ymin = 0, ymax = Inf),
              size = 3) +
  geom_bar(stat = "identity") +
  geom_text(dt[wants_to_go_count > 80],
            aes(label = paste0(name, " (", wants_to_go_count, ")")),
            hjust = "inward") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
