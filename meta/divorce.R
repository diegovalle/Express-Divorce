



express <- div.df$date > as.Date("2008-10-03")
season <- seasonaldummy(ts(div.df$divorces, freq = 12))
fit <- gamm(divorces ~ s(date) + season + express , 
            #family="poisson", 
            data = div.df,
            correlation = corARMA(form = ~ datenum, p = 3))
fit2 <- gamm(divorces ~ s(datenum) + season + express , 
            family="poisson", 
            data = div.df,
            correlation = corARMA(form = ~ datenum, p = 2))
fit3 <- gamm(divorces ~ s(as.numeric(date)) + season +express , 
            family="poisson", 
            data = div.df,
            correlation = corARMA(p = 1))
plot(fit3$lme)
acf(resid(fit3$lme, type = "normalized"))
pacf(resid(fit3$lme, type = "normalized"))
div.df$predict <- predict(fit3$gam, type = "response")
ggplot(div.df, aes(date, divorces)) +
  geom_line() +
  geom_line(aes(date, predict)) +
  geom_vline(xintercept = as.numeric(as.Date("2008-10-03"))) +
  scale_x_date()

anova(fit$lme, fit2$lme, fit3$lme)







## simulate from posterior distribution of beta
Rbeta <- mvrnorm(n = 10000, coef(fit$gam), vcov(fit$gam))
Xp <- predict(fit$gam,  type = "lpmatrix")
sim1 <- Xp %*% t(Rbeta)

## plot the observation and 25 of the 1000 trends
set.seed(321)
want <- sample(1000, 25)
ylim <- range(sim1[,want], div.df$divorces)
plot(divorces ~ date, data = div.df, ylim = ylim)
matlines(div.df$datenum, sim1[,want], col = "black", lty = 1, pch = NA)

qqnorm(fit$lme)




plot(stl(ts(div.df$divorces, f = 12), s.window = "periodic"))

div.df.before <- subset(div.df, date < as.Date("2008-10-03"))
div.df.after <- subset(div.df, date >= as.Date("2008-10-03"))

before.ts <- ts(div.df.before$divorces, start = 1993, f =12)
after.ts <- ts(div.df.after$divorces, start = 1993, f =12)
div.df.before$trend <- data.frame(stl(before.ts, s.window = "per")$time.series)$trend
div.df.after$trend <- data.frame(stl(after.ts, s.window = "per")$time.series)$trend

seasonplot(ts(div.df$divorces, start = 1993, f =12))
div.df$trend <- data.frame(decompose(ts(div.df$divorces, start = 1985, f =12))$trend)[,1]
div.df$trend <- data.frame(stl(ts(div.df$divorces, start = 1985, f =12), s.window = "per")$time.series)$trend
auto.arima()

ggplot(rbind(div.df.before, div.df.after), aes(date, trend)) +
  geom_line() +
  geom_line(aes(date, divorces)) +
  geom_vline(xintercept = as.numeric(as.Date("2008-10-03"))) +
  scale_x_date()



require( "twitteR" )

massageTwitter <- function( handle = "@fhollande", n = 2000, nwords = 5 ){

    getTweets <- function( handle, n ){
        tweets <- userTimeline( handle, n = n )
        sapply( tweets, function(txt) txt$text )
    }
    
    
    clean <- function( txt ){
        txt <- casefold(txt)
        txt <- gsub( "[,:\"!.]", "", txt )
        words <- unlist( strsplit( txt, "[[:space:]]+" ) )
        words <- words[ ! grepl( "http", words ) ]
        words <- words[ ! grepl( "www", words ) ]
        
        words
    }
    
    stats <- function( txt, n = 5, handle ){
      words <- table( clean( txt ) )
      most <- head( sort( words[ nchar( names(words) ) > 5 ], decreasing = TRUE ), n )
      res <- paste( sprintf( "%s (%d)", names(most), most ), collapse = ", " )
      sprintf( "#lesmotslesplustweetes par %s: %s", handle, res )
    }

    stats( getTweets( handle, n ), nwords, handle )
    
}
sea <- searchTwitter('#mexicorojo', n=50)
searchTwitter('#veracruz', n=50)
searchTwitter('#reynosafollow', n=50)
massageTwitter( "@EPNMexico", 500, 15 )
massageTwitter( "FelipeCalderon", 500, 15 )
massageTwitter( "josefinavm", 500, 15 )
massageTwitter( "lopezobrador_", 500, 15 )
massageTwitter( "bparedesrangel", 100, 15 )
