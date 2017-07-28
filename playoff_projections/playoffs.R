## Ben Kite
## 2017-07-28

##install.packages("BradleyTerry2")
library(BradleyTerry2)
library(rockchalk)

set.seed(6113

year <- 2017
dat <- read.csv(file = "data/2017Games.csv", stringsAsFactors = FALSE)
table(dat$R)
dat$R <- as.numeric(dat$R)

datprep <- function(dat){
## Need clean team name that is used at the end
    dat$hteam <- dat$Tm
    dat$ateam <- dat$AwayTeam

    ## Make win variables
    dat$HomeWin <- ifelse(dat$R > dat$RA, 1, 0)
    dat$AwayWin <- ifelse(dat$R < dat$RA, 1, 0)

    ## Make home field variables
    dat$HomeTeam <- data.frame(team = dat$Tm, at.home = 1)
    dat$AwayTeam <- data.frame(team = dat$AwayTeam, at.home = 0)

    dat$date <- as.character(dat$Date)
    dat$date <- substring(dat$date, 1, 8)
    dat$date <- as.Date(dat$date, "%Y%m%d")

    ## Remove unused variables
    #dat <- dat[,c("HomeTeam", "AwayTeam", "HomeWin", "AwayWin", "date", "hteam", "ateam")]
    dat
}

learner <- function(dat, datespan = NULL){
    dates <- unique(dat$date)
    curdate <- max(dat$date, na.rm = TRUE)
    enddate <- dates[which(dates == curdate)]
    if(is.null(datespan)){
        startdate <- min(dat$date)
    }else{
        startdate <- dates[which(dates == curdate) - datespan]
    }
    dat <- dat[which(dat$date >= startdate),]
    out <- BTm(outcome = cbind(HomeWin, AwayWin), player1 = HomeTeam,
           player2 = AwayTeam, formula = ~ team, id = "team",
           refcat = "KCR", data = dat)
    out <- out$coef
    out["teamKCR"] <- 0
    #out["teamTBR:at.home"] <- 0
    out
}

## function to predict game outcomes
predictor <- function(coefs, homeTeam, awayTeam, winloss = FALSE){
    home <- paste0("team", homeTeam)
    away <- paste0("team", awayTeam)
    #homef <- paste0("team", homeTeam, ":at.home")
    odds <- coefs[home] - coefs[away]
    prob <- exp(odds)/(1+exp(odds))
    if(winloss == TRUE){
        out <- ifelse(prob < .5, 0, 1)
    }else{
        out <- prob
    }
    names(out) <- "homeTeamWin"
    out
}

simulation2 <- function(workdat, upcoming, coefs, hot = FALSE, datespan = NULL){
    dates <- unique(upcoming$date)
    for (j in dates){
        tmpdat <- upcoming[which(upcoming$date == j),]
        for (i in 1:nrow(tmpdat)){
            home <- tmpdat[i, "HomeTeam"][1]
            away <- tmpdat[i, "AwayTeam"][1]
            home <- levels(home$team)[as.numeric(home)]
            away <- levels(away$team)[as.numeric(away)]
            prob <- predictor(coefs, home, away)
            roll <- rbinom(1, 1, prob)
            if(roll == 1){
                tmpdat[i,c("HomeWin", "AwayWin")] <- c(1, 0)
            }else{
                tmpdat[i,c("HomeWin", "AwayWin")] <- c(0, 1)
            }
        }
        workdat <- rbind(workdat, tmpdat)
        if(isTRUE(hot)){
            coefs <- learner(workdat, datespan)
        }
    }
    out <- list()
    out[[1]] <- workdat
    out[[2]] <- coefs
    out
}

wrapper2 <- function(rep, workdat, upcoming, coefs, hot = FALSE, datespan = NULL){
    output <- simulation2(workdat, upcoming, coefs, datespan, hot)
    gameoutcomes <- output[[1]]
    homewins <- aggregate(HomeWin ~ hteam, data = gameoutcomes, sum)
    awaywins <- aggregate(AwayWin ~ ateam, data = gameoutcomes, sum)
    wins <- cbind(homewins, awaywins[,"AwayWin"])
    wins$teamWins <- rowSums(wins[,2:3])
    out <- list()
    out[[1]] <- wins[,c(1, 4)]
    out[[2]] <- output[[2]]
    out
}

dat <- datprep(dat)
currentDate <- Sys.Date()
workdat <- dat[which(dat$date < currentDate),]
upcoming <- dat[which(dat$date >= currentDate),]
coefs <- learner(workdat)

output <- lapply(1:1000, wrapper2, workdat, upcoming, coefs, hot = TRUE)

processor <- function(output){

    al <- c('BAL', 'BOS', 'CHW', 'CLE', 'DET', 'KCR', 'HOU', 'LAA', 'MIN',
            'NYY', 'OAK', 'SEA', 'TBR', 'TEX', 'TOR')

    aleast <- c('BAL', 'BOS', 'NYY', 'TBR', 'TOR')
    alcent <- c('CHW', 'DET', 'CLE', 'KCR', 'MIN')
    alwest <- c('HOU', 'LAA', 'OAK', 'SEA', 'TEX')

    nl <- c('ATL', 'ARI', 'CHC', 'CIN', 'COL', 'LAD', 'MIA', 'MIL', 'NYM',
            'PHI', 'PIT', 'SDP', 'SFG', 'STL', 'WSN')

    nleast <- c('ATL', 'MIA', 'NYM', 'PHI', 'WSN')
    nlcent <- c('CHC', 'CIN', 'MIL', 'PIT', 'STL')
    nlwest <- c('ARI', 'COL', 'LAD', 'SDP', 'SFG')

    coefs <- output[[2]]

    tiebreaker <- function(x, coefs){
        if(nrow(x) > 1){
            teams <- x[,2]
            if (predictor(coefs, teams[1], teams[2], winloss = TRUE) == 1){
                return(x[which(x[,2] == teams[1]),])
            } else {
                return(x[which(x[,2] == teams[2]),])
            }
        } else {
            return(x)
        }
    }

    playoffer <- function(x){
        games <- x[[1]]
        tmpaleast <- games[games[,1] %in% aleast,]
        tmpalcent <- games[games[,1] %in% alcent,]
        tmpalwest <- games[games[,1] %in% alwest,]

        tmpnleast <- games[games[,1] %in% nleast,]
        tmpnlcent <- games[games[,1] %in% nlcent,]
        tmpnlwest <- games[games[,1] %in% nlwest,]

        aleastwinner <- cbind("AL_East", tmpaleast[tmpaleast[,2] == max(tmpaleast[,2]),1])
        alcentwinner <- cbind("AL_Central", tmpalcent[tmpalcent[,2] == max(tmpalcent[,2]),1])
        alwestwinner <- cbind("AL_West", tmpalwest[tmpalwest[,2] == max(tmpalwest[,2]),1])

        aleastwinner <- tiebreaker(aleastwinner, coefs)
        alcentwinner <- tiebreaker(alcentwinner, coefs)
        alwestwinner <- tiebreaker(alwestwinner, coefs)

        nleastwinner <- cbind("NL_East", tmpnleast[tmpnleast[,2] == max(tmpnleast[,2]),1])
        nlcentwinner <- cbind("NL_Central", tmpnlcent[tmpnlcent[,2] == max(tmpnlcent[,2]),1])
        nlwestwinner <- cbind("NL_West", tmpnlwest[tmpnlwest[,2] == max(tmpnlwest[,2]),1])

        nleastwinner <- tiebreaker(nleastwinner, coefs)
        nlcentwinner <- tiebreaker(nlcentwinner, coefs)
        nlwestwinner <- tiebreaker(nlwestwinner, coefs)

        winners <- rbind(aleastwinner, alcentwinner, alwestwinner, nleastwinner, nlcentwinner, nlwestwinner)
        winners <- data.frame(winners, stringsAsFactors = FALSE)

        wildcard <- games[order(games$teamWins, decreasing = TRUE),]
        wildcard <- wildcard[!wildcard$hteam %in% winners[,2],]
        alwildcard <- wildcard[wildcard$hteam %in% al,"hteam"][1:2]
        nlwildcard <- wildcard[wildcard$hteam %in% nl,"hteam"][1:2]

        wildcards <- c("AL_Wild1", "AL_Wild2", "NL_Wild1", "NL_Wild2")
        wildcards <- cbind(wildcards, c(alwildcard, nlwildcard))
        names(winners) <- c("Position", "Team")
        colnames(wildcards) <- c("Position", "Team")
        rbind(winners, wildcards)
    }

    results <- lapply(output, playoffer)

    summarizer <- function(x, div){
        x[x[,"Position"] == div,"Team"]
    }

    out <- list()
    out[["Win AL Central"]] <- table(unlist(sapply(results, summarizer, "AL_Central")))/length(output)
    out[["Win AL East"]] <- table(unlist(sapply(results, summarizer, "AL_East")))/length(output)
    out[["Win AL West"]] <- table(unlist(sapply(results, summarizer, "AL_West")))/length(output)
    out[["Win NL Central"]] <- table(unlist(sapply(results, summarizer, "NL_Central")))/length(output)
    out[["Win NL East"]] <- table(unlist(sapply(results, summarizer, "NL_East")))/length(output)
    out[["Win NL West"]] <- table(unlist(sapply(results, summarizer, "NL_West")))/length(output)
    out[["AL Wild Card 1"]] <- table(unlist(sapply(results, summarizer, "AL_Wild1")))/length(output)
    out[["AL Wild Card 2"]] <- table(unlist(sapply(results, summarizer, "AL_Wild2")))/length(output)
    out[["NL Wild Card 1"]] <- table(unlist(sapply(results, summarizer, "NL_Wild1")))/length(output)
    out[["NL Wild Card 2"]] <- table(unlist(sapply(results, summarizer, "NL_Wild2")))/length(output)
    out[["Make playoffs"]] <- table(unlist(lapply(results, function(x) unlist(x[,2]))))/length(output)
    out
}

processor(output)