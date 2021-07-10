#####################################################################################################
#
# Title :  GooglyPlusPLus - An interactive app to analyze T20 and ODI matches
# Designed and developed by: Tinniam V Ganesh
# Date : 28 Jun 2020
# File: analyzeTeamPerfOverall.R
# More details: https://gigadom.in/
#
#########################################################################################################
# Analyze an IPL team's performance in all matches
teamPerfOverallHelper <- function(input,output,t20type="IPL") {

    if(t20type == "IPL"){
        # Check and get the team indices of IPL teams in which the bowler has played
        dir1="./ipl/iplAllMatchesAllTeams/"
        IPLmatch <- paste("./ipl/iplAllMatchesAllTeams/", input$teamMatches,".RData",sep="")
        load(IPLmatch)
        matchesDF <- matches
    } else if (t20type == "T20M"){
        dir1="./t20/t20AllMatchesAllTeams/"
        T20Mmatch <- paste("./t20/t20AllMatchesAllTeams/", input$teamMatchesT20M,".RData",sep="")
        load(T20Mmatch)
        matchesDF <- matches
    } else if (t20type == "T20W"){
        dir1="./t20/t20WomenAllMatchesAllTeams/"
        T20Wmatch <- paste("./t20/t20WomenAllMatchesAllTeams/", input$teamMatchesT20W,".RData",sep="")
        load(T20Wmatch)
        matchesDF <- matches
    } else if (t20type == "BBL"){
        dir1="./bbl/bblAllMatchesAllTeams/"
        BBLmatch <- paste("./bbl/bblAllMatchesAllTeams/", input$teamMatchesBBL,".RData",sep="")
        load(BBLmatch)
        matchesDF <- matches
    } else if (t20type == "NTB"){
        dir1="./ntb/ntbAllMatchesAllTeams/"
        NTBmatch <- paste("./ntb/ntbAllMatchesAllTeams/", input$teamMatchesNTB,".RData",sep="")
        load(NTBmatch)
        matchesDF <- matches

    } else if (t20type == "PSL"){
        dir1="./psl/pslAllMatchesAllTeams/"
        PSLmatch <- paste("./psl/pslAllMatchesAllTeams/", input$teamMatchesPSL,".RData",sep="")
        load(PSLmatch)
        matchesDF <- matches
    } else if (t20type == "WBB"){
        dir1="./wbb/wbbAllMatchesAllTeams/"
        WBBmatch <- paste("./wbb/wbbAllMatchesAllTeams/", input$teamMatchesWBB,".RData",sep="")
        load(WBBmatch)
        matchesDF <- matches
    } else if (t20type == "ODIM"){
        dir1="./odi/odiAllMatchesAllTeams/"
        ODIMmatch <- paste("./odi/odiAllMatchesAllTeams/", input$teamMatchesODIM,".RData",sep="")
        load(ODIMmatch)
        matchesDF <- matches
    } else if (t20type == "ODIW"){
        dir1="./odi/odiWomenAllMatchesAllTeams/"
        cat("dir=","\n")
        cat(dir("./odi/odiWomenAllMatchesAllTeams/"))
        ODIWmatch <- paste("./odi/odiWomenAllMatchesAllTeams/", input$teamMatchesODIW,".RData",sep="")
        load(ODIWmatch)
        matchesDF <- matches
    } else if (t20type == "CPL"){
        dir1="./cpl/cplAllMatchesAllTeams/"
        # Check and get the team indices of CPL teams in which the bowler has played
        CPLmatch <- paste("./cpl/cplAllMatchesAllTeams/", input$teamMatchesCPL,".RData",sep="")
        load(CPLmatch)
        matchesDF <- matches
    } else if (t20type == "SSM"){
        dir1="./ssm/ssmAllMatchesAllTeams/"
        # Check and get the team indices of SSM teams in which the bowler has played
        SSMmatch <- paste("./ssm/ssmAllMatchesAllTeams/", input$teamMatchesSSM,".RData",sep="")
        load(SSMmatch)
        matchesDF <- matches
    }

    maxDate= as.Date(max(matchesDF$date))
    minDate= as.Date(min(matchesDF$date))
    cat("Date min=",as.Date(minDate),"max=",as.Date(maxDate),"\n")
    return(list(minDate,maxDate))
}
