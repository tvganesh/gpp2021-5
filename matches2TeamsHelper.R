#########################################################################################################
#
# Title :  GooglyPlusPLus - An interactive app to analyze T20 and ODI matches
# Designed and developed by: Tinniam V Ganesh
# Date : 28 Jun 2020
# File: matches2TeamsHelper.R
# More details: https://gigadom.in/
#
#########################################################################################################
#Analyze all IPL matches between 2 IPL teams
matches2TeamsHelper <- function(input,output,t20type="IPL") {

    print("Entering helper teams")
    cat("t20=",t20type,"\n")
    # Get the teams
    # Get the match
    if( t20type == "IPL")
        p <- strsplit(as.character(input$match2),"-")
    else if (t20type == "T20M")
        p <- strsplit(as.character(input$match2T20M),"-")
    else if (t20type == "T20W")
        p <- strsplit(as.character(input$match2T20W),"-")
    else if (t20type == "BBL")
        p <- strsplit(as.character(input$match2BBL),"-")
    else if (t20type == "NTB")
        p <- strsplit(as.character(input$match2NTB),"-")
    else if (t20type == "PSL")
        p <- strsplit(as.character(input$match2PSL),"-")
    else if (t20type == "WBB")
        p <- strsplit(as.character(input$match2WBB),"-")
    else if (t20type == "ODIM")
        p <- strsplit(as.character(input$match2ODIM),"-")
    else if (t20type == "ODIW")
        p <- strsplit(as.character(input$match2ODIW),"-")
    else if (t20type == "CPL")
        p <- strsplit(as.character(input$match2CPL),"-")
    else if (t20type == "SSM")
        p <- strsplit(as.character(input$match2SSM),"-")

    teams2 <- c(p[[1]][1],p[[1]][2])

    cat("teams=",teams2,"\n")

    if( t20type == "IPL"){
        # Set the IPL teams
        output$selectTeam2 <- renderUI({
            selectInput('team2', 'Choose team',choices=teams2,selected=input$team2)
        })

        #Find the other team
        otherTeam = setdiff(teams2,input$team2)
        cat("IPL team=",input$team2,"other team=",otherTeam)
    } else if (t20type == "T20M"){
        output$selectTeam2T20M <- renderUI({
            selectInput('team2T20M', 'Choose team',choices=teams2,selected=input$team2T20M)
        })
        otherTeam = setdiff(teams2,input$team2T20M)
        cat("T20 team=",input$team2T20M,"other team=",otherTeam)
    } else if (t20type == "T20W"){
        output$selectTeam2T20W <- renderUI({
            selectInput('team2T20W', 'Choose team',choices=teams2,selected=input$team2T20W)
        })
        otherTeam = setdiff(teams2,input$team2T20W)
        cat("T20 team=",input$team2T20W,"other team=",otherTeam)
    } else if (t20type == "BBL"){
        output$selectTeam2BBL <- renderUI({
            selectInput('team2BBL', 'Choose team',choices=teams2,selected=input$team2BBL)
        })
        otherTeam = setdiff(teams2,input$team2BBL)
        cat("T20 team=",input$team2BBL,"other team=",otherTeam)
    }  else if (t20type == "NTB"){
        output$selectTeam2NTB <- renderUI({
            selectInput('team2NTB', 'Choose team',choices=teams2,selected=input$team2NTB)
        })
        otherTeam = setdiff(teams2,input$team2NTB)
        cat("T20 team=",input$team2NTB,"other team=",otherTeam)
    }  else if (t20type == "PSL"){
        output$selectTeam2PSL <- renderUI({
            selectInput('team2PSL', 'Choose team',choices=teams2,selected=input$team2PSL)
        })
        otherTeam = setdiff(teams2,input$team2PSL)
        cat("T20 team=",input$team2PSL,"other team=",otherTeam)
    }  else if (t20type == "WBB"){
        output$selectTeam2WBB <- renderUI({
            selectInput('team2WBB', 'Choose team',choices=teams2,selected=input$team2WBB)
        })
        otherTeam = setdiff(teams2,input$team2WBB)
        cat("T20 team=",input$team2WBB,"other team=",otherTeam)
    } else if (t20type == "ODIM"){
        output$selectTeam2ODIM <- renderUI({
            selectInput('team2ODIM', 'Choose team',choices=teams2,selected=input$team2ODIM)
        })
        otherTeam = setdiff(teams2,input$team2ODIM)
        cat("T20 team=",input$team2ODIM,"other team=",otherTeam)
    }  else if (t20type == "ODIW"){
        output$selectTeam2ODIW <- renderUI({
            selectInput('team2ODIW', 'Choose team',choices=teams2,selected=input$team2ODIW)
        })
        otherTeam = setdiff(teams2,input$team2ODIW)
        cat("T20 team=",input$team2ODIW,"other team=",otherTeam)
    } else if (t20type == "CPL"){
        output$selectTeam2CPL <- renderUI({
            selectInput('team2CPL', 'Choose team',choices=teams2,selected=input$team2CPL)
        })
        otherTeam = setdiff(teams2,input$team2CPL)
        cat("T20 team=",input$team2CPL,"other team=",otherTeam)
    } else if (t20type == "SSM"){
        output$selectTeam2SSM <- renderUI({
            selectInput('team2SSM', 'Choose team',choices=teams2,selected=input$team2SSM)
        })
        otherTeam = setdiff(teams2,input$team2SSM)
        cat("T20 team=",input$team2SSM,"other team=",otherTeam)
    }


    if(t20type == "IPL"){
        dir1="./ipl/iplMatches2Teams"
        IPLmatch <- paste("./ipl/iplMatches2Teams/", input$match2,".RData",sep="")
        cat("IPL2=",getwd(),"\n")
        load(IPLmatch)
        matchesDF <- matches
    } else if (t20type == "T20M"){
        dir1="./t20/t20Matches2Teams"
        T20Mmatch <- paste("./t20/t20Matches2Teams/", input$match2T20M,".RData",sep="")
        cat("t20m2=",getwd(),"\n")
        load(T20Mmatch)
        matchesDF <- matches

    } else if (t20type == "T20W"){
        dir1="./t20/t20WomenMatches2Teams"
        T20Wmatch <- paste("./t20/t20WomenMatches2Teams/", input$match2T20W,".RData",sep="")
        cat("T20W2=",getwd(),"\n")
        load(T20Wmatch)
        matchesDF <- matches

    } else if (t20type == "BBL"){
        dir1="./bbl/bblMatches2Teams"
        BBLmatch <- paste("./bbl/bblMatches2Teams/", input$match2BBL,".RData",sep="")
        cat("BBL2=",getwd(),"\n")
        load(BBLmatch)
        matchesDF <- matches

    } else if (t20type == "NTB"){
        dir1="./ntb/ntbMatches2Teams"
        NTBmatch <- paste("./ntb/ntbMatches2Teams/", input$match2NTB,".RData",sep="")
        cat("NTB2=",getwd(),"\n")
        load(NTBmatch)
        matchesDF <- matches

    } else if (t20type == "PSL"){
        dir1="./psl/pslMatches2Teams"
        PSLmatch <- paste("./psl/pslMatches2Teams/", input$match2PSL,".RData",sep="")
        cat("PSL2=",getwd(),"\n")
        load(PSLmatch)
        matchesDF <- matches

    } else if (t20type == "WBB"){
        dir1="./wbb/wbbMatches2Teams"
        WBBmatch <- paste("./wbb/wbbMatches2Teams/", input$match2WBB,".RData",sep="")
        cat("WBB2=",getwd(),"\n")
        load(WBBmatch)
        matchesDF <- matches

    } else if (t20type == "ODIM"){
        dir1="./odi/odiMatches2Teams"
        ODIMmatch <- paste("./odi/odiMatches2Teams/", input$match2ODIM,".RData",sep="")
        cat("ODIM2=",getwd(),"\n")
        load(ODIMmatch)
        matchesDF <- matches

    } else if (t20type == "ODIW"){
        dir1="./odi/odiWomenMatches2Teams"
        ODIWmatch <- paste("./odi/odiWomenMatches2Teams/", input$match2ODIW,".RData",sep="")
        cat("ODIW2=",getwd(),"\n")
        load(ODIWmatch)
        matchesDF <- matches

    } else if (t20type == "CPL"){
        dir1="./cpl/cplMatches2Teams"
        CPLmatch <- paste("./cpl/cplMatches2Teams/", input$match2CPL,".RData",sep="")
        cat("CPL2=",getwd(),"\n")
        load(CPLmatch)
        matchesDF <- matches
    } else if (t20type == "SSM"){
        dir1="./ssm/ssmMatches2Teams"
        SSMmatch <- paste("./ssm/ssmMatches2Teams/", input$match2SSM,".RData",sep="")
        cat("SSM2=",getwd(),"\n")
        load(SSMmatch)
        matchesDF <- matches
    }

    maxDate= as.Date(max(matchesDF$date))
    minDate= as.Date(min(matchesDF$date))
    cat("Date min=",as.Date(minDate),"max=",as.Date(maxDate),"\n")
    return(list(minDate,maxDate))
}
