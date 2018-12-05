# get token from shinyapps.io
# deployApp()
# runApp()

# div(img(src = "husker.png", height = 600, width = 450, align='center')),br(), br(),
# colors
# https://www.w3schools.com/colors/colors_picker.asp?colorhex=87CEEB

library(shiny)
library(ggplot2)
library(shinyWidgets)
library(DT)
library(shinythemes)
library(rhandsontable)
library(sqldf)
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")


  try(load("out/OFF_RZ_PASS_RUSH_SCORE_melt.rda"))
  OFF_RZ_PASS_RUSH_SCORE_melt <- data.frame(OFF_RZ_PASS_RUSH_SCORE_melt) 
  Play_Type1 <<- sort(as.character(unique(OFF_RZ_PASS_RUSH_SCORE_melt$Activity)))


g_play <- function()
{
  try(load("out/melt_dat1.rda"))
  Play_Type2 <<- sort(as.character(unique(melt_dat1$variable)))
}
g_play()

try(load("out/melt_dat2.rda"))
play_def <- sort(as.character(unique(melt_dat2$variable)))



ui <- fluidPage(theme = shinytheme("united"), br(),
                HTML('<center><img src="finalPic.png" width="1100"></center>'),br(),br(),
  sidebarLayout(
    sidebarPanel( br(),
      h3("Cornhuskers and Top Ranked Teams"), br(),
      DT::dataTableOutput('RANK',width="80%"),
    
      tags$div(class="header", checked=NA, br(),
               h5(tags$em("Note: huskers are not the rank 26th.")), br(),br(),
               h5(tags$p("Data was downloaded from the NCAA website as of November, 24th 2018")), 
               tags$a(href="https://www.ncaa.com/stats/football/fbs", "Click Here!"),br(),br(),
               tags$a(href="http://fs.ncaa.org/Docs/stats/Stats_Manuals/Football/2018ez.pdf", "Click here for formulas!")
      )),
    
   mainPanel(align='center',br(), br(),
             titlePanel("Huskers Compared to Top 25 Ranked Teams in NCAA Football"), br(),
     tabsetPanel(
       tabPanel("Offensive Performance",
                tabsetPanel(
                  tabPanel("Per Game Statistics", br(),
                           h5(p("The main goal of this article was to evaluate how cornhuskers have performed throughout 
                            this season and where do they lie within the top ranked 25 teams in NCAA, 2018. I considered 
                            top 25 college football teams based on overall rankings as of 11/24/2018 game results, and also 
                            considered cornhuskers results. It is really interested to see how cornhuskers have improved 
                            their performance throughout this season. Huskers being unranked, and specially, ended up the 
                            season with 4-8, do not imply that they haven't performed well at all."), align="justify"),
                              
                            h4(tags$em(span("Compared to last couple of years of their final outcome, starting with one of the 
                            better coaches in NCAA football, should we ONLY look at the total number of wins OR should we look 
                            at their improvement and see where they will be in near future ??????",style="color:blue")), 
                            align="justify"),

                            h5("Let's go over some of the most important factors in scoring to see how this team has put their 
                            effort on the field. Specially, if you are not happy with husker's performance this year, let's dive 
                            into the real numbers and see.", align="justify"),
                           
                           selectInput("SelectedPlay","Select",choices=Play_Type1, multiple = FALSE),br(),
                           plotOutput("plot_off", width = 750, height=650) ,br(), br(),
                           
                           h4(tags$b("Better Performance of Husker Offensive Team"),align="left"),
                           
                           h5("One of the most important key factors achieving a better final outcome is successful passing 
completions. Washington State has gained the number one place in total offensive passing completion with a value of 70.5%, 
                           while Mississipi State is the last, with percentage of total passing completion of 51.9%. Most  
                           importantly, huskers have comparably performed well in" ,span("passing completion",style="color:blue"), 
"with a value of 63.6%.",
                           tags$b(span("Missouri, Utah, Northwestern, Florida, Syracuse, UCF, Texas A&M, LSU, Penn St., and 
                           Mississipi",style="color:red")), "even being within the top ranked 25 have lower total offensive 
                           completion percentage compared to" ,tags$b(span("unranked cornhuskers.",style="color:blue")), 
                           align="justify"),
                           
                           h5("What about the",span("total passing yards", style="color:blue"),"? Compared to these top teams, 
huskers are in a very good place with a total of 2966 yards taking 14th place among them. Alabama has made the highest number 
of passing touchdowns of 44 in the season, and what about the ",span("passing touchdowns",style="color:blue"), "made by cornhuskers? Even though, huskers has made 
                           only 19 touchdowns, still they are within the top 25 ranked teams. Washington, Utah, Northwestern, 
                           Kentucky, and LSU have lower passing touchdowns than huskers.", 
                           align="justify"),
                           
                           h5("Huskers take the 10th place of making 12 redzone field goals, which means, they could not score 
                           very well on the touch downs. Compared to red zone passing and rushing touchdowns, huskers have 
                           scored more on red zone rushing touchdowns (19) than passing touochdowns (8). Similarly, Northwestern, 
Kentucky, LSU also have performed better on red zone rushing touchdowns than red zone passing touchdowns. Alabama, Clemson have 
total of 18 red zone passing touchdowns, Oklahoma 17, and Notre Dame 15 red zone passing touchdowns. While, Washington St. 
has scored the highest number of total red zone passing touchdowns (26), Syracuse has the highest total red zone rushing 
touchdowns (31).", align="justify"), 
                           
                           h5("Huskers take the 11th place in the red zone scores per attempt among the top 25 with 88.6% which is 
even higher than the number one team, Alabama, the number three team, Notre Dame. Penn St. has the highest percentage of red zone 
scores per attempt of 92.3%. The offensive touchdowns in Oklahoma is the largest (77) in this season, scoreing 604 total points, 
while huskers have total of 44 offensive touchdowns, scoring 360 total points. Iowa St. had gained the lowest total offensive 
touchdowns (33), scoring total of 295 points.", align="justify"),
                           
                           h5("Further more, majority of the fans might not know the huskers performance in total offensive yards, 
just because we mostly look at the total number of wins only. Compared to total offensive yards gained by each team, cornhuskers 
take the 13th place, having 5474 of total offensive yards in this season while Oklahoma has gained the highest number in total 
                              offensive yards of 7005.",align="justify"),

h5("As well as, huskers had successfully earned the 10th place in the total yards per play with a value of 6.31 yards per play. 
Not only that, they had put their effort gaining more rushing yards taking the 6th place among top 25, with a value of 5.41 yards 
per rush while Oklahoma had taken the first place with a vlue of 6.97 yards per rush.",align="justify"),

br(),

                           
                           
                           h4(tags$b("Poor Performance of Husker Offensive Team"),align="left"), 
                           
                           h5("There are certain areas where cornhuskers haven't performed well offensively as well as defensively. 
                           Huskers has given 11 passing interceptions, while Northwestern has given the highest total number of 
                           interceptions of 13 through out this season. Alabama, Fresno St. and LSU have the lowest passing 
                           interceptions of this season(4).", 
                           span("Interceptions play a huge role in scoring, so, this is where huskers have to make a big 
                           improvement for next year. ",style="color:blue"), "As well as, they had not performed well on 3rd down 
conversions compared to the top 25 teams.", align="justify"),
                         
                          

                           h5("I am impressed with how huskers have performed in most of the areas, specially compared to top the 
25 ranked teams. Infact, this is what husker fans expect, winning more games, improving the areas where huskers perform poor, and get 
into a NCAA football championship game in the near future. Undoubltfully, we all can see that is where huskers are achieving  fast.", 
                              align="justify"),

h5(" ", align="justify"), 

                           
                           h6("As of November 24th, 2018"), br(),
                           h6(tags$em(span("Prepared by: Nirosha Rathnayake")),align="left")
                           ),
                  
                  tabPanel("1st, 3rd, 4th Conversions",
                           h3("Performance of Offence Based on 1st, 2nd, 3rd Down Conversions"), br(),
                           selectInput("SelectedDwn","Select here",choices=Play_Type2), br(),
                           plotOutput("p_dwns", width = 700, height=600), br(),br(), 
                           
                           h5("This team has performed well and stayed in a better place on passing, rushing, and total first downs, 
                              number of 4th down conversions compared with the top 25 teams. Not many teams go for 4th down conversions,
                              but we saw how huskers were confident on going for 4th down conversions towards the end of the season. Clearly
                               they have made a significant improvement even though they did not earn many", span(" \"wins\" ", style="color:blue"),
                              ". Having low number wins is related with poor performance of 3rd and 4th down completions, and also lower 
                              number of 3rd down conversions.",align="justify"), 
                           br(),
                           DT::dataTableOutput("DWN3rd4th_COMP", width="60%")
                           )
                  )
                ),
       
       tabPanel("Defensive Performance", br(),
                selectInput("SelectedDefAct", "select here", choices = play_def), br(),
                plotOutput("p_def", width=650, height=550),
                br(),
                h5("Majority of husker fans were also worried more about the performance of defensive team. Even though, husker defensive 
team had taken the second place of average points gained with an average value of 31.3 points in this season, the performance 
of defensive team in some other areas is poor. Specially, total number of given yards is second highest in huskers (5202 yards), and 
Michigan has given the lowest total number of yards (3150 yards). In addition to that, given yards per play, number of field goals 
opponent made, and total number of touch downs gained by opponent are some other areas where defensive play performance is poor.",align="justify"),
                
                h5("Husker defensive team has gained total of 25 sacks and lie within top 25 in terms of total number of sacks, additionally, 
huskers had gained considerablly high interceptions (11 interceptions), as well as larger total team sack yards of 172.
                   ", align="justify")
       ),
       
       tabPanel("Penelties", br(), 
                h5("As all the fans noticed, penelties were taken a huge contribution to give away points during the early games of 
this season, but it improved a lot towards the end of the season. Overall, it shows that the cornhuskers have the highest penelties 
per game (8.09 penelties per game) since the beginning of the this season compared to top ranked 25 teams in NCAA football, while 
Northwestern has given the lowest number of penelties, and also the smallest penelty yards per game", align='justify'), br(),
                
                h5("It is also clear that the cornhuskers have given the highest penelty yards per game with a value of 74.09 yards.", 
                   align="justify"),
                DT::dataTableOutput("tbl_penelties", width="55%")
       ),
       
       tabPanel("Summary of Recap Stories", br(),
                h3("Using Text Analytics"), 
                
                h5("This page includes the most frequent words had been used through out this season based on recap story after the 
each game. I considered the text data using recap story for each game from NCAA website in this season and created a wordcloud to 
                   display most frequent 500 words in the combined recap stories data set. The size of the word displays the 
                   frequency of that specific word. Histogram below shows the most frequent 20 words with the counts as well.
                   This is just an exploratory analysis for my future article to predict the binary outcome (win or lose) using 
                   text analytics techniques." ,align="justify"), br(),
                
                tags$a(href="https://www.ncaa.com/game/football/fbs/2018/11/17/michigan-st-nebraska/recap", 
                       "CLICK HERE for \"Recap story for Mchi.St. Vs. NE !\"" ),
                
                
                
                plotOutput("wc_pic", width=600, height=600), 
                
                plotOutput('plot_commWords', width = 600, height=600)
       )
       
       )
   )
   )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
      output$plot_off<- renderPlot({
        
        try(load("out/OFF_RZ_PASS_RUSH_SCORE_melt.rda"))
        OFF_melt <- data.frame(OFF_RZ_PASS_RUSH_SCORE_melt)
        OFF_melt_df <- sqldf(paste0("select Team,Value from OFF_melt
                      where Activity='", input$SelectedPlay, "' "))
        
        OFF_melt_df$Team <- factor(OFF_melt_df$Team, levels=OFF_melt_df$Team[order(OFF_melt_df$Value)])
        plot_offense <- ggplot(OFF_melt_df, aes(x=Team, y=Value,  fill=Value))+
          geom_col(width = 0.5)+
          # scale_colour_gradient(low ="blue", high="green")+
          scale_fill_gradient(low = "red", high = "blue") +
          coord_flip()+ggtitle(paste0(" ",input$SelectedPlay," "))+
          geom_text(aes(label=Value), size=3, position=position_stack(vjust=1.05))+
          theme(plot.title = element_text(hjust=0.5)) +ylab(paste0(" ",input$SelectedPlay," "))+xlab("Team")
        return(plot_offense)
     })
      


      try(load("out/RANK.rda"))
      output$RANK <- DT::renderDataTable(datatable(RANK,
                    options=list(autoWidth=TRUE,pageLength=26, dom='t',
                                 columnDefs=list(list(width="150px", targets="_all",className='dt-center')),
                                 initComplete = JS(
                                   "function(settings, json) {",
                                   "$(this.api().table().header()).css({'background-color':'#bfbfbf',
                                   'font-size': '11pt','font-color':'black'});",
                                   "}")
                                 ),rownames=FALSE)%>%
                      formatStyle(columns=1, target="cell", backgroundColor="#bfbfbf",color="black") %>%
                      formatStyle(columns=2:3, targets="_all",backgroundColor="lightblue",color="black"))
      

      background2 <- "value == 'Nebraska' ? 'red' : value != 'else' ? 'white' : ''"
      class(background2) <- "JS_EVAL"
      
      try(load("out/DWN3rd4th_COMP.rda"))
      output$DWN3rd4th_COMP <- DT::renderDataTable(datatable(DWN3rd4th_COMP,
        options=list(autoWidth=FALSE,pageLength=26, scrollCollapse=TRUE,dom='t',
                     columnDefs=list(list(width="100px", targets="_all" ,className='center')),
                       deferRender=TRUE),rownames=FALSE) %>%
      formatStyle(columns=1, target="cell", backgroundColor=background2,color="black") %>%
        formatStyle('Team',columns=2:4,target="row",fontWeight = 'bold',color='black', backgroundColor=background2) 
      )
      
      
      output$p_dwns <- renderPlot({
        
        try(load("out/melt_dat1.rda"))
        melt_dat1 <- data.frame(melt_dat1)
        
        df_dwns <- sqldf(paste0("select Team,value from melt_dat1
                               where variable='", input$SelectedDwn, "' "))
        
        df_dwns$Team <- factor(df_dwns$Team, levels=df_dwns$Team[order(df_dwns$value)])
        
        
        plot_dwns <- ggplot(df_dwns, aes(x=Team, y=value,  fill=value))+
          geom_col(width = 0.5)+
          scale_color_gradient2(midpoint=mid, low="blue", mid="white",high="red")+
          coord_flip()+ggtitle(paste0("Total ",input$SelectedDwn," "))+
          geom_text(aes(label=value), size=3, position=position_stack(vjust=1.05))+
          theme(plot.title = element_text(hjust=0.5)) +ylab(paste0(" ",input$SelectedDwn," "))+xlab("Team")
        return(plot_dwns)
      })
      
      
      background1 <- "value == 'Nebraska' ? 'red' : value != 'else' ? 'gray' : ''"
      class(background1) <- "JS_EVAL"
      
      # try(load("out/DWN3rd4th_COMP.rda"))
      # 
      # output$tbl_dwn_conv <- DT::renderDataTable(datatable(DWN3rd4th_COMP, class = 'cell-border stripe',
      #                   options=list(autoWidth=FALSE, pageLength=26, scrollCollapse=TRUE,dom='t',
      #                                columnDefs=list(list(width="100px", targets="_all",backgroundColor="black",className='dt-center')),
      #                                deferRender=TRUE,
      #                                initComplete = JS(
      #                                  "function(settings, json) {",
      #                                  "$(this.api().table().header()).css({'background-color':'#bfbfbf',
      #                                  'font-size': '11pt','font-color':'white'});",
      #                                  "}")
      #                                ),rownames=FALSE) %>%
      #                     formatStyle(columns=1, target="cell", backgroundColor=background1,color="black") %>%
      #                     # formatStyle(columns=2, target="cell", backgroundColor="#bee5f4",color="black") %>%
      #                     formatStyle('Team',columns=2:3,target="row",fontWeight = 'bold',color='black', backgroundColor=background1) 
      #     )
      # 
      try(load("out/df_penelty25.rda"))
      # my_vals = 'Nebraska'
      # my_colors = ifelse(my_vals=='Nebraska','red','yellow')
      
      background <- "value == 'Nebraska' ? 'red' : value != 'else' ? 'lightgreen' : ''"
      class(background) <- "JS_EVAL"
      
      output$tbl_penelties <- DT::renderDataTable(datatable(df_penelty25, 
                    options=list(autoWidth=FALSE, pageLength=26, scrollCollapse=TRUE,dom='t',
                           columnDefs=list(list(width="100px", targets="_all",backgroundColor="black",className='dt-center')),
                           deferRender=TRUE,
                                 initComplete = JS(
                                   "function(settings, json) {",
                                   "$(this.api().table().header()).css({'background-color':'#bfbfbf',
                                  'font-size': '11pt','font-color':'white'});",
                                "}")
                    ),rownames=FALSE) %>%
                      formatStyle(columns=1, target="cell", backgroundColor="#bfbfbf",color="black") %>%
                      formatStyle('Team', columns=2:3,target='row',color='black',fontWeight = "bold", backgroundColor = background)
      )
      
      
      output$p_def <- renderPlot({
        try(load("out/melt_dat2.rda"))
        melt_dat2 <- data.frame(melt_dat2)
        df_Def <- sqldf(paste0("select Team,value from melt_dat2
                      where variable='", input$SelectedDefAct, "' "))
        
        df_Def$Team <- factor(df_Def$Team, levels=df_Def$Team[order(df_Def$value)])
        
        plot_def <- ggplot(df_Def, aes(x=Team, y=value,  fill=value))+
          geom_col(width = 0.5)+
          scale_fill_gradient(low = "green", high = "blue") +
          coord_flip()+ggtitle(paste0("Total ",input$SelectedDefAct," "))+
          geom_text(aes(label=value), size=3, position=position_stack(vjust=1.05))+
          theme(plot.title = element_text(hjust=0.5)) +ylab(paste0(" ",input$SelectedDefAct," "))+xlab("Team")
        return(plot_def)
      })
      
      output$wc_pic <- renderPlot({
        try(load("out/d.rda"))
        set.seed(123)
        wc <- wordcloud(words = d$word, freq=d$freq, min.freq = 1, max.words = 500,
                        random.order = FALSE, rot.per = 0.45, colors = brewer.pal(8,"Dark2"))
        return(wc)
      })
      
      output$plot_commWords <- renderPlot({
        try(load("out/d.rda"))
        barplot(d[1:20,]$freq, las = 2, names.arg = d[1:20,]$word,
                col ="lightblue", main ="Most frequent words",
                ylab = "Word frequencies")
      })
      
        
}

# Run the application 
shinyApp(ui = ui, server = server)

