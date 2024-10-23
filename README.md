## The Data

The data for this report was obtained from four main sources on April
17th, 2024:

1.  **MLB.com**: The official website of Major League Baseball,
    [MLB.com](https://www.mlb.com/) offers comprehensive coverage of
    games, statistics, news, and multimedia content related to
    professional baseball in the United States. Articles from MLB.com
    were accessed to fact-check and gather detailed information for the
    report, including:
    -   [4 MVPs on 1 team … how rare is
        that?](https://www.mlb.com/news/teams-with-the-most-mvps-on-roster-at-same-time)
    -   [Dodgers Postseason
        Results](https://www.mlb.com/dodgers/history/postseason-results)
    -   [History of Baseball Around the
        World](https://www.mlb.com/history/baseball-around-the-world)
    -   [Competitive Balance
        Tax](https://www.mlb.com/glossary/transactions/competitive-balance-tax)
    -   [MLB, MLBPA agree to new
        CBA](https://www.mlb.com/news/mlb-mlbpa-agree-to-cba)
    -   [$700M stunner: Ohtani to Dodgers on Biggest Deal in Sports
        History](https://www.mlb.com/news/shohei-ohtani-contract-with-dodgers)
2.  **Baseball Reference**: [Baseball
    Reference](https://www.baseball-reference.com/) is a comprehensive
    online database and resource providing historical and statistical
    information on Major League Baseball. It offers detailed player and
    team statistics, records, game logs, and analytical tools for fans,
    researchers, and enthusiasts. Data from Baseball Reference included
    the Los Angeles Dodgers’ team statistics for every year from
    2014-2024, obtained by accessing the team statistics pages and
    downloading the data in CSV format:
    -   [Los Angeles Dodgers’ Team Statistics
        2024](https://www.baseball-reference.com/teams/LAD/2024.shtml)
        (stat14.csv - stat24.csv)
3.  **Baseball Cube**: The [Baseball
    Cube](https://www.thebaseballcube.com/) is an online database
    specializing in comprehensive statistical information, player
    profiles, and historical data covering various levels of baseball.
    This includes Major League Baseball, minor leagues, college
    baseball, and international leagues. Data from Baseball Cube
    included the Los Angeles Dodgers’ team payroll and winning
    percentage for every year from 2014-2023, obtained by navigating the
    team payroll pages and downloading the data in CSV format:
    -   [Los Angeles Dodgers’ Team Payroll
        2023](https://www.thebaseballcube.com/content/payroll_year/2023/)
        (pay2014.csv - pay2023.csv)
4.  **Forbes**: [Forbes](https://www.forbes.com/) is a global media
    company known for its coverage of business, finance, and industry
    trends. It provides insightful analysis, news, and rankings on
    various topics. Data from Forbes included the team valuations of
    every MLB team in 2024. This data was obtained by accessing the MLB
    Valuations List and requesting the data set via email:
    -   [MLB Valuations
        List](https://www.forbes.com/mlb-valuations/list/)
        (Revenue24.csv)

## Importing and Cleaning the Data

    #import data
    for (year in 2014:2023) {  # Loop through the years 2014 to 2023
      # construct the file path for the payroll file of each year
      file_path <- paste0("Data/", year, " Payroll.csv")
      
      # read the CSV file for the current year
      assign(paste0("pay", year), read_csv(file_path, locale=locale(encoding="latin1")))
      
      # remove rows with incomplete data
      assign(paste0("pay", year), get(paste0("pay", year))[complete.cases(get(paste0("pay", year))), ])
      
      # add a column "year" to the dataset
      assign(paste0("pay", year), mutate(get(paste0("pay", year)), year = year))
    }

    # Convert 'team payroll' column of pay2020, pay2021, and pay2023 to numeric after removing '$' and ',' symbols
    pay2020$`team payroll` = as.numeric(gsub("[\\$,]", "", pay2020$`team payroll`))
    # pay2021$`team payroll` = as.numeric(gsub("[\\$,]", "", pay2021$`team payroll`))  # Similar operation for pay2021, but commented out
    pay2023$`team payroll` = as.numeric(gsub("[\\$,]", "", pay2023$`team payroll`))

    # Read the CSV file "Revenue24.csv" and convert 'Value (Billions)' column to numeric after removing '$' and 'B' symbols
    Revenue24 <- read_csv("Data/Revenue24.csv")
    Revenue24 <- Revenue24 %>%
      mutate(`Value (Billions)` = gsub("\\$|B", "", `Current Value`),  # Remove '$' and 'B'
             `Value (Billions)` = as.numeric(`Value (Billions)`))      # Convert to numeric



    #import data, graph 3
    for (year in 14:24) {
      # Construct the file path for the file of each year
      file_path <- paste0("Data/stat", year, ".csv")
      
      # Read the CSV file for the current year
      assign(paste0("stat", year), read.csv(file_path, encoding = "latin1"))
      
      # Remove rows with incomplete data
      assign(paste0("stat", year), get(paste0("stat", year))[complete.cases(get(paste0("stat", year))), ])
      
      # Add a column "year" to the dataset
      assign(paste0("stat", year), transform(get(paste0("stat", year)), year = year))
    }


    for (year in 2014:2023) {
      # Get the dataset for the current year
      data <- get(paste0("pay", year))
      
      # Create a new column "Payroll Rank" with ranks in descending order of "team payroll"
      data <- mutate(data, Payroll_Rank = rank(-`team payroll`, ties.method = "min"))
      
      # Assign the modified dataset back to the environment
      assign(paste0("pay", year), data)
    }


    # Combine pay datasets from multiple years into one dataset
    combined_pay_data <- bind_rows(pay2023, pay2022, pay2021, pay2020, pay2019, pay2018, pay2017, pay2016, pay2015, pay2014)

    # Trim leading and trailing whitespace from column names
    colnames(combined_pay_data) <- trimws(colnames(combined_pay_data))

    # Replace special characters in column names with appropriate characters
    combined_pay_data[combined_pay_data$year == "2023", ]$`  w` = combined_pay_data[combined_pay_data$year == "2023",]$`Â Â w`
    combined_pay_data[combined_pay_data$year == "2023", ]$`  l` = combined_pay_data[combined_pay_data$year == "2023",]$`Â Â l`

    # Remove columns containing special characters from the dataset
    combined_pay_data <- subset(combined_pay_data, select = -c(`Â Â l`, `Â Â w`))

    # Select specific columns from the dataset and rename them
    combined_pay_data <- combined_pay_data %>%
      select(`team name`, roster, league, division, `team payroll`, `  w`, `  l`, wpct, 
             rank, lgrk, `mlb rk`, `last yr payroll`, `top salary`, year, Payroll_Rank)

    # Remove non-alphabetic characters from column names
    colnames(combined_pay_data) <- gsub("[^a-zA-Z]", "", colnames(combined_pay_data))
    # str(combined_pay_data)


    # Filter combined_pay_data to create a dataset containing only Los Angeles Dodgers
    dodgers <- combined_pay_data[combined_pay_data$teamname == "Los Angeles Dodgers",]

    # Filter combined_pay_data to create a dataset containing only Oakland Athletics
    athletics <- combined_pay_data[combined_pay_data$teamname == "Oakland Athletics",]

    # Create a copy of combined_pay_data named pay_data
    pay_data <- combined_pay_data

    # Filter pay_data to remove rows where teamname is either Los Angeles Dodgers or Oakland Athletics
    pay_data <- combined_pay_data[combined_pay_data$teamname != "Los Angeles Dodgers"
                                  & combined_pay_data$teamname != "Oakland Athletics",]

    # Calculate the yearly difference in winning percentage (wpct) within combined_pay_data
    yearly_difference <- combined_pay_data %>%
      group_by(year) %>%
      summarize(difference = max(wpct) - min(wpct))


    # Calculate the median value of "Value (Billions)" column in Revenue24 dataset
    median_value_2024 <- median(Revenue24$`Value (Billions)`)

    # Create vectors of team names for top 50% and bottom 50% based on "Value (Billions)" in Revenue24 dataset
    top_teams <- Revenue24$Team[Revenue24$`Value (Billions)` >= median_value_2024]
    bottom_teams <- Revenue24$Team[Revenue24$`Value (Billions)` < median_value_2024]

    # Create a new column Color_Category in pay_data based on teamname membership in top_teams or bottom_teams
    pay_data <- pay_data %>%
      mutate(Color_Category = ifelse(teamname %in% top_teams, "Top 50%", "Bottom 50%"))

    # Combine the stat datasets for the years 2014 to 2024 into a single dataset
    stats <- bind_rows(stat14, stat15, stat16, stat17, stat18, stat19, stat20, stat21, stat22, stat23, stat24)

    # Add 2000 to the "year" column in stats dataset to align with the years in the combined_pay_data dataset
    stats$year = stats$year + 2000


    # Filter rows in stats dataset where the player name is "Freddie Freeman", "Mookie Betts", or "Shohei Ohtani"
    trio <- stats[stats$Name == "Freddie Freeman" | stats$Name == "Mookie Betts" | stats$Name == "Shohei Ohtani", ]

    # Filter rows in stats dataset where the player name is not "Freddie Freeman", "Mookie Betts", or "Shohei Ohtani"
    dodgers2 <- stats[stats$Name != "Freddie Freeman" & stats$Name != "Mookie Betts" & stats$Name != "Shohei Ohtani", ]

    # Calculate the total wins above replacement (WAR) for each year in the stats dataset
    war_sum <- stats %>%
      group_by(year) %>%
      summarize(total_war = sum(WAR, na.rm = TRUE))

    # Calculate the total WAR for "Freddie Freeman", "Mookie Betts", and "Shohei Ohtani" for each year
    war_sum1 <- trio %>%
      group_by(year) %>%
      summarize(total_war = sum(WAR, na.rm = TRUE))

    # Calculate the total WAR for players other than "Freddie Freeman", "Mookie Betts", and "Shohei Ohtani" for each year
    war_sum2 <- dodgers2 %>%
      group_by(year) %>%
      summarize(total_war = sum(WAR, na.rm = TRUE))

    # Combine the calculated WAR sums for trio and dodgers2, and assign a source column to identify each group
    combined_war <- bind_rows(
      mutate(war_sum1, source = "trio"),
      mutate(war_sum2, source = "dodgers")
    )


    # Reorder levels of the source variable to ensure consistent plotting
    combined_war$source <- factor(combined_war$source, levels = c('trio', 'dodgers'))

    # Pivot the combined war data to have years as columns and total war as values
    combined_war_pivoted <- combined_war %>%
      pivot_wider(names_from = year,
                  values_from = total_war)

    # Replace any missing values with 0
    combined_war_pivoted <- combined_war_pivoted %>%
      mutate_all(~ ifelse(is.na(.), 0, .))

    # Extract the total WAR values for the trio (first row)
    trio_values <- combined_war_pivoted[1, -1]

    # Extract the total WAR values for the dodgers (second row)
    dodgers_values <- combined_war_pivoted[2, -1]

    # Perform element-wise division to calculate the percentage of trio WAR out of total WAR
    percentage_trio_war <- trio_values / (trio_values + dodgers_values)

    # Filter the combined war data for years starting from 2020
    combined_war <- combined_war %>%
      filter(year >= 2020)

    # Calculate the average WAR for dodgers excluding the year 2024
    average_dodgers_war <- mean(dodgers$WAR[dodgers$year != 2024], na.rm = TRUE)

    # Calculate the average WAR for trio excluding the year 2024
    average_trio_war <- mean(trio$WAR[trio$year != 2024], na.rm = TRUE)

## Baseball and Major League Baseball

### A. The Game of Baseball

Baseball, often referred to as “America’s pastime,” is a sport deeply
ingrained in the cultural fabric of the United States. Originating in
the 18th century and evolving over time, baseball has become one of the
most popular and widely followed sports in the country.

### B. Major League Baseball (MLB)

Major League Baseball (MLB) is the premier professional baseball league
in the world, consisting of 30 teams divided into two leagues: the
National League (NL) and the American League (AL). Each league is
further divided into three divisions: East, Central, and West.

MLB teams represent various cities across the United States and Canada,
with franchises deeply embedded in their respective communities. From
storied franchises like the New York Yankees and Boston Red Sox to newer
additions like the Tampa Bay Rays and Arizona Diamondbacks, each team
brings its unique history and fan base to the league.

The MLB operates under a Collective Bargaining Agreement (CBA)
negotiated between the league’s owners and the MLB Players Association
(MLBPA). The CBA governs various aspects of player contracts, free
agency, revenue sharing, and other key components of the league’s
operations. It serves as the foundation for the financial and
competitive structure of MLB.

### C. Financial Dynamics in MLB

Unlike many other major professional sports leagues, such as the NFL and
NBA, MLB does not enforce a strict salary cap limiting teams’
expenditures on player salaries. This absence of a salary cap allows
teams to spend as much as they desire on player contracts, subject to
certain financial constraints and penalties.

Instead of a salary cap, MLB operates under a Luxury Tax system, also
known as the Competitive Balance Tax (CBT). The Luxury Tax imposes
penalties on teams whose payrolls exceed a predetermined threshold set
by the league. Teams exceeding the threshold are required to pay a tax
on the amount by which they exceed the limit, with the severity of the
tax increasing for repeat offenders.

### D. Competitive Balance and League Parity

While MLB strives for competitive balance among its teams, the absence
of a salary cap and the presence of revenue disparities between
franchises pose challenges to achieving true parity. Wealthier teams
with larger market sizes and higher revenues often have a significant
financial advantage over smaller-market teams with more limited
resources.

The competitive landscape of MLB is shaped by the financial disparities
between teams, with wealthier franchises often dominating the league in
terms of talent acquisition and on-field success. This dynamic raises
questions about the efficacy of existing mechanisms, such as revenue
sharing and the Luxury Tax, in fostering competitive balance and
ensuring the integrity of the league.

------------------------------------------------------------------------

## A Tale of Two Teams: The Los Angeles Dodgers and the Oakland Athletics

A notable disparity between two prominent teams in the MLB, the Los
Angeles Dodgers and the Oakland Athletics, unveils these dynamics of
competition and resource allocation within baseball. The Dodgers,
epitomizing a model of sustained success, have emerged as a formidable
force propelled by considerable financial investment. Bolstered by the
acquisition of star athletes such as Mookie Betts, Freddie Freeman, and
Shohei Ohtani, the Dodgers have solidified their position as perennial
contenders, consistently dominating the MLB regular season.

In stark contrast, the Oakland Athletics, although possessing a rich
historical legacy and dedicated fan base, grapple with inherent
challenges stemming from limited financial resources. Despite sporadic
achievements, the Athletics find themselves in a perpetual struggle
against resource constraints and the widening gap in financial
capabilities.

------------------------------------------------------------------------

### The Los Angeles Dodgers: A Dynasty in the Making

The Los Angeles Dodgers are one of the most prominent and historically
successful franchises in Major League Baseball. They are also the second
most valuable in the sport, behind only the New York Yankees. Based in
Los Angeles, California, the Dodgers have established themselves as
perennial contenders, boasting a rich history of championships and
iconic players.

The Dodgers spare no expense in their pursuit of excellence on the
field. They allocate substantial funds to secure the services of elite
players, both through free agency and trades, ensuring that they
assemble a roster capable of competing at the highest level.

The graph below illustrates the Dodgers’ consistent investment in their
on-field product:

    # Create a ggplot object with pay_data as the data and specify aesthetics
    ggplot(pay_data, aes(x = teampayroll / 1000000, y = year, color = year)) +
      # Add jitter to the scatter plot for better visualization
      geom_point(position = position_jitter(width = .1, height = .2), alpha = .5) +  
      # Highlight the Dodgers' payroll with a different color and size
      geom_point(data = dodgers, color = "#EF3E42", size = 2) +  
      # Add labels and titles to the plot
      labs(x = "Payroll (Millions)", y = "",  
           subtitle = "MLB Team Payrolls: Past 10 Seasons",
           title = "The Dodgers Are Consistently One of the Top 5 Spenders",
           caption = "Data source: Baseball Cube") +
      # Set breaks for the y-axis
      scale_y_continuous(breaks = unique(combined_pay_data$year)) +
      # Change theme to minimal
      theme_minimal() +  
      # Add labels for the Dodgers' payroll rank
      geom_text(data = dodgers, aes(label = PayrollRank), fontface = "bold.italic", color = "#EF3E42", size = 4.5, vjust = -1.3, family = "serif") +  
      # Format title, subtitle, and axis titles
      theme(plot.title = element_text(size = 16, face = "bold.italic"),  
            plot.subtitle = element_text(size = 14, face = "bold", color = "#EF3E42", hjust = 0.02, family = "serif"),
            axis.title.x = element_text(size = 14, family = "serif", face = "bold", vjust = -2),
            plot.caption = element_text(size = 10, family = "serif"),
            axis.text.x = element_text(size = 12, family = "serif"),
            axis.text.y = element_text(size = 12, family = "serif"),
            panel.grid.minor.y = element_blank(),  
            panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank()
            ) +
      # Remove the legend
      guides(color = FALSE)  

![](Final-Project_files/figure-markdown_strict/unnamed-chunk-2-1.png)

Over the past decade, the Los Angeles Dodgers have consistently ranked
among the top five teams in terms of payroll expenditure, a testament to
their financial resources and unwavering commitment to fielding a
competitive team year after year.

During this period, the Dodgers have asserted their dominance with an
impressive nine NL West Division titles, in addition to clinching three
NL pennants and a World Series championship. These remarkable
achievements not only underscore the Dodgers’ supremacy within their
division but also highlight their stature as a powerhouse in the league.

In Major League Baseball, player payroll expenditure transcends mere
financial transactions; it profoundly influences a team’s
competitiveness, success, and overall financial health. The Dodgers
exemplify the significance of financial resources in shaping a team’s
trajectory, utilizing their substantial financial backing to secure
top-tier talent and maintain their winning tradition.

### The Oakland Athletics: Struggling with Financial Constraints

The Oakland Athletics, like the Los Angeles Dodgers, hold a significant
place in Major League Baseball history. Founded in Philadelphia in 1901
before moving to Kansas City and finally settling in Oakland,
California, the Athletics have a storied past characterized by periods
of success and innovation.

With a total of nine World Series championships to their name, the
Athletics have produced legendary players and memorable moments
throughout their history. However, in recent years, they have struggled
to achieve the same level of success as some of their counterparts,
including the Dodgers.

Challenges stemming from limited financial resources have hindered the
Athletics’ ability to assemble and maintain a competitive roster capable
of contending for championships. The Athletics generated the lowest
revenue in 2024 of any team in the MLB and are the second to least
valuable team overall. This disparity in financial capabilities between
teams like the Dodgers and the Athletics underscores the profound impact
of financial resources on a team’s ability to compete effectively in
MLB.

Comparing the Dodgers to the Athletics and other teams across the league
provides valuable insights into how financial dynamics shape the
competitive landscape of Major League Baseball.

    # Create a ggplot object with pay_data as the data and specify aesthetics
    ggplot(pay_data, aes(x = year, y = wpct, group = teamname, color = Color_Category)) +
      # Add a dashed line plot with transparency and smaller size
      geom_line(alpha = .5, size = .6, linetype = "dashed") +
      # Manually set colors for the color scale
      scale_color_manual(values = c("darkgray", "black"))+
      # Add points to the plot with size and transparency
      geom_point(size = 1.5, alpha = .5) +
      # Add a solid line plot for Dodgers' data with a specific color and size
      geom_line(data = dodgers, aes(x = year, y = wpct), color = "#005A9C", size = 2) +
      # Add a solid line plot for Athletics' data with a specific color and size
      geom_line(data = athletics, aes(x = year, y = wpct), color = "#11734C", size = 2) +
      # Add text labels for Dodgers' data with filters for specific years
      geom_text(data = filter(dodgers, year != 2014 & year != 2015 & year != 2016 & year != 2018 & year != 2021 & year != 2023), aes(x = year, y = wpct, label = mlbrk), color = "#005A9C", vjust = -1, family = "serif", fontface = "bold", size = 5) +
      # Add text labels for Dodgers' data with filters for specific years
      geom_text(data = filter(dodgers, year == 2014 | year == 2015), aes(x = year, y = wpct, label = mlbrk), color = "#005A9C", vjust = -.5, hjust = -.5, family = "serif", fontface = "bold", size = 5) +
      # Add text labels for Dodgers' data with filters for specific years
      geom_text(data = filter(dodgers, year == 2016 | year == 2018), aes(x = year, y = wpct, label = mlbrk), color = "#005A9C", vjust = .7, hjust = -1, family = "serif", fontface = "bold", size = 5) +
      # Add text labels for Dodgers' data with filters for specific years
      geom_text(data = filter(dodgers, year == 2021 | year == 2023), aes(x = year, y = wpct, label = mlbrk), color = "#005A9C", vjust = 1.5, family = "serif", fontface = "bold", size = 5) +
      # Add text labels for Athletics' data with filters for specific years
      geom_text(data = filter(athletics, year != 2017 & year != 2022 & year != 2015 & year != 2016 & year != 2021), 
                aes(x = year, y = wpct, label = mlbrk), color = "#11734C", vjust = -.7, hjust = -.2, family = "serif", fontface = "bold", size = 5) +
      # Add text labels for Athletics' data with filters for specific years
      geom_text(data = filter(athletics, year == 2015), aes(x = year, y = wpct, label = mlbrk), 
                color = "#11734C", vjust = -1, hjust = -.05, family = "serif", fontface = "bold", size = 5) +
      # Add text labels for Athletics' data with filters for specific years
      geom_text(data = filter(athletics,year == 2022), aes(x = year, y = wpct, label = mlbrk), 
                color = "#11734C", vjust = -.3, hjust = -.3, family = "serif", fontface = "bold", size = 5) +
      # Add text labels for Athletics' data with filters for specific years
      geom_text(data = filter(athletics, year == 2017), aes(x = year, y = wpct, label = mlbrk), color = "#11734C", vjust = -.3, hjust = 1.35, family = "serif", fontface = "bold", size = 5) +
      # Add text labels for Athletics' data with filters for specific years
      geom_text(data = athletics %>% filter(year == 2021), 
                aes(x = year, y = wpct, label = mlbrk), color = "#11734C", vjust = -.2, hjust = -.2, family = "serif", fontface = "bold", size = 5) +
      # Add text labels for Athletics' data with filters for specific years
      geom_text(data = athletics %>% filter(year == 2016), 
                aes(x = year, y = wpct, label = mlbrk), color = "#11734C", vjust = 2, family = "serif", fontface = "bold", size = 5) +
      # Add labels and titles to the plot
      labs(x = "Season", y = "Win %", 
           subtitle = "MLB Team Win Percentage Over the Past 10 Seasons",
           title = "As the Dodgers Get Better, the Disparity in the League Grows",
           caption = "Data source: Baseball Reference, Baseball Cube, MLB, & Forbes | Note: The Dodgers made the playoffs every year 2014-2023") +
      # Set breaks for the x-axis
      scale_x_continuous(breaks = unique(pay_data$year)) +
      # Change theme to minimal
      theme_minimal() +
      # Format title, subtitle, and axis titles
      theme(plot.title = element_text(size = 18, face = "bold.italic", hjust = .1), 
            plot.subtitle = element_text(size = 16, face = "bold", color = "#005A9C", hjust = 0.12, family = "serif"),
            plot.caption = element_text(size = 10, family = "serif"),
            axis.title.x = element_text(size = 16, family = "serif", face = "bold", margin = margin(t = 25)),
            axis.title.y = element_text(size = 16, family = "serif", face = "bold", margin = margin(r = 25)),
            axis.text.x = element_text(size = 12, family = "serif"),
            axis.text.y = element_text(size = 12, family = "serif", margin = margin(r = -25)),
            panel.grid.minor.y = element_blank(), 
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_blank(),
            legend.text = element_text(size = 14, family = "serif"),
            legend.title = element_text(size = 16, family = "serif")
      ) +
      # Add a legend for the color scale
      guides(color = guide_legend(title = "Team Value", position = "top", override.aes = list(size=10))) +
      # Annotate Dodgers and Athletics text labels
      annotate("text", x = 2015, y = 0.66, label = "Los Angeles Dodgers", color = "#005A9C", fontface = "bold", size = 5) +
      annotate("text", x = 2015, y = 0.38, label = "Oakland Athletics", color = "#11734C", fontface = "bold", size = 5)

![](Final-Project_files/figure-markdown_strict/unnamed-chunk-3-1.png)

Two historically successful teams are in very different positions now.
The stark contrast between the trajectories of the Los Angeles Dodgers
and the Oakland Athletics serves as a powerful illustration of the
pronounced disparity inherent in Major League Baseball, shedding light
on the intricate dynamics of competition and resource allocation within
the league. In the early years, although there were variations in
overall rankings, the Athletics consistently trailed the Dodgers in
terms of winning percentage. Despite experiencing fleeting success with
top-10 finishes from 2018 to 2020, the Athletics faced an anticipated
downturn, culminating in finishes of 29th and 30th in 2022 and 2023,
respectively. Despite maintaining similar rankings to those in 2015 and
2016, this decline underscores the widening gap, as their winning
percentage dropped below 40%.

Looking beyond the Dodgers and Athletics, the disparity becomes even
more apparent when examining teams based on their value. Sustained
success predominantly favors wealthier teams like the Dodgers. While
lower-value teams may occasionally excel, they struggle to maintain
competitiveness over time. Financial disparities exacerbate this
situation, with wealthier teams leveraging their resources to secure top
talent, coaching, and front office staff. This trend is unmistakably
reflected in the league, as illustrated in the accompanying graph.
Initially, in 2014, the range of winning percentages was relatively
narrow, suggesting a more evenly matched field. However, as the years
progressed, the spread widened significantly, highlighting the growing
gap between teams with varying financial resources. As these financial
gaps widen, teams face increasingly daunting challenges in remaining
competitive.

This disparity is further compounded by the direct correlation between
on-field success and increased revenue streams for MLB teams. Winning
teams attract larger crowds to stadiums, command higher ticket prices,
and generate more revenue from merchandise sales and sponsorship deals.
Consequently, teams with higher payrolls often experience a boost in
their overall revenue and team valuations. The consistent spending by
teams like the Dodgers at the top of the financial spectrum creates a
significant gap in resources among MLB teams. This disparity exacerbates
the division between the league’s financially affluent franchises and
those with more limited resources, making it increasingly challenging
for smaller-market teams to compete on an equal footing.

------------------------------------------------------------------------

## The Dodgers’ Investments: Building A Dynasty

The Dodgers’ financial prowess transcends player salaries, extending to
investments in coaching, accommodations, and facilities. This
comprehensive approach not only elevates the team’s performance but also
positions them as an attractive destination for players seeking both
competitive success and luxurious working conditions. The allure of
joining the Dodgers is such that players may even be willing to accept
pay cuts, further widening the competitive advantage enjoyed by
financially robust franchises. This trend, coupled with the Dodgers’
relentless pursuit of success, is exemplified by their recent
acquisitions.

Despite already boasting an impressive lineup of All-MLB players,
All-Stars, playoff MVPs, Silver Sluggers, and Golden Glove winners, the
Dodgers continue to augment their roster. In 2020, they secured Mookie
Betts from the Boston Red Sox, a former MVP and perennial All-Star known
for his exceptional athleticism and all-around skills. His defensive
prowess and offensive capabilities make him a cornerstone player for any
team. Then in 2022, the Dodgers added another player that lead the
Atlanta Braves to a World Series Championships the season before.
Another former MVP, All-Star, Golden Glover, and Silver Slugger to their
lineup in Freddie Freeman.

However, it was their acquisition of Shohei Ohtani in 2024 that truly
made waves. Ohtani, renowned as one of the best players in the league
and a rare two-way player, entered free agency with immense
anticipation. His signing with the Dodgers for $700 million over 10
years marked the largest contract in sports history.

In 2024, this trio have constituted 54% of the team’s bWAR so far.

    # Plot the stacked bar chart with percentage labels
    ggplot(combined_war, aes(x = factor(year), y = total_war, fill = source)) +
      # Add stacked bar chart
      geom_bar(stat = "identity") +
      # Add percentage labels for "trio" source
      geom_text(aes(label = ifelse(source == "trio", paste0(round(percentage_trio_war * 100), "%"), "")),
                position = position_stack(1),
                vjust = -.5, color = "black", size = 4, 
                family = "serif") +  
      # Add threshold line
      geom_hline(yintercept = 40, linetype = "dashed", color = "black") + 
      # Add labels and titles
      labs(x = "Year", y = "Total bWAR", 
           subtitle = "Dodgers' Team bWAR Since 2020",
           title = "Betts, Freeman, and Ohtani Alone Have 54% of LA's 2024 bWAR",
           caption = "Data source: Baseball Reference & MLB | Note: the 2020 was a shortened season, the 2024 season is currently ongoing, 2024 season data correct as of April 17th, 2024") +
      # Apply minimal theme
      theme_minimal() +
      # Format title, subtitle, and axis titles
      theme(plot.title = element_text(size = 18, face = "bold.italic"), 
            plot.subtitle = element_text(size = 16, face = "bold", color = "#EF3E42", hjust = 0.02, family = "serif"),
            axis.title.x = element_text(size = 14, family = "serif", face = "bold", margin = margin(t = 25)),
            axis.title.y = element_text(size = 14, family = "serif", face = "bold", margin = margin(r = 25)),
            plot.caption = element_text(size = 10, family = "serif", margin = margin(t = 25)),
            axis.text.x = element_text(size = 12, family = "serif"),
            axis.text.y = element_text(size = 12, family = "serif"),
            panel.grid.minor.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_blank(),
            legend.text = element_text(size = 12, family = "serif")
            ) +
      # Set colors and legend labels
      scale_fill_manual(values = c("trio" = "#EF3E42", "dodgers" = "#005A9C"),  
                        labels = c("trio" = "Trio", "dodgers" = "Other Dodgers")) +  
      # Add legend for the color scale
      guides(fill = guide_legend(title = "", position = "top", override.aes = list(size=7))) +
      # Add annotation text
      annotate("text", x = .7, y = 40, label = "Postseason Threshold:", 
               size = 4, family = "serif", vjust = -.5, hjust = .1, color = "black") +
      annotate("text", x = .7, y = 40, label = "~40 WAR", 
               size = 4, family = "serif", vjust = 1.5, hjust = -.3, color = "black") +
      annotate("text", x = 1, y = 15, label = "Betts Joins", 
               size = 4, family = "serif", vjust = 0, color = "white") +
      annotate("text", x = 1, y = 13, label = "in 2020", 
               size = 4, family = "serif", color = "white") +
      annotate("text", x = 3, y = 15, label = "Freeman Joins", 
               size = 4, family = "serif", vjust = 0, color = "white") +
      annotate("text", x = 3, y = 13, label = "in 2022", 
               size = 4, family = "serif", color = "white") +
      annotate("text", x = 5, y = 15, label = "Ohtani Joins", 
               size = 4, family = "serif", vjust = 0, color = "black") +
      annotate("text", x = 5, y = 13, label = "in 2024", 
               size = 4, family = "serif", color = "black")

![](Final-Project_files/figure-markdown_strict/unnamed-chunk-4-1.png)

### Shohei Ohtani as a Reflection of Spending Disparity

The acquisition of Shohei Ohtani by the Dodgers this past offseason
exemplifies the profound impact of spending disparity in Major League
Baseball. Despite the Dodgers’ already substantial payroll, their
ability to secure Ohtani’s services with Betts and Freeman demonstrates
their capacity to attract and retain elite talent, further solidifying
their position as perennial contenders.

Moreover, the signing of Ohtani not only enhances the Dodgers’ on-field
capabilities but also carries significant financial implications. The
influx of a player of Ohtani’s caliber can drive increased revenue
through heightened fan engagement, ticket sales, merchandise, and
sponsorship deals, bolstering the Dodgers’ overall financial standing
and team valuations.

The manner in which the Dodgers structured Ohtani’s contract highlights
the creative financial maneuvering facilitated by their resources. Faced
with the challenge of accommodating Ohtani’s fair compensation within
their existing payroll expenditure, the Dodgers exploited a loophole in
the competitive bargaining agreement to defer a significant portion of
Ohtani’s salary to the end of his contract.

While Ohtani will still benefit from lucrative endorsements, the
deferred salary arrangement effectively diminishes his annual earnings
to a fraction of his market value. The Dodgers’ ability to secure such a
deal underscores the widening disparity in financial resources across
MLB teams. Such advantageous financial arrangements further solidify the
competitive advantage enjoyed by financially robust franchises like the
Dodgers, exacerbating the divide between teams with ample resources and
those with more limited financial means.

Ohtani’s acquisition by the Dodgers exemplifies a deal that would be
unattainable for lesser-funded teams such as the Athletics. The
financial resources and flexibility required to negotiate and execute
such agreements are beyond the reach of teams with more constrained
budgets, perpetuating the cycle of spending disparity in MLB.

In the 2024 season, Betts, Freeman, and Ohtani are poised to contend for
the NL MVP award, further solidifying the Dodgers’ dominance in the
league. The presence of three MVP-caliber players underscores the
unparalleled depth and talent of the Dodgers’ roster.

Furthermore, the 2024 Dodgers are only the 6th team in history with 4
MVPs on its roster, joining the list of the 2022 Dodgers, 2021 Dodgers,
1996 Red Sox, 1982 Angels, and the 1978 Reds. Such a rare achievement
speaks volumes about the Dodgers’ dominance, as the franchise owns half
of these 6 instances, all occurring within the past 5 seasons. This
achievement stands in stark contrast to the Oakland Athletics, whose
starting lineup would likely only crack the Dodgers’ Triple-A team.

------------------------------------------------------------------------

## Addressing Disparity in the MLB

The Dodgers’ acquisition of top-tier talent, including players like
Mookie Betts, Freddie Freeman, and Shohei Ohtani is unprecedented. This
concentration of talent within a single franchise not only solidifies
the Dodgers’ dominance but also exacerbates the competitive disparity
across the MLB.

Moreover, the Dodgers’ strategic maneuvers, such as recruiting talent
from other teams and exploiting loopholes in the Collective Bargaining
Agreement, further accentuate their advantage. Their substantial
financial resources afford them the capability to outbid competitors and
secure coveted players, perpetuating their stronghold on the league.

As Major League Baseball continues to witness the widening chasm between
financially robust franchises like the Los Angeles Dodgers and their
less affluent counterparts, the specter of a sport dominated by a select
few looms ominously. Left unchecked, this trend risks undermining the
essence of baseball as a sport for all, jeopardizing its appeal to fans.

Given the considerable influence wielded by wealthier teams, a critical
question emerges: Does MLB need to establish new regulations to foster
greater parity within the league? One potential solution is the
implementation of a salary cap, akin to those enforced in other major
sports leagues in the United States.

A salary cap would impose a predetermined limit on the total amount a
team can spend on player salaries, thereby leveling the playing field
and mitigating the advantage enjoyed by wealthier franchises. By
instituting financial constraints, the league can promote fair
competition, prevent teams from monopolizing talent, and enhance the
overall competitiveness of MLB.

The implementation of a salary cap in MLB would entail various
challenges and considerations. The league would need to navigate
negotiations with players’ unions, address concerns regarding revenue
sharing and competitive balance, and establish mechanisms for
enforcement and compliance. Yet, the challenge is worth it. The adoption
of a salary cap has the potential to yield significant benefits for MLB.
Beyond promoting competitive balance, it could enhance fan engagement,
stimulate interest in smaller-market teams, and safeguard the integrity
of the sport. Moreover, by fostering a more equitable environment, MLB
can ensure the sustainability and longevity of baseball as a premier
professional sport.
