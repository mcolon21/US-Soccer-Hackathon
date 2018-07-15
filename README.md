# US-Soccer-Hackathon

## Inspiration

With the World Cup wrapping up, many questions remain as to the results of the tournament.  How did Croatia, a country of just over 4 million, find themselves in the World Cup final?  How do the unquestioned best players in the world (Ronaldo and Messi) continue to fall so short of hoisting the FIFA World Cup Trophy?  And how were the reigning champions of the World Cup eliminated in the group stage for the third consecutive World Cup (Italy in 2010, Spain in 2014, Germany in 2018)?

After pondering these questions, and many others pertaining to the 2018 edition of the most famous global soccer tournament, it dawned on us that unlike club soccer play, where players compete with their teammates on a regular basis, playing soccer on a national team does not happen as frequently.  This most likely results in a larger disparity in an essential, yet rarely calculated variable, due to the difficulty of doing so: chemistry.  We reasoned that if talent isn't responsible for telling the complete story of what happens at the World Cup, chemistry may play a significant role in some of the outcomes that many deemed unlikely entering the tournament.

That is why for our project submission for the 2018 US Soccer Hackathon, we decided to create a metric that quantifies team chemistry, and thus, may help us to answer some of soccer's biggest questions in terms of team-building and the importance of team cohesion.

## What it does

We define chemistry as when one player's strengths on a team match up with the strengths of their teammates.  This means that if a player is effective at completing a certain task on the pitch, which in our case is receiving the ball at a certain location, their teammates are effective "playing off" of these tasks, again in our case meaning passing the pall into those locations.  Therefore, our metric quantifies team chemistry for any team based on the level of locational alignment between each player's passing strengths to certain zones of the pitch and the strengths of their teammates when receiving the ball in these zones.

This tool can be especially useful for both team-building and better understanding past game results.  In terms of team-building, maximizing chemistry can ensure that a team is getting the optimal level of production out of all of its players.  By utilizing our metric, managers and others who must make roster decisions can use our statistic in order to determine which players fit best with the already-existing chemistry of their club.  Second, this metric can be used to better understand past game results. This metric can explain some of the deviation between expected and actual outcomes. Currently, expected outcomes do not factor into team chemistry, at least not explicitly. By quantifying team chemistry, we can start to explain our puzzlement when the talented favorite falls to the scrappy underdog.

## How I built it

We began by creating a goal probability model as the baseline for our analysis.  Using the 2016-17 MLS soccer season data as our training set, we created a logistic regression model that took location on the pitch (broken down into 400 zones of 5 units by 5 units), speed of advancement of the ball up the pitch (based on time elapsed of prior touches), and general event type as inputs and output a predicted goal probability given those same conditions from 0 (a goal is impossible) to 1 (a goal is certain).

We then applied this logistic regression goal probability model to the 2017-18 MLS soccer season data, generating a goal probability for every pass and shot.  Then, looking at sequential events, we calculated the goal-probability-added of each pass or shot.  Goal-probability-added and goal probability would be the two most essential statistics in our analysis of calculating team chemistry.

Our approach for calculating team chemistry was to measure the overlap of each player's ball receiving strengths with their teammates' passing strengths. We took a location-based approach, this time breaking the field up into 25 sections (10 units by 10 units). 

First, for each player and for each zone in the 2017-18 MLS season, we calculated the sum of goal probability experienced by the player upon receiving the ball in the zone. We then calculated a z-score for their goal probability generated when receiving a ball in each of the 25 zones - comparing the player's goal-probability when receiving the ball in a specific zone to that of all other players in the data set in that specific zone.

Next, we found the sum of the goal-probabilities added when a pass occurred to each of the 25 zones on the field.  However, we did so multiple times for each team, as we had to eliminate each player for each zone one at a time.  This is because when comparing a team's goal probability-added when passing to a zone to a specific player's goal probability from that zone, it doesn't make sense to include the player in both, as he cannot pass the ball to himself.  Then, we looked at the goal probabilities-added for each team for passes to each of the 25 zones,  and computed z-scores for each of the 25 zones for each team to capture their differences from the league averages.

In order to quantify chemistry, we want to see how close a player's goal probability z-score when receiving a ball in a specific zone is in value to the team's goal probability-added z-score when passing into that zone.  The rationale behind this is that in order for a player to fit well in a team, he should produce above-average when receiving the ball in zones where his team requires it (because his team does well in increasing goal probability when passing the ball into those zones), and produces below-average when receiving the ball in zones where his team does not need average or above-average performance (because his team is not good at increasing goal probability when passing the ball into those zones).  Therefore, we look at the difference between the two z-scores for each player in a zone, then create a weighted average by multiplying each player's difference by the number of passes or shots he attempted from those zones.  Next, we add up all of the player's weighted z-score differences for each zone and divide them by the total touches (passes and shots) by the team.  And finally, we add up the values for each of the 25 zones.  

This process gives us a chemistry rating for a specific team, measuring how well the strengths of each teammate fit into the strengths of his team.


## Challenges I ran into

We ran into two significant challenges in the process of creating our metric.

First, we faced many nuanced forks in the road in terms of how to approach our model that often took lengthy conversations in order to not only make a decision, but to even comprehend the decision that needed to be made.  For example, when putting this model together, one of us originally thought the optimal model would be based on a team's ability to get the ball to a player's receiving "hot zone", thus making our metric one that measures how the team fits into the player's playing style (then iterating over all players on a team, of course).  On the other hand, the other one of us thought the superior model was one based on a player's ability to pass the ball to where his team can receive it effectively and make a maximum impact, thus measuring the player's individual passing "hot zone", and therefore measuring instead how the player fits into the chemistry of the team as a whole.  Subtle differences like these took a while for us to tackle.

Second, we had trouble deciphering when it made sense to weight our averages based on sample size, and due to this, we lost track of the meaning of some of the statistics we were using in order to calculate team chemistry in our first model.  We thought it was evident that some sort of weighting would be necessary in the creation of our chemistry metric, as locations on the field that were more frequently passed to by a player and zones that teams more frequently receive the ball in deserve higher ratings in order to prevent small sample sizes from overpowering the model.  However, due to the many stages of our analysis, first looking at individual players on both the passing and receiving ends, then combining all players on the receiving end to get an overall goal probability for the team in all 25 zones, then comparing the team passing goal-probability-added statistics to the player's goal probabilities on the matching zones of the field, it quickly became unclear where this weighting should take place. 


## Accomplishments that I'm proud of

Above all, we are proud that we came up with a unique, original idea in measuring chemistry, and were able to follow through with an analysis of a potential chemistry statistic.  Regardless of the chemistry metric's accuracy, we believe that we have accomplished something significant in pursuing the creation of a statistic of this type that can help to shed light on the inner workings of soccer team dynamics for years to come, whether it be by our model or a model that we help to inspire down the road.

At the start of the Hackathon, we believed that taking on an intangible concept such as team chemistry and attempting to quantify it was very ambitious.  However, after discussing the idea of chemistry and what makes a team's chemistry strong for a while, we were able to come up with a game plan, one that we were able to execute.  Unfortunately, the data did not trend in the direction we anticipated for the MLS data (more about this discussed in final section), but we are very proud of the work we have done at this Hackathon, and the model we have created, and are hopeful that the judges see the promise in a model such as this one that quantifies team chemistry.

## What I learned

We learned how much an idea develops over the course of a project.  At the start of the Hackathon, once we had it settled that we wanted to create a metric to measure team chemistry, we mapped out our approach to answering this analytics question, and genuinely believed we had it all planned out.  However, over the course of the Hackathon, we ran into several nuanced questions that truly challenged our knowledge pertaining to the question we wanted to answer, and exactly how we sought out to answer it.  Simply saying we wanted to create a team chemistry metric is one thing, but actually carrying in out in one of the myriad of possible ways is another, and over the course of the Hackathon, we constantly shifted our approach to answering the question, eventually to a reasonable and effective concluding result.

## What's next for Quantifying Team Chemistry

When testing our chemistry metric model on the 2017-18 MLS data, the results were the inverse of what we expected - the teams that performed better than expected in terms of points above projection were actually found to have worse chemistry, and this relationship was to a significant degree (p < 0.05).  However, after analyzing player talent disparity in the MLS compared to the World Cup and other leagues such as the English Premier league, all arrows point toward the MLS being an All-Star talent-based soccer league, where talent disparity is large, and having some of the best players will win you points regardless of chemistry.  In fact, in the 2017-18 season, the three MLS soccer clubs that performed the best compared to their preseason projection (Toronto FC, Chicago Fire, Atlanta United FC) each had at least 3 of the 24 MLS All-Stars, and 58% of MLS All-Stars in the 2017-18 season played on teams that ranked in the bottom 31% of the MLS in chemistry, according to our metric.  Therefore, moving forward, we would like to have the opportunity to test our model on other leagues with a smaller divide between good players and decent players.

Additionally, we would want to look into how much value having a "better-fit" player brings, and thus how much more clubs should be willing to pay for a player who is a better fit.
