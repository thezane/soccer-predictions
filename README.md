# soccer-predictions
![alt-tag](https://github.com/thezane/soccer-predictions/blob/master/forecasts/June28.png)

This is a statistical forecasting model for predicting game outcomes in soccer tournaments (World Cup, Euro Cup and Copa America).   The model is named Sequential Offense-Defense (ODM-S) after the fact that it rates teams by attack and defense strengths match-by-match.  It is based on the [original Offense-Defense Model](http://meyer.math.ncsu.edu/Meyer/Talks/OD_RankingCharleston.pdf) by mathematicians Anjela Govan, Amy Langville and Carl Meyer.   We discuss how it works and how predictions should be interpreted.  Its accuracy and inner workings are in a [technical readme](https://github.com/thezane/soccer-predictions/blob/master/technicalReadme.pdf).

###How It Works###

#####Step 1: Rate Teams#####

ODM-S starts by rating teams by attack and defense strengths.  Goals scored are a measure of attack strength and goals conceded are a measure of defense strength.  Ratings are updated match-by-match.  Usually, a team’s ratings increases after winning and decreases after losing but not always because strength of schedule and home advantage are considered.  When a highly-rated team ekes a 4-3 win against a weakly-rated team at home, its ratings decrease while its opponent’s ratings increase.  To make the ratings reflective of how teams play with their best players, the model only rates teams in matches where those players are likely to show up, such as tournament qualifiers and tournaments themselves.  It also considers recency of previous matches.  Matches played two days ago have a greater effect on a team’s ratings than those played two years ago.

#####Step 2: Determine Match Outcome#####

Once teams are rated, ODM-S calculates the chance that a team wins, draws or loses against an opposing team.  To demonstrate how this works, imagine a game between Germany and Ukraine.  The model calculates the chance that the game ends with a scoreline of 0-0, 0-1, 1-0, etc.  This requires an appropriate probability distribution.  Our choice is a bivariate Poisson distribution, which predicts the frequency of two rare events over a fixed time interval.  In our case, it is the goals scored by each team in a game.

However, our chosen distribution requires us to estimate the expected goals scored by each team.  This is where attack and defense ratings
come in.  The more goals that Germany scores in previous games, the higher its attack ratings; the more goals conceded by Ukraine in its
previous games, the lower its defense ratings.  Thus, the expected goals of Germany is porportional to its attack rating minus Ukraine’s
defense rating.  Likewise for the expected goals of Ukraine.

After determining the chance that the game ends with a certain scoreline, ODM-S sums probabilities of scorelines that end with win, tie or loss for Germany to get a prediction of it winning, drawing or losing against Ukraine.

###How To Interpret Predictions###

ODM-S is a useful model because it performs well against the Elo Ratings, bettings markets and other statistical models.  Also, it gives a good sense of attack and defense strengths of each team.  However, one should not rely on it too much.  Like all soccer statistical models, it cannot accurately account for missing players and requires teams to play many games over a short period to perform at its best.
