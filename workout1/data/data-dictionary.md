data-dictionary
================
Raymond Chang
3/11/2019

1.  Title of Databases: andre-iguodala.csv, draymond-green.csv, kevin-durant.csv, klay-thompson.csv, stephen-curry.csv

2.  Sources: from <https://github.com/ucb-stat133/stat133-hws/tree/master/data>

3.  No past usages.

4.  Attribute Information:

| Name               | Data Type      | Measurement | Description                                        |
|--------------------|----------------|-------------|----------------------------------------------------|
| team\_name         | nominal        | NA          | team name of basketball team that is playing       |
| game\_date         | month/day/year | NA          | date of when the game was played                   |
| season             | integer        | NA          | season of the basketball game                      |
| period             | integer        | NA          | the i-th 12 minutes of the game                    |
| minutes\_remaining | integer        | minutes     | minutes remaining to be played                     |
| seconds\_remaining | integer        | seconds     | seconds remaining to be played                     |
| shot\_made\_flag   | nominal        | NA          | y if shot was made, n if not                       |
| action\_type       | nominal        | NA          | basketball moves made by player                    |
| shot\_type         | nominal        | NA          | indicates 2 or 3 point field goal                  |
| shot\_distance     | integer        | feet        | distance to basket                                 |
| opponent           | nominal        | NA          | opponent team                                      |
| x                  | integers       | inches      | court coordinates of where shot occurred on x axis |
| y                  | integers       | inches      | court coordinates of where shot occurred on y axis |
