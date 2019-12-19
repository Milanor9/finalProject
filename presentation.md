Final project presentation
========================================================
author: Charles Murier & John NGO
date: 17-12-2019
autosize: true

Shiny app : https://charlesmurier.shinyapps.io/shiny-r/ <br/>
R presentation : http://rpubs.com/charlesmurier/finalProject <br/>
Repo Github : https://github.com/Milanor9/finalProject



The side panel for trains
========================================================

![plot of chunk unnamed-chunk-1](train-ui.jpg)
<font size="5"> 

- We can choose the value aggregated between year and station
- If the value aggregated is year we can choose to see for only one station or for all stations
- Then we can choose the Y variable so we can see the graphic that we want

</font> 

Examples of plots by year for all the stations
========================================================




<img src="presentation-figure/unnamed-chunk-3-1.png" title="plot of chunk unnamed-chunk-3" alt="plot of chunk unnamed-chunk-3" width="100%" />
<font size="5"> 
- We choose to plot a line when the graphic is shown by year because it's easier to see the evolution
</font>

Examples of plots by year for Paris Est station
========================================================

<img src="presentation-figure/unnamed-chunk-4-1.png" title="plot of chunk unnamed-chunk-4" alt="plot of chunk unnamed-chunk-4" width="100%" />

Example : number of trains late at departure by departure station 
========================================================
<img src="presentation-figure/unnamed-chunk-5-1.png" title="plot of chunk unnamed-chunk-5" alt="plot of chunk unnamed-chunk-5" width="100%" />



The side panel for flights
========================================================

<img src="ui.jpg" title="plot of chunk unnamed-chunk-6" alt="plot of chunk unnamed-chunk-6" width="50%" />
<font size="5"> 

- We can choose the value aggregated between airline and origin airport
- If the value aggregated is airline we can choose to see for only one airport or for all airports
- Then we can choose the Y variable so we can see the graphic that we want
- If we click on map, the map representing the airports is displayed

</font> 


Example: Average time in the air by airlines for all the airports
========================================================

<img src="presentation-figure/unnamed-chunk-7-1.png" title="plot of chunk unnamed-chunk-7" alt="plot of chunk unnamed-chunk-7" width="100%" />
<font size="5"> 

- This time we chose barplot because there is no evolution, it's only a comparison between the airlines

</font> 

Example: Average time in the air by airlines for LAS VEGAS airport
========================================================

<img src="presentation-figure/unnamed-chunk-8-1.png" title="plot of chunk unnamed-chunk-8" alt="plot of chunk unnamed-chunk-8" width="100%" />

Example: Total distance of flights by airport
========================================================

<img src="presentation-figure/unnamed-chunk-9-1.png" title="plot of chunk unnamed-chunk-9" alt="plot of chunk unnamed-chunk-9" width="100%" />

How the map is displayed
========================================================

![plot of chunk unnamed-chunk-10](all_airport.JPG)
- When "all airports" is selected, the positions of all the airports are visible on the map

![plot of chunk unnamed-chunk-11](las-airport.jpg)
- When one specific airport is selected, the position of this specific airport is visible on the map


The map displayed when a specific airport is selected (1/2)
========================================================

![plot of chunk unnamed-chunk-12](las-airport.jpg)![plot of chunk unnamed-chunk-12](las.jpg)

The map displayed when a specific airport is selected (2/2)
========================================================

![plot of chunk unnamed-chunk-13](las2.jpg)

The map displayed when all airports are selected
========================================================

![plot of chunk unnamed-chunk-14](all_airport.JPG)![plot of chunk unnamed-chunk-14](capture.jpg)

