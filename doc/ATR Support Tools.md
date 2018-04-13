1. Delay
2. Passenger Km (Passenger X km)
3. Variance of Headway(Corresponds to waiting time)

Available Data
1. OnlineTimetable
2. Passenger Load
3. Intertation Stops record(when, where, how many etc.)

The following points are associated with online function of Reports and Graphs

S. No.|Requirement|Comment
------|------|--------
1.|Support for Excel and other formats|Achieved easily, because our output is a Diagram
2.|Statistical reports|Easily derivable from corresponding graphs, just the same data displayed in a different format
3.|Punctuality reports|Different kinds of delay records:<ul><li>Arrival Delay (per train/averaged on trains) for the required time period</li><li> Departure Delay (per train/averaged on trains) for the required time period [NOT NEEDED - One station's departure delay is next station's arrival delay]</li><li> Interstation Stops Graph (per train, averaged on trains)</li></ul>
4.|Unusual Occurences|DATA NOT AVAILABLE
5.|Crew Link and Rake Utilisation|DATA NOT AVAILABLE
6.|Blocks overview(Blocks granted/refused)|DATA NOT AVAILABLE
7.|Sectional Running Times|(Per train/Averaged on trains) Per train might not be significant, averaged makes more sense
8.|Daily report of trains|ALREADY COVERED in Point 3
9.|Department wise booking of trains delays|What is department? Might be covered in Point 3
10.|Difference between actual and scheduled running times|Actual covered in Point 7, just need to display scheduled together with scheduled for comparison
11.|Adding remarks|May be difficult to implement, will require some feature mirroring comments
12.|Performance Analysis, Incident Evaluation|Performance Analysis already covered in Points 3 & 7, Incident Evaluation Data Not Available
13.|Train performance graph, actual performance vs timetabled performance|Already covered in Points 7 and 10
14.|Comparison of actual departure vs timetable|might be covered by Point 3, sub-point 2
15.|Headway Interval Graph – headway interval in seconds versus time in minutes|Doesn't seem possible, headway interval cannot be plotted against time, since it requires two departure times to calculate one headway interval<br/>Alternate - Plot average headway interval for the entire day for each station.<br/>Also since headway interval basically corresponds to waiting time, this might be already covered in Point 3
16.|Inter-station stopping chart|Already Covered in Point 3
17.|The capability to produce online and offline graphical analysis|Automatically provided, since offline and online timetable formats are the same
18.|The capability to produce the train graph for a particular train along with the ATP commands/signals it has encountered|What is Train Graph? Might already be covered above. Also, ATP commands are available
19.|The capability to give complete information about a selected train viz. Train number, crew information, rake details, etc.|Already available from Line Overview Config
20.|The capability to produce interactive train graphs/time table in off line mode where it will be possible to alter the running times of any train interactively to see the effect instantly, simulate and observe the effect graphically of various parameters like speed restrictions, dwell time, coasting etc on headway|Already available in Timetable Monitoring application.
21.|TDG printing|Already in talks with Mr. Rupesh

