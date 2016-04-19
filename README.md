#2014 summer project, Sewanee CS Department

###Model
--------
The model is developed by Luis Torgo in his book (Data Mining with R), which loads data of past prices and technical indicators and attempt to predict future prices. Data is divided into training (1970 - 99) and testing set (2000 - 09). Different variables are ranked by variable ranking by random forest and loaded into a feed forward single hidden layer neural network to find the underlying representation function (if any exists). The basic code is provided in the website http://www.dcc.fc.up.pt/~ltorgo/DataMiningWithR/code3.html.


###File structure
Change: I added a number of new features from Quandl, attempted different variable ranking systems (information gain and correlation), optimized parameters of the feed forward net and compared the efficiency of different choices from the variable pool.  
