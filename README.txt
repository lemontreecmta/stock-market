Code for my summer research at Sewanee in 2014

*Disclaimer* This model is developed by Luis Torgo in his book (Data Mining with R), which loads data of past prices and technical indicators and attempt to predict future prices. Data is divided into training (1970 - 99) and testing set (2000 - 09). Different variables are ranked by variable ranking by random forest and loaded into a feed forward single hidden layer neural network to find the underlying representation function (if any exists).

Change: I added a number of new features from Quandl, attempted different variable ranking systems (information gain and correlation), optimized parameters of the feed forward net and compared the efficiency of different choices from the variable pool.  