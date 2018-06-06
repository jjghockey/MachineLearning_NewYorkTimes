# NYT-Recommendation-ML-Model
Machine Learning approach to predicting comment recommendations based on New York Times articles and comments made on those articles.

001a_mk_data.r - Initial data construction for articles and comments
001b_an_data.r - Exploratory data analysis
002_an_model.r - Initial baseline modeling (GLM, GBM, RF)
003_an_model.r - Initial baseline modeling (NN, AutoML)
004_an_model.r - Final Modeling (GBM, RF)
005_an_model.r - Average ensemble (All Models)
006_an_lime.r - Lime exploration of the GBM model

Disclaimer

This code was run on a server with 8 cores and 64 GB of memory.   h2o was used to develop and run all models.  

The data used for this modeling is NOT stored on github due to size. It can be found here: https://www.kaggle.com/aashita/nyt-comments

Therefore, the pathways are set to run as-is. There is no guarantee that the models will run out of the box on another system.
