# Building a Recommender System from Scratch: Workshop Material for PyDataDC 2018

The recommendation system is a classic application of machine learning that aims to predict which item a user will like best. Personalized recommendations play an integral role for e-commerce platforms, with the goal of driving user engagement through item recommendations.

In this workshop, we will build two types of recommendation systems using data from the [MovieLens dataset](https://grouplens.org/datasets/movielens/):

1. an item-item recommender using k Nearest Neighbors (kNN) and cosine similarity
2. a top N recommender using matrix factorization

We will also cover the following topics on recommendations:

- collaborative vs. content-based filtering
- implicit vs. explicit feedback
- handling the cold start problem
- evaluation metrics 

By the end of this workshop, you will have a better understanding of the different techniques and tools used to build recommendation systems in real-life scenarios.

### Requirements

- Python 3+
- pandas
- numpy
- scipy
- matplotlib
- seaborn
- scikit-learn

You will need to have (1) jupyter installed on your local machine, or (2) a gmail account to access [Google Colab](https://colab.research.google.com), which allows you to run jupyter notebooks in the cloud.

### Material

- [Documentation](https://topspinj.github.io/pydata-workshop)
- [Notebook](tutorial.ipynb)
- [Slides](recommender-slides.pdf)
