.. pydata-tutorial documentation master file, created by
   sphinx-quickstart on Mon Nov 12 19:41:02 2018.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

PyDataDC Recommender System Workshop
=====================================

The recommender system is a classic application of machine learning that aims to predict which item a user will like best. Personalized recommendations play an integral role for e-commerce platforms, with the goal of driving user engagement through item recommendations.

In this workshop, we will build two types of recommender systems using the `MovieLens dataset <https://grouplens.org/datasets/movielens/>`_: 

1. an item-item recommender using k Nearest Neighbors (kNN) and cosine similarity
2. a top N recommender using matrix factorization

We will also cover the following topics on recommendations:

- collaborative vs. content-based filtering
- implicit vs. explicit feedback
- handling the cold start problem
- evaluation metrics

By the end of this workshop, you will have a better understanding of the different techniques and tools used to build recommendation systems in real-life scenarios.


See slides for this workshop `here <https://topspinj.github.io/presentations/recommender-tutorial-pydatadc.pdf/>`_.

Environment Setup Instructions:
-------------------------------
- Option 1: Running Jupyter notebook locally
- Option 2: Running Jupyter notebook via Google Colab

.. toctree::
   :maxdepth: 2
   :caption: Recommender Basics:

   recommender_basics.md

.. toctree::
   :maxdepth: 2
   :caption: Workshop Material:

   ../tutorial.ipynb

