## What is a recommender system?

A recommender system is an application of machine learning that predicts  future behavior based on our historical preferences. In the context of e-commerce, a recommender will predict a person's preferences toward a set of items, which effectively filters ou the most relevant items for that particular user.

**Examples of recommender include:**

- recommending products based on past purchases or product searches (Amazon)
- suggesting TV shows or movies based on prediction of a user's interests (Netflix)
- creating personalized playlists based on song listening history (Spotify)
- personalized ads based on "liked" posts or previous websites visited (Facebook)

The two most common recommender system techniques are: 1) collaborative filtering, and 2) content-based filtering.

### Collaborative Filtering

Collaborative filering (CF) is based on the concept of `homophily` - similar people like similar things. It predicts which item a user will like best based on the item preferences of other similar users. Collaborative filtering uses a user-item (utility) matrix to generate recommendations. This matrix is populated with values that indicate a user's degree of preference towards a given item. These values can represent either explicit feedback (direct user ratings) or implicit feedback (indirect user behaviour such as listening, purchasing, watching). It's extremely unlikely that a user will have interacted with every item in the matrix. In most cases, the user-item matrix is very sparse.

Having a very sparse and high-dimensional matrix can lead to poor results. To overcome this problem, you can convert your original user-item matrix to a lower-dimensionality space using dimensionality reduction techniques such as Singular Value Decomposition (SVD) and Alternating Least Squares (ALS). These techniques work by factorizing the original user-item matrix into two factor matrices:

- user-factor matrix
- item-factor matrix

These factor matrices have latent features that represent the underlying interactions between users and items. Though we are not able to interpret what a latent feature represents, we can imagine that one latent feature may explain for users who like romance comedies from the 1990s, while another latent feature may symbolize independent foreign language films. 


### Content-based Filtering

A major disadvantage of collaborative filtering is the **cold-start problem**. You can only get recommendations for users and items that already have "interactions" in the user-item matrix. Collaborative filtering fails to provide personalized recommendations for brand new users or newly released items.

Content-based filtering handles the cold-start problem because it can generate recommendations for users based on their features and the features of the items. Given a set of item features (movie genre, release date, country, language, etc.), it predicts how a user will rate an item based on their ratings of previous movies. 

