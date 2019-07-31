I took my data set from here: https://archive.ics.uci.edu/ml/datasets/YouTube+Spam+Collection

Those documents contained different youtube comments for five singers and then those comments were labeled as spam or not spam. 

**1.	Distance function**

I implemented cosine distance function and then compared it with Euclidean distance function. Cosine similarity looks at the angle between the vectors and
does not care about the weight or magnitude of the vectors.  
I compared documents as follows: took one document and calculated distance(cosine and Euclidean) to every other document and then plotted it to the graph. 
Cosine was more accurate because Euclidean did not take into account the fact that documents(text data case) can be similar even if the value in the same dimension differs a lot. 
That could happen for example if one document is longer than the other etc.  

**2.	Clustering**

To visualize centroids, I had to implement PCA. Which is dimensionality reduction method. It sorts eigenvalues in descending order so it is possible to choose dimensionality
count that is wanted. The bigger the eigenvalue the bigger the variance in this dimension. And that means that this dimension is the best describing this 
data set(PCA makes new dimensions). In my code, I chose  2 best eigenvectors and then plotted points and centroids. I tried with different artists. 
For some, clusters were very well discriminated but for others not so well. I think that the reason were that for some artist the spam comments and no spam comments 
contained many same words. 
Tried it with Euclidian and cosine function and I did not see very big performance difference. 

**3.	Decision boundary**

I used the same PCA code that I wrote for clustering and took again two best dimensions. 
For plotting I used library that makes plane into a grid.
And points inside grid were colored according to their nearest data points labels. 
For decision boundary I plotted new points with third color. I looped through every point inside the grid and if any of its neighbours were from different class I drew a point. 
