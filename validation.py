"""Validation for Haskell implementation of k-means
based on: https://www.kaggle.com/code/shrutimechlearn/step-by-step-kmeans-explained-in-detail/notebook
"""

from sklearn.cluster import KMeans
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

df = pd.read_csv('./data/data.csv')
df.drop_duplicates(inplace=True)

# using only Spending_Score and income variable for easy visualisation
X = df.iloc[:, [2, 3]].values

K = 5

kmeans = KMeans(n_clusters = K, init = 'k-means++', random_state = 42)
y_kmeans = kmeans.fit_predict(X)

# Visualising the clusters
MARKER_SIZE = 100

plt.figure(figsize=(15,7))
sns.scatterplot(x=X[y_kmeans == 0, 0], y=X[y_kmeans == 0, 1],
                color = 'yellow', label = 'Cluster 1',s=MARKER_SIZE)
sns.scatterplot(x=X[y_kmeans == 1, 0], y=X[y_kmeans == 1, 1],
                color = 'blue', label = 'Cluster 2',s=MARKER_SIZE)
sns.scatterplot(x=X[y_kmeans == 2, 0], y=X[y_kmeans == 2, 1],
                color = 'green', label = 'Cluster 3',s=MARKER_SIZE)
sns.scatterplot(x=X[y_kmeans == 3, 0], y=X[y_kmeans == 3, 1],
                color = 'grey', label = 'Cluster 4',s=MARKER_SIZE)
sns.scatterplot(x=X[y_kmeans == 4, 0], y=X[y_kmeans == 4, 1],
                color = 'orange', label = 'Cluster 5',s=MARKER_SIZE)
sns.scatterplot(x=kmeans.cluster_centers_[:, 0], y=kmeans.cluster_centers_[:, 1],
                color = 'red', label = 'Centroids',s=5*MARKER_SIZE,marker=',')

plt.grid(False)
plt.title('Clusters of customers')
plt.xlabel('Annual Income (k$)')
plt.ylabel('Spending Score (1-100)')
plt.legend()
plt.show()
