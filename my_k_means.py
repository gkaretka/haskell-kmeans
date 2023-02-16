"""My implementation of k-means in python
"""

import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt


def eu_dist(X, pts, Ks):
    """Returns eu_dist from Ks pts

    Args:
        X (_type_): data
        pts (_type_): centroids
        Ks (_type_): number of centroids

    Returns:
        _type_: n x Ks array of dist from centroids
    """
    diff = np.repeat(X, Ks, axis=1) - np.reshape(pts, newshape=(1,4), order='F')
    diff = np.reshape(diff, newshape=(400, 2), order='F')
    norms = np.linalg.norm(diff, axis=1)
    return np.reshape(norms, newshape=(200, 2), order='F')


df = pd.read_csv('./data/data.csv')
df.drop_duplicates(inplace=True)

# using only Spending_Score and income variable for easy visualisation
X = df.iloc[:, [2, 3]].values
K = 2

start_points = [X[15], X[16]]
cur_clusters = []
prev_clusters = []

n = 100
while n > 0:
    eucl_dist = eu_dist(X, start_points, K)
    closest_cluster_map = np.argmin(eucl_dist, axis=1)

    # for every cluster
    prev_clusters = cur_clusters.copy()
    cur_clusters = []
    start_points = []
    for i in range(K):
        cur_clusters.append(np.where(closest_cluster_map == i))
        cur_clust_points = eucl_dist[cur_clusters[i]]
        cur_clust_centroids = np.mean(cur_clust_points.T, axis=1)
        start_points.append(cur_clust_centroids)

    print(f"Iter: {n}: {start_points}")
    n -= 1

result = None
for idx, cluster in enumerate(cur_clusters):
    cluster_labeles = np.ones(cluster[0].shape[0]) * idx

    data = np.c_[cluster[0], cluster_labeles]
    if result is None:
        result = data
    else:
        result = np.r_[result, data]

# sort by fisrt column
sorted_result = result[result[:, 0].argsort()]
#print(sorted_result)

# take second column and append it to original data
resulting_data = np.c_[X[:], sorted_result[:]]

# display
MARKER_SIZE = 100
res_df = pd.DataFrame(data=resulting_data, columns=['c0', 'c1', 'c2', 'c3'])
print(res_df.head())

plt.figure(figsize=(15, 7))
sns.scatterplot(data=res_df[res_df['c3'] == 1], x='c0', y='c1',
                color = 'red', label = 'Cluster 1',s=MARKER_SIZE)
sns.scatterplot(data=res_df[res_df['c3'] == 0], x='c0', y='c1',
                color = 'blue', label = 'Cluster 2',s=MARKER_SIZE)

plt.grid(False)
plt.title('Clusters of customers')
plt.xlabel('Annual Income (k$)')
plt.ylabel('Spending Score (1-100)')
plt.legend()
plt.show()
