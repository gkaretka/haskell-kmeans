import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
from matplotlib.colors import ListedColormap

# data d_X_features
res_df = pd.read_csv('./out/cluster_info.csv', delimiter=',')

# cluster centroids
red_df_centroids = pd.read_csv('./out/cluster_position.csv', delimiter=',')

MARKER_SIZE = 100

fig = plt.figure(figsize=(15, 7))

# for 2D
# sns.scatterplot(data=res_df, x='Age', y='Annual_Income_(k$)', hue='ClusterID', palette=sns.color_palette(), s=MARKER_SIZE)
# sns.scatterplot(data=red_df_centroids, x='x', y='y', color='red', s=5*MARKER_SIZE)
# plt.grid(False)
# plt.title('Clusters of customers')
# plt.xlabel('Annual Income (k$)')
# plt.ylabel('Spending Score (1-100)')
# plt.axis('scaled')
# plt.legend()
# plt.show()
# end 2D

# for 3D
ax = fig.add_subplot(projection='3d')
fig.add_axes(ax)
cmap = ListedColormap(sns.color_palette("viridis", 256).as_hex())

sc = ax.scatter(res_df['Age'],
           res_df['Annual_Income_(k$)'],
           res_df['Spending_Score'],
           c=res_df['ClusterID'],
           s=50,
           marker='o',
           cmap=cmap,
           alpha=1)

sc = ax.scatter(red_df_centroids['x'],
           red_df_centroids['y'],
           red_df_centroids['z'],
           c='red',
           s=100,
           marker='o',
           alpha=1)

plt.legend(*sc.legend_elements(), bbox_to_anchor=(1.05, 1), loc=2)
plt.show()
# end 3D
