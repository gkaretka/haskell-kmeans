import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
from matplotlib.colors import ListedColormap

# data d_X_features
res_df = pd.read_csv('./out/cluster_info.csv', delimiter=',')

# cluster centroids
res_df_centroids = pd.read_csv('./out/cluster_position.csv', delimiter=',')

# sum of squares depending on number of clusters
sum_of_sqr_to_n_of_clusters = pd.read_csv('./out/sum_of_square_for_diffent_k.csv', delimiter=',')

MARKER_SIZE = 30

# plot sum of sqrt depending on number of clusters
figSums, ax = plt.subplots(figsize=(15,7))
ax.scatter(sum_of_sqr_to_n_of_clusters['k'], sum_of_sqr_to_n_of_clusters['sumSqrt'], s=MARKER_SIZE)

fig = plt.figure(figsize=(15, 7))
# 2 features + 1 clustering data
if res_df.shape[1] == 2+1:
    # for 2D
    sns.scatterplot(data=res_df, x=res_df.columns[0], y=res_df.columns[1], 
                    hue=res_df.columns[2], palette=sns.color_palette(), s=MARKER_SIZE)

    sns.scatterplot(data=res_df_centroids, x='x', y='y', color='red', s=5*MARKER_SIZE)
    plt.grid(False)
    plt.title('Clusters of customers')
    plt.xlabel('Annual Income (k$)')
    plt.ylabel('Spending Score (1-100)')
    plt.axis('scaled')
    plt.legend()
    plt.show()
    # end 2D
else:
    # for 3D
    ax = fig.add_subplot(projection='3d')

    fig.add_axes(ax)
    cmap = ListedColormap(sns.color_palette("viridis", 256).as_hex())

    sc = ax.scatter(res_df[res_df.columns[0]],
            res_df[res_df.columns[1]],
            res_df[res_df.columns[2]],
            c=res_df[res_df.columns[3]],
            s=MARKER_SIZE,
            marker='o',
            cmap=cmap,
            alpha=1)

    sc = ax.scatter(res_df_centroids['x'],
            res_df_centroids['y'],
            res_df_centroids['z'],
            c='red',
            s=5*MARKER_SIZE,
            marker='o',
            alpha=1)

    plt.legend(*sc.legend_elements(), bbox_to_anchor=(1.05, 1), loc=2)
    plt.axis('scaled')
    plt.show()
    # end 3D
