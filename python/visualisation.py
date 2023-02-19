import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

res_df = pd.read_csv('./out/data.csv', delimiter=',')
res_df_centroids = pd.read_csv('./out/cents.csv', delimiter=',')

MARKER_SIZE = 100

plt.figure(figsize=(15, 7))
sns.scatterplot(data=res_df, x='c0', y='c1', hue='c3', palette=sns.color_palette(), s=MARKER_SIZE)
sns.scatterplot(data=res_df_centroids, x='x', y='y', color='red', s=5*MARKER_SIZE)

plt.grid(False)
plt.title('Clusters of customers')
plt.xlabel('Annual Income (k$)')
plt.ylabel('Spending Score (1-100)')
plt.legend()
plt.show()
