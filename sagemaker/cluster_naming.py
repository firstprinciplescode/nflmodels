# ============================================================================
# CLUSTER AUTO-NAMING FUNCTIONS
# ============================================================================
# 
# These functions take centroid DataFrames and return a dict mapping
# cluster number -> name based on the established naming conventions.
#
# Usage:
#   names = name_route_clusters(centroids_df)
#   df['cluster_name'] = df['cluster'].map(names)
#
# ============================================================================

import pandas as pd
import numpy as np


def name_alignment_clusters(centroids_df):
    """
    Auto-name alignment clusters based on centroid values.
    
    Rules (in order):
    - highest inline_rate → ITE
    - highest slot_rate → SWR  
    - highest behind_rate → RB
    - highest wide_rate → WWR
    - highest wide + slot combo (not already SWR/WWR) → WSWR
    - highest slot + inline combo (not already ITE) → STE
    
    Args:
        centroids_df: DataFrame with columns [cluster, wide_rate, slot_rate, inline_rate, behind_rate]
    
    Returns:
        dict: {cluster_number: name}
    """
    df = centroids_df.copy()
    names = {}
    assigned = set()
    
    # 1. Highest inline_rate → ITE
    remaining = df[~df['cluster'].isin(assigned)]
    idx = remaining['inline_rate'].idxmax()
    cluster = remaining.loc[idx, 'cluster']
    names[cluster] = "ITE"
    assigned.add(cluster)
    
    # 2. Highest slot_rate → SWR
    remaining = df[~df['cluster'].isin(assigned)]
    idx = remaining['slot_rate'].idxmax()
    cluster = remaining.loc[idx, 'cluster']
    names[cluster] = "SWR"
    assigned.add(cluster)
    
    # 3. Highest behind_rate → RB
    remaining = df[~df['cluster'].isin(assigned)]
    idx = remaining['behind_rate'].idxmax()
    cluster = remaining.loc[idx, 'cluster']
    names[cluster] = "RB"
    assigned.add(cluster)
    
    # 4. Highest wide_rate → WWR
    remaining = df[~df['cluster'].isin(assigned)]
    idx = remaining['wide_rate'].idxmax()
    cluster = remaining.loc[idx, 'cluster']
    names[cluster] = "WWR"
    assigned.add(cluster)
    
    # 5. Highest wide + slot combo → WSWR
    remaining = df[~df['cluster'].isin(assigned)]
    if len(remaining) > 0:
        remaining = remaining.copy()
        remaining['wide_slot'] = remaining['wide_rate'] + remaining['slot_rate']
        idx = remaining['wide_slot'].idxmax()
        cluster = remaining.loc[idx, 'cluster']
        names[cluster] = "WSWR"
        assigned.add(cluster)
    
    # 6. Highest slot + inline combo → STE
    remaining = df[~df['cluster'].isin(assigned)]
    if len(remaining) > 0:
        remaining = remaining.copy()
        remaining['slot_inline'] = remaining['slot_rate'] + remaining['inline_rate']
        idx = remaining['slot_inline'].idxmax()
        cluster = remaining.loc[idx, 'cluster']
        names[cluster] = "STE"
        assigned.add(cluster)
    
    return names


def name_route_clusters(centroids_df):
    """
    Auto-name route clusters based on centroid values.
    
    Rules (in order):
    - highest deep_rte_rate → DT
    - highest medium_rte_rate → MT
    - highest short_rte_rate → ST
    - highest behind_los_rte_rate → BT
    - highest short + medium combo → SMT
    - highest behind + short combo → RB
    
    Args:
        centroids_df: DataFrame with columns [cluster, deep_rte_rate, medium_rte_rate, 
                                              short_rte_rate, behind_los_rte_rate]
    
    Returns:
        dict: {cluster_number: name}
    """
    df = centroids_df.copy()
    names = {}
    assigned = set()
    
    # 1. Highest deep → DT
    remaining = df[~df['cluster'].isin(assigned)]
    idx = remaining['deep_rte_rate'].idxmax()
    cluster = remaining.loc[idx, 'cluster']
    names[cluster] = "DT"
    assigned.add(cluster)
    
    # 2. Highest medium → MT
    remaining = df[~df['cluster'].isin(assigned)]
    idx = remaining['medium_rte_rate'].idxmax()
    cluster = remaining.loc[idx, 'cluster']
    names[cluster] = "MT"
    assigned.add(cluster)
    
    # 3. Highest short → ST
    remaining = df[~df['cluster'].isin(assigned)]
    idx = remaining['short_rte_rate'].idxmax()
    cluster = remaining.loc[idx, 'cluster']
    names[cluster] = "ST"
    assigned.add(cluster)
    
    # 4. Highest behind → BT
    remaining = df[~df['cluster'].isin(assigned)]
    idx = remaining['behind_los_rte_rate'].idxmax()
    cluster = remaining.loc[idx, 'cluster']
    names[cluster] = "BT"
    assigned.add(cluster)
    
    # 5. Highest short + medium → SMT
    remaining = df[~df['cluster'].isin(assigned)]
    if len(remaining) > 0:
        remaining = remaining.copy()
        remaining['short_medium'] = remaining['short_rte_rate'] + remaining['medium_rte_rate']
        idx = remaining['short_medium'].idxmax()
        cluster = remaining.loc[idx, 'cluster']
        names[cluster] = "SMT"
        assigned.add(cluster)
    
    # 6. Highest behind + short → RB
    remaining = df[~df['cluster'].isin(assigned)]
    if len(remaining) > 0:
        remaining = remaining.copy()
        remaining['behind_short'] = remaining['behind_los_rte_rate'] + remaining['short_rte_rate']
        idx = remaining['behind_short'].idxmax()
        cluster = remaining.loc[idx, 'cluster']
        names[cluster] = "RB"
        assigned.add(cluster)
    
    return names


def name_target_clusters(centroids_df):
    """
    Auto-name target clusters based on centroid values.
    
    Rules (in order):
    - highest deep_tgt_rate → DT
    - highest medium_tgt_rate → MT
    - highest short_tgt_rate → ST
    - highest behind_los_tgt_rate → BT
    - 2nd highest medium (ignoring MT) → ML
    - highest short + medium combo → SMT
    - highest behind + short combo → RB
    - whatever's left → G (generalist)
    
    Args:
        centroids_df: DataFrame with columns [cluster, deep_tgt_rate, medium_tgt_rate,
                                              short_tgt_rate, behind_los_tgt_rate]
    
    Returns:
        dict: {cluster_number: name}
    """
    df = centroids_df.copy()
    names = {}
    assigned = set()
    
    # 1. Highest deep → DT
    remaining = df[~df['cluster'].isin(assigned)]
    idx = remaining['deep_tgt_rate'].idxmax()
    cluster = remaining.loc[idx, 'cluster']
    names[cluster] = "DT"
    assigned.add(cluster)
    
    # 2. Highest medium → MT
    remaining = df[~df['cluster'].isin(assigned)]
    idx = remaining['medium_tgt_rate'].idxmax()
    cluster = remaining.loc[idx, 'cluster']
    names[cluster] = "MT"
    assigned.add(cluster)
    
    # 3. Highest short → ST
    remaining = df[~df['cluster'].isin(assigned)]
    idx = remaining['short_tgt_rate'].idxmax()
    cluster = remaining.loc[idx, 'cluster']
    names[cluster] = "ST"
    assigned.add(cluster)
    
    # 4. Highest behind → BT
    remaining = df[~df['cluster'].isin(assigned)]
    idx = remaining['behind_los_tgt_rate'].idxmax()
    cluster = remaining.loc[idx, 'cluster']
    names[cluster] = "BT"
    assigned.add(cluster)
    
    # 5. 2nd highest medium (ignoring MT) → ML
    remaining = df[~df['cluster'].isin(assigned)]
    if len(remaining) > 0:
        idx = remaining['medium_tgt_rate'].idxmax()
        cluster = remaining.loc[idx, 'cluster']
        names[cluster] = "ML"
        assigned.add(cluster)
    
    # 6. Highest short + medium → SMT
    remaining = df[~df['cluster'].isin(assigned)]
    if len(remaining) > 0:
        remaining = remaining.copy()
        remaining['short_medium'] = remaining['short_tgt_rate'] + remaining['medium_tgt_rate']
        idx = remaining['short_medium'].idxmax()
        cluster = remaining.loc[idx, 'cluster']
        names[cluster] = "SMT"
        assigned.add(cluster)
    
    # 7. Highest behind + short → RB
    remaining = df[~df['cluster'].isin(assigned)]
    if len(remaining) > 0:
        remaining = remaining.copy()
        remaining['behind_short'] = remaining['behind_los_tgt_rate'] + remaining['short_tgt_rate']
        idx = remaining['behind_short'].idxmax()
        cluster = remaining.loc[idx, 'cluster']
        names[cluster] = "RB"
        assigned.add(cluster)
    
    # 8. Whatever's left → G (generalist)
    remaining = df[~df['cluster'].isin(assigned)]
    for _, row in remaining.iterrows():
        names[row['cluster']] = "G"
    
    return names


# ============================================================================
# EXAMPLE USAGE
# ============================================================================
#
# After fitting KMeans:
#
#   # Create centroids dataframe
#   centroids_rte = pd.DataFrame(kmeans_rte.cluster_centers_, columns=features_rte)
#   centroids_rte['cluster'] = range(len(centroids_rte))
#
#   # Auto-name
#   route_names = name_route_clusters(centroids_rte)
#   print(route_names)  # {0: 'ST', 1: 'DT', 2: 'MT', ...}
#
#   # Apply to data
#   df['cluster_name'] = df['cluster'].map(route_names)
#
# ============================================================================