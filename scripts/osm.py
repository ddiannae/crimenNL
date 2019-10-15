import osmium as om
from osmium._osmium import InvalidLocationError
import pandas as pd


class OSMHandler(om.SimpleHandler):
    """Return all the node tags available"""

    def __init__(self):
        super().__init__()
        self.node_tags = []
        self.count = 0

    def tag_inventory(self, data, data_type):
        if data.tags:
            for tag in data.tags:
                self.node_tags.append([data_type,
                                       data.id,
                                       data.location.lon,
                                       data.location.lat,
                                       data.version,
                                       data.visible,
                                       tag.k,
                                       tag.v])
        else:
            self.node_tags.append([data_type,
                                   data.id,
                                   data.location.lon,
                                   data.location.lat,
                                   data.version,
                                   data.visible,
                                   "",
                                   ""])

    def node(self, w):
        self.count += 1
        self.tag_inventory(w, "node")


class OSMWayHandler(om.SimpleHandler):
    def __init__(self):
        super().__init__()
        self.node_tags = []
        self.count = 0

    def tag_inventory_way(self, data, data_type):
        base_list = [data_type,
                     data.id,
                     data.version,
                     data.visible]
        for tag in data.tags:
            tag_list = [tag.k, tag.v]
            for node in data.nodes:
                node_list = (base_list + tag_list + [node.ref])
                self.node_tags.append(node_list)

    def way(self, w):
        self.count += 1
        self.tag_inventory_way(w, "way")


if __name__ == '__main__':
    path_to_file = '/Users/rdora/Downloads/osm/monterrey.osm'
    amenities = OSMHandler()
    amenities.apply_file(path_to_file)
    cols = ["type", "id", "lon", "lat", "version", "visible", "tag_key",
            "tag_value"]
    df = pd.DataFrame(amenities.node_tags, columns=cols)
    rint(f"Number of nodes: {amenities.count}")
    print(f"Number of nodes in DF: {df.shape}")
    print(df.head())
    df.to_csv("/Users/rdora/Downloads/osm_nodes.csv", index=False)
    ways = OSMWayHandler()
    ways.apply_file(path_to_file)
    cols_way = ["type", "id", "version", "visible", "tag_key",
                "tag_value", "ref"]
    df = pd.DataFrame(ways.node_tags, columns=cols_way)
    print(f"Number of ways: {ways.count}")
    print(f"Number of nodes in DF ways: {df.shape}")
    df.to_csv("/Users/rdora/Downloads/osm_ways.csv", index=False)
