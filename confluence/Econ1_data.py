if __name__ == '__main__':
    import os
    import pandas as pd
    
    data_dir = "/Users/bloh356/Google Drive/Data/Data_Econ1"
    data = pd.read_csv(os.path.join(data_dir, "existcapacity_annual.csv"), header=0)

    data.columns = pd.Series(data.columns).str.replace(' ', '_')
    data.columns = [x.lower() for x in data.columns] # Use list comprehensions where possible
    data.sort(['producer_type', 'fuel_source', 'state_code', 'year'], inplace=1)

    data['summer_capacity_(megawatts)'] = data["summer_capacity_(megawatts)"].convert_objects(convert_numeric='force')
    data['summer_capacity_change'] = data.groupby(['state_code', 'producer_type', 'fuel_source'])\
        ['summer_capacity_(megawatts)'].transform(lambda x: x.diff())

    data.to_csv(os.path.join(data_dir, "capacity_fuel_net.csv"), index=0)
    mask = (data.producer_type == "Electric Generators, Electric Utilities") & (data.state_code == "US")
    data[mask].to_csv(os.path.join(data_dir, "uscapacity_fuel_net.csv"), index=0)

