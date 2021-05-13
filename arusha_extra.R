bananaDT = pd.read_csv('ArushaBananaDT.csv')
bananaDT.pop(".id")

bananaDT.pop(".id")
bananaDT["First_Pollination_Date"] = pd.to_datetime(bananaDT["First_Pollination_Date"], utc=True, errors='ignore')
bananaDT["Bunch_Harvest_Date"] = pd.to_datetime(bananaDT["Bunch_Harvest_Date"], utc=True, errors='ignore')
bananaDT["Seed_Extraction_Date"] = pd.to_datetime(bananaDT["Seed_Extraction_Date"], utc=True, errors='ignore')
bananaDT["Embryo_Rescue_Date"] = pd.to_datetime(bananaDT["Embryo_Rescue_Date"], utc=True, errors='ignore')
bananaDT["Germination_Date"] = pd.to_datetime(bananaDT["Germination_Date"], utc=True, errors='ignore')
bananaDT[['Total_Seeds', 'Good_Seeds','Number_of_Embryo_Rescued','Number_of_Embryo_Germinating','Cycle']] = bananaDT[['Total_Seeds', 'Good_Seeds','Number_of_Embryo_Rescued','Number_of_Embryo_Germinating','Cycle']].apply(pd.to_numeric)

dt = df.append(bananaDT, ignore_index=True)
dt['Location'] = "Arusha"
dt = dt.dropna(subset=['Crossnumber'])
dt = dt.drop_duplicates()

dt.First_Pollination_Date = pd.to_datetime(dt.First_Pollination_Date, utc=True, errors='ignore')
dt.Bunch_Harvest_Date = pd.to_datetime(dt.Bunch_Harvest_Date, utc=True, errors='ignore')
dt.Seed_Extraction_Date = pd.to_datetime(dt.Seed_Extraction_Date, utc=True, errors='ignore')
dt.Embryo_Rescue_Date = pd.to_datetime(dt.Embryo_Rescue_Date, utc=True, errors='ignore')
dt.Germination_Date = pd.to_datetime(dt.Germination_Date, utc=True, errors='ignore')
dt[['Total_Seeds', 'Good_Seeds','Number_of_Embryo_Rescued','Number_of_Embryo_Germinating','Cycle']] = dt[['Total_Seeds', 'Good_Seeds','Number_of_Embryo_Rescued','Number_of_Embryo_Germinating','Cycle']].apply(pd.to_numeric)

banana = pd.DataFrame(
  dt.groupby(
    ['Crossnumber']
  ).agg(
    {
      'TrialName': 'first',
      'FemalePlotName': 'first',
      'Mother': 'first',
      'Cycle':  [max],
      'Father': 'first',
      'MalePlotName': 'first',
      'First_Pollination_Date': [min],
      'Bunch_Harvest_Date': [min],
      'Seed_Extraction_Date': [min], 
      'Total_Seeds': [max],
      'Embryo_Rescue_Date': [min],
      'Number_of_Embryo_Rescued': [max],
      'Good_Seeds': [max],
      'Number_of_Embryo_Germinating': [max],
      'Germination_Date': [min],
      'Location': 'first'                
    }
  )
)
banana.reset_index(inplace=True)
banana.columns = ['Crossnumber','TrialName','FemalePlotName','Mother', 'Cycle','Father', 'MalePlotName', 'First_Pollination_Date', 'Bunch_Harvest_Date', 'Seed_Extraction_Date', 'Total_Seeds', 'Embryo_Rescue_Date', 'Number_of_Embryo_Rescued', 'Good_Seeds', 'Number_of_Embryo_Germinating', 'Germination_Date', 'Location']

banana['First_Pollination_Date'] = pd.to_datetime(banana['First_Pollination_Date']).dt.date
banana["Bunch_Harvest_Date"] = pd.to_datetime(banana["Bunch_Harvest_Date"]).dt.date
banana["Seed_Extraction_Date"] = pd.to_datetime(banana["Seed_Extraction_Date"]).dt.date
banana["Embryo_Rescue_Date"] = pd.to_datetime(banana["Embryo_Rescue_Date"]).dt.date
banana["Germination_Date"] = pd.to_datetime(banana["Germination_Date"]).dt.date
banana[['Total_Seeds', 'Good_Seeds','Number_of_Embryo_Rescued','Number_of_Embryo_Germinating','Cycle']] = banana[['Total_Seeds', 'Good_Seeds','Number_of_Embryo_Rescued','Number_of_Embryo_Germinating','Cycle']].apply(pd.to_numeric)

bcols = ['Location','Crossnumber','FemalePlotName', 'Mother', 'Cycle',
         'Father', 'MalePlotName', 'First_Pollination_Date',
         'Bunch_Harvest_Date', 'Seed_Extraction_Date', 'Total_Seeds', 'Embryo_Rescue_Date',
         'Number_of_Embryo_Rescued', 'Good_Seeds',
         'Number_of_Embryo_Germinating', 'Germination_Date']

banana = banana[bcols]