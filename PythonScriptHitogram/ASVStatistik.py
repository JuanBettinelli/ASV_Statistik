import pandas as pd
from datetime import date
import numpy as np
import matplotlib.pyplot as plt


def age(birthdate):
    today = date.today()
    age = today.year - birthdate.year - ((today.month, today.day) < (birthdate.month, birthdate.day))
    return age

personen = pd.read_csv('personen.csv', encoding="iso-8859-1", delimiter=";")
personen.fillna('')  
personen.replace("0000-00-00", "", inplace=True)
personen['geb_datum'] =  pd.to_datetime(personen['geb_datum'])
personen['ausscheidedatum'] =  pd.to_datetime(personen['ausscheidedatum'])
personen['ah_ernennung'] =  pd.to_datetime(personen['ah_ernennung'])
personen['erworben_am'] =  pd.to_datetime(personen['erworben_am'])

# alter histogram
now = pd.Timestamp('now')
personen['geb_datum'] = personen['geb_datum'].where(personen['geb_datum'] < now, personen['geb_datum'] -  np.timedelta64(100, 'Y'))   # 2
personen['alter'] = (now - personen['geb_datum']).astype('<m8[Y]')    # 3
alter_list = personen.groupby(['p_id'])['alter'].backfill() 
alter_hist = np.histogram(alter_list, bins=[30, 40, 50, 60, 70, 80])

alt_auf_List = personen.groupby(['p_id'])['aufnahmedatum', 'alter'].bfill() 
auf = alt_auf_List["aufnahmedatum"].tolist()
test1 = alt_auf_List["alter"].tolist()
plt.plot_date(auf, test1)
plt.show()

# print(auf, len(test1))

