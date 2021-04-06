# import packages
import numpy as np
import pandas as pd
import seaborn as sns
import statsmodels.api as sm
import scipy.stats as stats
import warnings

warnings.filterwarnings("ignore")


def import_data(path):
    data = pd.read_csv(path).drop(0)
    data.columns = ["DateTime", "Programme", "MLCourse", "IRCourse", "StatCourse", "DBCourse", "Gender", "Chocolate",
                "Birthdate", "Neighbours", "Standup", "Stresslevel", "Reward", "RandomNo", "Bedtime", "Goodday1",
                "Goodday2"]
    return data


def main():
    path_ODI = 'data/ODI/ODI-2021.csv'
    df = import_data(path_ODI)

# Preprocessing tests
# Split date time
    df[['Date', 'Time']] = df.DateTime.str.split(" ", expand=True,) # Split
    df = df.drop(columns = 'DateTime') # Drop original datetime
# Programme clusters
# Convert 3:6 to binary

#    df['MLCourse', 'IRCourse', 'StatCourse', 'DBCourse'] = df['MLCourse', 'IRCourse', 'StatCourse', 'DBCourse'].astype(bool)

# Gender -1 0 1 ?
# Chocolate
# Birthdate
# Neighbours
# Standup
# Stresslevel
# Reward
# RandomNo
# Bedtime
# Gday1/2

#   print(df)
#    print(list(df.columns.values))
#    print(df['MLCourse'].astype(bool).unique())

    for col in df:
        print(df[col].unique())

if __name__ == "__main__":
    main()
