import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

def importdf():
    train_df = pd.read_csv("data/titanic/train.csv")
    test_df = pd.read_csv("data/titanic/test.csv")
    gender_submission_df = pd.read_csv("data/titanic/gender_submission.csv")
    return(train_df, test_df, gender_submission_df)

def dfPreprocessing(train_df, test_df, gender_submission_df):
    train_df = train_df.drop(['PassengerId', 'Name', 'Ticket'], axis=1)
    test_df = test_df.drop(['Name', 'Ticket'], axis=1)
    return(train_df, test_df, gender_submission_df)

def main():
    train_df, test_df, gender_submission_df = importdf()
    train_df, test_df, gender_submission_df = dfPreprocessing(train_df, test_df, gender_submission_df)
    train_df.head()
    test_df.head()
    gender_submission_df.head()

    train_df.info()
if __name__ == "__main__":
    main()
