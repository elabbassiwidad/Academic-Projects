
"""This is a template for for developing the code of Final exercise
    
    Remember to rename this filename using the following convention:
    
        final_exercise_group-<groupNumber>.py
    
    """

import pandas as pd

def exercise1_commitMetrics(commitsByGroupByProject="./files/commitsByGroupByProject.csv",
                        issuesbyproject="./files/issuesbyproject.csv",
                        milestonesbyproject="./files/milestonesbyproject.csv",
                        usersByGroup="./files/usersByGroup.csv"):
    """Compute commits metrics per group
        
        Keyword arguments:
        commitsByGroupByProject -- path + name of file containing commitsByGroupByProject
        issuesbyproject         -- path + name of file containing issuesbyproject
        milestonesbyproject     -- path + name of file containing milestonesbyproject
        usersByGroup            -- path + name of file containing usersByGroup
        """

    # Function body
    commits = pd.read_csv(commitsByGroupByProject, sep='|')
    users = pd.read_csv(usersByGroup, sep="|")

    df = commits[['idGroup', 'idCommit', 'diffBytes']].groupby(['idGroup', 'idCommit']).agg(sumBytes=('diffBytes', 'sum')).reset_index()
    df = df.groupby(['idGroup']).agg(numCommits=('idGroup', 'size'), avgCommitSize=('sumBytes', 'mean')).round({'avgCommitSize': 1}).reset_index()
    df = pd.merge(df, users[['idGroup', 'fullNameGroup']].drop_duplicates(), on='idGroup')
    df = df[['fullNameGroup', 'numCommits', 'avgCommitSize']]
    # return Pandas DataFrame
    return df


def exercise2_issuesMetrics(commitsByGroupByProject="./files/commitsByGroupByProject.csv",
                            issuesbyproject="./files/issuesbyproject.csv",
                            milestonesbyproject="./files/milestonesbyproject.csv",
                            usersByGroup="./files/usersByGroup.csv"):
    """Compute percentage of closed and open issues per group
        
        Keyword arguments:
        commitsByGroupByProject -- path + name of file containing commitsByGroupByProject
        issuesbyproject         -- path + name of file containing issuesbyproject
        milestonesbyproject     -- path + name of file containing milestonesbyproject
        usersByGroup            -- path + name of file containing usersByGroup
        """

    # Function body
    issues = pd.read_csv(issuesbyproject, sep='|')
    users = pd.read_csv(usersByGroup, sep='|')

    df = issues[['idGroup', 'stateIssue']].groupby(['idGroup', 'stateIssue']).size().reset_index(name='closed')
    df = df[df['stateIssue'] != 'opened'] #closed
    del df['stateIssue']
    df2 = issues[['idGroup', 'stateIssue']].groupby('idGroup').size().reset_index(name="numIssues")
    df = pd.merge(df2, df, on='idGroup')
    df['closed'] = round(df['closed']/df['numIssues']*100,1)
    df['open'] = 100.0 - df['closed']
    df = pd.merge(df, users[['idGroup', 'fullNameGroup']].drop_duplicates(), on='idGroup')
    df = df[['fullNameGroup', 'numIssues', 'closed', 'open']]
    # return Pandas DataFrame
    return df
