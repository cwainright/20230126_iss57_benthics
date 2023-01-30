# a module for main.py

import pandas as pd
import numpy as np
import pyodbc

def getQueryResults(dbq):
    try:
        driver = "{Microsoft Access Driver (*.mdb, *.accdb)}"
        con = pyodbc.connect(f'Driver={driver};DBQ={dbq};') # establish db connection
        cursor = con.cursor() # establish db cursor to interact with db
        tables = []
        for i in cursor.tables(tableType='TABLE'): # test db connection by extracting table names
            tables.append(i.table_name)
            print(i.table_name)
        
        query = 'SELECT TOP 2 * FROM tlu_Macroinverts'
        # query = "SELECT 'sqlserver' dbms,t.TABLE_CATALOG,t.TABLE_SCHEMA,t.TABLE_NAME,c.COLUMN_NAME,c.ORDINAL_POSITION,c.DATA_TYPE,c.CHARACTER_MAXIMUM_LENGTH,n.CONSTRAINT_TYPE,k2.TABLE_SCHEMA,k2.TABLE_NAME,k2.COLUMN_NAME FROM INFORMATION_SCHEMA.TABLES t LEFT JOIN INFORMATION_SCHEMA.COLUMNS c ON t.TABLE_CATALOG=c.TABLE_CATALOG AND t.TABLE_SCHEMA=c.TABLE_SCHEMA AND t.TABLE_NAME=c.TABLE_NAME LEFT JOIN(INFORMATION_SCHEMA.KEY_COLUMN_USAGE k JOIN INFORMATION_SCHEMA.TABLE_CONSTRAINTS n ON k.CONSTRAINT_CATALOG=n.CONSTRAINT_CATALOG AND k.CONSTRAINT_SCHEMA=n.CONSTRAINT_SCHEMA AND k.CONSTRAINT_NAME=n.CONSTRAINT_NAME LEFT JOIN INFORMATION_SCHEMA.REFERENTIAL_CONSTRAINTS r ON k.CONSTRAINT_CATALOG=r.CONSTRAINT_CATALOG AND k.CONSTRAINT_SCHEMA=r.CONSTRAINT_SCHEMA AND k.CONSTRAINT_NAME=r.CONSTRAINT_NAME)ON c.TABLE_CATALOG=k.TABLE_CATALOG AND c.TABLE_SCHEMA=k.TABLE_SCHEMA AND c.TABLE_NAME=k.TABLE_NAME AND c.COLUMN_NAME=k.COLUMN_NAME LEFT JOIN INFORMATION_SCHEMA.KEY_COLUMN_USAGE k2 ON k.ORDINAL_POSITION=k2.ORDINAL_POSITION AND r.UNIQUE_CONSTRAINT_CATALOG=k2.CONSTRAINT_CATALOG AND r.UNIQUE_CONSTRAINT_SCHEMA=k2.CONSTRAINT_SCHEMA AND r.UNIQUE_CONSTRAINT_NAME=k2.CONSTRAINT_NAME WHERE t.TABLE_TYPE='BASE TABLE';"
        df = pd.read_sql(query,con) # use pandas to extract query results as dataframe
        return df
        con.close() # close db connection
        cursor.close()
    except:
        print("there was an exception")


