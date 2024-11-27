## README ----------------------------------------------------------------------------------------------
# 
#   file: LinReg.py
#   goal: create custom python module that calculates linear regression coefficients
# 
## -----------------------------------------------------------------------------------------------------

# dependencies -----------------------------------------------------------------------------------------

import numpy as np
import pandas as pd 


class model: 

    def __init__(self, X, y): 

        self.X = X 
        self.y = y 

        if (X.shape[0] != y.shape[0]): 
            raise Exception("ERROR: number of instances is not consistent between design matrix and outcome vector.")

        print(f"Initialized model object for data with {X.shape[0]} rows and {X.shape[1]} features.")


    def fit(self): 

        print("fitting model...") 

        ## closed-form OLS solution
        X_TX = np.matmul(np.transpose(self.X), self.X)
        X_Ty = np.matmul(np.transpose(self.X), self.y)
        X_TX_inv = np.linalg.inv(X_TX)

        coef = np.matmul(X_TX_inv, X_Ty)

        self.coef = coef 


    def summary(self): 

        df = pd.DataFrame({"coefficient": self.X.columns, "estimate": self.coef})
        df.set_index("coefficient", inplace = True)

        return df

