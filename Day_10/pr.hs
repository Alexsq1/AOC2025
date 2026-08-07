
import Data.LinearProgram
import Data.LinearProgram.GLPK

type Var = String

lp :: LP Var Int
lp = execLPM $ do
  -- Minimizar
  setDirection Min
  setObjective [("x",1), ("y",1), ("z",1)]

  -- Restricciones
  addConstraint [("x",1), ("y",2)] :>=: 4
  addConstraint [("y",1), ("z",1)] :>=: 2

  -- Variables enteras y no negativas
  mapM_ (\v -> do
           setVarKind v IntVar
           setVarBounds v (0, Nothing)
        ) ["x","y","z"]

main :: IO ()
main = print $ glpSolveVars mipDefaults lp

