module InstantError where

import AbsInstant

data CompilationError = UndeclaredVar Ident

instance Show CompilationError where
    show (UndeclaredVar (Ident ident)) = "Usage of undeclared variable " ++ ident


