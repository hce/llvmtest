{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Int
import Foreign.Ptr
import LLVM
import LLVM.Analysis
import LLVM.AST
import LLVM.AST.CallingConvention
import LLVM.AST.Global
import LLVM.AST.Linkage
import LLVM.Context
import LLVM.Module
import LLVM.PassManager

import qualified LLVM.AST.Constant as ASTC
import qualified LLVM.AST.Operand as ASTO
import qualified LLVM.ExecutionEngine as EE

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import qualified Data.ByteString.Char8 as S8

import LLVM.AST.Instruction

int64type :: Type
int64type = IntegerType 64

printT :: Type
printT = FunctionType VoidType [int64type] False

passes :: PassSetSpec
passes = defaultCuratedPassSetSpec { optLevel = Just 3 }

jit :: Context -> (EE.MCJIT -> IO a) -> IO a
jit c = EE.withMCJIT c optlevel model pretlim fastins
  where
    optlevel    = Just 3
    model       = Nothing
    pretlim     = Nothing
    fastins     = Nothing

foreign import ccall "dynamic" haskFun :: FunPtr (IO Int64) -> (IO Int64)

run :: FunPtr a -> IO Int64
run fn = haskFun (castFunPtr fn :: FunPtr (IO Int64))

main :: IO ()
main = do
    let adam        = ASTC.Int 64 67
        adam2       = ASTC.Int 64 10
        weishaupt   = ASTC.Int 64 23
        mul         = ASTC.Int 64 100

        success     = ASTC.Int 64 1723

        tmp1        = Mul False False (ASTO.ConstantOperand adam) (ASTO.ConstantOperand mul) []
        nameTmp1    = UnName 1

        sum         = Add False False (LocalReference int64type nameTmp1) (ASTO.ConstantOperand weishaupt) []
        nameSum     = UnName 2

        printFun    = ASTC.GlobalReference printT $ Name "putchar"
        printit     = Call Nothing C [] (Right $ ASTO.ConstantOperand printFun)
                            [(LocalReference int64type nameTmp1, [])] [] []
        printItLabel = UnName 5
        printit2    = Call Nothing C [] (Right $ ASTO.ConstantOperand printFun)
                            [(ASTO.ConstantOperand adam2, [])] [] []
        printItLabel2 = UnName 6

        instrs      = [ (nameTmp1 := tmp1), (nameSum := sum), (printItLabel := printit), (printItLabel2 := printit2) ]

        mainblock   = BasicBlock (Name "hello") instrs (UnName 3 := Ret (Just $ ASTO.ConstantOperand success) [])

        moduleDef   = GlobalDefinition $ functionDefaults {
                            name        = Name "haskmain"
                          --, linkage     = AvailableExternally
                          , parameters  = ([], False)
                          , returnType  = int64type
                          , basicBlocks = [mainblock] }

        printDef    = GlobalDefinition $ functionDefaults {
                            name        = Name "putchar"
                          , linkage     = External
                          , parameters  = ([Parameter int64type (Name "val") []], False)
                          , returnType    = int64type }

        fun         = defaultModule { moduleName = "HelloWorld"
                                    , moduleDefinitions = [moduleDef, printDef] }


    withContext $ \context ->
        withModuleFromAST context fun $ \m ->
            withPassManager passes $ \pm -> do
                putStrLn "Verifying IR..."
                verify m
                putStrLn "Optimizing IR..."
                runPassManager pm m
                s <- moduleLLVMAssembly m
                S8.putStrLn s
                putStrLn "Generating object code..."
                optMod <- moduleAST m
                jit context $ \ee ->
                    EE.withModuleInEngine ee m $ \eee -> do
                        putStrLn "Loading function into executable memory..."
                        mainfn <- EE.getFunction eee (Name "haskmain")
                        case mainfn of
                            Just fn -> do
                                putStrLn "Running!"
                                res <- run fn
                                putStrLn $ "Retval: " ++ show res
                            Nothing -> do
                                putStrLn "no fun"
                print "ok"
    -- writeObjectToFile "output.o" m
