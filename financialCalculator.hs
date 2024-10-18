import Text.Printf (printf)
import Text.Read (readMaybe)

-- Function to calculate Simple Interest
simpleInterest :: Float -> Float -> Float -> Float
simpleInterest principal rate time = principal * (rate / 100) * time

-- Function to calculate Compound Interest
compoundInterest :: Float -> Float -> Float -> Float -> Float
compoundInterest principal rate n time =
    principal * (1 + (rate / (n * 100))) ** (n * time)

-- Main function where execution begins
main :: IO ()
main = do
    putStrLn "Welcome to the Basic Financial Calculator!"
    calculatorLoop

calculatorLoop :: IO ()
calculatorLoop = do
    putStrLn "\nSelect an option:"
    putStrLn "1. Calculate Simple Interest"
    putStrLn "2. Calculate Compound Interest"
    putStrLn "3. Exit"
    
    choice <- getLine
    
    case choice of
        "1" -> do
            calculateSimpleInterest
            calculatorLoop
        "2" -> do
            calculateCompoundInterest
            calculatorLoop
        "3" -> putStrLn "Exiting the calculator. Goodbye!"
        _   -> do
            putStrLn "Invalid choice! Please try again."
            calculatorLoop

-- Safely read a Float from input with prompt
readFloat :: String -> IO Float
readFloat prompt = do
    putStrLn prompt
    input <- getLine
    case readMaybe input of
        Just value -> return value
        Nothing    -> do
            putStrLn "Invalid input, please enter a valid number."
            readFloat prompt

-- Calculate Simple Interest
calculateSimpleInterest :: IO ()
calculateSimpleInterest = do
    principal <- readFloat "\nEnter Principal Amount (P):"
    rate <- readFloat "Enter Rate of Interest (R) in %:"
    time <- readFloat "Enter Time (T) in years:"
    
    let interest = simpleInterest principal rate time
    printf "Simple Interest: %.2f\n" interest

-- Calculate Compound Interest
calculateCompoundInterest :: IO ()
calculateCompoundInterest = do
    principal <- readFloat "\nEnter Principal Amount (P):"
    rate <- readFloat "Enter Annual Rate of Interest (R) in %:"
    frequency <- readFloat "Enter Compounding Frequency (n, e.g., annually = 1, semi-annually = 2):"
    time <- readFloat "Enter Time (T) in years:"
    
    let futureValue = compoundInterest principal rate frequency time
    printf "Future Value with Compound Interest: %.2f\n" futureValue