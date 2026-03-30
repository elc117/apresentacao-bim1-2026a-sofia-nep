Código original
```hs
import Data.Char

cpfValid :: [Int] -> Bool
cpfValid cpf =
  let digits = take 9 cpf
      dv1 = cpfDV digits [10,9..]
      dv2 = cpfDV (digits ++ [dv1]) [11,10..]
   in dv1 == cpf !! 9 && dv2 == cpf !! 10

cpfDV :: [Int] -> [Int] -> Int
cpfDV digits mults = if res < 2 then 0 else 11-res
  where res = (sum $ zipWith (*) digits mults) `mod` 11

main :: IO()
main = do
 putStrLn "Digite o CPF: "
 cpf <- getLine
 let digits = (map digitToInt cpf)
 putStrLn (if cpfValid digits then "Válido" else "Inválido")
```

Código modificado
```hs
import Data.Char

cpfValid :: [Int] -> Bool
cpfValid cpf = dv1 == cpf !! 9 && dv2 == cpf !! 10
    where
        digits = take 9 cpf
        dv1 = cpfDV digits [10,9..]
        dv2 = cpfDV (digits ++ [dv1]) [11,10..]

cpfDV :: [Int] -> [Int] -> Int
cpfDV digits mults =
    let res = (sum $ zipWith (*) digits mults) `mod` 11
    in if res < 2 then 0 else 11-res

main :: IO()
main = do
    putStrLn "Digite o CPF: "
    cpf <- getLine
    let digits = (map digitToInt cpf)
    putStrLn (if cpfValid digits then "Válido" else "Inválido")
```

Achei fácil entender a diferença teórica, mas não tenho certeza sobre quando qual deve ser usado (talvez eu ache `where` mais legível?).

![Video](https://bottomservices.club/video.webm)