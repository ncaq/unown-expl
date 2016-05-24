module Main where

import           Application
import           ClassyPrelude
import           Control.Monad.Trans.State
import           Test.Hspec

main :: IO ()
main = hspec $ do
    describe "readExpr" $ do
        it "give" $ do
            readExpr "give make"  `shouldBe` Right (Give Make)
            readExpr "give make " `shouldBe` Right (Give Make)
            readExpr "give  make" `shouldBe` Right (Give Make)
        it "join" $ do
            readExpr "join make make" `shouldBe` Right (Join Make Make)
        it "keep" $ do
            readExpr "keep increase make tell make" `shouldBe`
                Right (Keep (Increase Make) (Tell Make))
            readExpr "keep zoom observe make perform i i i m i i i i m tell make" `shouldBe`
                Right (Keep (Zoom (Observe Make)
                             (Perform
                              (Increase (Increase (Increase Make)))
                              (Increase (Increase (Increase (Increase Make))))))
                       (Tell Make))
            readExpr "keep make perform make make ! make" `shouldBe`
                Right (Keep Make (Perform Make Make) `Exclaim` Make)
        it "make" $ do
            readExpr "make"  `shouldBe` Right Make
            readExpr "make " `shouldBe` Right Make
            readExpr " make" `shouldBe` Right Make
            readExpr "m"     `shouldBe` Right Make
            readExpr "mike"  `shouldBe` Right Make
        it "xxxxx" $ do
            readExpr "x m m" `shouldBe` Right (XXXXX Make Make)
            readExpr "k m x t m t m" `shouldBe` Right (Keep Make (Tell Make `XXXXX` Tell Make))
            readExpr "k m x t m x t m t m" `shouldBe`
                Right (Keep Make (Tell Make `XXXXX` (Tell Make `XXXXX` Tell Make)))
        it "exclaim" $ do
            readExpr "make ! make" `shouldBe` Right (Make `Exclaim` Make)
            readExpr "make ! increase make" `shouldBe` Right (Make `Exclaim` Increase Make)
            readExpr "increase make ! increase make" `shouldBe`
                Right (Increase Make `Exclaim` Increase Make)
            readExpr "make ! make ! make" `shouldBe` Right (Make `Exclaim` (Make `Exclaim` Make))
            readExpr "give join make make ! observe make" `shouldBe`
                Right (Give (Join Make Make) `Exclaim` Observe Make)
    describe "eval" $ do
        it "give" $ do
            runStateT (eval (Give Make)) [] `shouldReturn` (UnownInt 0, [UnownInt 0])
            runStateT (eval (Give (Increase Make))) [] `shouldReturn` (UnownInt 1, [UnownInt 1])
            runStateT (eval ((Give Make) `Exclaim` (Give Make))) [] `shouldReturn`
                (UnownInt 0, [UnownInt 0, UnownInt 0])
        it "increase" $ do
            runStateT (eval (Increase Make)) [] `shouldReturn` (UnownInt 1, [])
            runStateT (eval (Increase (Increase Make))) [] `shouldReturn` (UnownInt 2, [])
        it "join" $ do
            runStateT (eval (Join Make Make)) [] `shouldReturn` (UnownInt 0, [])
            runStateT (eval (Join Make (Increase Make))) [] `shouldReturn` (UnownInt 1, [])
            runStateT (eval (Join (Increase Make) Make)) [] `shouldReturn` (UnownInt 1, [])
            runStateT (eval (Join (Increase Make) (Increase Make))) [] `shouldReturn` (UnownInt 2, [])
            runStateT (eval (Join (Join Make (Increase Make)) (Increase Make))) [] `shouldReturn`
                (UnownInt 2, [])
        it "keep" $ do
            runStateT (eval (Give Make `Exclaim`
                      Keep (Zoom (Observe Make) (Increase Make))
                      (Give (Increase (Observe Make))))) [] `shouldReturn`
                (UnownList [UnownInt 1], [UnownInt 1, UnownInt 0])
        it "make" $ do
            runStateT (eval Make) [] `shouldReturn` (UnownInt 0, [])
            runStateT (eval (Make `Exclaim` Make)) [] `shouldReturn` (UnownInt 0, [])
        it "observe" $ do
            runStateT (eval (Give Make `Exclaim` Observe Make)) [] `shouldReturn`
                (UnownInt 0, [UnownInt 0])
            runStateT (eval (Give (Increase Make) `Exclaim` Observe Make)) [] `shouldReturn`
                (UnownInt 1, [UnownInt 1])
            runStateT (eval (Give Make `Exclaim` Give (Increase Make) `Exclaim`
                      Observe (Increase Make))) [] `shouldReturn`
                (UnownInt 0, [UnownInt 1, UnownInt 0])
        it "perform" $ do
            runStateT (eval (Perform Make Make)) [] `shouldReturn` (UnownInt 0, [])
            runStateT (eval (Perform (Increase (Increase Make)) (Increase (Increase Make)))) []
                `shouldReturn` (UnownInt 4, [])
        it "zoom" $ do
            runStateT (eval (Zoom Make Make)) [] `shouldReturn` (UnownBool False, [])
            runStateT (eval (Zoom Make (Increase Make))) [] `shouldReturn` (UnownBool True, [])
