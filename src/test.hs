import Test.HUnit
--import TA
main = do
  runTestTT tests
  print $ "test end"
tests = TestList [TestLabel "test1" test1]
test1 = TestCase(assertEqual "that's heresy onil-chan!" (1,2) (1,2))
