
import unittest
from assignment2 import solveWithAStar
from assignment2 import solveWithBranchAndBound
from assignment2 import expandElementsChildren

class Test(unittest.TestCase):


    def testAStarART(self):
        pass
    
    def testAStarNO(self):
        result = solveWithAStar(['N', 'O', '-', '-'], ['O', 'N', '-', '-'])
        self.assertIsNone(result)
    
    def testAStarTooSmallLength(self):
        result = solveWithAStar(['N', 'O'], ['O', 'N'])
        self.assertIsNone(result)
    
    def testBranchAndBoundART(self):
        result = solveWithBranchAndBound(['A', 'R', 'T', '-', '-', '-'], ['-', '-', '-', 'T', 'A', 'R'])
        self.assertEqual(result,
                         [
                             ['A', 'R', 'T', '-', '-', '-'], 
                             ['A', 'R', '-', 'T', '-', '-'], 
                             ['A', '-', 'R', 'T', '-', '-'], 
                             ['A', '-', '-', 'T', 'R', '-'], 
                             ['-', 'A', '-', 'T', 'R', '-'], 
                             ['-', 'A', '-', 'T', '-', 'R'], 
                             ['-', '-', 'A', 'T', '-', 'R'], 
                             ['-', '-', '-', 'T', 'A', 'R']
                        ])
    
    def testBranchAndBoundNO(self):
        result = solveWithBranchAndBound(['N', 'O', '-', '-'], ['O', 'N', '-', '-'])
        self.assertIsNone(result)
        
    def testBranchAndBoundTooSmallLength(self):
        result = solveWithBranchAndBound(['N', 'O'], ['O', 'N'])
        self.assertIsNone(result)
        
    def testexpandElementsChildren(self):
        result = expandElementsChildren(['A', 'R', 'T', '-', '-', '-'])
        self.assertListEqual(result, 
        [
            ['A', '-', 'T', 'R', '-', '-'],
            ['A', 'R', '-', 'T', '-', '-'] 
        ])

    def testBranchAndBoundCooper(self):
        result = solveWithBranchAndBound(['w', 'e', 'r', '-', '-', '-'], ['w', '-', '-', '-', 'e', 'r'])
        print result
        

if __name__ == "__main__":
    #import sys;sys.argv = ['', 'Test.testName']
    unittest.main()