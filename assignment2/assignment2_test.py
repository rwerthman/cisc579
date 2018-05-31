
import unittest
from assignment2 import solveWithAStar
from assignment2 import solveWithBranchAndBound
from assignment2 import expandElementsChildren
from assignment2 import removeDuplicates

class Test(unittest.TestCase):
    
    def testAStarNO(self):
        result = solveWithAStar('no--', 'on--')
        #self.assertIsNone(result)
        self.assertFalse(result)
     
    def testAStarTooSmallLength(self):
        result = solveWithAStar('no', 'on')
        #self.assertIsNone(result)
        self.assertFalse(result)
    
    def testAStarART(self):
        # result = solveWithAStar(['A', 'R', 'T', '-', '-', '-'], ['-', '-', '-', 'T', 'A', 'R'])
        result = solveWithAStar('ART---', '---TAR')
        self.assertTrue(result)
        
        result = solveWithAStar('cooper---', 'C---pooer')
        self.assertTrue(result)
        #self.assertEqual(result[-1], ['-', '-', '-', 'T', 'A', 'R'])
    
    def testBranchAndBoundART(self):
        result = solveWithBranchAndBound('ART---', '---TAR')
        self.assertTrue(result)
#         self.assertEqual(result,
#                          [
#                              ['A', 'R', 'T', '-', '-', '-'], 
#                              ['A', 'R', '-', 'T', '-', '-'], 
#                              ['A', '-', 'R', 'T', '-', '-'], 
#                              ['A', '-', '-', 'T', 'R', '-'], 
#                              ['-', 'A', '-', 'T', 'R', '-'], 
#                              ['-', 'A', '-', 'T', '-', 'R'], 
#                              ['-', '-', 'A', 'T', '-', 'R'], 
#                              ['-', '-', '-', 'T', 'A', 'R']
#                         ])
     
    def testBranchAndBoundNO(self):
        result = solveWithBranchAndBound('no--', 'on--')
        self.assertFalse(result)
        # self.assertIsNone(result)
         
    def testBranchAndBoundTooSmallLength(self):
        result = solveWithBranchAndBound('no', 'on')
        # self.assertIsNone(result)
        self.assertFalse(result)
         
    def testexpandElementsChildren(self):
        result = expandElementsChildren('art---')
        self.assertListEqual(result, 
        [
            'a-tr--',
            'ar-t--' 
        ])
    
    def testRemoveDuplicates(self):
        result = removeDuplicates([['art--', 'a-tr--'], ['art--', 'a-tr-t'], ['art--', 'a-tr--']])
        self.assertListEqual(result, [['art--', 'a-tr--'], ['art--', 'a-tr-t']])

if __name__ == "__main__":
    #import sys;sys.argv = ['', 'Test.testName']
    unittest.main()