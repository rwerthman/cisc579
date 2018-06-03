
import unittest
import timeit
from assignment2 import solveWithAStar
from assignment2 import solveWithBranchAndBound
from assignment2 import expandElementsChildren
from assignment2 import removeDuplicates
from assignment2 import heuristicEstimateofDistanceToGoal
from assignment2 import timingWrapper
from assignment2 import sortWithDistanceTravelledAndHeuristic

class Test(unittest.TestCase):
    
    def testAStarNO(self):
        result = solveWithAStar('no--', 'on--')
        self.assertFalse(result)
     
    def testAStarTooSmallLength(self):
        result = solveWithAStar('no', 'on')
        self.assertFalse(result)
    
    def testAStarART(self):
        result = solveWithAStar('art---', '---tar')
        self.assertTrue(result)
     
    def testBranchAndBoundART(self):
        result = solveWithBranchAndBound('art---', '---tar')
        self.assertTrue(result)
     
    def testBranchAndBoundNO(self):
        result = solveWithBranchAndBound('no--', 'on--')
        self.assertFalse(result)
         
    def testBranchAndBoundTooSmallLength(self):
        result = solveWithBranchAndBound('no', 'on')
        self.assertFalse(result)
         
    def testexpandElementsChildren(self):
        result = expandElementsChildren('art---')
        self.assertListEqual(result, ['a-tr--', 'ar-t--' ])
    
    def testRemoveDuplicates(self):
        result = removeDuplicates([['art--', 'a-tr--'], ['art--', 'a-tr-t'], ['art--', 'a-tr--']])
        self.assertListEqual(result, [['art--', 'a-tr--'], ['art--', 'a-tr-t']])
    
    def testHeuristicEstimateofDistanceToGoalStillPossible(self):
        result = heuristicEstimateofDistanceToGoal('art---', '---tar')
        self.assertEqual(result, 9)
    
    def testHeuristicEstimateofDistanceToGoalNotPossible(self):
        result = heuristicEstimateofDistanceToGoal('ar---t', '---tar')
        self.assertEqual(result, 36)
    
    def testHeuristicEstimateofDistanceToGoalString1(self):
        result = heuristicEstimateofDistanceToGoal('--t-ar', '---tar')
        self.assertEqual(result, 1)

    def testHeuristicEstimateofDistanceToGoalString2(self):
        result = heuristicEstimateofDistanceToGoal('a-tr--', '---tar')
        self.assertEqual(result, 7)
        
    def testsortWithDistanceTravelledAndHeuristic(self):
        result = sortWithDistanceTravelledAndHeuristic([['a-tr--', 'art---'], ['--t-ar', 'art--']], '---tar')
        self.assertListEqual(result, [ ['--t-ar', 'art--'], ['a-tr--', 'art---']])
        
    def testTimingBranchAndBound(self):
        
        timingWrapped = timingWrapper(solveWithBranchAndBound, 'art---', '---tar')
        print 'Time Branch and Bound takes to find path from art--- to ---tar in seconds', timeit.timeit(timingWrapped, number=1)
         
        timingWrapped = timingWrapper(solveWithBranchAndBound, 'googleeyes---', 'google---eyes')
        print 'Time Branch and Bound takes to find path from googleyes--- to google---eyes in seconds', timeit.timeit(timingWrapped, number=1)
        
        timingWrapped = timingWrapper(solveWithBranchAndBound, 'abcd----', '----dcba')
        print 'Time Branch and Bound takes to find path from abcd---- to ----dcba in seconds', timeit.timeit(timingWrapped, number=1)
    
    def testTimingAStar(self):
        
        timingWrapped = timingWrapper(solveWithAStar, 'art---', '---tar')
        print 'Time A* takes to find path from art--- to ---tar to in seconds', timeit.timeit(timingWrapped, number=1)
        
        timingWrapped = timingWrapper(solveWithAStar, 'googleeyes---', 'google---eyes')
        print 'Time A* takes to find path from googleeyes--- to google---eyes in seconds', timeit.timeit(timingWrapped, number=1)
        
        timingWrapped = timingWrapper(solveWithAStar, 'abcd----', '----dcba')
        print 'Time A* takes to find path from abcd---- to ----dcba in seconds', timeit.timeit(timingWrapped, number=1)
        

if __name__ == "__main__":
    #import sys;sys.argv = ['', 'Test.testName']
    unittest.main()