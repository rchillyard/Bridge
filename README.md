# Bridge
This is where my bridge-related code goes

# DoubleDummyAnalysis

The DoubleDummyAnalysis branch applies the method of Monte Carlo Tree Search to the repository.
In particular, the MCTS code is used here to solve for various contracts to see whether they
can be made or defeated. But MCTS is far more general and can be applied to many game-playing
situations.

As it stands, the tree package is concerned with MCTS. The specifics of the 
double dummy analysis are provided through type classes which are defined in, for example,
the Whist class.

For the tree search, we use DecisionTree.

