module Helper.Happened where

import Game.Component (Happened (..))

isPlayerMove :: Happened -> Bool
isPlayerMove PlayerMove = True
isPlayerMove _ = False

isPlayerAttack :: Happened -> Bool
isPlayerAttack PlayerAttack = True
isPlayerAttack _ = False

isEnemyAttack :: Happened -> Bool
isEnemyAttack EnemyAttack = True
isEnemyAttack _ = False

isSodaPicked :: Happened -> Bool
isSodaPicked (SodaPicked _) = True
isSodaPicked _ = False

isFruitPicked :: Happened -> Bool
isFruitPicked (FruitPicked _) = True
isFruitPicked _ = False

isPlayerWin :: Happened -> Bool
isPlayerWin PlayerWin = True
isPlayerWin _ = False

isPlayerDie :: Happened -> Bool
isPlayerDie PlayerDie = True
isPlayerDie _ = False

isRestart :: Happened -> Bool
isRestart Restart = True
isRestart _ = False
