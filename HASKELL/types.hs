module Types where
import Data.Map (Map)
import Data.String
import qualified Data.Map as Map

data State = State
  { you_are_at :: Location
  , crew :: Int
  , disembarked :: Bool
  , holding :: [String]
  , game_over :: Bool
  , aeolusAccess :: Bool
  , wind_bag_available :: Bool
  , magic_herb_available :: Bool
  , accessToUnderworld :: Bool
  , visitedUnderworld :: Bool
  , scyllaSurvivalRate :: Maybe Double
  , crewSurvivedSirens :: Maybe Int
  , charybdis_lure :: Bool
  , longerStay :: Bool
  , mapAvailable :: Bool
  , inventory :: Inventory
  , potion_recipe :: Bool
  , helios_blood :: Bool
  , strength_elixir :: Bool
  , raftStepsCompleted :: [RaftStep]
  , ferryPuzzleState :: Maybe FerryPuzzleState
  }

data Condition = AlwaysTrue | Holding String | FlagCondition (State -> Bool)

data Location = Location {
    name:: String,
    descriptions:: [(Condition, String)]
}
instance Eq Location where
  (Location name1 _) == (Location name2 _) = name1 == name2

data FerryItem = Crew | Wine | Cerber | Charon | None deriving (Eq, Show)
data FerryPuzzleState = FerryPuzzleState
  { itemsOnStartSide :: [FerryItem]
  , itemsOnEndSide :: [FerryItem]
  } deriving (Show)

data RaftStep = Base | Frame | Binding | Mast deriving (Eq, Ord, Show)
type Material = String
type Inventory = Map Material Int

requiredMaterials :: [(Material, Int)]
requiredMaterials =
  [ ("wood", 2),
    ("logs", 2),
    ("rope", 2),
    ("cloth", 1)
  ]