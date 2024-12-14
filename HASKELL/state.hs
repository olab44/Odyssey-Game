module State where
import Data.Map (Map)
import Data.String
import qualified Data.Map as Map

data RaftStep = Base | Frame | Binding | Mast deriving (Eq, Ord, Show)
type Material = String
type Inventory = Map Material Int

data FerryItem = Crew | Wine | Cerber | Charon | None deriving (Eq, Show)
data FerryPuzzleState = FerryPuzzleState
  { itemsOnStartSide :: [FerryItem]
  , itemsOnEndSide :: [FerryItem]
  } deriving (Show)


requiredMaterials :: [(Material, Int)]
requiredMaterials =
  [ ("wood", 2),
    ("logs", 2),
    ("rope", 2),
    ("cloth", 1)
  ]

data State = State
  { you_are_at :: String
  , crew :: Int
  , disembarked :: Bool
  , holding :: [String]
  , game_over :: Bool
  , aeolus_island :: Bool
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
  } deriving Show


init_state :: State
init_state = State
  { you_are_at = "open_sea"
  , crew = 600
  , disembarked = False
  , holding = []
  , game_over = False
  , aeolus_island = False
  , wind_bag_available = False
  , magic_herb_available = False
  , accessToUnderworld = False
  , visitedUnderworld = False
  , scyllaSurvivalRate = Nothing
  , crewSurvivedSirens = Nothing
  , charybdis_lure = False
  , potion_recipe = False
  , mapAvailable = False
  , helios_blood = False
  , longerStay = False
  , inventory = Map.empty
  , strength_elixir = False
  , raftStepsCompleted = []
  , ferryPuzzleState = Nothing
  }
