module InitSetup where
import qualified Data.Map as Map
import Types
import WorldMap

init_state :: State
init_state = State
  { you_are_at = open_sea
  , crew = 600
  , disembarked = False
  , holding = []
  , game_over = False
  , aeolusAccess = False
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