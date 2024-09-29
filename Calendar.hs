module Calendar where


data Date = Date Year Month Day

newtype Year  = Year Int
data Month = Jan | Feb | Mar
           | Apr | May | Jun
           | Jul | Aug | Sep
           | Oct | Nov | Dec
newtype Day = Day Int

data Week = Monday   | Tuesday | Wednesday
          | Thursday | Friday  | Saturday 
          | Sunday

data Time = Time Hour Minute Second

newtype Hour   = Hour Int
newtype Minute = Minute Int
newtype Second = Second Int

instance Show Week where
  show Monday    = "Mon"
  show Tuesday   = "Tue"
  show Wednesday = "Wed"
  show Thursday  = "Thu"
  show Friday    = "Fri"
  show Saturday  = "Sat"
  show Sunday    = "Sun"

instance Show Time where
  show (Time h m s) = show h ++ ":" ++ show m ++ ":" ++ show s

instance Show Hour where
  show (Hour h) = addZero $ show h

instance Show Minute where
  show (Minute m) = addZero $ show m

instance Show Second where
  show (Second s) = addZero $ show s

addZero :: String -> String
addZero [x] = ['0', x]
addZero xs = xs
