{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Data.Char
import Data.Generics
import Data.List

data UnitSection = UnitSection
  { description :: String
  }
  deriving (Data)

data ServiceSection = ServiceSection
  { workingDirectory :: String,
    execStart :: String,
    restart :: String,
    standardOutput :: String,
    standardInput :: String,
    tTYPath :: String
  }
  deriving (Data)

data InstallSection = InstallSection
  { wantedBy :: String
  }
  deriving (Data)

data SystemdService = SystemdService
  { unit :: UnitSection,
    service :: ServiceSection,
    install :: InstallSection
  }
  deriving (Data)

type Title = String

type KVPair = (String, String)

type Body = [KVPair]

data Section = Section Title Body

unknown = error "unknown type"

getFields :: (Data k) => k -> [String]
getFields = map upperHead . constrFields . toConstr

getValues :: (Data k) => k -> [String]
getValues = gmapQ tShow
  where
    tShow :: (Data d) => d -> String
    tShow = mkQ unknown id `extQ` (show @String)

getBody :: (Data d) => d -> Body
getBody = zip <$> getFields <*> getValues

getSections :: (Data d) => d -> [Section]
getSections = zipWith Section <$> getFields <*> gmapQ getBody

upperHead :: String -> String
upperHead "" = ""
upperHead (x : xs) = toUpper x : xs

sectionToStrings :: Section -> String
sectionToStrings (Section title body) = unlines $ ("[" ++ title ++ "]") : ((\(a, b) -> a ++ "=" ++ b) <$> body)

main :: IO ()
main =
  do
    let service =
          SystemdService
            { unit =
                UnitSection
                  { description = "Service created by hatty"
                  },
              service =
                ServiceSection
                  { workingDirectory = "sss",
                    -- For test use
                    execStart = "/usr/bin/cmatrix",
                    restart = "always",
                    standardOutput = "tty",
                    standardInput = "tty",
                    tTYPath = "/dev/%i"
                  },
              install =
                InstallSection
                  { wantedBy = "multi-user.target"
                  }
            }
    putStrLn . intercalate "\n" . map sectionToStrings . getSections $ service
