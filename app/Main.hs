{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad
import Data.Char
import Data.Generics
import Data.List
import OptParser
import Options.Applicative (execParser)
import System.FilePath
import System.Process

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

unknown :: a
unknown = error "unknown type"

getFields :: (Data k) => k -> [String]
getFields = map upperHead . constrFields . toConstr

getValues :: (Data k) => k -> [String]
getValues = gmapQ tShow
  where
    tShow :: (Data d) => d -> String
    tShow = mkQ unknown id `extQ` (id @String)

getBody :: (Data d) => d -> Body
getBody = zip <$> getFields <*> getValues

getSections :: (Data d) => d -> [Section]
getSections = zipWith Section <$> getFields <*> gmapQ getBody

upperHead :: String -> String
upperHead "" = ""
upperHead (x : xs) = toUpper x : xs

sectionToStrings :: Section -> String
sectionToStrings (Section title body) =
  unlines $
    ("[" ++ title ++ "]")
      : (map (\(k, v) -> k ++ "=" ++ v) . filter (not . null . snd)) body

execute :: [String] -> IO ()
execute (x : xs) = callProcess x xs
execute [] = error "You shouldn't see this"

main :: IO ()
main =
  do
    option <- execParser hattyOptionsParserInfo
    let tty = _tty option
    let user = _user option
    let exec = _exec option
    let start = _start option
    let enable = _enable option
    let template = _template option
    let stdio = if _force option then "tty-force" else "tty"
    when ((not template || enable || start) && null tty) $ error "TTY must be set"

    let ttyPath = "/dev/" <> if template then "%i" else tty
    let baseServiceName = if null name then takeFileName exec else name where name = _serviceName option
    let serviceName = baseServiceName ++ (if template then "@" else "") ++ ".service"
    let serviceToExec = baseServiceName ++ (if template then "@" ++ tty else "") ++ ".service"

    let filePath =
          "/usr/lib/systemd/"
            ++ (if user then "user" else "system")
            ++ "/"
            ++ serviceName
    let service =
          SystemdService
            { unit =
                UnitSection
                  { description = _desc option
                  },
              service =
                ServiceSection
                  { workingDirectory = "",
                    execStart = exec,
                    restart = "always",
                    standardOutput = stdio,
                    standardInput = stdio,
                    tTYPath = ttyPath
                  },
              install =
                InstallSection
                  { wantedBy = if user then "default.target" else "multi-user.target"
                  }
            }

    putStrLn ("Create " ++ filePath)
    writeFile filePath (intercalate "\n" . map sectionToStrings . getSections $ service)

    let execSystemCtl action =
          putStrLn ("Executing: " ++ unwords execArgs)
            >> execute execArgs
          where
            execArgs = "systemctl" : ["--user" | user] ++ [action, serviceToExec]
    when enable $ execSystemCtl "enable"
    when start $ execSystemCtl "start"
