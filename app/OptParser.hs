module OptParser
  ( hattyOptionsParser,
    hattyOptionsParserInfo,
    HattyOptions (..),
  )
where

import Options.Applicative

data HattyOptions = HattyOptions
  { _desc :: String,
    _tty :: String,
    _template :: Bool,
    _serviceName :: String,
    _force :: Bool,
    _user :: Bool,
    _enable :: Bool,
    _start :: Bool,
    _exec :: String
  }
  deriving (Show)

hattyOptionsParser :: Parser HattyOptions
hattyOptionsParser =
  HattyOptions
    <$> strOption
      ( long "desc"
          <> short 'd'
          <> metavar "Description"
          <> value "A service created by Hatty"
          <> showDefault
      )
    <*> strOption
      ( long "tty"
          <> short 't'
          <> metavar "TTY"
          <> help
            ( "The TTY to bind to. Must specify if `enable` or `start` is set or `template` is not set;"
                ++ " if `template` is set and both `enable` or `start` is not set, this option makes no sense"
            )
          <> value ""
      )
    <*> switch
      ( long "template"
          <> short 'p'
          <> help "If set, creates a template instead"
      )
    <*> strOption
      ( long "service-name"
          <> short 'n'
          <> metavar "SERVICE_NAME"
          <> help "The name of the service to create. If not set, use the filename of `EXEC`"
          <> value ""
      )
    <*> switch
      ( short 'f'
          <> long "force"
          <> help "If specified, use `tty-force` instead of `tty`"
      )
    <*> switch
      ( short 'u'
          <> long "user"
          <> help "If specified, the service is created as a user service instead of system service"
      )
    <*> switch
      ( short 'e'
          <> long "enabled"
          <> help "If specified, enable the service immediately"
      )
    <*> switch
      ( long "start"
          <> short 's'
          <> help "If specified, start the service immediately"
      )
    <*> argument
      str
      ( metavar "EXEC"
          <> help "The path of the file to execute."
      )

hattyOptionsParserInfo :: ParserInfo HattyOptions
hattyOptionsParserInfo = info (hattyOptionsParser <**> helper) fullDesc
