{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module GHCupTypes where

import           Control.DeepSeq                ( NFData )
import           Data.Map.Strict                ( Map )
import           Data.List.NonEmpty             ( NonEmpty (..) )
import           Data.Text                      ( Text )
import           Data.Versions
import           Control.Applicative            ( (<|>) )
import           Data.Aeson              hiding (Key)
import           Data.Aeson.TH
import           Data.Aeson.Types        hiding (Key)
import           Data.Functor
import           Data.Text.Encoding            as E
import           Data.Void

import qualified Data.List.NonEmpty            as NE
import qualified Data.Text                     as T
import qualified Text.Megaparsec               as MP
import qualified Text.Megaparsec.Char          as MPC
import qualified GHC.Generics                  as GHC


parseUntil :: MP.Parsec Void Text a -> MP.Parsec Void Text Text
parseUntil p = do
  (MP.try (MP.lookAhead p) $> mempty)
    <|> (do
          c  <- T.singleton <$> MP.anySingle
          c2 <- parseUntil p
          pure (c `mappend` c2)
        )

verP :: MP.Parsec Void Text Text -> MP.Parsec Void Text Versioning
verP suffix = do
  ver <- parseUntil suffix
  if T.null ver
    then fail "empty version"
    else do
      rest <- MP.getInput
      MP.setInput ver
      v <- versioning'
      MP.setInput rest
      pure v

data GHCupInfo = GHCupInfo
  { _toolRequirements :: ToolRequirements
  , _ghcupDownloads   :: GHCupDownloads
  , _globalTools      :: Map GlobalTool DownloadInfo
  }
  deriving (Show, Read, GHC.Generic)

instance NFData GHCupInfo

type ToolRequirements = Map Tool ToolReqVersionSpec
type ToolReqVersionSpec = Map (Maybe Version) PlatformReqSpec
type PlatformReqSpec = Map Platform PlatformReqVersionSpec
type PlatformReqVersionSpec = Map (Maybe VersionRange) Requirements


data Requirements = Requirements
  { _distroPKGs :: [Text]
  , _notes      :: Text
  }
  deriving (Show, Read, GHC.Generic)

instance NFData Requirements

-- | Description of all binary and source downloads. This is a tree
-- of nested maps.
type GHCupDownloads = Map Tool ToolVersionSpec
type ToolVersionSpec = Map Version VersionInfo
type ArchitectureSpec = Map Architecture PlatformSpec
type PlatformSpec = Map Platform PlatformVersionSpec
type PlatformVersionSpec = Map (Maybe VersionRange) DownloadInfo


-- | An installable tool.
data Tool = GHC
          | Cabal
          | GHCup
          | HLS
          | Stack
  deriving (Eq, GHC.Generic, Ord, Show, Read, Enum, Bounded)

instance NFData Tool

data GlobalTool = ShimGen
  deriving (Eq, GHC.Generic, Ord, Show, Read, Enum, Bounded)

instance NFData GlobalTool


-- | All necessary information of a tool version, including
-- source download and per-architecture downloads.
data VersionInfo = VersionInfo
  { _viTags        :: [Tag]              -- ^ version specific tag
  , _viChangeLog   :: Maybe Text
  , _viSourceDL    :: Maybe DownloadInfo -- ^ source tarball
  , _viArch        :: ArchitectureSpec   -- ^ descend for binary downloads per arch
  -- informative messages
  , _viPostInstall :: Maybe Text
  , _viPostRemove  :: Maybe Text
  , _viPreCompile  :: Maybe Text
  }
  deriving (Eq, GHC.Generic, Show, Read)

instance NFData VersionInfo

-- | A tag. These are currently attached to a version of a tool.
data Tag = Latest
         | Recommended
         | Prerelease
         | Base PVP
         | Old                -- ^ old versions are hidden by default in TUI
         | UnknownTag String  -- ^ used for upwardscompat
         deriving (Ord, Eq, GHC.Generic, Show, Read) -- FIXME: manual JSON instance

instance NFData Tag

data Architecture = A_64
                  | A_32
                  | A_PowerPC
                  | A_PowerPC64
                  | A_Sparc
                  | A_Sparc64
                  | A_ARM
                  | A_ARM64
  deriving (Eq, GHC.Generic, Ord, Show, Read)

instance NFData Architecture

data Platform = Linux LinuxDistro
              -- ^ must exit
              | Darwin
              -- ^ must exit
              | FreeBSD
              | Windows
              -- ^ must exit
  deriving (Eq, GHC.Generic, Ord, Show, Read)

instance NFData Platform

data LinuxDistro = Debian
                 | Ubuntu
                 | Mint
                 | Fedora
                 | CentOS
                 | RedHat
                 | Alpine
                 | AmazonLinux
                 -- rolling
                 | Gentoo
                 | Exherbo
                 -- not known
                 | UnknownLinux
                 -- ^ must exit
  deriving (Eq, GHC.Generic, Ord, Show, Read)

instance NFData LinuxDistro

data DownloadInfo = DownloadInfo
  { _dlUri    :: Text
  , _dlSubdir :: Maybe TarDir
  , _dlHash   :: Text
  }
  deriving (Eq, Ord, GHC.Generic, Show, Read)

instance NFData DownloadInfo

data TarDir = RealDir FilePath
            | RegexDir String     -- ^ will be compiled to regex, the first match will "win"
            deriving (Eq, Ord, GHC.Generic, Show, Read)

instance NFData TarDir

data URLSource = GHCupURL
               | OwnSource Text
               | OwnSpec GHCupInfo
               | AddSource (Either GHCupInfo Text) -- ^ merge with GHCupURL
               deriving (GHC.Generic, Show, Read)

instance NFData URLSource

data VersionRange = SimpleRange (NonEmpty VersionCmp) -- And
                  | OrRange (NonEmpty VersionCmp) VersionRange
  deriving (Eq, GHC.Generic, Ord, Show, Read)

instance NFData VersionRange

-- | A comparator and a version.
data VersionCmp = VR_gt Versioning
                | VR_gteq Versioning
                | VR_lt Versioning
                | VR_lteq Versioning
                | VR_eq Versioning
  deriving (Eq, GHC.Generic, Ord, Show, Read)

instance NFData VersionCmp

deriving instance Read Versioning
deriving instance Read Version
deriving instance Read PVP
deriving instance Read SemVer
deriving instance Read Mess
deriving instance Read MChunk
deriving instance Read VSep


deriveJSON defaultOptions { fieldLabelModifier = (\str' -> maybe str' T.unpack . T.stripPrefix (T.pack "_") . T.pack $ str') } ''Architecture
deriveJSON defaultOptions { fieldLabelModifier = (\str' -> maybe str' T.unpack . T.stripPrefix (T.pack "_") . T.pack $ str') } ''LinuxDistro
deriveJSON defaultOptions { fieldLabelModifier = (\str' -> maybe str' T.unpack . T.stripPrefix (T.pack "_") . T.pack $ str') } ''VSep
deriveJSON defaultOptions { fieldLabelModifier = (\str' -> maybe str' T.unpack . T.stripPrefix (T.pack "_") . T.pack $ str') } ''VUnit
deriveJSON defaultOptions { fieldLabelModifier = (\str' -> maybe str' T.unpack . T.stripPrefix (T.pack "_") . T.pack $ str') } ''MChunk
deriveJSON defaultOptions { fieldLabelModifier = (\str' -> maybe str' T.unpack . T.stripPrefix (T.pack "_") . T.pack $ str') } ''Platform
deriveJSON defaultOptions { fieldLabelModifier = (\str' -> maybe str' T.unpack . T.stripPrefix (T.pack "_") . T.pack $ str') } ''Mess
deriveJSON defaultOptions { fieldLabelModifier = (\str' -> maybe str' T.unpack . T.stripPrefix (T.pack "_") . T.pack $ str') } ''SemVer
deriveJSON defaultOptions { fieldLabelModifier = (\str' -> maybe str' T.unpack . T.stripPrefix (T.pack "_") . T.pack $ str') } ''Tool
deriveJSON defaultOptions { fieldLabelModifier = (\str' -> maybe str' T.unpack . T.stripPrefix (T.pack "_") . T.pack $ str') } ''GlobalTool

instance ToJSON Tag where
  toJSON Latest             = String "Latest"
  toJSON Recommended        = String "Recommended"
  toJSON Prerelease         = String "Prerelease"
  toJSON Old                = String "old"
  toJSON (Base       pvp'') = String ("base-" <> prettyPVP pvp'')
  toJSON (UnknownTag x    ) = String (T.pack x)

instance FromJSON Tag where
  parseJSON = withText "Tag" $ \t -> case T.unpack t of
    "Latest"                             -> pure Latest
    "Recommended"                        -> pure Recommended
    "Prerelease"                         -> pure Prerelease
    "old"                                -> pure Old
    ('b' : 'a' : 's' : 'e' : '-' : ver') -> case pvp (T.pack ver') of
      Right x -> pure $ Base x
      Left  e -> fail . show $ e
    x -> pure (UnknownTag x)

instance ToJSON Versioning where
  toJSON = toJSON . prettyV

instance FromJSON Versioning where
  parseJSON = withText "Versioning" $ \t -> case versioning t of
    Right x -> pure x
    Left  e -> fail $ "Failure in Version (FromJSON)" <> show e

instance ToJSONKey Versioning where
  toJSONKey = toJSONKeyText $ \x -> prettyV x

instance FromJSONKey Versioning where
  fromJSONKey = FromJSONKeyTextParser $ \t -> case versioning t of
    Right x -> pure x
    Left  e -> fail $ "Failure in Versioning (FromJSONKey)" <> show e

instance ToJSONKey (Maybe Versioning) where
  toJSONKey = toJSONKeyText $ \case
    Just x  -> prettyV x
    Nothing -> T.pack "unknown_versioning"

instance FromJSONKey (Maybe Versioning) where
  fromJSONKey = FromJSONKeyTextParser $ \t ->
    if t == T.pack "unknown_versioning" then pure Nothing else just t
   where
    just t = case versioning t of
      Right x -> pure $ Just x
      Left  e -> fail $ "Failure in (Maybe Versioning) (FromJSONKey)" <> show e

instance ToJSONKey Platform where
  toJSONKey = toJSONKeyText $ \case
    Darwin  -> T.pack "Darwin"
    FreeBSD -> T.pack "FreeBSD"
    Linux d -> T.pack ("Linux_" <> show d)
    Windows -> T.pack "Windows"

instance FromJSONKey Platform where
  fromJSONKey = FromJSONKeyTextParser $ \t -> if
    | T.pack "Darwin" == t -> pure Darwin
    | T.pack "FreeBSD" == t -> pure FreeBSD
    | T.pack "Windows" == t -> pure Windows
    | T.pack "Linux_" `T.isPrefixOf` t -> case
        T.stripPrefix (T.pack "Linux_") t
      of
        Just dstr ->
          case
              (decodeStrict (E.encodeUtf8 (T.pack "\"" <> dstr <> T.pack "\"")) :: Maybe
                  LinuxDistro
              )
            of
              Just d -> pure $ Linux d
              Nothing ->
                fail
                  $  "Unexpected failure in decoding LinuxDistro: "
                  <> show dstr
        Nothing -> fail "Unexpected failure in Platform stripPrefix"
    | otherwise -> fail "Failure in Platform (FromJSONKey)"

instance ToJSONKey Architecture where
  toJSONKey = genericToJSONKey defaultJSONKeyOptions

instance FromJSONKey Architecture where
  fromJSONKey = genericFromJSONKey defaultJSONKeyOptions

instance ToJSONKey (Maybe Version) where
  toJSONKey = toJSONKeyText $ \case
    Just x  -> prettyVer x
    Nothing -> T.pack "unknown_version"

instance FromJSONKey (Maybe Version) where
  fromJSONKey = FromJSONKeyTextParser $ \t ->
    if t == T.pack "unknown_version" then pure Nothing else just t
   where
    just t = case version t of
      Right x -> pure $ Just x
      Left  e -> fail $ "Failure in (Maybe Version) (FromJSONKey)" <> show e

instance ToJSON Version where
  toJSON = toJSON . prettyVer

instance FromJSON Version where
  parseJSON = withText "Version" $ \t -> case version t of
    Right x -> pure x
    Left  e -> fail $ "Failure in Version (FromJSON)" <> show e

instance ToJSONKey Version where
  toJSONKey = toJSONKeyText $ \x -> prettyVer x

instance FromJSONKey Version where
  fromJSONKey = FromJSONKeyTextParser $ \t -> case version t of
    Right x -> pure x
    Left  e -> fail $ "Failure in Version (FromJSONKey)" <> show e

instance ToJSON PVP where
  toJSON = toJSON . prettyPVP

instance FromJSON PVP where
  parseJSON = withText "PVP" $ \t -> case pvp t of
    Right x -> pure x
    Left  e -> fail $ "Failure in PVP (FromJSON)" <> show e

instance ToJSONKey Tool where
  toJSONKey = genericToJSONKey defaultJSONKeyOptions

instance FromJSONKey Tool where
  fromJSONKey = genericFromJSONKey defaultJSONKeyOptions

instance ToJSONKey GlobalTool where
  toJSONKey = genericToJSONKey defaultJSONKeyOptions

instance FromJSONKey GlobalTool where
  fromJSONKey = genericFromJSONKey defaultJSONKeyOptions

instance ToJSON TarDir where
  toJSON (RealDir  p) = toJSON p
  toJSON (RegexDir r) = object ["RegexDir" .= r]

instance FromJSON TarDir where
  parseJSON v = realDir v <|> regexDir v
   where
    realDir = withText "TarDir" $ \t -> do
      fp <- parseJSON (String t)
      pure (RealDir fp)
    regexDir = withObject "TarDir" $ \o -> do
      r <- o .: "RegexDir"
      pure $ RegexDir r


instance ToJSON VersionCmp where
  toJSON = String . versionCmpToText

instance FromJSON VersionCmp where
  parseJSON = withText "VersionCmp" $ \t -> do
    case MP.parse versionCmpP "" t of
      Right r -> pure r
      Left  e -> fail (MP.errorBundlePretty e)

versionCmpToText :: VersionCmp -> T.Text
versionCmpToText (VR_gt   ver') = "> " <> prettyV ver'
versionCmpToText (VR_gteq ver') = ">= " <> prettyV ver'
versionCmpToText (VR_lt   ver') = "< " <> prettyV ver'
versionCmpToText (VR_lteq ver') = "<= " <> prettyV ver'
versionCmpToText (VR_eq   ver') = "== " <> prettyV ver'

versionCmpP :: MP.Parsec Void T.Text VersionCmp
versionCmpP =
  fmap VR_gt (MP.try $ MPC.space *> MP.chunk ">" *> MPC.space *> versioningEnd)
    <|> fmap
          VR_gteq
          (MP.try $ MPC.space *> MP.chunk ">=" *> MPC.space *> versioningEnd)
    <|> fmap
          VR_lt
          (MP.try $ MPC.space *> MP.chunk "<" *> MPC.space *> versioningEnd)
    <|> fmap
          VR_lteq
          (MP.try $ MPC.space *> MP.chunk "<=" *> MPC.space *> versioningEnd)
    <|> fmap
          VR_eq
          (MP.try $ MPC.space *> MP.chunk "==" *> MPC.space *> versioningEnd)
    <|> fmap
          VR_eq
          (MP.try $ MPC.space *> versioningEnd)

instance ToJSON VersionRange where
  toJSON = String . verRangeToText

verRangeToText :: VersionRange -> T.Text
verRangeToText  (SimpleRange cmps) =
  let inner = foldr1 (\x y -> x <> " && " <> y)
                     (versionCmpToText <$> NE.toList cmps)
  in  "( " <> inner <> " )"
verRangeToText (OrRange cmps range) =
  let left  = verRangeToText (SimpleRange cmps)
      right = verRangeToText range
  in  left <> " || " <> right

instance FromJSON VersionRange where
  parseJSON = withText "VersionRange" $ \t -> do
    case MP.parse versionRangeP "" t of
      Right r -> pure r
      Left  e -> fail (MP.errorBundlePretty e)

versionRangeP :: MP.Parsec Void T.Text VersionRange
versionRangeP = go <* MP.eof
 where
  go =
    MP.try orParse
      <|> MP.try (fmap SimpleRange andParse)
      <|> fmap (SimpleRange . pure) versionCmpP

  orParse :: MP.Parsec Void T.Text VersionRange
  orParse =
    (\a o -> OrRange a o)
      <$> (MP.try andParse <|> fmap pure versionCmpP)
      <*> (MPC.space *> MP.chunk "||" *> MPC.space *> go)

  andParse :: MP.Parsec Void T.Text (NonEmpty VersionCmp)
  andParse =
    fmap (\h t -> h :| t)
         (MPC.space *> MP.chunk "(" *> MPC.space *> versionCmpP)
      <*> MP.try (MP.many (MPC.space *> MP.chunk "&&" *> MPC.space *> versionCmpP))
      <*  MPC.space
      <*  MP.chunk ")"
      <*  MPC.space

versioningEnd :: MP.Parsec Void T.Text Versioning
versioningEnd =
  MP.try (verP (MP.chunk " " <|> MP.chunk ")" <|> MP.chunk "&&") <* MPC.space)
    <|> versioning'

instance ToJSONKey (Maybe VersionRange) where
  toJSONKey = toJSONKeyText $ \case
    Just x -> verRangeToText x
    Nothing -> "unknown_versioning"

instance FromJSONKey (Maybe VersionRange)  where
  fromJSONKey = FromJSONKeyTextParser $ \t ->
    if t == T.pack "unknown_versioning" then pure Nothing else just t
   where
    just t = case MP.parse versionRangeP "" t of
      Right x -> pure $ Just x
      Left  e -> fail $ "Failure in (Maybe VersionRange) (FromJSONKey)" <> MP.errorBundlePretty e



deriveJSON defaultOptions { fieldLabelModifier = (\str' -> maybe str' T.unpack . T.stripPrefix (T.pack "_") . T.pack $ str') } ''Requirements
deriveJSON defaultOptions { fieldLabelModifier = (\str' -> maybe str' T.unpack . T.stripPrefix (T.pack "_") . T.pack $ str') } ''DownloadInfo
deriveJSON defaultOptions { fieldLabelModifier = (\str' -> maybe str' T.unpack . T.stripPrefix (T.pack "_") . T.pack $ str') } ''VersionInfo
deriveJSON defaultOptions { fieldLabelModifier = (\str' -> maybe str' T.unpack . T.stripPrefix (T.pack "_") . T.pack $ str') } ''GHCupInfo
deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField } ''URLSource

