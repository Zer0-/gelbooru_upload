module Types where

import Data.Time.Clock (UTCTime)

data Attachment = Attachment
    { attachmentFilename :: String
    , attachmentUrl :: String
    , thumbnailUrl :: String
    } deriving Show

data PostPart
    = SimpleText String
    | PostedUrl String
    | Skip
    | Quote String
        -- Quotes don't seem to be able to be spoilered
        -- board links (which appear as quotes but start with >>>) break the tag
    | GreenText     [ PostPart ]
    | OrangeText    [ PostPart ]
    | RedText       [ PostPart ]
    | Spoiler       [ PostPart ]
    -- you can't seem to spoiler greentext
    | Bold          [ PostPart ]
    | Underlined    [ PostPart ]
    | Italics       [ PostPart ]
    | Strikethrough [ PostPart ]
    deriving Show

data Post = Post
    { attachments :: [ Attachment ]
    , subject :: Maybe String
    , email :: Maybe String
    , name :: String
    , postNumber :: Int
    , postBody :: [ PostPart ]
    , timestamp :: UTCTime
    } deriving Show

type PostId = (String, Int) -- String is the board name

type PostWithDeps = (Post, PostId, [ PostId ]) -- post, thread identifier, dependencies ids
