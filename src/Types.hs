module Types where

data Attachment = Attachment
    { attachmentFilename :: String
    , attachmentUrl :: String
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
    } deriving Show

type PostWithDeps = (Post, [ Int ])