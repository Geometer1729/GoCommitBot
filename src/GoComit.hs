{-# Language LambdaCase #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-} -- this is temporary but the warnings are annoying as hell on unfinished code
import Control.Monad
import Control.Monad.State
import Data.Char
import Data.Text (pack,unpack,isPrefixOf)
import Discord
import Discord.Internal.Rest.Channel
import Discord.Internal.Types.Channel
import Discord.Internal.Types.Prelude
import Discord.Internal.Types.Events
import System.Random
import Token

main :: IO ()
main = runDiscord goCommit >>= print

goCommit :: RunDiscordOpts
goCommit = def {
   discordToken = pack goCommitToken
  ,discordOnStart = const $ putStrLn "start"
  ,discordOnEnd = putStrLn "end"
  ,discordOnEvent = goCommitEvent
  }

goCommitEvent :: DiscordHandle -> Event -> IO ()
goCommitEvent dh = \case
  MessageCreate m -> (liftRand $ plan dh m) >>= runPlan
  TypingStart ti -> do
    let ch = typingChannelId ti
    let ui = typingUserId ti
    -- don't start typing in response to own typing
    when (ui /= 496442504573616131) $ restCall dh (TriggerTypingIndicator ch) >> return ()
  _ -> return ()

data Plan = Nop | Enlighten Context User | Scold Context User | SuggestionLog Context Message | Mock Context User String
data Context = Context DiscordHandle Snowflake

plan :: DiscordHandle -> Message -> Rand Plan
plan dh m 
  | isPrefixOf (pack "!suggestion") contents = return $ SuggestionLog con m
  | userName author == pack "UwUBot" = fairChoice [Scold con author,Nop]
  | userName author == pack "GOCommitBot" = return Nop
  | or [ userName u == (pack "GOCommitBot") | u <- mentions ] = return $ Enlighten con author
  | otherwise = weightChoice [(1,Enlighten con author),(1,Mock con author (unpack contents)),(2,Nop)]
    where
      author = messageAuthor m
      contents = messageText m
      mentions = messageMentions m
      con = Context dh (messageChannel m)

runPlan :: Plan -> IO ()
runPlan Nop = return ()
runPlan (Enlighten con _) = do
  txt <- liftRand . runTemplate $ response
  simpleSay con txt
runPlan (Scold con _) = simpleSay con "Fuck you uwubot. You are distilled cancer."
runPlan (SuggestionLog con m) = do
        let 
          author = userName . messageAuthor $ m 
          contents = messageText m
        appendFile "logs" ("\n" ++ (unpack author) ++ ": " ++ (unwords . lines . drop 11 . unpack  $ contents) ++ "\n\t" ++ show m)
        simpleSay con "suggestion loged"
runPlan (Mock con u t) = do
  quote <- liftRand $ sponge t
  simpleSay con ("hurr I'm " ++ (unpack $ userName u) ++ " and " ++ quote)

simpleSay :: Context -> String -> IO ()
simpleSay (Context dh ch) txt = restCall dh (CreateMessage ch (pack txt)) >> return ()

type Rand = State StdGen

sponge :: String -> Rand String
sponge = mapM spongeChar
  where
    spongeChar :: Char -> Rand Char
    spongeChar c = do
      isCap <- state random :: Rand Bool
      return (if isCap then toUpper c else toLower c)

fairChoice :: [a] -> Rand a
fairChoice xs = do
  ind <- state $ randomR (0,length xs - 1)
  return $ xs !! ind

weightChoice :: [(Double,a)] -> Rand a
weightChoice xs = do
  let up = sum . map fst $ xs
  v <- state $ randomR (0,up)
  return $ pick v xs

pick :: Double -> [(Double,a)] -> a
pick v ((a,r):xs) = if v <= a then r else pick (v-a) xs
pick _ [] = error "pick called with invalid value"

atribute :: String -> Rand String
atribute msg = do
  auth <- getAuthor
  y <- year
  return $ concat ["```",msg,"``` - ",auth," (circa ",y,")"]

year :: Rand String
year = fmap show (state $  randomR (1500,2000) :: State StdGen Int)

bify :: String -> Rand String
bify ('b':w)     = fmap (":b:"  ++) (bify w)
bify ('B':w)     = fmap (":b:"  ++) (bify w)
bify (' ':'b':w) = fmap (" :b:" ++) (bify w)
bify (' ':'B':w) = fmap (" :b:" ++) (bify w)
bify (' ':c:w)  = do
  n <- state $ randomR (0,20) :: Rand Int
  if n /= 0 || (notbsuitable c)
    then fmap (\x -> ' ':c:x) (bify w)
    else fmap (" :b:" ++) (bify w)
bify (c:w) = fmap (c:) (bify w)
bify [] = return []

notbsuitable :: Char -> Bool
notbsuitable c = (not . elem c $ ['a'..'z'] ++ ['A'..'Z']) || elem c bads || elem c (map toUpper bads)
  where
    bads = "aeiousxyz"
--There is no real rhyme or reason to this list it's just based on experience of which letters when replaced make it unreadable

data Template = JT String | Choice [Template] | ChoiceWeight [(Double,Template)] | Lift (Rand String) | Join [Template] | Modifier (String -> Rand String) Template | Modifiers [(String -> Rand String)] Template

runTemplate :: Template -> Rand String
runTemplate (JT w)      = return w
runTemplate (Choice ts) = fairChoice ts >>= runTemplate
runTemplate (ChoiceWeight wts) = weightChoice wts >>= runTemplate
runTemplate (Lift r)    = r
runTemplate (Join ts)   = fmap unwords $ mapM runTemplate ts
runTemplate (Modifier f t) = runTemplate t >>= f
runTemplate (Modifiers fs t) = runTemplate (Modifier (foldl (>=>) return fs) t)

maybeMod :: (String -> Rand String) -> String -> Rand String
maybeMod f w = do
  m <- state random :: Rand Bool
  if m then f w else return w

liftRand :: Rand a -> IO a
liftRand s = let f = runState s in do
  g <- getStdGen
  let (x,g2) = f g 
  setStdGen g2
  return x

response :: Template
response = Modifiers [bify] $ Choice [
   Join [ JT "this is so" , adj , Choice [
     JT "alexa play despacito!"
    ,JT "can we live in a soceity?"
    ,Join [ JT "can we throw", sub , JT "against the wall?"]
    ,Join [ JT "can we" , verb , sub , JT "?" ]
    ]
   ]
  ,Join [ JT "Go commit", sudoku , JT "!"]
  ]

verb :: Template
verb = Choice . map JT $ [ 
  "hit",
  "say the N-word to",
  "wumbo",
  "cuck",
  "ligma",
  "boffa",
  "sugma",
  "chokonma",
  "kill",
  "dab on",
  "get 10 likes for",
  "commit literal genocide against",
  "show the emoji movie to",
  "Shrek",
  "abolish",
  "take a minute to appreciate",
  "Hadamard Hadamard Hadamard, Bam on",
  "sell heroin to",
  "do heroin with",
  "perform universal computation with",
  "ass-blast",
  "benevolently stab",
  "eat cake with",
  "socialy construct",
  "play fortnite with",
  "go whaling with",
  "go poach rhinos with",
  "farm" 
           ]


adj :: Template
adj = Choice . map JT $ [ 
  "nuclear",
  "well ordered",
  "Sad",
  "absolutely haram",
  "cool",
  "upsetting",
  "thin",
  "stupid",
  "kawaii",
  "inspirational",
  "turing complete",
  "offensive",
  "old",
  "jank",
  "Boneless",
  "cancer",
  "ligma",
  "boffa",
  "sugma",
  "chokonma",
  "problematic",
  "demeaning",
  "disheartening",
  "sublime",
  "np-complete" 
          ]

sub :: Template
sub = Choice . map JT $ [
  "peoplesoft",
  "cripiling fsu debt",
  "3 kids in a trench coat",
  "wumbology",
  "The consecutive niner",
  "Elsa",
  "a well ordering",
  "off brand Reese's puffs",
  "the sweet release of death",
  "loss",
  "horses in lava", -- this triggers dabby I guese
  "JOHN CENA",
  "a social construct",
  "Home stuck",
  "Tonny Collins",
  "vim-like key-bindings",
  "GOCommitBot",
  "Harambe (1999-2016 Rest In Peace)",
  "a six pack ring",
  "ligma",
  "boffa",
  "sugma",
  "chokonma",
  "Boop-$",
  "cancer",
  "nuclear cancer",
  "javascript",
  "women and minorities",
  "our boys in blue",
  "young children",
  "layers of complexity",
  "The penguins of madagascar",
  "gay frogs",
  "the gays",
  "the jews",
  "jewish conspiracies",
  "Warren Robinson (aka pi-guy)",
  "Ted Cruz (the Zodiac Killer)",
  "Richard Nixon",
  "Joseph Stalin",
  "Joseph Ballin'",
  "Jeb Bush",
  "Joseph Kony",
  "Vladimir Lenin",
  "Mao ZeDong",
  "Hitler",
  "God Emporor Kim Jong Un",
  "UwUBot" ,
  "Michelle Obama",
  "the military",
  "the subscribe button",
  "the holocaust",
  "the meme playlist",
  "a platonic ideal",
  "a rare pepe",
  "Gabe's dead cat Simon",
  "Nate's Communist propaganda",
  "Brian's topological dogma",
  "Reece's rock farm", 
  "sufferage",
  "democracy",
  "the fabric of reality",
  "soceity",
  "vietnam",
  "Despacito",
  "bottom text",
  "Monadic brain damage",
  "Osteoporosis",
  "the true meaning of christmas",
  "the false meaning of christmas",
  "10 kilos of heroin",
  "20 kilos of heroin"
      ] 

getAuthor :: Rand String
getAuthor = fairChoice [ 
  "Joseph Stalin",
  "Mao ZeDong",
  "Adof Hitler",
  "Mohotma Ghandi",
  "The Prophet Mohamed",
  "Martin Luther King J.R.",
  "Pope Francis",
  "Richard Stalman",
  "Martin Luther",
  "John McAfee",
  "Kim Jong Un",
  "Donald Trump",
  "Tonny Collins",
  "Harambe",
  "French Proverb",
  "The anti-christ",
  "God himself"     
                    ]

sudoku :: Template
sudoku = Choice [ JT "sudoku" , Join [first,second]]

first :: Template
first = Choice . map JT $ [ "rope", "toaster", "finna", "not", "death-pacito", "dab", "good bye"];

second :: Template
second = Choice . map JT $ [ "neck","bath", "woke", "alive", "breath", "the big gay", "cruel world","end it"]
