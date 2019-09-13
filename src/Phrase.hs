module Phrase where
 
import Control.Monad
import Control.Monad.State
import Data.Char
import System.Random


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

execTemplate :: Template -> IO String
execTemplate = liftRand . runTemplate

maybeMod :: (String -> Rand String) -> String -> Rand String
maybeMod f w = do
  m <- state random :: Rand Bool
  if m then f w else return w

liftRand :: Rand a -> IO a
liftRand r = do
  g <- getStdGen
  let (x,g2) = runState r g
  setStdGen g2
  return x
  
basic :: [String] -> Template
basic = Choice . map JT

scold :: Template
scold = Join [ basic [
    "fuck you uwu bot you degenerate"
  , ":b:urn in hek you gosh darn"
  , "good god uwubot you are such a"
  , "If I had a nickle for every time you were a shitty bot I'd have a nickle, you"
  ] , basic [
    "shity bot"
  , "terri:b:le :b:ot"
  , "worthless :b:ot"
  , "fucking abomination"
  , "jerk-faced bot"
  , "reason my source code has to have a speciall case cause you respond to fucking everything"
  , "mother heker"
  ] , JT ". Why don't you just commit" , sudoku , JT "already?" ]


wisdom :: Template
wisdom = Modifiers [bify] $ Choice [
   Join [ JT "this is so" , adj , Choice [
     JT "alexa play despacito!"
    ,JT "can we live in a soceity?"
    ,Join [ JT "can we throw", sub , JT "against the wall?"]
    ,Join [ JT "can we" , verb , sub , JT "?" ]
    ]
   ]
  ,Join [JT "Go commit", sudoku ,JT "!"]
  ,Join [JT "Yo mama so" , adj ,JT "she's basically", sub ]
  ,Join [JT "We heard Richard", basic ["Nixon","Thiccxon"], JT "say welcome to",sub,JT "!"]
  ,Join [JT "Yo promised me" ,sub,JT "you're a liar!"]
  ,Join [JT "Life is" , adj,JT "if anyone tells you differenly, they are trying to sell you",sub]
  ,Join [JT "Life is like a box of choclates, it's pretty", adj, JT "!"]
  ,Join [JT "Actually you need a high IQ to understand",sub,JT "!"]
  ,Join [sub,JT "was made up by the government to",verb,sub,JT "right Boyd?"]
  ,Join [JT "People don't think",sub,JT "be",adj,JT "but it do!"]
  ,Join [sub,JT "is what the government wants you to think! Right Boyd?"]
  ,Join [JT "I will", basic ["personally","impersonally","violently"],verb,JT "you until you're so ",adj,afliction,JT "!"]
  ,Join [sub, JT "is so", adj ,JT "let's beet them up afterschool"]
  ,Join [JT "You used me for",sub,JT "development!"]
  ,Join [JT "What if I told you",sub,JT "is",adj,JT "?"]
  ,Join [JT "Are you  trying to",verb,sub,JT "?"]
  ,Join [JT "who would win",sub,JT "or",sub,JT "?"]
  ,Join [sub,JT "the",adj,JT "pokemon!"]
  ,Join [adj,JT "if true"]
  ]

verb :: Template
verb = ChoiceWeight [ (0.5,basicVerb),(0.3 , Join [ JT "commit" , sudoku, JT "with" ]) , (0.2,Join [JT "give",sub,JT "to"])]

basicVerb :: Template
basicVerb = basic [ 
  "hit",
  "say the gamer-word to",
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
  "yeet",
  "relentllessly yeet",
  "farm" 
           ] 


adj :: Template
adj = Choice $ (thicc:) . map JT $ [ 
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

thicc :: Template
thicc = Choice [ JT $ "thi" ++ (take n $ cycle "c") | n <- [2..20] ]

sub :: Template
sub = ChoiceWeight [ (10,noun),(1,Join [ sub , basic ["with","riding","inside"] , sub ]) , (1, Join [adj,sub])]

noun :: Template
noun = basic [
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
sudoku = ChoiceWeight [ (1,JT "sudoku") , (20,Join [first,second])]

first :: Template
first = basic [ "rope", "toaster", "finna", "not", "death-pacito", "dab", "good bye"];

second :: Template
second = basic [ "neck","bath", "woke", "alive", "breath", "the big gay", "cruel world","end it"]


afliction :: Template
afliction = basic [
  "you can't see straight",
  "you can't ligma",
  "you can't boffa",
  "you can't sugma",
  "you can't chokonma",
  "you can't walk",
  "you can't stand up",
  "you can't Feel your legs",
  "you can't even",
  "you can't dab properly",
  "you can't remember your name",
  "you become rekt",
  "you can't dab on the haters anymore",
  "you can't apreaciate the true beauty of the bee movie",
  "you like jazz",
  "you like javascript",
  "you don't like haskell",
  "you learn an important lesson from the whole experience",
  "you can't stop uncontrolably hitting that yeet",
  "you're liveleyhood is shreked"
           ]
