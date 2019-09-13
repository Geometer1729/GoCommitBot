{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}
import Data.Text (pack,unpack,isPrefixOf)
import Token
import Phrase

import Discord

import Discord.Internal.Rest.Channel

import Discord.Internal.Types.Channel
import Discord.Internal.Types.Prelude
import Discord.Internal.Types.Events

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
    restCall dh (TriggerTypingIndicator ch) >> return ()
  _ -> return ()

data Plan = Nop | Enlighten Context User | Scold Context User | SuggestionLog Context Message 
data Context = Context DiscordHandle Snowflake

plan :: DiscordHandle -> Message -> Rand Plan
plan dh m 
  | isPrefixOf "!suggestion" contents = return $ SuggestionLog con m
  | userName author == "UwUBot" = fairChoice [Scold con author,Nop, Nop ]
  | userName author == "GOCommitBot" = return Nop
  | or [ userName u == "GOCommitBot" | u <- mentions ] = return $ Enlighten con author
  | otherwise = weightChoice [(1,Enlighten con author),(2,Nop)]
    where
      author = messageAuthor m
      contents = messageText m
      mentions = messageMentions m
      con = Context dh (messageChannel m)

runPlan :: Plan -> IO ()
runPlan Nop = return ()
runPlan (Enlighten con _) = do
  putStrLn "enlighten" 
  msg <- liftRand . runTemplate $ wisdom
  putStrLn msg 
  simpleSay con msg
runPlan (Scold con _) = simpleSay con "Fuck you uwubot. You are distilled cancer."
runPlan (SuggestionLog con m) = do
        let 
          author = userName . messageAuthor $ m 
          contents = messageText m
        appendFile "logs" ("\n" ++ (unpack author) ++ ": " ++ (unwords . lines . drop 11 . unpack  $ contents) ++ "\n\t" ++ show m)
        simpleSay con "suggestion loged"

simpleSay :: Context -> String -> IO ()
simpleSay (Context dh ch) txt = restCall dh (CreateMessage ch (pack txt)) >>= print

