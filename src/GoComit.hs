{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}
import Data.Text (pack,unpack,isPrefixOf)
import System.Process

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
  ,discordOnEnd = putStrLn "end" >> callCommand "cabal install && dist/dist-sandbox-1ed89d9e/build/goComit/goComit"
  ,discordOnEvent = goCommitEvent
  }

goCommitEvent :: DiscordHandle -> Event -> IO ()
goCommitEvent dh = \case
  MessageCreate m -> (liftRand $ plan dh m) >>= runPlan
  TypingStart ti -> do
    let ch = typingChannelId ti
    restCall dh (TriggerTypingIndicator ch) >> return ()
  _ -> return ()

data Plan = Nop | Enlighten Context User | Scold Context User | Correct Context String User | Mock Context String User | SuggestionLog Context Message | Restart DiscordHandle
data Context = Context DiscordHandle Snowflake

plan :: DiscordHandle -> Message -> Rand Plan
plan dh m 
  | mentioned && isPrefixOf "!suggestion" contents = return $ SuggestionLog con m
  | mentioned &&mentioned &&  isPrefixOf "!restart" contents = return $ Restart dh
  | userName author == "UwUBot" = fairChoice [Scold con author,Nop, Nop ]
  | userName author == "GOCommitBot" = return Nop
  | or [ head w == 'b' || head w == 'B' | w <- words (unpack contents) ] = return $ Correct con (unpack contents) author
  | mentioned = return $ Enlighten con author
  | otherwise = weightChoice [(1,Enlighten con author),(2,Mock con (unpack contents) author),(2,Nop)]
    where
      author = messageAuthor m
      contents = messageText m
      mentions = messageMentions m
      mentioned = or [ userName u == "GOCommitBot" | u <- mentions ]
      con = Context dh (messageChannel m)

runPlan :: Plan -> IO ()
runPlan Nop = return ()
runPlan (Enlighten con _) = execTemplate wisdom >>= simpleSay con
runPlan (Scold con _) = execTemplate scold >>= simpleSay con
runPlan (SuggestionLog con m) = do
        let 
          author = userName . messageAuthor $ m 
          contents = messageText m
        appendFile "logs" ("\n" ++ (unpack author) ++ ": " ++ (unwords . lines . drop 11 . unpack  $ contents) ++ "\n\t" ++ show m)
        simpleSay con "suggestion loged"
runPlan (Mock con msg u) = do
  msg' <- liftRand $ sponge msg
  simpleSay con $ "hurr I'm " ++ (unpack . userName $ u) ++ " and " ++ msg'
runPlan (Correct con msg _) = sequence [ simpleSay con $ "*:b:" ++ (tail w) | w <- words msg , head w == 'b' || head w == 'B' ] >> return ()
runPlan (Restart dh) = stopDiscord dh

simpleSay :: Context -> String -> IO ()
simpleSay (Context dh ch) txt = restCall dh (CreateMessage ch (pack txt)) >>= print

