{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}
import qualified Data.Text as T
import System.Process

import Token
import Phrase

import Discord

import Discord.Internal.Rest.Channel
import Discord.Internal.Rest.Guild

import Discord.Internal.Types.Channel
import Discord.Internal.Types.Prelude
import Discord.Internal.Types.Events
import Discord.Internal.Types.Guild

main :: IO ()
main = runDiscord goCommit >>= print

goCommit :: RunDiscordOpts
goCommit = def {
   discordToken = T.pack goCommitToken
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

data Plan = Nop 
  | Enlighten Context User 
  | Scold Context User 
  | Correct Context String User 
  | Mock Context GuildId String User 
  | SuggestionLog Context Message 
  | Restart DiscordHandle 
  | Src Context

data Context = Context DiscordHandle Snowflake

plan :: DiscordHandle -> Message -> Rand Plan
plan dh m 
  | userName author == "UwUBot" = fairChoice [Scold con author,Nop, Nop ]
  | or [ head w == 'b' || head w == 'B' | w <- words (T.unpack contents) ] 
                                     = return $ Correct con (T.unpack contents) author
  | userName author == "GOCommitBot" = return Nop
  | isCmd "suggestion"               = return $ SuggestionLog con m
  | isCmd "restart"                  = return $ Restart dh
  | isCmd "src"                      = return $ Src con
  | mentioned                        = return $ Enlighten con author
  | otherwise = weightChoice [(1,Enlighten con author),(2,Mock con gid (T.unpack contents) author),(2,Nop)]
    where
      author = messageAuthor m
      contents = messageText m
      mentions = messageMentions m
      mentioned = or [ userName u == "GOCommitBot" | u <- mentions ]
      con = Context dh (messageChannel m)
      Just gid = messageGuild m
      isCmd :: T.Text -> Bool
      isCmd t = mentioned && T.isPrefixOf ( T.concat ["!",t]) contents

runPlan :: Plan -> IO ()
runPlan Nop = return ()
runPlan (Enlighten con _) = execTemplate wisdom >>= simpleSay con
runPlan (Scold con _) = execTemplate scold >>= simpleSay con
runPlan (SuggestionLog con m) = do
        let 
          author = userName . messageAuthor $ m 
          contents = messageText m
        appendFile "logs" ("\n" ++ (T.unpack author) ++ ": " ++ (unwords . lines . drop 11 . T.unpack  $ contents) ++ "\n\t" ++ show m)
        simpleSay con "suggestion loged"
runPlan (Mock con gid msg u) = do
  let Context dh _ = con
  msg' <- liftRand $ sponge msg
  Right gu <- restCall dh (GetGuildMember gid (userId u))
  let nick = maybe (userName u) id (memberNick gu)
  simpleSay con $ "hurr I'm " ++ (T.unpack nick) ++ " and " ++ msg'
runPlan (Correct con msg _) = sequence [ simpleSay con $ "*:b:" ++ (tail w) | w <- words msg , head w == 'b' || head w == 'B' ] >> return ()
runPlan (Restart dh) = stopDiscord dh
runPlan (Src con) = simpleSay con "https://github.com/Geometer1729/GoCommitBot"

simpleSay :: Context -> String -> IO ()
simpleSay (Context dh ch) txt = restCall dh (CreateMessage ch (T.pack txt)) >>= print

