module Main where


import Network.Socket
import System.IO
import RSA
import StringMod
import Control.Concurrent
import Control.Concurrent.Chan


main = do
		print createFunctions
		print "start server"
		sok <- socket AF_INET Stream 0
		ioChan <- newChan
		forkIO $ ioConsoleWriter ioChan
		bind sok (SockAddrInet 6666 iNADDR_ANY )
		listen sok 10
		handleNetwork sok ioChan  

handleNetwork sok ioChan = do
					client <- accept sok
					forkIO $ handleClient client ioChan 
					handleNetwork sok ioChan 

handleClient client ioChan = do
					handler  <- socketToHandle (fst client) ReadWriteMode
					hPutStr handler $ ((show (pubInt createFunctions)) ++ "\n"++(show (prod createFunctions))  ++ "\n") 
					content <- hGetLine handler
					writeChan ioChan ((show (snd client)) ++ "\n \t wrote:\t" ++ content ++ "\n \t with message:\t " ++ (decrypt content) )
					where	decrypt content = encStr $ map (priv createFunctions ) ((read content)::[Integer])

ioConsoleWriter chan = do
						write <- readChan chan
						putStr write
						putStr "\n\n"
						ioConsoleWriter chan