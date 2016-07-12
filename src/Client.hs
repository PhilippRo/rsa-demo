import Network
import System.IO
import RSA
import StringMod

main = do
		handler <- connectTo "localhost" $ PortNumber 6666
		print "Client start; enter message"
		mssg <- getLine
		eStr <- hGetLine handler
		pStr <- hGetLine handler 
		print "crypt and send ..."
		hPutStr handler $ show (cryp (read eStr ::Integer)(read pStr ::Integer)( mssg)) ++ "\n"
		where cryp e n mssg = map (\x -> mod (x^e) n) (decStr mssg)

