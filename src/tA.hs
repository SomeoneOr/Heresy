{-# LANGUAGE DoAndIfThenElse #-}
module TA where
import Text.Read
ang1 (x1,y1) (x2,y2) l (x,y) (ex,ey)= 
  if ((x1 == x) && (y1 == y)) || ((x1 == ex)&&(y1 == ey)) then [] else if x1 == x2 then if y1 == y2 then ((x1,y1):l) else if y1 > y2 then ang1 (x1,y1-1) (x2,y2) ((x1,y1):l) (x,y) (ex,ey) else ang1 (x1,y1+1) (x2,y2) ((x1,y1):l) (x,y) (ex,ey) else if (x1 > x2) then ang1 (x1-1,y1) (x2,y2) ((x1,y1):l) (x,y) (ex,ey) else ang1 (x1+1,y1) (x2,y2) ((x1,y1):l) (x,y) (ex,ey)
st = do
  print $ "start x ="
  sx <- getLine
  print $ "start y ="
  sy <- getLine
  print $ "finish x="
  fx <- getLine
  print $ "finish y="
  fy <- getLine
  print $ "angle x="
  anx1 <- getLine
  print $ "angle y="
  any1 <- getLine
  print $ "angle x="
  anx2 <- getLine
  print $ "angle y="
  any2 <- getLine
  n<-sec sx sy fx fy anx1 any1 anx2 any2
  print n
sec sx sy fx fy anx1 any1 anx2 any2= do
    let mx = readMaybe sx ::Maybe Int
    case mx of
      Just x -> do
        let my = readMaybe sy ::Maybe Int
        case my of
          Just y -> do
            let mex = readMaybe fx ::Maybe Int
            case mex of
              Just ex -> do
                let mey = readMaybe fy ::Maybe Int
                case mey of
                  Just ey -> do
                    let max1 = readMaybe anx1 ::Maybe Int
                    case max1 of
                      Just ax1 -> do
                        let may1 = readMaybe any1 ::Maybe Int
                        case may1 of
                          Just ay1 -> do
                            let max2 = readMaybe anx2 ::Maybe Int
                            case max2 of
                              Just ax2 -> do
                                let may2 = readMaybe any2 ::Maybe Int
                                case may2 of
                                  Just ay2 -> do
                                    if x > a || x < 0 || y > a || y <0 || ex > a || ex <0 || ey > a|| ey <0 || ax1 > a || ax1 < 0 || ay1 > a || ay1 < 0 || ax2 > a || ax2 <0 || ay2 >a || ay2 <0 then return(False,[]) else do
                                      let up = ang1 (ax1,ay1) (ax2,ay2) [] (x,y) (ex,ey)
                                      if up == [] then do
                                        return (True,[])
                                      else do
                                        return (True,sommonheresy [[(x,y)]] up (ex,ey))
                                  Nothing -> return(False,[])
                              Nothing -> return(False,[])
                          Nothing -> return(False,[])
                      Nothing -> return(False,[])
                  Nothing -> return(False,[])
              Nothing -> return(False,[])
          Nothing -> return(False,[])
--    (readLn :: IO Int) >>= \n -> mapM_ putStrLn $ replicate n "Hello World!"
--    let test = [[(2,3),(3,3)],[(2,3)],[(8,6)],[(8,8)]]
--  print $ take a $ repeat $ take a (repeat 0)
--    print $ conjureallheresy [[(x,y)]]
--    print $ cleanheresy (conjureallheresy [[(x,y)]])  [[(x,y)]]
--    print $ conjureheresy [(x,y),(2,3),(5,6)]
--    print $ sommonheresy [[(x,y)]] [] (ex,ey)

sommonheresy sl el en =
--  let up = map (\x -> (x,length x + (floor $sqrt(((fst $ head x)- (fst en))^2 + ((snd $ head x)- (snd en))^2)))) $cleanheresy (conjureallheresy sl) (sl++el)
  let up = map (\x -> (x,length x + (max (abs $ (fst $ head x)-(fst en))(abs $ (snd $ head x)-(snd en))))) $cleanheresy (conjureallheresy sl) (sl++(map (\x -> [x]) el))
      down e [] = fst e
      down e (h:l) = if (snd e) < (snd h) then down e l else down h l
      iter =  if up == [] then [] else down (head up) (tail up)
  in if (iter == []) || (((fst $ head iter) == (fst en)) && ((snd $head iter) == (snd en))) then iter else sommonheresy (iter:sl) el en
--  in iter

conjureheresy ((x,y):l) = 
  let s = [(-1,-1),(0,-1),(1,-1),(1,0),(1,1),(0,1),(-1,1),(-1,0)]
      ft (sx,sy) (ax,ay) = (sx >0 && ax < a) || ((sx <0 && ax > 0) ||(sx == 0)) && ((sy >0 && ay < a) || (sy < 0 && ay >0) || (sy == 0))
  in map (\h -> h:(x,y):l) [(\(x1,y1) (x2,y2) -> (x1+x2,y1+y2)) l1 l2| l1 <-s, l2<-[(x,y)],ft l1 l2]
conjureallheresy sl= 
  let up [] [] l3 = l3
      up [] (h:l2) l3 = up (conjureheresy h) l2 l3
      up (h:l1) l2 l3 = up l1 l2 (h:l3)
  in up [] sl []
cleanheresy sl1 sl2 =
  let up l1 [] []= if l1 == [] then[] else down (head l1) [] (tail l1) []
      up [] l2 l3= up l3 (tail l2) []
      up (((x1,y1):t1):l1) (((x2,y2):t2):l2) l3= if (x1 == x2)&& (y1 == y2) then up l1 (((x2,y2):t2):l2) l3 else up l1 (((x2,y2):t2):l2) (((x1,y1):t1):l3)
      down e [] [] l3= e:l3
      down e [] (h:l2) l3 = down h l2 [] (e:l3)
      down ((x1,y1):t1) (((x2,y2):t2):l1) l2 l3= if (x1 == x2) && (y1 == y2) then if (length t1) < (length t2) then down ((x1,y1):t1) l1 l2 l3 else down ((x1,y1):t2) l1 l2 l3 else down ((x1,y1):t1) l1  (((x2,y2):t2):l2) l3
  in up sl1 sl2 []
a = 20

