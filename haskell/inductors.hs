--this block of imports is for chart rendering
import Graphics.Rendering.Chart 
import Graphics.Rendering.Chart.Backend.Diagrams
import Data.Colour
import Data.Colour.Names
import Data.Default.Class
import Control.Lens
--end block

import Data.List
import Data.Maybe

import System.IO
import Text.Printf

testlist :: [(Double, Double)]
testlist = [polar_to_rect (pi - x, (r x 5)) | x <- makeList 50 (pi / 2)]
polar_to_rect :: (Double, Double) -> (Double, Double)
polar_to_rect (x, y) = (y * cos x, y * sin x)
r x ir = (ir / cos x) - (ir*ir/((cos x)*(cos x)) - (ir*ir)/(cos x))**0.5

dutyCycleChart = toRenderable layout
  where
    sinusoid2 x = plot_points_style .~ filledCircles 2 (opaque red)
              $ plot_points_values .~ (inVlist (dutyCycleOther x))
              $ plot_points_title .~ (show x) ++ " VAC"
              $ def
    layout = layout_title .~ "Duty Cycle"
           $ layout_plots .~ [toPlot (sinusoid2 x) | x <- inputVoltagesRms]
           $ def

diodeCurrentChart = toRenderable layout
  where
    sinusoid2 = plot_points_style .~ filledCircles 2 (opaque red)
              $ plot_points_values .~ (slist)
              $ plot_points_title .~ "Amps"
              $ def
    sinusoid3 = plot_points_style .~ filledCircles 2 (opaque red)
              $ plot_points_values .~ (rmsList)
              $ plot_points_title .~ "Amps"
              $ def
    slist :: [(Double, Double)]
    slist = [(x, averageDiodeCurrent x) | x <- [0,0.0001..0.02]]
    plist = [averageDiodeCurrent x | x <- [0,0.0001..0.02]]
    rmsList :: [(Double, Double)]
    rmsList = [(x, rmsCalc plist) | x <- [0,0.0001..0.02]]
    layout = layout_title .~ "Diode Current"
           $ layout_plots .~ [toPlot sinusoid2, toPlot sinusoid3]
           $ def
           
inVlist :: [Double] -> [(Double, Double)]
inVlist vl = [(1.0 * fromIntegral(x), vl!!x) | x <- [0..length vl - 1]]

rescale inp num = [(minimum inp) + scale * x / actual_num | x <- list]
    where actual_num = num-1
          list = [0..actual_num]
          scale = (maximum inp) - (minimum inp)

{-|
chart2 = toRenderable layout
  where
    am :: Double -> Double
    am x = (sin (x*3.14159/45) + 1) / 2 * (sin (x*3.14159/5))
    sinusoid2 = plot_points_style .~ filledCircles 2 (opaque red)
              $ plot_points_values .~ (testList (manipShape 0.09 0.03) [0.02,0.021..0.060])
              $ plot_points_title .~ "Inner Diameter 0.09 0.03"
              $ def
    sinusoid2a = plot_points_style .~ filledCircles 2 (opaque green)
              $ plot_points_values .~ (testList (manipShape 0.11 0.03) [0.020,0.021..0.060])
              $ plot_points_title .~ "Inner Diameter 0.11 0.03"
              $ def
    sinusoid2b = plot_points_style .~ filledCircles 2 (opaque aqua)
              $ plot_points_values .~ (testList (manipShape 0.12 0.03) [0.020,0.021..0.060])
              $ plot_points_title .~ "Inner Diameter 0.12 0.03"
              $ def
    sinusoid2c = plot_points_style .~ filledCircles 2 (opaque blanchedalmond)
              $ plot_points_values .~ (testList (manipShape 0.13 0.03) [0.020,0.021..0.060])
              $ plot_points_title .~ "Inner Diameter 0.13 0.03"
              $ def
    sinusoid2d = plot_points_style .~ filledCircles 2 (opaque brown)
              $ plot_points_values .~ (testList (manipShape 0.13 0.03) [0.020,0.021..0.060])
              $ plot_points_title .~ "Inner Diameter 0.13 0.03"
              $ def
    sinusoid3 = plot_points_style .~ filledCircles 2 (opaque blue)
              $ plot_points_values .~ (testList (manipShape 0.10 0.03) [0.02,0.021..0.060])
              $ plot_points_title .~ "Inner Diameter 0.10 0.03"
              $ def
    layout = layout_title .~ "Total Power 0.03 thick"
           $ layout_plots .~ [toPlot sinusoid2, toPlot sinusoid2a, toPlot sinusoid2b, toPlot sinusoid2c, toPlot sinusoid2d, toPlot sinusoid3]
           $ def
-}
manipShape a b x = maybeInductorPower (makeInductorFindWire PC40 (ToroidRect x a b) 15.4 1.5e-3 0.1 pfc_signal) 
manipShape2 a b x = maybeInductorPower (makeInductorFindWire PC40 (ToroidRect a x b) 15.4 1.5e-3 0.1 pfc_signal) 
manipShape3 a b x = maybeInductorPower (makeInductorFindWire PC40 (ToroidRect a b x) 15.4 1.5e-3 0.1 pfc_signal) 

notmain = do prettyPrintInductor (fromJust(makeInductorFindD3 KoolMu26 (ToroidRect 0.02 0.15 0.02) 15.4 1.5e-3 0.01 pfc_signal))
             prettyPrintInductor (fromJust(makeInductorFindD3 KoolMu40 (ToroidRect 0.02 0.15 0.02) 15.4 1.5e-3 0.01 pfc_signal))
             prettyPrintInductor (fromJust(makeInductorFindD3 KoolMu60 (ToroidRect 0.02 0.15 0.02) 15.4 1.5e-3 0.01 pfc_signal))
             prettyPrintInductor (fromJust(makeInductorFindD3 KoolMu75 (ToroidRect 0.02 0.15 0.02) 15.4 1.5e-3 0.01 pfc_signal))
             prettyPrintInductor (fromJust(makeInductorFindD3 KoolMu90 (ToroidRect 0.02 0.15 0.02) 15.4 1.5e-3 0.01 pfc_signal))
             prettyPrintInductor (fromJust(makeInductorFindD3 PC40 (ToroidRect 0.02 0.15 0.02) 15.4 1.5e-3 0.01 pfc_signal))

main = do renderableToFile def "duty_cycle.svg" dutyCycleChart
          renderableToFile def "diode_current.svg" diodeCurrentChart
          prettyPrintInductor (stageInductor)

testList :: (Double -> Maybe Double) -> [Double] -> [(Double, Double)]
testList func xin = [(x, fromJust(func x)) | x <- xin, isJust (func x)]

testFunction x 
 | x < 5 = Nothing
 | x < 10 = Just x
 | x < 15 = Nothing
 | x < 20 = Just x
 | otherwise = Nothing

toroidChart a b c d e f mat max_curr targetL toleranceL driver = [inductorOptimumWire mat x max_curr targetL toleranceL driver | x <- (toroidMaker a b c d e f)]
toroidMaker a b c d e f = [ToroidRect (mmToMeter (a*x)) (mmToMeter (b*y)) (mmToMeter (c*z)) | x <- d, y <- e, z <- f, (a*x) < (b*y)]

inductorFilterSort ind_l = notFullList
    where notFullList = filter notfull ind_l
          sorted = inductorSort notFullList totalInductorPower
       
absolute_permeability = 4*pi/10000000

lineCalc :: [(Double, Double)] -> [Double]
lineCalc points = [a, b, c]
    where x1 = fst (points!!0)
          y1 = snd (points!!0)
          x2 = fst (points!!1)
          y2 = snd (points!!1)
          a = y1 - y2
          b = x2 - x1
          c = ((x1 - x2) * y1) + ((y2 - y1) * x1)

ycalc points x = ((-1) * (points!!2) - (points!!0) * x) / (points!!1)
          
fstLess a it = (fst it) < a
fstMore a it = (fst it) >= a
linearInterpolation :: [(Double, Double)] -> Double -> Double
linearInterpolation array a
    | a <= (fst (head array)) = ycalc (lineCalc [array!!0, array!!1]) a
    | a >= (fst (last array)) = ycalc (lineCalc last_two) a
    | otherwise = ycalc (lineCalc [lower_pair, upper_pair]) a
    where lower_pair = last (filter (fstLess a) array)
          upper_pair = head (filter (fstMore a) array)
          last_two = drop ((length array) - 2) array

arrayLess a arr = (fst arr) < a
arrayMore a arr = (fst arr) > a

biLinearInterpolation :: [(Double, [(Double, Double)])] -> Double -> Double -> Double
biLinearInterpolation graph_data b freq
    | b <= (fst (head ilist)) = ycalc ( lineCalc [ilist!!0, ilist!!1]) b
    | b >= (fst (last ilist)) = ycalc (lineCalc last_two) b
    | otherwise = ycalc (lineCalc [lower_pair, upper_pair]) b
    where lower_pair = last (filter (fstLess b) ilist)
          upper_pair = head (filter (fstMore b) ilist)
          ilist = [( (fst x), linearInterpolation (snd (x)) freq) | x <- graph_data]
          last_two = drop ((length ilist) - 2) ilist

--array helper
makeArray x = [0..(x-1)]

makeList x y = map ((*y) . (/x)) [1..x]

listRange l a b = take (b - a + 1)(drop a l)

gaussToTesla x = x / 10000
oerstedToAtCm x = 250 * x / pi

--to convert various units to meters
inchToMeter x = x * 0.0254
meterToFeet x = x / (12 * 0.0254)
footToMeter x = inchToMeter (x * 12)
cmToMeter x = x * 0.01
mmToMeter x = x * 0.001
perMFTtoPerMeter x = x / 304.8
feetPerLbTolbsPerMeter x = 1 / (footToMeter x)

coreLossConversion mat freq input = calcCoreLossDensity mat input freq

graphMaker :: Material -> Double -> (Double -> Double) -> Double -> Double -> Double -> [[Char]]
graphMaker mat freq func minx maxx steps= [do printf "%.4f, %.4f" (x) (logBase 10 (func (10**x))) | x <- values]
    where values = [(logBase 10 minx) + (x * (logBase 10 (maxx/minx))/steps) | x <- [1..steps]]

--ToroidRect is a toroid with a rectangular cross section
--EEcore is a core made of two E shape sections

--name, diameter, resistance/meter
data Wire = Wire [Char] Double Double Double deriving (Show)
data Shape = ToroidRect Double Double Double | EEcore Double Double deriving (Show)
data Material = KoolMu14 | KoolMu26 | KoolMu40 | KoolMu60 | KoolMu75 | KoolMu90 | KoolMu125 | PC40 deriving (Show, Enum, Bounded)
data SineWave = SineWave { frequency :: Double, rms_current :: Double} deriving (Show)
data RmsPower = RmsPower Double Double deriving (Show)

inTolerance value expectation percent = (value >= min_val) && (value <= max_val)
    where min_val = (1.0 - percent) * expectation
          max_val = (1.0 + percent) * expectation
getPercentError value expectation = (value - expectation) / expectation
          
findInput t guess goal tolerance
    | abs(closeness) < tolerance = [(result, guess, closeness)]
    | otherwise = (result, guess, closeness): (findInput t (guess / (1 + 0.5 * closeness)) goal tolerance)
    where result = t guess
          closeness = getPercentError result goal

--things to calculate
--number of turns to achieve inductance at peak current
--power loss in copper
--length of wire required
--power loss in core
          
data Inductor = Inductor { 
    ind_minL :: Double,
    ind_maxL :: Double,
    ind_mat :: Material,
    ind_shape :: Shape,
    ind_turns :: Double,
    ind_filled :: Double,
    ind_w_pwr :: Double,
    ind_wire :: Wire,
    ind_wire_length :: Double,
    ind_c_pwr :: Double,
    ind_driving :: [SineWave]
    } deriving (Show)

inductorImpedance ind freq = [a, b]
    where a = 2 * pi * freq * (ind_minL ind)
          b = 2 * pi * freq * (ind_maxL ind)
    
engineeringPrint :: Double -> [Char] -> [Char]
engineeringPrint num unit
    | num < 1e-6 = printf "%.3f n" (num / 1e-9) ++ unit
    | num < 1e-3 = printf "%.3f u" (num / 1e-6) ++ unit
    | num < 1 = printf "%.3f m" (num / 1e-3) ++ unit
    | num < 1e3 = printf "%.3f" (num) ++ unit
    | num < 1e6 = printf "%.3f K" (num / 1e3) ++ unit
    | otherwise = printf "%.3f " num ++ unit

totalInductorPower ind = (ind_w_pwr ind) + (ind_c_pwr ind)

maybeInductorPower ind
 | isJust ind = Just((ind_w_pwr (fromJust ind)) + (ind_c_pwr (fromJust ind)))
 | otherwise = Nothing

inductorSort :: [a] -> (a -> Double) -> [a]
inductorSort [] func = []
inductorSort (x:xs) func =
    let smaller = inductorSort [a | a <- xs, (func a) <= (func x)] func
        bigger = inductorSort [a | a <- xs, (func a) > (func x)] func
    in smaller ++ [x] ++ bigger
   
prettyPrintInductor :: Inductor -> IO ()
prettyPrintInductor ind = do (putStr . show) (ind_mat ind)
                             putStr " "
                             print (ind_shape ind)
                             (putStr) (engineeringPrint (ind_minL ind) "H")
                             putStr ", "
                             (putStr) (engineeringPrint (ind_maxL ind) "H")
                             putStr ", "
                             (putStr . show) (ind_turns ind)
                             putStr " turns, "
                             (putStr) (printf "%.1f%% full " ((ind_filled ind) * 100))
                             (putStr . show) (ind_wire ind)
                             putStr "\n"
                             (putStr) (printf "%.1f feet " (meterToFeet (ind_wire_length ind)))
                             putStr "\n"
                             (putStr) (printf "%.2f pounds " ((wire_density (ind_wire ind)) * (ind_wire_length ind)))
                             putStr "\n"
                             (putStr) (engineeringPrint ((ind_w_pwr ind) + (ind_c_pwr ind)) "W")
                             putStr ", "
                             (putStr) (engineeringPrint (ind_w_pwr ind) "W")
                             putStr " copper, "
                             (putStr) (engineeringPrint (ind_c_pwr ind) "W")
                             putStr " core"
                             putStr "\n\n"

                             
tweakInductor :: Shape -> Double -> Double -> [Shape]                             
tweakInductor (ToroidRect x y z) gain steps  = outp
    where mul = map ((10**) . (*gain) . (/10)) [-steps..steps]
          outp = [ToroidRect mx y z | mx <- map (*x) mul, my <- map (*y) mul, mz <- map (*z) mul, my > mx]

headOrEmpty :: [a] -> [a]
headOrEmpty l
    | null l = []
    | otherwise = [head l]

maybeHead :: [a] -> Maybe a
maybeHead l
    | null l = Nothing
    | otherwise = Just (head l)

maybeThing thing it
    | isJust it = thing it
    | otherwise = Nothing
    
lastOrEmpty :: [a] -> [a]
lastOrEmpty l
    | null l = []
    | otherwise = [last l]

tweakInductorCalc mat max_curr targetL toleranceL driver gain steps shape = headOrEmpty lessPower
    where best_result = shape
          result_list = [inductorOptimumWire mat x max_curr targetL toleranceL driver | x <- tweak_shapes]
          resf = inductorFilterSort result_list
          opower = totalInductorPower (inductorOptimumWire mat shape max_curr targetL toleranceL driver)
          lessPower = filter ((< opower) . totalInductorPower) resf
          tweak_shapes = tweakInductor shape gain steps

tweakInductorStage :: [Inductor] -> (Shape -> [Inductor]) -> [Inductor]
tweakInductorStage [] _ = []
tweakInductorStage (x:_) calc = newval ++ tweakInductorStage newval calc
    where newval = calc (ind_shape x)

testL = head (inductorSort (makeInductorMatShape 15.4 1.5e-3 0.1 pfc_signal (PC40, me3)) totalInductorPower)

tweakInductorRecursive ind max_curr targetL toleranceL gain steps
    | null calc = []
    | otherwise = [ind] ++ stuff
    where calc = tweakInductorCalc (ind_mat ind) max_curr targetL toleranceL (ind_driving ind) gain steps (ind_shape ind)
          stuff = (tweakInductorRecursive (head calc) max_curr targetL toleranceL (gain * 0.9) steps)

runInductorRecursive mat shape max_curr targetL toleranceL signal gain steps = [firstL] ++ (tweakInductorRecursive firstL max_curr targetL toleranceL gain steps)
    where firstL = head (inductorSort (makeInductorMatShape max_curr targetL toleranceL signal (mat, shape)) totalInductorPower)
          
inductorOptimumWire mat shape max_curr targetL toleranceL driver = head notFullList
    where rlist = [makeInductor mat shape x max_curr targetL toleranceL driver | x <- combinedMagnetWire]
          notFullList = filter notfull rlist

powerResults mat max_curr targetL toleranceL driver a = ilist
    where ilist = [(a, makeInductorFindWire mat x max_curr targetL toleranceL driver) | x <- shapeMaker a]
          shapeMaker y = [ToroidRect (x/1000) 0.05 0.06 | x <- y]

makeInductorFindD3 mat (ToroidRect _ od _) max_curr targetL toleranceL driver = best
    where calcList = [makeInductorFindD2 mat (ToroidRect 0 x 0) max_curr targetL toleranceL driver | x <- (makeList 50 (2*od))]
          realList = map fromJust (filter isJust calcList)
          sortedList = inductorSort realList totalInductorPower
          best = maybeHead sortedList


makeInductorFindD2 mat (ToroidRect _ od _) max_curr targetL toleranceL driver = best
    where calcList = [makeInductorFindD1 mat (ToroidRect 0 od x) max_curr targetL toleranceL driver | x <- (makeList 50 (2*od))]
          realList = map fromJust (filter isJust calcList)
          sortedList = inductorSort realList totalInductorPower
          best = maybeHead sortedList

makeInductorFindD1 :: Material -> Shape -> Double -> Double -> Double -> [SineWave] -> Maybe Inductor
makeInductorFindD1 mat (ToroidRect _ od thick) max_curr targetL toleranceL driver = best
    where calcList = [makeInductorFindWire mat (ToroidRect x od thick) max_curr targetL toleranceL driver | x <- (makeList 50 od)]
          realList = map fromJust (filter isJust calcList)
          sortedList = inductorSort realList totalInductorPower
          best = maybeHead sortedList

makeInductorFindWire :: Material -> Shape -> Double -> Double -> Double -> [SineWave] -> Maybe Inductor          
makeInductorFindWire mat shape max_curr targetL toleranceL driver = maybeHead notFullList
    where rlist = [makeInductor mat shape x max_curr targetL toleranceL driver | x <- combinedMagnetWire]
          notFullList = filter notfull rlist

makeInductor mat shape wire max_curr targetL toleranceL driver = 
    Inductor achieved maxl mat shape turns percent wpower wire length cpower driver
    where turns_math = last(calcTurns shape mat wire max_curr targetL toleranceL initial_turns)
          initial_turns = fromIntegral(truncate ((targetL / (calcShapeAL shape mat))**0.5))
          maxl = calcL shape mat 0.01 turns
          achieved = snd (turns_math)
          turns = fst (turns_math)
          wpower = (calcWirePower shape wire turns driver)
          cpower = calcCorePower shape mat turns wire driver
          percent = calcWindingPercent shape turns wire
          length = (calcWireLength shape wire turns)

comboMatShapePair mats shapes = [(a, b) | a <- mats, b <- shapes]
          
makeInductorMatShape max_curr targetL toleranceL driver pair =
    filter (notfull) [makeInductor (fst pair) (snd pair) wire max_curr targetL toleranceL driver | wire <- combinedMagnetWire]
    
buildInductor pairs max_curr targetL toleranceL driver =
    [makeInductor mat shape wire max_curr targetL toleranceL driver | mat <- (take 7 allMaterial), wire <- combinedMagnetWire, shape <- specialToroids]
          
power_i2r (SineWave f i) r = RmsPower f (i * i * r);

power (RmsPower _ p) = p
freq (RmsPower f _) = f

total_power t = sqrt (sum [(power x) * (power x) | x <- t])

allMaterial = [(minBound::Material) ..]

pfc_signal = [ SineWave 60 14, SineWave 40000 1.4]

--http://www.coonerwire.com/magnet-wire/
magnetWire = [
    Wire "8AWG magnet wire" (inchToMeter 0.1324) (perMFTtoPerMeter 0.6281) (feetPerLbTolbsPerMeter 19.91),
    Wire "9AWG magnet wire" (inchToMeter 0.1181) (perMFTtoPerMeter 0.7925) (feetPerLbTolbsPerMeter 25.13),
    Wire "10AWG magnet wire" (inchToMeter 0.1054) (perMFTtoPerMeter 0.9987) (feetPerLbTolbsPerMeter 31.68),
    Wire "11AWG magnet wire" (inchToMeter 0.0941) (perMFTtoPerMeter 1.261) (feetPerLbTolbsPerMeter 39.92),
    Wire "12AWG magnet wire" (inchToMeter 0.0840) (perMFTtoPerMeter 1.588) (feetPerLbTolbsPerMeter 50.18),
    Wire "13AWG magnet wire" (inchToMeter 0.0750) (perMFTtoPerMeter 2.001) (feetPerLbTolbsPerMeter 63.25),
    Wire "14AWG magnet wire" (inchToMeter 0.0670) (perMFTtoPerMeter 2.524) (feetPerLbTolbsPerMeter 80.80),
    Wire "15AWG magnet wire" (inchToMeter 0.0599) (perMFTtoPerMeter 3.181) (feetPerLbTolbsPerMeter 100.50),
    Wire "16AWG magnet wire" (inchToMeter 0.0534) (perMFTtoPerMeter 4.018) (feetPerLbTolbsPerMeter 126.70),
    Wire "17AWG magnet wire" (inchToMeter 0.0478) (perMFTtoPerMeter 5.054) (feetPerLbTolbsPerMeter 159.70),
    Wire "18AWG magnet wire" (inchToMeter 0.0426) (perMFTtoPerMeter 6.386) (feetPerLbTolbsPerMeter 201.20),
    Wire "19AWG magnet wire" (inchToMeter 0.0382) (perMFTtoPerMeter 8.046) (feetPerLbTolbsPerMeter 253.20),
    Wire "20AWG magnet wire" (inchToMeter 0.0341) (perMFTtoPerMeter 10.13) (feetPerLbTolbsPerMeter 319.50),
    Wire "21AWG magnet wire" (inchToMeter 0.0306) (perMFTtoPerMeter 12.77) (feetPerLbTolbsPerMeter 402.70),
    Wire "22AWG magnet wire" (inchToMeter 0.0273) (perMFTtoPerMeter 16.2) (feetPerLbTolbsPerMeter 507.60),
    Wire "23AWG magnet wire" (inchToMeter 0.0244) (perMFTtoPerMeter 20.3) (feetPerLbTolbsPerMeter 650.00),
    Wire "24AWG magnet wire" (inchToMeter 0.0218) (perMFTtoPerMeter 25.67) (feetPerLbTolbsPerMeter 805.50),
    Wire "25AWG magnet wire" (inchToMeter 0.0195) (perMFTtoPerMeter 32.37) (feetPerLbTolbsPerMeter 1012.1),
    Wire "26AWG magnet wire" (inchToMeter 0.0174) (perMFTtoPerMeter 41.02) (feetPerLbTolbsPerMeter 1276),
    Wire "27AWG magnet wire" (inchToMeter 0.0156) (perMFTtoPerMeter 51.44) (feetPerLbTolbsPerMeter 1605),
    Wire "28AWG magnet wire" (inchToMeter 0.0139) (perMFTtoPerMeter 65.31) (feetPerLbTolbsPerMeter 2820),
    Wire "29AWG magnet wire" (inchToMeter 0.0126) (perMFTtoPerMeter 81.21) (feetPerLbTolbsPerMeter 2538),
    Wire "30AWG magnet wire" (inchToMeter 0.0112) (perMFTtoPerMeter 103.7) (feetPerLbTolbsPerMeter 3205),
    Wire "31AWG magnet wire" (inchToMeter 0.0100) (perMFTtoPerMeter 130.9) (feetPerLbTolbsPerMeter 4032),
    Wire "32AWG magnet wire" (inchToMeter 0.0091) (perMFTtoPerMeter 162) (feetPerLbTolbsPerMeter 5086),
    Wire "33AWG magnet wire" (inchToMeter 0.0081) (perMFTtoPerMeter 205.7) (feetPerLbTolbsPerMeter 6369),
    Wire "34AWG magnet wire" (inchToMeter 0.0072) (perMFTtoPerMeter 261.3) (feetPerLbTolbsPerMeter 8039),
    Wire "35AWG magnet wire" (inchToMeter 0.0064) (perMFTtoPerMeter 330.7) (feetPerLbTolbsPerMeter 10111),
    Wire "36AWG magnet wire" (inchToMeter 0.0058) (perMFTtoPerMeter 414.8) (feetPerLbTolbsPerMeter 12690),
    Wire "37AWG magnet wire" (inchToMeter 0.0052) (perMFTtoPerMeter 512.1) (feetPerLbTolbsPerMeter 16026),
    Wire "38AWG magnet wire" (inchToMeter 0.0047) (perMFTtoPerMeter 648.2) (feetPerLbTolbsPerMeter 20243),
    Wire "39AWG magnet wire" (inchToMeter 0.0041) (perMFTtoPerMeter 846.6) (feetPerLbTolbsPerMeter 25445),
    Wire "40AWG magnet wire" (inchToMeter 0.0037) (perMFTtoPerMeter 1079)(feetPerLbTolbsPerMeter 31949) 
    ]

strandedMagnetWire = [Wire (show x ++ " strands of " ++ (wireName y)) ((x**0.5) * (diameter y)) ((resist y) / x) (x * (wire_density y)) | x <- [2..10], y <- magnetWire]
sortedStrandedMagnetWire = inductorSort notTooLarge resist
    where notTooLarge = filter wireNotTooLarge strandedMagnetWire

wireNotTooLarge x = (diameter x) < (diameter (magnetWire!!0))
    
combinedMagnetWire = inductorSort (notTooLarge ++ magnetWire) resist
    where notTooLarge = filter wireNotTooLarge strandedMagnetWire
   
lotsToroids = [ToroidRect (mmToMeter (10 * x**1.2)) (mmToMeter (10 * y**1.2)) (mmToMeter (10 * z**1.2)) | x <- [1..20], y <- [1..20], z <- [1..5], x < y]
specialToroids = [ToroidRect (mmToMeter 45.3) (mmToMeter 74.1) (mmToMeter z) | z <- [20..75]]
me = ToroidRect (mmToMeter 14.7) (mmToMeter 26.9) (mmToMeter 11.2)
me2 = ToroidRect (mmToMeter 102.4) (mmToMeter 165.1) (mmToMeter 31.75)
me3 = ToroidRect (mmToMeter 65) (mmToMeter 102) (mmToMeter 20)

calcAL relPerm area length = (absolute_permeability) * relPerm * area / length
calcShapeAL t mat = calcAL (permeability mat) (calcMagArea t) (calcMagLength t)
calcL t mat current turns = (calcShapeAL t mat) * turns * turns * (permeability_bias mat (calcH t turns current)) 

calcTurns shape mat wire current targetL tolerance guess
    | abs(closeness) < tolerance = [(guess, result)]
    | guess > (calcWireMaxTurns shape wire) = [(guess * 10, 0)]
    | otherwise = (guess, result): (calcTurns shape mat wire current targetL tolerance (newguess))
    where result = calcL shape mat current guess
          closeness = getPercentError result targetL
          newguess = guess / (1 + 0.5 * closeness)

permeability :: Material -> Double
permeability x = what x
 where what KoolMu14 = 14
       what KoolMu26 = 26
       what KoolMu40 = 40
       what KoolMu60 = 60
       what KoolMu75 = 75
       what KoolMu90 = 90
       what KoolMu125 = 125
       what PC40 = 2300

permeability_bias x h =  what x
 where what KoolMu14 = 0.01 / (0.01 + 8.220e-8 * h**1.990)
       what KoolMu26 = 0.01 / (0.01 + 7.979e-7 * h**1.819)
       what KoolMu40 = 0.01 / (0.01 + 3.213e-6 * h**1.704)
       what KoolMu60 = 0.01 / (0.01 + 5.184e-6 * h**1.749)
       what KoolMu75 = 0.01 / (0.01 + 1.272e-5 * h**1.664)
       what KoolMu90 = 0.01 / (0.01 + 2.698e-5 * h**1.558)
       what KoolMu125 = 0.01 / (0.01 + 6.345e-5 * h**1.462)
       what PC40 = (calcB PC40 h) / ((permeability x) * 100 * h * absolute_permeability)

--material and H input, output B in tesla
calcB :: Material -> Double -> Double
calcB mat j
 | j > 0 = what mat
 | otherwise = (-(what mat))
 where h = abs(j)
       what KoolMu14 =  ((1.105e-1 + (1.301e-2 * h) + (6.115e-4 * h * h)) / (1 + (1.386e-1 * h) + (5.735e-4 * h * h))) ** 1.760
       what KoolMu26 =  ((1.008e-1 + (1.452e-2 * h) + (7.846e-4 * h * h)) / (1 + (1.035e-1 * h) + (7.573e-4 * h * h))) ** 1.754
       what KoolMu40 =  ((5.180e-2 + (2.132e-2 * h) + (7.941e-4 * h * h)) / (1 + (8.447e-2 * h) + (7.652e-4 * h * h))) ** 1.756
       what KoolMu60 =  ((5.214e-2 + (2.299e-2 * h) + (8.537e-4 * h * h)) / (1 + (7.029e-2 * h) + (8.183e-4 * h * h))) ** 1.658
       what KoolMu75 =  ((4.489e-2 + (2.593e-2 * h) + (7.949e-4 * h * h)) / (1 + (6.463e-2 * h) + (7.925e-4 * h * h))) ** 1.595
       what KoolMu90 =  ((4.182e-2 + (2.990e-2 * h) + (7.826e-4 * h * h)) / (1 + (6.542e-2 * h) + (7.669e-4 * h * h))) ** 1.569
       what KoolMu125 = ((1.414e-2 + (2.850e-2 * h) + (1.135e-3 * h * h)) / (1 + (7.550e-2 * h) + (1.088e-3 * h * h))) ** 1.274
       what PC40 = (linearInterpolation pc40_bh_60c (h * 100)) * 0.001

       --  - 904.56
calcCoreLossDensity mat flux freq = what mat
 where what KoolMu14 =  1 * 21.5 * (flux ** 1.000) * ((freq/1000) ** 1.330)
       what KoolMu26 =  1 * 45.5 * (flux ** 1.774) * ((freq/1000) ** 1.460)
       what KoolMu40 =  1 * 45.5 * (flux ** 1.774) * ((freq/1000) ** 1.460)
       what KoolMu60 =  1 * 62.7 * (flux ** 1.781) * ((freq/1000) ** 1.360)
       what KoolMu75 =  1 * 146.8 * (flux ** 2.022) * ((freq/1000) ** 1.330)
       what KoolMu90 =  1 * 146.8 * (flux ** 2.022) * ((freq/1000) ** 1.330)
       what KoolMu125 = 1 * 71.9 * (flux ** 1.928) * ((freq/1000) ** 1.470)
       what PC40 = biLinearInterpolation pc40_cl_60c flux freq

wire_power :: Shape -> Wire -> Double -> [SineWave] -> Double
wire_power (ToroidRect a b c) d e f =  calcWirePower (ToroidRect a b c) d e f
wire_power (EEcore a b) c d e = a * d
       
calcMagLength (ToroidRect id od thick) = pi * (id + od) * 0.5
calcMagArea (ToroidRect id od thick) = (od-id) * 0.5 * thick

calcWindingArea :: Shape -> Double
calcWindingArea (ToroidRect id od thick) = pi * 0.25 * id * id

calcWindingPercent s turns (Wire _ d _ _) = (pi * 0.25 * d * d * turns) / (calcWindingArea s)

calcWireLayers :: Shape -> Wire -> Double
calcWireLayers (ToroidRect id od thick) (Wire _ d _ _) = fromIntegral(floor ((sqrt 0.3) * id * 0.5 / d))

calcWireLayerTurns :: Shape -> Wire -> Double -> Double
calcWireLayerTurns (ToroidRect id od thick) (Wire _ d _ _) n = fromIntegral (floor ((id - ((n - 0.5) * d)) * pi / d))

calcWireMaxTurns t w = fromIntegral(floor((calcWindingArea t) / (pi * (diameter w)**2 * 0.25)))

calcWireLayerTurnLength :: Shape -> Wire -> Double -> Double
calcWireLayerTurnLength (ToroidRect id od thick) (Wire _ d _ _) n = od-id+thick+thick+d+d+4*d*(n-1)

calcWireMaxVolume :: Shape -> Double
calcWireMaxVolume (ToroidRect id od th) = pi*ir*ir * (ir + h + 2*(or - ir)) + (pi * id * id / od + pi * h) * ((id*id+od*od)/4 - or*or)
    where od2 = ((id*id)+(od*od))**0.5
          ir = id * 0.5
          or = od * 0.5
          h = th * 0.5

calcWireMaxLength t w = wire_volume / wire_area
    where wire_area = (diameter w)**2 * pi * 0.25
          wire_volume = calcWireMaxVolume t
calcWireMaxResist t w = (resist w) * wire_volume / wire_area
    where wire_area = (diameter w)**2 * pi * 0.25
          wire_volume = calcWireMaxVolume t

calcWireResist t w turns = (turns / (calcWireMaxTurns t w)) * (calcWireMaxResist t w)
calcWireLength t w turns = (turns / (calcWireMaxTurns t w)) * (calcWireMaxLength t w)
calcWirePower t w turns s = total_power [power_i2r x (calcWireResist t w turns) | x <- s]

calcVolume :: Shape -> Double
calcVolume (ToroidRect id od thick) = ((od * od * 0.25) - (id * id * 0.25)) * pi * thick

pfc_core_loss :: Shape -> Material -> Double -> [SineWave] -> [Double]
pfc_core_loss shape mat turns signal
    | low_freq > 0.0 = [calcCorePowerMinMax shape mat turns high_freq (calc * (1 - (high_peak / low_peak))) (calc * (1 + (high_peak / low_peak))) | calc <- range]
    | otherwise = [calcCorePowerMinMax shape mat turns high_freq ((rms_current (signal!!0)) - high_peak) ((rms_current (signal!!0)) + high_peak)]
    where low_freq = (frequency (signal!!0))
          high_freq = (frequency (signal!!1))
          low_peak = (rms_current (signal!!0)) * sqrt(2)
          high_peak = (rms_current (signal!!1)) * sqrt(2)
          volume = calcVolume shape
          h = calcH shape turns
          density x y = (calcCoreLossDensity mat (calcB mat (h x)) y)
          range = (map ((low_peak * ) . sin . (0.01*pi*)) [0..99])

calcCorePowerMinMax shape mat turns fr minI maxI = density * volume * 1000
    where volume = calcVolume shape
          minh = calcH shape turns minI
          maxh = calcH shape turns maxI
          density = (calcCoreLossDensity mat deltab fr)
          deltab = ((calcB mat maxh) - (calcB mat minh)) * 0.5

rmsCalc :: [Double] -> Double
rmsCalc list = sqrt (((sum) (map (** 2) list)) / fromIntegral(length list))
          
calcCorePower :: Shape -> Material -> Double -> Wire -> [SineWave] -> Double
calcCorePower shape mat turns wire signal
    | (length signal == 2) = rmsCalc power_list
    | (length signal == 1) = density * volume * 1000
    where volume = calcVolume shape
          flux = calcB mat
          h = calcH shape turns
          density = (calcCoreLossDensity mat (flux (h (sqrt(2) * (rms_current (signal!!0))))) (frequency (signal!!0)))
          power_list = pfc_core_loss shape mat turns signal

wireName (Wire n _ _ _) = n
resist (Wire _ _ w _) = w
diameter (Wire _ d _ _) = d
wire_density (Wire _ _ _ d) = d

--at / cm
calcH :: Shape -> Double -> Double -> Double
calcH (ToroidRect id od thick) turns current = 0.01 * turns * current / (pi * (id + od) * 0.5)

notfull ind = (ind_filled ind) < 0.7

--[(calcWireLayerTurnLength me (magnetWire!!4) x) | x <- [1..(fromIntegral (calcWireLayers me (magnetWire!!4)))]]

--sum [(calcWireLayerTurns me (magnetWire!!4) x) | x <- [1..(fromIntegral (calcWireLayers me (magnetWire!!4)))]]

--max wire length
--sum [(calcWireLayerTurnLength me (magnetWire!!4) x) * fromIntegral(calcWireLayerTurns me (magnetWire!!4) x) | x <- [1..(fromIntegral (calcWireLayers me (magnetWire!!4)))]]




--[(tesla, [(hertz, kw/m^3)]]
pc40_cl_60c = [ 
    (0.05, 
    [(0, 0), (1.57685589328112e+004, 1.04409013545220e+000),(1.65791452694110e+004, 1.11352170946351e+000),
    (1.74318265687665e+004, 1.18980865623945e+000),(1.83283620829475e+004, 1.27132199258548e+000),
    (1.92851482160587e+004, 1.35332161057505e+000),(2.02918809674423e+004, 1.44061016196597e+000),
    (2.13516901900829e+004, 1.53641900514261e+000),(2.24492786510919e+004, 1.63859025094385e+000),
    (2.36032888950612e+004, 1.74755584349141e+000),(2.48348315719123e+004, 1.85677286171253e+000),
    (2.61312716115963e+004, 1.97653376118710e+000),(2.74745555360558e+004, 2.10797246122450e+000),
    (2.88861843589188e+004, 2.24392268544747e+000),(3.03941151007945e+004, 2.38865455044938e+000),
    (3.19573135127610e+004, 2.55230036086114e+000),(3.36000864381917e+004, 2.72202730816868e+000),
    (3.53540946048739e+004, 2.89759667669120e+000),(3.72005767314187e+004, 3.09030348448396e+000),
    (3.91138379755096e+004, 3.30201899522683e+000),(4.11244936587601e+004, 3.52160193013746e+000),
    (4.32712946159787e+004, 3.74874345263670e+000),(4.55301639280848e+004, 3.99053548710941e+000),
    (4.78706522974094e+004, 4.25590449176721e+000),(5.03314540004527e+004, 4.53892042848728e+000),
    (5.29588812102066e+004, 4.83167847357083e+000),(5.57234666618522e+004, 5.14331926275873e+000),
    (5.86352401464833e+004, 5.49571774497744e+000),(6.16976555223027e+004, 5.86121451396669e+000),
    (6.49184267500893e+004, 6.23925985094918e+000),(6.82539028132080e+004, 6.64165067498736e+000),
    (7.17607539561708e+004, 7.06999303480675e+000),(7.55659488382531e+004, 7.52604730384353e+000),
    (7.95126253255077e+004, 8.02657264019818e+000),(8.36674772754262e+004, 8.57651944912074e+000),
    (8.79705756606384e+004, 9.16409351900857e+000),(9.23548709851326e+004, 9.82875311018040e+000),
    (9.70311931470871e+004, 1.05218498683188e+001),(1.01867053356596e+005, 1.12849857330219e+001),
    (1.06943924144326e+005, 1.21034708333902e+001),(1.12279312339793e+005, 1.30302971881551e+001),
    (1.17880880848206e+005, 1.40280955065585e+001),(1.23761909295413e+005, 1.51023004847512e+001),
    (1.29939519681522e+005, 1.62894056945368e+001),(1.36422148638524e+005, 1.75367710750113e+001),
    (1.43116165534778e+005, 1.88795450648959e+001),(1.50028557166140e+005, 2.04017018244546e+001),
    (1.57274811557776e+005, 2.20465819437502e+001),(1.64862984705461e+005, 2.37345312229210e+001),
    (1.73083695124865e+005, 2.55039419827054e+001),(1.81576635539563e+005, 2.74567547204334e+001),
    (1.90346635848504e+005, 2.96704460275911e+001),(1.99535336753683e+005, 3.20023010409921e+001),
    (2.09326216966017e+005, 3.44526869912046e+001),(2.19941445435686e+005, 3.70911238282322e+001),
    (2.30739267416395e+005, 4.00064139429504e+001),(2.41883781910308e+005, 4.32319171634804e+001),
    (2.53566567174929e+005, 4.67174754601911e+001),(2.65813621240301e+005, 5.04840556832214e+001),
    (2.78652197819666e+005, 5.45543151277158e+001),(2.91896675074915e+005, 5.91748216479304e+001),
    (3.05314945493191e+005, 6.45495211530732e+001),(3.18858474782152e+005, 7.05442840792949e+001),
    (3.32506473900036e+005, 7.75316191022734e+001),(3.46467438024100e+005, 8.52105514437524e+001),
    (3.61014581770436e+005, 9.36500251306563e+001),(3.76752444336782e+005, 1.02732931842967e+002),
    (3.92868846385463e+005, 1.12696120461927e+002),(4.09374266272407e+005, 1.24091269590784e+002),
    (4.26573120846654e+005, 1.36638627181978e+002),(4.44831596118499e+005, 1.50172535388165e+002),
    (4.63520103340351e+005, 1.65357072609078e+002),(4.82981942556939e+005, 1.81734465309664e+002),
    (5.04036780277653e+005, 1.99360485083992e+002),(5.26009470514962e+005, 2.18696013137642e+002),
    (5.48081640678602e+005, 2.39904088035743e+002),(5.71527020289148e+005, 2.63170334181754e+002),
    (5.95989911777549e+005, 2.89237071083785e+002),(6.22442784787646e+005, 3.16694484800017e+002),
    (6.49561302107290e+005, 3.46756447272142e+002),(6.78391928762815e+005, 3.79674202924439e+002),
    (7.07965357316946e+005, 4.16497955629495e+002),(7.38250109298166e+005, 4.56890530988786e+002),
    (7.70432961034367e+005, 5.01203322842492e+002),(8.03999097726855e+005, 5.48779634415174e+002),
    (8.39048173144915e+005, 6.02004545119198e+002),(8.75603729690098e+005, 6.59149329484009e+002),
    (9.13059576241675e+005, 7.23074586749604e+002),(9.53585512241057e+005, 7.91716403656699e+002),
    (9.95910182442851e+005, 8.66874421125464e+002),(1.03772550761470e+006, 9.52731908760287e+002)] ), 
    (0.1, 
    [(0, 0), (1.60199480127544e+004, 7.26072310351126e+000),(1.68690109912777e+004, 7.71454102255371e+000),
    (1.77496153640367e+004, 8.21212497099767e+000),(1.86761894775069e+004, 8.74180282950385e+000),
    (1.96660342043819e+004, 9.28819286699837e+000),(2.06926501526531e+004, 9.88727655417542e+000),
    (2.17558280933689e+004, 1.05249402282605e+001),(2.28909762876033e+004, 1.11827175421972e+001),
    (2.40859422418024e+004, 1.19039971015012e+001),(2.53432883939343e+004, 1.26717988233034e+001),
    (2.66662711456399e+004, 1.34891233632797e+001),(2.80583168908333e+004, 1.43591649178615e+001),
    (2.95223084068167e+004, 1.52565698175762e+001),(3.10634464077598e+004, 1.62406107640624e+001),
    (3.26858355319010e+004, 1.73207045818538e+001),(3.43921175250516e+004, 1.84378811644235e+001),
    (3.62149120634791e+004, 1.95903064452532e+001),(3.81044881281909e+004, 2.08146420588802e+001),
    (4.00936372777635e+004, 2.21571758208793e+001),(4.21546597656409e+004, 2.36306191000102e+001),
    (4.43227142388212e+004, 2.52495437839605e+001),(4.66364702729739e+004, 2.68781264378995e+001),
    (4.90698092747638e+004, 2.85579290238087e+001),(5.16301119739128e+004, 3.03427142517984e+001),
    (5.43240029222959e+004, 3.22390432232927e+001),(5.71598511260753e+004, 3.43184450145597e+001),
    (6.00966954820916e+004, 3.65317568013764e+001),(6.32338926098464e+004, 3.88880364218594e+001),
    (6.65348592383938e+004, 4.13962948721614e+001),(7.00081445434146e+004, 4.40663347090384e+001),
    (7.37186012614759e+004, 4.68206185499706e+001),(7.75668957855652e+004, 4.98405244884539e+001),
    (8.16140835692464e+004, 5.29554083369323e+001),(8.59396592096654e+004, 5.62652871011958e+001),
    (9.03596180851245e+004, 6.01200004183616e+001),(9.50092247238918e+004, 6.43598688316526e+001),
    (9.98199481695234e+004, 6.88983505639111e+001),(1.04876825649826e+005, 7.38958823729171e+001),
    (1.10187187938575e+005, 7.91068176704371e+001),(1.15772102828402e+005, 8.50047252529112e+001),
    (1.21452854965664e+005, 9.15134567405190e+001),(1.27508967270476e+005, 9.83357905587392e+001),
    (1.33867060918244e+005, 1.05666729781946e+002),(1.40435703741874e+005, 1.13757531431831e+002),
    (1.47326659375306e+005, 1.22467838118665e+002),(1.54555743193214e+005, 1.31845084757727e+002),
    (1.62143514555681e+005, 1.42207851711038e+002),(1.70103800540119e+005, 1.53385111970080e+002),
    (1.78585838445588e+005, 1.64819981484102e+002),(1.87348760243236e+005, 1.77440091720101e+002),
    (1.96546474468519e+005, 1.91386537445009e+002),(2.06190695983294e+005, 2.06040823766932e+002),
    (2.16313437628897e+005, 2.22235231343737e+002),(2.27110783488855e+005, 2.39703868071793e+002),
    (2.38260577494956e+005, 2.58544125387413e+002),(2.49957760332484e+005, 2.78865190245164e+002),
    (2.62434471988837e+005, 3.00785183196049e+002),(2.75116571081830e+005, 3.25648475626561e+002),
    (2.87756430649497e+005, 3.54558071984289e+002),(3.00977011506552e+005, 3.86034131335460e+002),
    (3.14804994108906e+005, 4.20304492637602e+002),(3.29268284710015e+005, 4.57617221358699e+002),
    (3.43874365164716e+005, 5.00116479354684e+002),(3.59400681038388e+005, 5.45537653995524e+002),
    (3.76197908959124e+005, 5.92854089990055e+002),(3.93780185102380e+005, 6.44274450065404e+002),
    (4.11881963780421e+005, 7.02792270170465e+002),(4.30489434391325e+005, 7.68065543295826e+002),
    (4.49926518442172e+005, 8.37822171237113e+002),(4.70597785122023e+005, 9.12200227954306e+002),
    (4.92604061743962e+005, 9.93186950490223e+002),(5.14845691356647e+005, 1.08338937290875e+003),
    (5.38104724073546e+005, 1.18401138234240e+003),(5.61974623659516e+005, 1.29397141631785e+003),
    (5.87348412472609e+005, 1.41149144236591e+003),(6.14348376847947e+005, 1.53969364298991e+003),
    (6.42589509260499e+005, 1.67954012550717e+003),(6.71619586952404e+005, 1.83553085845966e+003),
    (7.01961147945318e+005, 2.00600955058472e+003),(7.33655486396002e+005, 2.18819772851852e+003),
    (7.66799625013637e+005, 2.39143107932251e+003),(8.00814249366078e+005, 2.61352509518578e+003),
    (8.36337735528169e+005, 2.85624515054012e+003),(8.74120719982732e+005, 3.12152477545541e+003),
    (9.13610615238578e+005, 3.41144278947463e+003),(9.54137658899025e+005, 3.72826606552608e+003),
    (9.96486836001252e+005, 4.08219214914240e+003),(1.04150482937212e+006, 4.46133411528347e+003),
    (1.08852994843571e+006, 4.86651779617896e+003)] ),
    (0.15, 
    [(0, 0), (1.62994919774096e+004, 2.16003686838596e+001),(1.71377904404657e+004, 2.30802016876726e+001),
    (1.80324257696563e+004, 2.45688628864478e+001),(1.89732989675067e+004, 2.61043434008674e+001),
    (1.99632638619342e+004, 2.77357868591587e+001),(2.10213239918525e+004, 2.94693603470860e+001),
    (2.21181481562721e+004, 3.13111073092673e+001),(2.32733400993631e+004, 3.33934756342184e+001),
    (2.45074351966519e+004, 3.55475424127831e+001),(2.57861532803489e+004, 3.77691575914181e+001),
    (2.71315907055215e+004, 4.01296170801499e+001),(2.85695747238318e+004, 4.26378437478873e+001),
    (3.00609776544886e+004, 4.53879628519514e+001),(3.16302355312926e+004, 4.83154632309987e+001),
    (3.32561949391463e+004, 5.15284221004556e+001),(3.49657371579269e+004, 5.49550414422849e+001),
    (3.67901353478994e+004, 5.83895673173224e+001),(3.87400256892345e+004, 6.20390980260367e+001),
    (4.07623514818004e+004, 6.60405881034696e+001),(4.29227706999648e+004, 7.01683315579697e+001),
    (4.51987990077506e+004, 7.46945831399241e+001),(4.75582888480420e+004, 7.95123455314236e+001),
    (5.00801207317439e+004, 8.46413385081083e+001),(5.26944277191232e+004, 9.01006615311266e+001),
    (5.54438511938292e+004, 9.57316836405657e+001),(5.83367306233765e+004, 1.01714627805383e+002),
    (6.13835553722777e+004, 1.08479234276628e+002),(6.45879297902133e+004, 1.15476089378926e+002),
    (6.79579174347215e+004, 1.22692999902715e+002),(7.14513095430988e+004, 1.30852035041693e+002),
    (7.51830856630665e+004, 1.39554446308559e+002),(7.91078299190664e+004, 1.48555636685810e+002),
    (8.31743857992634e+004, 1.58434526767346e+002),(8.74521240182087e+004, 1.69288815649415e+002),
    (9.19498704054618e+004, 1.80886727714759e+002),(9.66080498324883e+004, 1.94007320126372e+002),
    (1.01499728632274e+005, 2.07688185148349e+002),(1.06722568459209e+005, 2.22335067548491e+002),
    (1.12041429389695e+005, 2.38460736147727e+002),(1.17625372785926e+005, 2.55755978177910e+002),
    (1.23397061919364e+005, 2.75338971751005e+002),(1.29553290361397e+005, 2.96423123481160e+002),
    (1.36116458631281e+005, 3.17924131508165e+002),(1.42903755317203e+005, 3.41625395030167e+002),
    (1.49915814298668e+005, 3.67783328709545e+002),(1.57271943812433e+005, 3.95944150652876e+002),
    (1.65247426480755e+005, 4.26266129102767e+002),(1.73495798083270e+005, 4.59772464321298e+002),
    (1.82151431776002e+005, 4.94979657612714e+002),(1.91234211490234e+005, 5.31880421475643e+002),
    (2.00612859261071e+005, 5.71528848591321e+002),(2.10456611781476e+005, 6.15290272463169e+002),
    (2.20956205200561e+005, 6.62406281432773e+002),(2.31979618997789e+005, 7.13130210762204e+002),
    (2.43362488309373e+005, 7.67733917129615e+002),(2.55316392804905e+005, 8.29636974544831e+002),
    (2.68060582102848e+005, 8.94849978067531e+002),(2.81000808245702e+005, 9.65177886372586e+002),
    (2.94356916120098e+005, 1.04692413699843e+003),(3.08121748693715e+005, 1.13987188246149e+003),
    (3.22522364811611e+005, 1.23873705624636e+003),(3.37852012876404e+005, 1.34365254518281e+003),
    (3.53642125592296e+005, 1.46019234621651e+003),(3.70161158469805e+005, 1.58385498774652e+003),
    (3.87755102039928e+005, 1.71800042208704e+003),(4.05569986898222e+005, 1.87051671638070e+003),
    (4.24203352598790e+005, 2.03657271632636e+003),(4.44029248247500e+005, 2.21321196730106e+003),
    (4.64793116616817e+005, 2.40970477448983e+003),(4.86896879385566e+005, 2.61872218516631e+003),
    (5.10026848104670e+005, 2.83517282397147e+003),(5.33850792319863e+005, 3.07528157506501e+003) ]),
    (0.2, 
    [(0, 0), (1.65664294083920e+004, 4.65713650154764e+001),(1.74312382691828e+004, 4.95751942285273e+001),
    (1.83416411593519e+004, 5.28722291479607e+001),(1.92986482083127e+004, 5.61765854784039e+001),
    (2.03050918504804e+004, 5.95751735288447e+001),(2.13650681989488e+004, 6.34177417369924e+001),
    (2.24803779518453e+004, 6.75081536283335e+001),(2.36354084462979e+004, 7.18619812003120e+001),
    (2.48692356126042e+004, 7.64970422161416e+001),(2.61674716288684e+004, 8.14310622957435e+001),
    (2.75334787974182e+004, 8.66833241457543e+001),(2.89700859724586e+004, 9.21007728801989e+001),
    (3.04816506271028e+004, 9.78567959722787e+001),(3.20728686804327e+004, 1.04168509239420e+002),
    (3.37463264228942e+004, 1.10678729793482e+002),(3.55079686808736e+004, 1.17817451231141e+002),
    (3.73606587442461e+004, 1.25180689874301e+002),(3.93100161360645e+004, 1.33004108930038e+002),
    (4.13307564020065e+004, 1.41848828671580e+002),(4.34543098451635e+004, 1.50997136070465e+002),
    (4.57585236153843e+004, 1.60737299629077e+002),(4.81472324737484e+004, 1.71104773206291e+002),
    (5.07002937571825e+004, 1.82141992322226e+002),(5.33469753205432e+004, 1.93890057625422e+002),
    (5.61318202509917e+004, 2.06395867129219e+002),(5.90605956634258e+004, 2.19294992078485e+002),
    (6.21452266091316e+004, 2.33879367546723e+002),(6.53909623991811e+004, 2.49433687680739e+002),
    (6.88062172559977e+004, 2.66022459367149e+002),(7.23998448620301e+004, 2.83714479570720e+002),
    (7.61792970187421e+004, 3.02013918327064e+002),(8.01580076017720e+004, 3.22099577100062e+002),
    (8.42146894601809e+004, 3.44164514277979e+002),(8.85480984264173e+004, 3.68436175005000e+002),
    (9.31022118257972e+004, 3.93677596554366e+002),(9.77422586703531e+004, 4.22230512326841e+002),
    (1.02616068311147e+005, 4.53707822942476e+002),(1.07896351280185e+005, 4.85704853114973e+002),
    (1.13445563830668e+005, 5.18980306986682e+002),(1.19096572179226e+005, 5.55574178174853e+002),
    (1.25032131207315e+005, 5.95869239040979e+002),(1.31269932580653e+005, 6.41498077441063e+002),
    (1.37818935287580e+005, 6.90620955736699e+002),(1.44804384668653e+005, 7.42111063560580e+002),
    (1.52143896450177e+005, 7.97440080675721e+002),(1.59730383599074e+005, 8.56889287646714e+002),
    (1.67699266449882e+005, 9.22505802598422e+002),(1.76061404921984e+005, 9.91278666816816e+002),
    (1.84980673765807e+005, 1.06318092033173e+003),(1.94351792671508e+005, 1.14029859331808e+003),
    (2.04042929890084e+005, 1.22530792345540e+003),(2.14222547369192e+005, 1.31913618906561e+003),
    (2.24904517796790e+005, 1.41747787294516e+003),(2.36119132867156e+005, 1.52315093539533e+003),
    (2.47899018861643e+005, 1.63978660533182e+003),(2.60260229299250e+005, 1.76203279742945e+003),
    (2.73244504621696e+005, 1.89696090665515e+003),(2.86666206276983e+005, 2.04991452417687e+003),
    (3.00739818720206e+005, 2.21103381449617e+003),(3.15504362158660e+005, 2.38481676732765e+003),
    (3.30484265364103e+005, 2.57707695114678e+003),(3.46454858595471e+005, 2.79010146642197e+003),
    (3.63197227898782e+005, 3.02073486376334e+003),(3.80450861832040e+005, 3.27041388455686e+003),
    (3.98826321680655e+005, 3.53408986741668e+003),(4.17772510772008e+005, 3.82620027673396e+003),
    (4.37961292907933e+005, 4.14247894234942e+003),(4.58777810516572e+005, 4.49332838328122e+003),
    (4.80936396768650e+005, 4.85560142259537e+003),(5.04165224286621e+005, 5.24708259980174e+003),
    (5.27728279753249e+005, 5.70218087977048e+003) ] ),
    (0.25, 
    [(0, 0), (1.66066886234087e+004, 8.55522400470830e+001),(1.74731715043256e+004, 9.08989994052342e+001),
    (1.83992558480387e+004, 9.65804713695314e+001),(1.93592690436533e+004, 1.02616462231086e+002),
    (2.03693725985393e+004, 1.09029684484910e+002),(2.14489566368518e+004, 1.15844381013258e+002),
    (2.25680932787687e+004, 1.23084308663674e+002),(2.37636287461320e+004, 1.30531451995159e+002),
    (2.50224972135883e+004, 1.38429180331360e+002),(2.63486985219411e+004, 1.47081437366523e+002),
    (2.77234877241376e+004, 1.56273587697327e+002),(2.91928424962859e+004, 1.66041176043425e+002),
    (3.07400735972684e+004, 1.76419269232373e+002),(3.23439904366926e+004, 1.87444946388310e+002),
    (3.40565665914487e+004, 1.98412246670089e+002),(3.58606992115048e+004, 2.10417062358754e+002),
    (3.77317936041991e+004, 2.23567499993843e+002),(3.97315921701556e+004, 2.37541168479240e+002),
    (4.18373807758743e+004, 2.52388234980651e+002),(4.40203189309016e+004, 2.68161745487534e+002),
    (4.63534115899574e+004, 2.84922693890193e+002),(4.87719815123306e+004, 3.02729510860370e+002),
    (5.13569139889135e+004, 3.21651052791222e+002),(5.40365503569603e+004, 3.41753282442067e+002),
    (5.69005068665990e+004, 3.63113932235857e+002),(5.98708549910292e+004, 3.86534595060607e+002),
    (6.29947210483755e+004, 4.10691852220116e+002),(6.62848244376011e+004, 4.38005217280366e+002),
    (6.97484710086599e+004, 4.68015482225632e+002),(7.33913108695218e+004, 4.99141197651382e+002),
    (7.72244098437374e+004, 5.32336951777897e+002),(8.12577048298794e+004, 5.67740414058365e+002),
    (8.53742192660265e+004, 6.08921382184488e+002),(8.95612040370396e+004, 6.54312740967236e+002),
    (9.40983736520202e+004, 7.01773220198715e+002),(9.89427855800513e+004, 7.52680572404520e+002),
    (1.03873916119477e+005, 8.07271499537648e+002),(1.09050805339077e+005, 8.65821834465419e+002),
    (1.14572514073511e+005, 9.26877219605970e+002),(1.20470984283578e+005, 9.94113847781462e+002),
    (1.26478136692624e+005, 1.06822509616443e+003),(1.32788078682258e+005, 1.15002470435741e+003),
    (1.39515121293025e+005, 1.23344157855148e+003),(1.46468304097476e+005, 1.32290146606069e+003),
    (1.53771784110032e+005, 1.42152385157137e+003),(1.61443394893917e+005, 1.53037740173887e+003),
    (1.69493591248906e+005, 1.64446713099177e+003),(1.78075776775364e+005, 1.76043045050735e+003),
    (1.86950753319041e+005, 1.88811214440251e+003),(1.96268042736917e+005, 2.02505442280485e+003),
    (2.06205933268775e+005, 2.16785571608855e+003),(2.16482866235552e+005, 2.32508742602104e+003),
    (2.27105334645503e+005, 2.50311717314578e+003),(2.38435525970445e+005, 2.69479400676412e+003),
    (2.50318723890374e+005, 2.89024385448915e+003),(2.62800590243419e+005, 3.10571171127981e+003),
    (2.75702542325495e+005, 3.34981460481810e+003),(2.89244982177379e+005, 3.61991308709914e+003),
    (3.03445197239973e+005, 3.90443120760231e+003),(3.18342558736383e+005, 4.21131189840673e+003),
    (3.33718239259216e+005, 4.55084745663841e+003),(3.49836552348925e+005, 4.91775795125210e+003),
    (3.67011454546761e+005, 5.30428415111207e+003),(3.85038965306106e+005, 5.73197322284222e+003),
    (4.03951982880436e+005, 6.19414723860370e+003),(4.23462527307760e+005, 6.69354820703451e+003),
    (4.43926278478814e+005, 7.24684556264514e+003),(4.65731826132677e+005, 7.83116506050041e+003),
    (4.88226285548835e+005, 8.46255002190101e+003),(5.11807208625050e+005, 9.14484017638631e+003),
    (5.36920770728802e+005, 9.84505194729781e+003) ] ),
    (0.3, 
    [(0, 0), (1.67008331545364e+004, 1.32168894064190e+002),(1.75726582255028e+004, 1.40693720100848e+002),
    (1.85035625737317e+004, 1.49206310461118e+002),(1.94842580129202e+004, 1.58532171860338e+002),
    (2.05013847914263e+004, 1.68757415824827e+002),(2.15716081201254e+004, 1.79642182792793e+002),
    (2.26971443166425e+004, 1.90869282415637e+002),(2.39001010891687e+004, 2.02799209961435e+002),
    (2.51655832597032e+004, 2.14664879724263e+002),(2.64987196914000e+004, 2.27653051367741e+002),
    (2.79031613194452e+004, 2.41882079601225e+002),(2.93820388564535e+004, 2.57000466634219e+002),
    (3.09158544376365e+004, 2.73576865225650e+002),(3.25289429483328e+004, 2.90674601807578e+002),
    (3.42521503336530e+004, 3.08261696750756e+002),(3.60401978591310e+004, 3.28144418454465e+002),
    (3.79215859171661e+004, 3.49309565534125e+002),(3.99304663477202e+004, 3.70444334294126e+002),
    (4.20457663935582e+004, 3.92857849743632e+002),(4.42731235900681e+004, 4.16627481695128e+002),
    (4.66196149865434e+004, 4.42668003288266e+002),(4.90916726304722e+004, 4.71222576351027e+002),
    (5.16935488507403e+004, 5.00675462235359e+002),(5.43907497285763e+004, 5.31966182520770e+002),
    (5.72734789316854e+004, 5.65215733909912e+002),(6.03089938143435e+004, 6.00543486327437e+002),
    (6.34060878336256e+004, 6.38071985570631e+002),(6.67160434815712e+004, 6.79227302140146e+002),
    (7.01473134125174e+004, 7.25760905038736e+002),(7.37532519160706e+004, 7.74023713278836e+002),
    (7.76033552906000e+004, 8.23947846719383e+002),(8.15925733124175e+004, 8.78740047084514e+002),
    (8.57889574021418e+004, 9.38942191894178e+002),(9.02033729820543e+004, 1.00515961228033e+003),
    (9.47707555971731e+004, 1.07604071600463e+003),(9.95718410370965e+004, 1.15409117239444e+003),
    (1.04536880779373e+005, 1.24012874014867e+003),(1.09832718013996e+005, 1.33008129743676e+003),
    (1.15309405039785e+005, 1.42923893981732e+003),(1.21059181007746e+005, 1.53578879052486e+003),
    (1.27192037790864e+005, 1.64718700644358e+003),(1.33635585031403e+005, 1.76666547570603e+003),
    (1.40299175698729e+005, 1.89837049537943e+003),(1.47291434365073e+005, 2.03605679842013e+003),
    (1.54635958858168e+005, 2.18784495774040e+003),(1.62350682963234e+005, 2.35537974134804e+003),
    (1.70446120234479e+005, 2.53097344566753e+003),(1.78945227532954e+005, 2.71965767142413e+003),
    (1.88010590798644e+005, 2.91692764394606e+003),(1.97385530998217e+005, 3.13438477887891e+003),
    (2.07227942223610e+005, 3.36805335657118e+003),(2.17561134400573e+005, 3.61914194107585e+003),
    (2.28409579778531e+005, 3.88894919497022e+003),(2.39798970888560e+005, 4.17887059620648e+003),
    (2.51565523305835e+005, 4.49884276894445e+003),(2.64309829959504e+005, 4.82516567301253e+003),
    (2.77910338307309e+005, 5.16545288830759e+003),(2.91989273000230e+005, 5.54012826015547e+003),
    (3.06541493385326e+005, 5.94194644073126e+003),(3.22307215600717e+005, 6.34902692063315e+003)] ) 
    ]
      
--[(at/m, mT)]
pc40_bh_60c = [ (0, 0),
                (1.50834849769187e+001, 5.15698535384674e+001),
                (4.10183107505049e+001, 1.73407540974357e+002),
                (7.26721106543865e+001, 2.68165560127333e+002),
                (1.06289008995291e+002, 3.35200725238138e+002),
                (1.15640522791402e+002, 3.49380338256397e+002),
                (1.23128781601288e+002, 3.58403108925752e+002),
                (1.30622913555339e+002, 3.65491779807396e+002),
                (1.39994004498665e+002, 3.73224393533288e+002),
                (1.53122928849985e+002, 3.80955493089199e+002),
                (1.64378809641473e+002, 3.86753249942390e+002),
                (1.75636648147682e+002, 3.91906306866344e+002),
                (1.86898402083335e+002, 3.95769963931825e+002),
                (1.98162113733709e+002, 3.98988921068069e+002),
                (2.09423867669361e+002, 4.02852578133550e+002),
                (2.22566496023732e+002, 4.06070778184804e+002),
                (2.35711082092824e+002, 4.08644278306821e+002),
                (2.52611108807473e+002, 4.12004230839037e+002),
                (2.69512005617554e+002, 4.15077650069370e+002),
                (2.92049217491909e+002, 4.18292064695674e+002),
                (3.08953594683717e+002, 4.20219350718475e+002),
                (3.29613847568797e+002, 4.22789822500532e+002),
                (3.48399099179323e+002, 4.24071651509106e+002),
                (3.69061309779125e+002, 4.25997423361926e+002),
                (3.89725478093648e+002, 4.27278495285510e+002),
                (4.12268563112168e+002, 4.28558810124104e+002),
                (4.34809690415967e+002, 4.30483824891935e+002),
                (4.55473858730490e+002, 4.31764896815519e+002),
                (5.09952654572793e+002, 4.34966440996993e+002),
                (5.70066242812366e+002, 4.38810413852735e+002),
                (6.26425913073388e+002, 4.41366501019983e+002),
                (6.75269916518422e+002, 4.43925616527191e+002),
                (7.35387420187438e+002, 4.46480189524459e+002),
                (7.99262757264447e+002, 4.49033248351747e+002),
                (8.83806178085423e+002, 4.51577979244145e+002),
                (9.60835889805133e+002, 4.53481038547267e+002),
                (1.01344164208760e+003, 4.54749240026021e+002),
                (1.06792631107407e+003, 4.56016684419786e+002),
                (1.11865314665255e+003, 4.57285642983530e+002),
                (1.19380589938298e+003, 4.58544759442405e+002),
                (1.25559833675820e+003, 4.59522726314881e+002),
                (1.31593309238034e+003, 4.59283515498235e+002),
                (1.39505396970844e+003, 4.60039600388280e+002),
                (1.50528200948540e+003, 4.60568251339301e+002),
                (1.56978887270300e+003, 4.61401857993628e+002),
                (1.59755552126534e+003, 4.61401857993628e+002) ]

{-|
-------------------------------- power supply calculations for FAN9673 pfc controller
---------------------------------------------------------- for FAN9673 pfc controller
-------------------------------- power supply calculations for FAN9673 pfc controller
-}
--given values
inputVoltageRms = [100.0,101.0..240]
inputVoltageRmsMinMax = sort [x * y | x <- inputVoltageRms, y <- [0.85,1,1.1]]
inputVoltagesRms = rescale inputVoltageRmsMinMax 20
inputFrequency = [50.0,60.0]
assumedEfficiency = 0.95
maxOverload = 1.1
switchingFrequency = 40000.0
rippleFactor = 0.1
voltage_pfc = 400.0
outputWattage = 2400.0
hold_time = 15e-3
minimum_hold_voltage = 0.75
outputPowerPercent = [x * 0.01 | x <- [0..100]]

--selected component values
rVIR = 100.0e3
rIAC = 6.0e6    --universal AC input
rM = 7500
vRipple = 0.05

--calculated values
powerInput = outputWattage / assumedEfficiency
fIC = switchingFrequency * 0.13
rRI = 800e6 / switchingFrequency
vVIR = rVIR * 10e-6
iLimit1 = 1.2*1.0208 / rRI
iLimit2 = 1.2*1.03215/ rRI

timeHold = (voltage_pfc**2 - (0.75 * voltage_pfc)**2)*cOUT / (2 * outputWattage)
rCS v = v**2 * 2 * rM / (maxOverload * rIAC * outputWattage / 3)
rILimit v = 1.8 * outputWattage / (3 * assumedEfficiency) * 2**0.5 * (rCS v) / v * 4 / iLimit1
rILimit2 = 1.5 * 0.395 / iLimit2
stageInductance v = (2**0.5 * v * voltage_pfc - 2**0.5 * v) / (rippleFactor * (averageStageCurrent v) * voltage_pfc * switchingFrequency)
stageInductor :: Inductor
stageInductor = fromJust (makeInductorFindD3 PC40 (ToroidRect 0 0.1 0) max_curr targetL 0.05 driver)
    where max_curr = (peakStageCurrent vmin) + (deltaIL vmin)
          driver = [ SineWave (minimum inputFrequency) (peakStageCurrent vmin), SineWave switchingFrequency (deltaIL vmin)]
          targetL = stageInductance vmax
          vmin = minimum inputVoltagesRms
          vmax = maximum inputVoltagesRms

cOUT = maximum cOUT_list
cOUT_list = [ripple_spec, hold_spec]
    where ripple_spec = outputWattage / (voltage_pfc * 2 * pi * voltage_pfc * assumedEfficiency)
          hold_spec = (2 * outputWattage * hold_time) / (voltage_pfc**2 - (voltage_pfc * minimum_hold_voltage)**2)

dutyCycle vin = [(voltage_pfc - v) / voltage_pfc | v <- vlist vin]
dutyCycleOther vin = [(v) / voltage_pfc | v <- vlist vin]
vlist vin = [(abs . (*vin) . (*(2**0.5)) . sin) (x * 0.02 * pi) | x <- [0..100]]

averageStageCurrent v = 2**0.5 * outputWattage / (3 * v * assumedEfficiency)
peakStageCurrent v = (averageStageCurrent v) * (1 + rippleFactor * 0.5)
deltaIL v = (2**0.5 * v * voltage_pfc - 2**0.5*v) / ((stageInductance v) * voltage_pfc * switchingFrequency)
vRippleCalc = outputWattage / (voltage_pfc * 2 * pi * (minimum inputFrequency) * cOUT)
averageDiodeCurrent t = (outputWattage / (3 * voltage_pfc)) * (1 - cos(4*pi*(minimum inputFrequency)*t))
rmsDiodeCurrent = rmsCalc [averageDiodeCurrent x | x <- (makeList 1000 (1/(minimum inputFrequency)))]
iCurrent vin = vlist (outputWattage / (vin * assumedEfficiency * 3))

rcs_power vin = i * i * (rCS vin)
    where i = rmsCalc (iCurrent vin)

pfc_diode_power d freq pfcv rmsI vin = (diodeSwitchingLoss d freq pfcv) + (diodePowerLoss d diode_duty rmsI) + (diodeLeakage d duty voltage_pfc)
    where diode_duty = rmsCalc (dutyCycleOther vin)
          duty = rmsCalc (dutyCycle vin)

singleStageLoss ind dio trans cs vin = p_ind + p_dio + p_trans + pcs
    where p_ind = 5
          p_dio = pfc_diode_power dio switchingFrequency voltage_pfc rmsDiodeCurrent vin
          p_trans = transistorPowerLoss trans charge_duty (rmsCalc (iCurrent vin))
          pcs = 5
          diode_duty = rmsCalc (dutyCycleOther vin)
          charge_duty = rmsCalc (dutyCycle vin)

--charging, discharging

--charging portion
--transistor conducts
--diode leaks

--discharging portion
--diode conducts

--IGBT resistance, turn on loss, turn off loss
--ohms, joules, joules
data Transistor = IGBT Double Double Double

--forward voltage, reverse recovery (coulombs), leakage
data Diode = Diode Double Double Double

--watts
transistorSwitchingLoss (IGBT _ lossOn lossOff) freq = freq * lossOn + freq * lossOff
transistorPowerLoss (IGBT r _ _) d a = r * a * a * d
diodeSwitchingLoss (Diode _ a _) freq v = a * freq * v
diodePowerLoss (Diode v _ _) d a = v * a * d
diodeLeakage (Diode _ _ i) d v = v * i * d

igbt_FGH20N60UFDTU = IGBT (1.8 / 20) 0.38e-3 0.26e-3
diode_VS_20ETF06FPPbF = Diode 1.3 1.25e-6 0.1e-3
diode_IDH08G65C6XKSA1 = Diode 1.25 12.2e-9 27.0e-6
diode_list = [diode_VS_20ETF06FPPbF, diode_IDH08G65C6XKSA1]