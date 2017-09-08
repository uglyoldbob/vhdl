absolute_permeability = 4*pi/10000000

--array helper
makeArray x = [0..(x-1)]

gaussToTesla x = x / 10000
oerstedToAtCm x = 250 * x / pi

--to convert various units to meters
inchToMeter x = x * 0.0254
cmToMeter x = x * 0.01
mmToMeter x = x * 0.001
perMFTtoPerMeter x = x / 304.8

--ToroidRect is a toroid with a rectangular cross section
--EEcore is a core made of two E shape sections

--name, diameter, resistance/meter
data Wire = Wire [Char] Double Double deriving (Show)
data Shape = ToroidRect Double Double Double | EEcore Double Double deriving (Show)
data Material = KoolMu14 | KoolMu26 | KoolMu40 | KoolMu60 | KoolMu75 | KoolMu90 | KoolMu125 | PC40 deriving (Show, Enum, Bounded)
data SineWave = SineWave { frequency :: Double, rms_current :: Double} deriving (Show)
data RmsPower = RmsPower Double Double deriving (Show)

allMaterial = [(minBound::Material) ..]

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
    ind_mat :: Material,
    ind_shape :: Shape,
    ind_turns :: Double,
    ind_w_pwr :: Double,
    ind_wire :: Wire,
    ind_wire_length :: Double,
    ind_c_pwr :: Double,
    ind_driving :: [SineWave]
    } deriving (Show)

prettyPrintInductor :: Inductor -> IO ()
prettyPrintInductor ind = do (putStr . show) (ind_mat ind)
                             putStr " "
                             print (ind_shape ind)
                             (putStr . show) (ind_minL ind)
                             putStr " mH "
                             print "haha"
                             print 3.2
                             print [3,4,3]

makeInductor mat shape wire max_curr targetL toleranceL driver = 
    Inductor achieved mat shape turns wpower wire length cpower driver
    where turns_math = last(calcTurns shape mat wire max_curr targetL toleranceL 1)
          achieved = snd (turns_math)
          turns = fst (turns_math)
          wpower = (calcWirePower shape wire turns driver)
          cpower = 0.4321
          length = (calcWireLength shape wire turns)
    
pfc_signal = [ SineWave 60 14, SineWave 40000 1.4]

power_i2r (SineWave f i) r = RmsPower f (i * i * r);

power (RmsPower _ p) = p
freq (RmsPower f _) = f

total_power t = sqrt (sum [(power x) * (power x) | x <- t])

magnetWire = [
    Wire "8AWG magnet wire" (inchToMeter 0.1324) (perMFTtoPerMeter 0.6281),
    Wire "9AWG magnet wire" (inchToMeter 0.1181) (perMFTtoPerMeter 0.7925),
    Wire "10AWG magnet wire" (inchToMeter 0.1054) (perMFTtoPerMeter 0.9987),
    Wire "11AWG magnet wire" (inchToMeter 0.0941) (perMFTtoPerMeter 1.261),
    Wire "12AWG magnet wire" (inchToMeter 0.0840) (perMFTtoPerMeter 1.588),
    Wire "13AWG magnet wire" (inchToMeter 0.0750) (perMFTtoPerMeter 2.001),
    Wire "14AWG magnet wire" (inchToMeter 0.0670) (perMFTtoPerMeter 2.524),
    Wire "15AWG magnet wire" (inchToMeter 0.0599) (perMFTtoPerMeter 3.181),
    Wire "16AWG magnet wire" (inchToMeter 0.0534) (perMFTtoPerMeter 4.018),
    Wire "17AWG magnet wire" (inchToMeter 0.0478) (perMFTtoPerMeter 5.054),
    Wire "18AWG magnet wire" (inchToMeter 0.0426) (perMFTtoPerMeter 6.386),
    Wire "19AWG magnet wire" (inchToMeter 0.0382) (perMFTtoPerMeter 8.046),
    Wire "20AWG magnet wire" (inchToMeter 0.0341) (perMFTtoPerMeter 10.13),
    Wire "21AWG magnet wire" (inchToMeter 0.0306) (perMFTtoPerMeter 12.77),
    Wire "22AWG magnet wire" (inchToMeter 0.0273) (perMFTtoPerMeter 16.2),
    Wire "23AWG magnet wire" (inchToMeter 0.0244) (perMFTtoPerMeter 20.3),
    Wire "24AWG magnet wire" (inchToMeter 0.0218) (perMFTtoPerMeter 25.67),
    Wire "25AWG magnet wire" (inchToMeter 0.0195) (perMFTtoPerMeter 32.37),
    Wire "26AWG magnet wire" (inchToMeter 0.0174) (perMFTtoPerMeter 41.02),
    Wire "27AWG magnet wire" (inchToMeter 0.0156) (perMFTtoPerMeter 51.44),
    Wire "28AWG magnet wire" (inchToMeter 0.0139) (perMFTtoPerMeter 65.31),
    Wire "29AWG magnet wire" (inchToMeter 0.0126) (perMFTtoPerMeter 81.21),
    Wire "30AWG magnet wire" (inchToMeter 0.0112) (perMFTtoPerMeter 103.7),
    Wire "31AWG magnet wire" (inchToMeter 0.0100) (perMFTtoPerMeter 130.9),
    Wire "32AWG magnet wire" (inchToMeter 0.0091) (perMFTtoPerMeter 162),
    Wire "33AWG magnet wire" (inchToMeter 0.0081) (perMFTtoPerMeter 205.7),
    Wire "34AWG magnet wire" (inchToMeter 0.0072) (perMFTtoPerMeter 261.3),
    Wire "35AWG magnet wire" (inchToMeter 0.0064) (perMFTtoPerMeter 330.7),
    Wire "36AWG magnet wire" (inchToMeter 0.0058) (perMFTtoPerMeter 414.8),
    Wire "37AWG magnet wire" (inchToMeter 0.0052) (perMFTtoPerMeter 512.1),
    Wire "38AWG magnet wire" (inchToMeter 0.0047) (perMFTtoPerMeter 648.2),
    Wire "39AWG magnet wire" (inchToMeter 0.0041) (perMFTtoPerMeter 846.6),
    Wire "40AWG magnet wire" (inchToMeter 0.0037) (perMFTtoPerMeter 1079)
    ]

me = ToroidRect (mmToMeter 102.4) (mmToMeter 165.1) (mmToMeter 31.75)
    
calcAL relPerm area length = (absolute_permeability) * relPerm * area / length
calcShapeAL t mat = calcAL (permeability mat) (calcMagArea t) (calcMagLength t)
calcL t mat current turns = (calcShapeAL t mat) * turns * turns * (permeability_bias mat (calcH t turns current)) 

calcTurns shape mat wire current targetL tolerance guess
    | abs(closeness) < tolerance = [(guess, result)]
    | result > targetL = [(guess, result)]
    | guess > (calcWireMaxTurns shape wire) = [(guess * 10, 0)]
    | otherwise = (guess, result): (calcTurns shape mat wire current targetL tolerance (guess+1))
    where result = calcL shape mat current guess
          closeness = getPercentError result targetL

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
       what PC40 = 1.0

wire_power :: Shape -> Wire -> Double -> [SineWave] -> Double
wire_power (ToroidRect a b c) d e f =  calcWirePower (ToroidRect a b c) d e f
wire_power (EEcore a b) c d e = a * d
       
calcMagLength (ToroidRect id od thick) = pi * (id + od) * 0.5
calcMagArea (ToroidRect id od thick) = (od-id) * 0.5 * thick

calcWindingArea :: Shape -> Double
calcWindingArea (ToroidRect id od thick) = pi * 0.25 * id * id

calcWindingPercent s turns (Wire _ d _) = (pi * 0.25 * d * d * turns) / (calcWindingArea s)

calcWireLayers :: Shape -> Wire -> Double
calcWireLayers (ToroidRect id od thick) (Wire _ d _) = fromIntegral(floor ((sqrt 0.3) * id * 0.5 / d))

calcWireLayerTurns :: Shape -> Wire -> Double -> Double
calcWireLayerTurns (ToroidRect id od thick) (Wire _ d _) n = fromIntegral (floor ((id - ((n - 0.5) * d)) * pi / d))

calcWireMaxTurns t w = sum [(calcWireLayerTurns t w x) | x <- [1..( (calcWireLayers t w))]]

calcWireLayerTurnLength :: Shape -> Wire -> Double -> Double
calcWireLayerTurnLength (ToroidRect id od thick) (Wire _ d _) n = od-id+thick+thick+d+d+4*d*(n-1)

calcWireMaxLength t w = sum [(calcWireLayerTurnLength t (w) x) * (calcWireLayerTurns t (w) x) | x <- [1..( (calcWireLayers t (w)))]]
calcWireMaxResist t w = (resist w) * sum [(calcWireLayerTurnLength t (w) x) * (calcWireLayerTurns t (w) x) | x <- [1..( (calcWireLayers t (w)))]]

calcWireResist t w turns = (turns / (calcWireMaxTurns t w)) * (calcWireMaxResist t w)
calcWireLength t w turns = (turns / (calcWireMaxTurns t w)) * (calcWireMaxLength t w)
calcWirePower t w turns s = total_power [power_i2r x (calcWireResist t w turns) | x <- s]

resist (Wire _ _ w) = w
diameter (Wire _ d _) = d

calcH :: Shape -> Double -> Double -> Double
calcH (ToroidRect id od thick) turns current = 0.01 * turns * current / (pi * (id + od) * 0.5)

--material and H input, output B in tesla
calcB :: Material -> Double -> Double
calcB mat h =  what mat
 where what KoolMu14 =  ((1.105e-1 + 1.301e-2 * h + 6.115e-4 * h * h) / (1 + 1.386e-1 * h + 5.735e-4 * h * h)) ** 1.760
       what KoolMu26 =  ((1.008e-1 + 1.452e-2 * h + 7.846e-4 * h * h) / (1 + 1.035e-1 * h + 7.573e-4 * h * h)) ** 1.754
       what KoolMu40 =  ((5.180e-2 + 2.132e-2 * h + 7.941e-4 * h * h) / (1 + 8.447e-2 * h + 7.652e-4 * h * h)) ** 1.756
       what KoolMu60 =  ((5.214e-2 + 2.299e-2 * h + 8.537e-4 * h * h) / (1 + 7.029e-2 * h + 8.183e-4 * h * h)) ** 1.658
       what KoolMu75 =  ((4.489e-2 + 2.593e-2 * h + 7.949e-4 * h * h) / (1 + 6.463e-2 * h + 7.925e-4 * h * h)) ** 1.595
       what KoolMu90 =  ((4.182e-2 + 2.990e-2 * h + 7.826e-4 * h * h) / (1 + 6.542e-2 * h + 7.669e-4 * h * h)) ** 1.569
       what KoolMu125 = ((1.414e-2 + 2.850e-2 * h + 1.135e-3 * h * h) / (1 + 7.550e-2 * h + 1.088e-3 * h * h)) ** 1.274
       what PC40 = 1.0

calcCoreLossDensity mat flux freq = what mat
 where what KoolMu14 =  1
       what KoolMu26 =  (flux ** 1.766) * (1e-6 * 0.2546 * freq - 904.56)
       what KoolMu40 =  (flux ** 1.766) * (1e-6 * 0.2546 * freq - 904.56)
       what KoolMu60 =  4
       what KoolMu75 =  5
       what KoolMu90 =  6
       what KoolMu125 = 7
       what PC40 = 1.0
       
--[(calcWireLayerTurnLength me (magnetWire!!4) x) | x <- [1..(fromIntegral (calcWireLayers me (magnetWire!!4)))]]

--sum [(calcWireLayerTurns me (magnetWire!!4) x) | x <- [1..(fromIntegral (calcWireLayers me (magnetWire!!4)))]]

--max wire length
--sum [(calcWireLayerTurnLength me (magnetWire!!4) x) * fromIntegral(calcWireLayerTurns me (magnetWire!!4) x) | x <- [1..(fromIntegral (calcWireLayers me (magnetWire!!4)))]]