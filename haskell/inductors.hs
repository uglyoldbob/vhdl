absolute_permeability = 4*pi/10000000

--to convert various units to meters
inchToMeter x = x * 0.0254
cmToMeter x = x * 0.01
mmToMeter x = x * 0.001
perMFTtoPerMeter x = x * 25.4/ 12000

--ToroidRect is a toroid with a rectangular cross section
--EEcore is a core made of two E shape sections

--name, diameter, resistance/meter
data Wire = Wire [Char] Double Double deriving (Show)
data Shape = ToroidRect Double Double Double | EEcore Double Double deriving (Show)
data Material = KoolMu14 | KoolMu26 | KoolMu40 | KoolMu60 | KoolMu75 | KoolMu90 | KoolMu125 | PC40

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
calcShapeL t mat turns current = (calcShapeAL t mat) * turns * turns * (permeability_bias mat (calcH t turns current)) 

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
   
calcMagLength (ToroidRect id od thick) = pi * (id + od) * 0.5
calcMagArea (ToroidRect id od thick) = (od-id) * 0.5 * thick
calcWindingArea (ToroidRect id od thick) = pi * 0.25 * id * id
calcWindingPercent s turns (Wire _ d _) = (pi * 0.25 * d * d * turns) / (calcWindingArea s)
calcWireLength (ToroidRect id od thick) (Wire _ d _) turns = 2 * thick + (od - id) * 0.5

calcWireLayers (ToroidRect id od thick) (Wire _ d _) = round ((sqrt 0.3) * id * 0.5 / d)

calcH (ToroidRect id od thick) turns current = 0.01 * turns * current / (pi * (id + od) * 0.5)