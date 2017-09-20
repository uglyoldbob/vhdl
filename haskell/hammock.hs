import System.IO
import Text.Printf

data Cord = Cord { diameter :: Double, density :: Double, gramdenier :: Double} deriving (Show)

--cotton is 1.54 g / cm^3
--1.54e-3 kg

kgToPounds x = x * 2.20462
densitycm3Toin3 x = x * (2.54 * 2.54 * 2.54)

gcm3Tolbin3 x = (densitycm3Toin3 . kgToPounds) (x / 1000)

--density = mass / volume
--denier = (grams / 9000 meters) (mass / length)
--grams / denier = force / 

--denier / density = area

meterToInch x = x / (0.0254)
perMeterToPerInch x = x * 0.0254

gramsToPounds x = x * 0.00220462
denierToArea x d = gramsToPounds(perMeterToPerInch (x / 9000)) / d
gramDenierToPSI x d = gramsToPounds (x) / (denierToArea 1 d)
options = [
    Cord 0.125 (gcm3Tolbin3 1.54) 2.5, 
    Cord 0.0625 (gcm3Tolbin3 1.54) 2.5, 
    Cord 0.03125 (gcm3Tolbin3 1.54) 2.5]

cordLoad cord = (gramDenierToPSI (gramdenier cord) (density cord)) * 0.25 * pi * ((diameter cord)**2) * 0.2

    
cordWeight cord length = pi * 0.25 * ((diameter cord) ** 2) * (density cord) * length

firstRowWidth cord = 10*d
    where d = diameter cord

additionalRowWidth cord = 6*d
    where d = diameter cord
 
runLength cord = 2 * d
    where d = diameter cord

cordLengthPerRun cord = 2*d*6 + 3*pi*2*d * ((run**2 + 1)**0.5 / (run))
    where d = diameter cord
          run = ((firstRowWidth cord) / d)-1


rowsRequired cord width = 1 + fromIntegral(round((width - firstRowWidth cord) / (additionalRowWidth cord)))

runsRequired cord length = length / (runLength cord)
lengthPerColumn cord length = (runsRequired cord length) * (cordLengthPerRun cord)

numStrands cord width = 6 + 4 * (numCols-1)
    where numCols = rowsRequired cord width

totalCapacity cord width length = ns * cl
    where ns = numStrands cord width
          cl = cordLoad cord

totalLength cord width length angle = a + b
    where a = totalRunLength cord width length
          b = lengthMainSupport cord width length angle
          
totalRunLength cord width length = nr * nruns * perRun
    where nr = rowsRequired cord width
          nruns = runsRequired cord length
          perRun = cordLengthPerRun cord
          
lengthMainSupport cord width length angle = (sum tailStrings) * 4 + ns * length
    where d = diameter cord
          ns = numStrands cord width
          nr = rowsRequired cord width
          index = [1..(nr+1)]
          offset = map ((+d) . (* (3*d))) index
          offset2 = map (+(-d)) (tail offset)
          tailLen = (maximum offset) * (tan (angle * 2 * pi / 360))
          tailStrings = map ((**0.5) . (+(tailLen*tailLen)) . (**2)) (offset ++ offset2)