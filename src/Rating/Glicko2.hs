{-# LANGUAGE TemplateHaskell #-}
module Rating.Glicko2
    (
    Rating (..),
    MatchRes (..),
    rate
    ) where

import           Lens.Micro ((^.))
import           Lens.Micro.TH (makeLenses)


data Rating = R { _mu :: Double -- rating
                , _phi :: Double -- rating deviation
                , _sigma :: Double -- rating volatility
                } deriving (Eq, Show)

makeLenses ''Rating

data MatchRes = Win | Draw | Loss deriving (Eq, Show)

--constants
tau = 1 -- scelte sensate 0.3 - 1.2
epsilon = 0.000001
initialRating = R 1500 350 0.06

resScore :: MatchRes -> Double
resScore Win = 1
resScore Draw = 0.5
resScore Loss = 0

scaleDown :: Rating -> Rating
scaleDown (R m p s) = R m' p' s
    where
        m' = (m - 1500) / ratio
        p' = p / ratio
        ratio = 173.7178

scaleUp :: Rating -> Rating
scaleUp (R m p s) = R m' p' s
    where
        m' = m * ratio + 1500
        p' = p * ratio
        ratio = 173.7178

-- The original form is `g(RD)`. This function reduces the impact of
-- games as a function of an opponent's RD.
g :: Double -> Double
g phi = 1 / (sqrt (1 + ((3 * phi ^ 2) / pi ^ 2)))

-- expected score
e :: Double -> Double -> Double -> Double
e m mj phij = 1 / (1 + exp (- (g phij) * (m - mj)))

rate :: Double -> Double -> Rating -> [(MatchRes, Rating)] -> Rating
rate epsilon tau currentRating matches = scaleUp $ R mu' phi' sigma'
    where
        -- step 1. determinate initial rating of period and constant
        -- step 2. scale down to glicko 2 scale
        r = scaleDown currentRating
        ms = over (mapped._2) scaleDown matches
        -- step 3. calc the quantity v = estimated team variance for the period
        v = 1 / sum (zipWith3 (\x y z -> x * y * z)
                    (fmap (\m -> (g (m^._2.phi) ^ 2)) ms)
                    (fmap (\m -> e (r^.mu) (m^._2.mu) (m^._2.phi)) ms)
                    (fmap (\m -> 1 - e (r^.mu) (m^._2.mu) (m^._2.phi)) ms))
        -- step 4. calc the value delta = estimated rating improvement
        delta = v * sum (zipWith (*)
                        (fmap (\m -> g (m^._2.phi)) ms)
                        (fmap (\m -> (resScore (m^._1)) - (e (r^.mu) (m^._2.mu) (m^._2.phi))) ms))
        -- step 5. calc the new volatility, iterative process
        alpha = log ((r^.sigma) ^ 2)
        f x = (exp x * (delta ^ 2 - (r^.phi) ^ 2 - v - exp x)) / (2 * ((r^.phi) ^ 2 + v + exp x)) - (x - alpha) / (tau ^ 2)
        -- initialize algorithm variables
        a = alpha
        b = if (delta ^ 2 > (r^.phi) ^ 2 + v)
               then log (delta ^ 2 - (r^.phi) ^ 2 - v)
               else
                 let f' k = f (alpha - k * tau) 
                     i    = findIndex (> 0) (fmap f' [1..])
                     in alpha - (fromIntegral (fromJust i) + 1) * tau
        fa = f a
        fb = f b
        iter a b fa fb = if abs (b - a) > epsilon
                            then
                                let c = a + (a - b) * fa / (fb - fa)
                                    fc = f c
                                    (a', fa') = if fc * fb < 0
                                                   then (b, fb)
                                                   else (a, fa / 2)
                                    b' = c
                                    fb' = fc
                                 in iter a' b' fa' fb'
                            else
                                exp (a / 2)
        sigma' = iter a b fa fb

        phistar = sqrt ((r^.phi) ^ 2 + sigma' ^ 2)
        phi' = 1 / sqrt ((1 / (phistar ^ 2)) + 1 / v)
        mu' = r^.mu + phi' ^ 2 * sum (zipWith (*)
                                     (fmap (\m -> g (m^._2.phi)) ms)
                                     (fmap (\m -> (resScore (m^._1)) - (e (r^.mu) (m^._2.mu) (m^._2.phi))) ms))

{-

test rating

p1 = R 1500 200 0.06
p2 = R 1400 30 0.06
p3 = R 1550 100 0.06
p4 = R 1700 300 0.06

matches = [(Win, p2), (Loss, p3), (Loss, p4)]

rate 0.000001 0.5 p1 matches


r = scaleDown p1
r
R {_mu = 0.0, _phi = 1.1512924985234674, _sigma = 6.0e-2}


ms = over (mapped._2) scaleDown matches
ms
v = 1 / sum (zipWith3 (\x y z -> x * y * z)
            (fmap (\m -> (g (m^._2.phi) ^ 2)) ms)
            (fmap (\m -> e (r^.mu) (m^._2.mu) (m^._2.phi)) ms)
            (fmap (\m -> 1 - e (r^.mu) (m^._2.mu) (m^._2.phi)) ms))
v
1.7789770897239976


delta = v * sum (zipWith (*)
                (fmap (\m -> g (m^._2.phi)) ms)
                (fmap (\m -> (resScore (m^._1)) - (e (r^.mu) (m^._2.mu) (m^._2.phi))) ms))
delta
-0.4839332609836549

alpha = log ((r^.sigma) ^ 2)
tau = 0.5
f x = (exp x * (delta ^ 2 - (r^.phi) ^ 2 - v - exp x)) / (2 * ((r^.phi) ^ 2 + v + exp x)) - (x - alpha) / (tau ^ 2)
iter a b fa fb = if (abs (b - a)) > epsilon
                    then
                        let c = a + (a - b) * fa / (fb - fa)
                            fc = f c
                            (a', fa') = if fc * fb < 0
                                           then (b, fb)
                                           else (a, fa / 2)
                            b' = c
                            fb' = fc
                         in iter a' b' fa' fb'
                    else
                        exp (a / 2)
sigma' = iter (-5.62682) (-6.12682) (-0.00053567) 1.999675
sigma'
5.9987525892463714e-2

phistar = sqrt ((r^.phi) ^ 2 + sigma' ^ 2)
phistar
1.1528542494257925

phi' = 1 / sqrt ((1 / (phistar ^ 2)) + 1 / v)
phi'
0.8721989975283095

-}
