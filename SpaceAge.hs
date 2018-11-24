module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

ageOn :: Planet -> Float -> Float
ageOn planet second = second / secondsInDay / earthYear / orbitalPeriod planet
  where
    secondsInDay = 86400
    earthYear = 365.25
    orbitalPeriod Earth   = 1
    orbitalPeriod Mercury = 0.240846
    orbitalPeriod Venus   = 0.61519726
    orbitalPeriod Mars    = 1.8808158
    orbitalPeriod Jupiter = 11.862615
    orbitalPeriod Saturn  = 29.447498
    orbitalPeriod Uranus  = 84.016846 
    orbitalPeriod Neptune = 164.79132
