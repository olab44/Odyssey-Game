module Utils where

-- Sekwencje escape ANSI
red, green, yellow, reset :: String
red = "\ESC[31m"
green = "\ESC[32m"
yellow = "\ESC[33m"
reset = "\ESC[0m"
