library(stringr)
library(jsonlite)
library(tidyverse)

current.filename = c("The Front Bottoms-Talon Of The Hawk-Au Revoir (Adios).json")

(name.without.json = str_sub(current.filename, start = 1, nchar(current.filename) - 5))

#Setting Song, Artist, album

(artist = str_split_i(name.without.json, "-", 1))

(album = str_split_i(name.without.json, "-", 2))

(track = str_split_i(name.without.json, "-", 3))


#Pulling JSON Data

essentia.output = paste("EssentiaOutput", current.filename, sep = "/")

(jsondata = fromJSON(essentia.output)) 


overall_loudness = jsondata$lowlevel$loudness_ebu128$integrated
spectral_energy = jsondata$lowlevel$spectral_energy$mean
dissonance = jsondata$lowlevel$dissonance$mean
pitch_salience = jsondata$lowlevel$pitch_salience$mean
bpm = jsondata$rhythm$bpm
beats_loudness = jsondata$rhythm$beats_loudness$mean
danceability = jsondata$rhythm$danceability
tuning_frequency = jsondata$tonal$tuning_frequency

name.with.json = list.files("EssentiaOutput")
#Making empty vectors
overall_loudness = c()
spectral_energy = c()
dissonance = c()
pitch_salience = c()
bpm = c()
beats_loudness = c()
danceability = c()
tuning_frequency = c()


name.without.json = c()

artist = c()
album = c()
track = c()

#Putting everything as vectors and filling them in
for(i in 1:length(list.files("EssentiaOutput"))){
  current.filename = list.files("EssentiaOutput")[i]
  
  (name.without.json[i] = str_sub(current.filename, start = 1, nchar(current.filename) - 5))
  
  #Setting Song, Artist, album
  
  (artist[i] = str_split_i(name.without.json[i], "-", 1))
  
  (album[i] = str_split_i(name.without.json[i], "-", 2))
  
  (track[i] = str_split_i(name.without.json[i], "-", 3))
  
  
  #Pulling JSON Data
  
  essentia.output = paste("EssentiaOutput", current.filename, sep = "/")
  
  (jsondata = fromJSON(essentia.output))
  
  overall_loudness[i] = jsondata$lowlevel$loudness_ebu128$integrated
  spectral_energy[i] = jsondata$lowlevel$spectral_energy$mean
  dissonance[i] = jsondata$lowlevel$dissonance$mean
  pitch_salience[i] = jsondata$lowlevel$pitch_salience$mean
  bpm[i] = jsondata$rhythm$bpm
  beats_loudness[i] = jsondata$rhythm$beats_loudness$mean
  danceability[i] = jsondata$rhythm$danceability
  tuning_frequency[i] = jsondata$tonal$tuning_frequency
}

#Saving as data frame
data.from.all.songs = tibble(name.with.json, artist, album, track, overall_loudness, spectral_energy, dissonance, pitch_salience, bpm, beats_loudness, danceability, tuning_frequency)

valence = c()

more.info <- read_csv("EssentiaModelOutput.csv") %>%
  mutate(valence = rowMeans(select(., contains("valence")))) %>%
  mutate(arousal = rowMeans(select(., contains("arousal")))) %>%
  mutate(aggressive = rowMeans(select(., contains("aggressive") & !contains("not_aggressive")))) %>%
  mutate(happy = rowMeans(select(., contains("happy") & !contains("not_happy")))) %>%
  mutate(sad = rowMeans(select(., contains("sad") & !contains("not_sad")))) %>%
  mutate(party = rowMeans(select(., contains("party") & !contains("not_party")))) %>%
  mutate(relaxed = rowMeans(select(., contains("relax") & !contains("not_relax")))) %>%
  mutate(acoustic = rowMeans(select(., contains("acoustic") & !contains("not_acoustic")))) %>%
  mutate(electric = rowMeans(select(., contains("electronic") & !contains("not_electronic")))) %>%
  mutate(instrumental = rowMeans(select(., contains("instrumental") & !contains("not_instrumental")))) %>%
  mutate(timbreBright = eff_timbre_bright) %>%
  select(artist,album,track,valence,arousal,aggressive,happy,sad,party,relaxed,acoustic,electric,instrumental,timbreBright)

  
LIWCOutput = read_csv("LIWCOutput/LIWCOutput.csv")

two = merge(LIWCOutput, more.info)         
final.df = merge(data.from.all.songs,two)
         



