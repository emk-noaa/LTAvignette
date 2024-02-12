# validation - data processing

# Local paths to these files
SIGHTS <- read.csv('data/SIGHTS.csv')
EFFORT <- read.csv('data/EFFORT.csv')

abund <-
  SIGHTS %>%
  tidyr::pivot_longer(cols = 31:101,
                      names_to = 'species',
                      values_to = 'best') %>%
  filter(best > 0) %>%
  mutate(Region = gsub(' ','',Region)) %>%
  mutate(DateTime = paste0(Yr,'-',Mo,'-',Da,' ',Hr,':',Min))

abund_summ <-
  abund %>%
  group_by(cruise = CruzNo, species) %>%
  summarize(ntot_abund = n(),
            nsys_abund = length(which(! Region %in% c('NONE',
                                                      'Off-Transect') &
                                        EffortSeg > 0))) %>%
  mutate(species = gsub('SP','',species))

ltabundr <-
  cruz$cohorts$all$sightings %>%
  # Filter out species that ABUND ignored based on its INP file
  filter(!species %in% c('CU', 'PU'))

ltabundr_summ <-
  ltabundr %>%
  filter(OnEffort == TRUE) %>%
  group_by(cruise = Cruise, species) %>%
  summarize(ntot_ltabundr = n(),
            nsys_ltabundr = length(which(included == TRUE &
                                           EffType %in% c('S','F'))))

mr <- full_join(abund_summ, ltabundr_summ, by=c('cruise', 'species'))

mr$ntot_abund %>% sum(na.rm=TRUE)

