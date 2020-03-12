
setwd("C:/Bike Data/")
stations <- read.csv("stations_with_census_data.csv")

stations$lakefront_trail_north<-ifelse(stations$station_name %in% c('Michigan Ave & Oak St','Lakeshore Dr & Belmont Ave',
'Richie Ct & Banks St','Clark St & North Ave','Clark St & Lincoln Ave','Clark St & Armitage Ave','Cannon Dr & Fullerton Ave',
      'Stockton Dr & Wrightwood Ave','Lake Shore Dr & Diversey Pkwy','Lake Shore Dr & Wellington Ave'),1,0)

stations$lakefront_trail_south<-ifelse(stations$station_name %in% c('Streeter Dr & Grand Ave','Ft Dearborn Dr & 31st St',
                                                                    'Lakeshore Dr & Ohio St','DuSable Harbor','Burnham Harbor',
                                                                    'Rhodes Ave & 32nd St'),1,0)

stations$the_606<-ifelse(stations$station_name %in% c('Marshfield Ave & Courtland St','Damen Ave & Courtland St','Milwaukee Ave & Wabansia St',
                                                      'Western Ave & Winnebago Ave','Albany Ave & Bloomingdale Ave'),1,0)

stations$milwaukee_ave<-ifelse(stations$station_name %in% c('Milwaukee & Desplaines','Milwaukee & Grand (Blue Line Station)',
                                                            'Milwaukee & Division (Blue Line Station)','Milwaukee & Damen (Blue Line Station)',
                                                            'Milwaukee & California (Blue Line Station)','Milwaukee & Kedzie (Blue Line Station)',
                                                            'Milwaukee & Central Park Ave','Milwaukee & Cuyler Ave'),1,0)

stations$navy_pier<-ifelse(stations$station_name %in% c('Streeter Dr & Grand Ave','Lakeshore Dr & Ohio St'),1,0)

stations$museum_campus<-ifelse(stations$station_name %in% c('Adler Planetarium','Shedd Aquarium','Field Museum','Burnham Harbor',
                                                            'Michigan Ave & 14th St','Calumet Ave & 18th St'),1,0)

stations$promontory_point<-ifelse(stations$station_name %in% c('Shore Dr & 55th St','Lake Park Ave & 53rd St'),1,0)

stations$humboldt_park<-ifelse(stations$station_name %in% c('Humblodt Dr & Luis Munoz Marin Dr','California Ave & Division St','Troy St & North Ave',
                                                            'California Ave & North Ave'),1,0)

stations$grant_park<-ifelse(stations$station_name %in% c('Columbus Dr & Randolph St','Michigan Ave & Washington St','Michigan Ave & Madison St',
                                                         'Lake Shore Dr & Monroe St','Millennium Park','Michigan Ave & Jackson Blvd',
                                                         'Michigan Ave & Congress Pkwy','Michigan Ave & Balbo Ave','Indiana Ave & Roosevelt Rd',
                                                         'Shedd Aquarium','Field Museum','Adler Planetarium'),1,0)

stations$millenium_park<-ifelse(stations$station_name %in% c('Millennium Park','Michigan Ave & Madison St','Michigan Ave & Washington St'),1,0)

stations$hyde_park<-ifelse(stations$station_name %in% c('Blackstone Ave & Hyde Park Blvd','Cornell Ave & Hyde Park Blvd','Lake Park Ave & 53rd St',
                                                        'Kimbark Ave & 53rd St','Ellis Ave & 53rd St','Ellis Ave & 55th St',
                                                        'Woodlawn Ave & 55th St','University Ave & 57th St','Ellis Ave & 58th St',
                                                        'Harper Ave & 59th St','Museum of Science and Industry','Lake Park Ave & 56th St',
                                                        'Shore Dr & 55th S'),1,0)

stations$wrigley_field<-ifelse(stations$station_name %in% c('Sheffield Ave & Waveland Ave','Clark St & Grace St'),1,0)

stations$lincoln_park<-ifelse(stations$station_name %in% c('Clark St & Armitage Ave','Clark St & Lincoln Ave','Sedgwick St & Webster Ave'),1,0)

stations$northwestern_university<-ifelse(stations$station_name %in% c('Chicago Ave & Sheridan Rd','Sheridan Rd & Noyes St (NU)',
                                                                      'University Library (NU)'),1,0)

write.csv(stations,"stations_with_census_data.csv")




