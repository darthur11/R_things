source('functions.R')

drive_auth('C:/Users/artur.dossatayev/Documents/credentials/googledrive_token.rds')

drive_update(as_id('id'), media = 'output/index.xlsx')
