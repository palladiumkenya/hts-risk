# HTS Risk
HTS Risk Assessment Model

## Get
`git clone https://github.com/palladiumkenya/hts-risk.git`

## Build
`cd hts-risk`
`docker build -t hts-risk .`

## Run
`docker run --name hts-risk -d -p 80:3838 -v ${PWD}:/srv/shiny-server hts-risk`
