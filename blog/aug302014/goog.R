library(rMaps)
map <- Leaflet$new()
map$setView(c(51.505, -0.09), zoom = 13)
map$tileLayer(provider = 'Stamen.Watercolor')
# map$marker(
#   c(51.5, -0.09),
#   bindPopup = 'Hi. I am a popup'
# )
map$geoJson(
  type = 'LineString',
  c(51.5, -0.09, 50.5, -0.09)
)
map


{
  "type": "Feature",
  "properties": {},
  "geometry": {
    "type": "LineString",
    "coordinates": [
      [
        17.578125,
        58.26328705248601
        ],
      [
        29.8828125,
        50.958426723359935
        ],
      [
        41.1328125,
        54.36775852406841
        ],
      [
        53.0859375,
        46.55886030311719
        ]
      ]
  }
},
