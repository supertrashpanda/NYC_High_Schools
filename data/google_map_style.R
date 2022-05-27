create_style_string<- function(style_list){
  style_string <- ""
  for(i in 1:length(style_list)){
    if("featureType" %in% names(style_list[[i]])){
      style_string <- paste0(style_string, "feature:", 
                             style_list[[i]]$featureType, "|")      
    }
    elements <- style_list[[i]]$stylers
    style_string<-paste0(style_string,"element:",style_list[[i]]$elementType,"|")
    a <- lapply(elements, function(x)paste0(names(x), ":", x)) %>%
      unlist() %>%
      paste0(collapse="|")
    style_string <- paste0(style_string, a)
    if(i < length(style_list)){
      style_string <- paste0(style_string, "&style=")       
    }
  }  
  
  # google wants 0xff0000 not #ff0000
  style_string <- gsub("#", "0x", style_string)
  return(style_string)
}

style <- '[
  {
    "elementType": "labels",
    "stylers": [
      {
        "visibility": "off"
      }
    ]
  },
  {
    "featureType": "administrative.land_parcel",
    "stylers": [
      {
        "color": "#bdb775"
      },
      {
        "visibility": "off"
      }
    ]
  },
  {
    "featureType": "administrative.neighborhood",
    "stylers": [
      {
        "visibility": "off"
      }
    ]
  },
  {
    "featureType": "landscape",
    "stylers": [
      {
        "color": "#f7f0ca"
      },
      {
        "visibility": "on"
      }
    ]
  },
  {
    "featureType": "landscape",
    "elementType": "labels",
    "stylers": [
      {
        "visibility": "off"
      }
    ]
  },
  {
    "featureType": "landscape.natural.terrain",
    "stylers": [
      {
        "visibility": "on"
      }
    ]
  },
  {
    "featureType": "road.highway",
    "stylers": [
      {
        "weight": 0.5
      }
    ]
  },
  {
    "featureType": "road.highway",
    "elementType": "geometry.fill",
    "stylers": [
      {
        "visibility": "off"
      }
    ]
  },
  {
    "featureType": "road.local",
    "elementType": "geometry.fill",
    "stylers": [
      {
        "visibility": "simplified"
      }
    ]
  },
  {
    "featureType": "water",
    "stylers": [
      {
        "color": "#8d94fc"
      }
    ]
  }
]'

style_list <- fromJSON(style, asText=TRUE)