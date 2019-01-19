module CityGML.Namespaces where


modules = [
        (""      , "http://www.opengis.net/citygml/2.0"                 ),
        ("gml"  , "http://www.opengis.net/gml"                          ),
        ("wtr"  , "http://www.opengis.net/citygml/waterbody/2.0"        ),
        ("app"  , "http://www.opengis.net/citygml/appearance/2.0"       ),
        ("tex"  , "http://www.opengis.net/citygml/texturedsurface/2.0"  ),
        ("veg"  , "http://www.opengis.net/citygml/vegetation/2.0"       ),
        ("dem"  , "http://www.opengis.net/citygml/relief/2.0"           ),
        ("tran" , "http://www.opengis.net/citygml/transportation/2.0"   ),
        ("bldg" , "http://www.opengis.net/citygml/building/2.0"         ),
        ("grp"  , "http://www.opengis.net/citygml/cityobjectgroup/2.0"  ),
        ("tun"  , "http://www.opengis.net/citygml/tunnel/2.0"           ),
        ("frn"  , "http://www.opengis.net/citygml/cityfurniture/2.0"    ),
        ("brid" , "http://www.opengis.net/citygml/bridge/2.0"           ),
        ("gen"  , "http://www.opengis.net/citygml/generics/2.0"         ),
        ("luse" , "http://www.opengis.net/citygml/landuse/2.0"          ),
        ("xal"   , "urn:oasis:names:tc:ciq:xsdschema:xAL:2.0"           ),
        ("xlink" , "http://www.w3.org/1999/xlink"                       ),
        ("xsi"   , "http://www.w3.org/2001/XMLSchema-instance"          )
    ]

customNs = [
        ("", "xmlns", "http://www.opengis.net/citygml/2.0"),
        ("xsi", "schemaLocation", "http://www.opengis.net/citygml/waterbody/2.0 http://schemas.opengis.net/citygml/waterbody/2.0/waterBody.xsd http://www.opengis.net/citygml/appearance/2.0 http://schemas.opengis.net/citygml/appearance/2.0/appearance.xsd http://www.opengis.net/citygml/texturedsurface/2.0 http://schemas.opengis.net/citygml/texturedsurface/2.0/texturedSurface.xsd http://www.opengis.net/citygml/vegetation/2.0 http://schemas.opengis.net/citygml/vegetation/2.0/vegetation.xsd http://www.opengis.net/citygml/relief/2.0 http://schemas.opengis.net/citygml/relief/2.0/relief.xsd http://www.opengis.net/citygml/transportation/2.0 http://schemas.opengis.net/citygml/transportation/2.0/transportation.xsd http://www.opengis.net/citygml/building/2.0 http://schemas.opengis.net/citygml/building/2.0/building.xsd http://www.opengis.net/citygml/cityobjectgroup/2.0 http://schemas.opengis.net/citygml/cityobjectgroup/2.0/cityObjectGroup.xsd http://www.opengis.net/citygml/tunnel/2.0 http://schemas.opengis.net/citygml/tunnel/2.0/tunnel.xsd http://www.opengis.net/citygml/cityfurniture/2.0 http://schemas.opengis.net/citygml/cityfurniture/2.0/cityFurniture.xsd http://www.opengis.net/citygml/bridge/2.0 http://schemas.opengis.net/citygml/bridge/2.0/bridge.xsd http://www.opengis.net/citygml/generics/2.0 http://schemas.opengis.net/citygml/generics/2.0/generics.xsd http://www.opengis.net/citygml/landuse/2.0 http://schemas.opengis.net/citygml/landuse/2.0/landUse.xsd" )
        ]

modulesNs :: [(String, String)] -> [(String, String)]
modulesNs []         = []
modulesNs ((a,b):xs) = ("xmlns:" ++ a, b) : modulesNs xs

specialNs :: [(String, String, String)] -> [(String, String)]
specialNs []            = []
specialNs (("",b,c):xs) = (b,c) : specialNs xs
specialNs ((a,b,c):xs)  = (a ++ ":" ++ b, c) : specialNs xs

namespaces :: [(String, String)]
namespaces = specialNs customNs ++ modulesNs modules
