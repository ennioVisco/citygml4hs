module CityGML.Namespaces where


modules = [
        ("core"  , "http://www.opengis.net/citygml/1.0"                 ),
        ("gen"   , "http://www.opengis.net/citygml/generics/1.0"        ),
        ("grp"   , "http://www.opengis.net/citygml/cityobjectgroup/1.0" ),
        ("app"   , "http://www.opengis.net/citygml/appearance/1.0"      ),
        ("bldg"  , "http://www.opengis.net/citygml/building/1.0"        ),
        ("gml"   , "http://www.opengis.net/gml"                         ),
        ("xal"   , "urn:oasis:names:tc:ciq:xsdschema:xAL:2.0"           ),
        ("xlink" , "http://www.w3.org/1999/xlink"                       ),
        ("xsi"   , "http://www.w3.org/2001/XMLSchema-instance"          )
    ]

customNs = [
        ("", "xmlns", "http://www.opengis.net/citygml/1.0"),
        ("xsi", "schemaLocation", "http://www.opengis.net/citygml/1.0 http://repository.gdi-de.org/schemas/adv/citygml/1.0/cityGMLBaseLoD2.xsd http://www.opengis.net/citygml/building/1.0 http://repository.gdi-de.org/schemas/adv/citygml/building/1.0/buildingLoD2.xsd http://www.opengis.net/citygml/appearance/1.0 http://repository.gdi-de.org/schemas/adv/citygml/appearance/1.0/appearanceLoD2.xsd http://www.opengis.net/citygml/generics/1.0 http://repository.gdi-de.org/schemas/adv/citygml/generics/1.0/genericsLoD2.xsd http://www.opengis.net/citygml/cityobjectgroup/1.0 http://repository.gdi-de.org/schemas/adv/citygml/cityobjectgroup/1.0/cityObjectGroupLoD2.xsd")
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
