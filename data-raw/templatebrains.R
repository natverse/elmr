v14iurl="https://neuropil.janelia.org/tracing/fafb/v14/1/stack/5/info"
v13iurl="https://neuropil.janelia.org/tracing/fafb/v13/1/stack/5/info"
v13i=jsonlite::fromJSON(content(GET(v13iurl), 'text'))
v14i=jsonlite::fromJSON(content(GET(v14iurl), 'text'))

FAFB14=make_template(v14i)
FAFB14$type=FAFB13$type
FAFB14$sex=FAFB13$sex
FAFB14$units=FAFB13$units
# actually we end up making this manually in data/templatebrains.R
# after doing dput on the object created above
# devtools::use_data(FAFB14)
