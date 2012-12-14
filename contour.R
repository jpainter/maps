library(RODBC)


# import data from access database
  geodb <- file.path("C:/Users/bzp3/Desktop/R/Maps/geocode.accdb") 
  channel <- odbcConnectAccess2007(geodb) 
  Data <- sqlFetch(channel,"Table1") 
 
# SQL query
sql_beg = 'SELECT COUNT(*) FROM sightings WHERE MBRContains(GeomFromText(\'LineString('
sql_end = ')\'), lat_lng)'

# Northwest and southeast points of the United States
lat_NW = 54.162434
lng_NW = -135.351563
lat_SE = 23.725012
lng_SE = -61.347656

# About 20 miles
lat_incr = -.29
lng_incr = .29

lat_curr = lat_NW
lng_curr = lng_NW
cnt_matrix = []
num_rows = 0

# North to south
while lat_curr > lat_SE:
lat_nxt = lat_curr + lat_incr
lng_curr = lng_NW
row_curr = []

# West to east
while lng_curr < lng_SE:
lng_nxt = lng_curr + lng_incr
sql = sql_beg + str(lat_curr) + " " + str(lng_curr) + ", " + str(lat_nxt) + " " + str(lng_nxt) + sql_end

# Execute the SQL and store data in array
cursor.execute(sql)
result = cursor.fetchone()
cnt = result[0]
row_curr.append(cnt)
lng_curr += lng_incr

cnt_matrix.append(row_curr)
lat_curr += lat_incr
num_rows += 1

# Output the counts matrix
for i in range(len(cnt_matrix)):
s = ', '.join(map(str, cnt_matrix[i]))
print s

# Counts matrix derived using Python/MySQL
cnts <- read.csv("cnt_matrix.txt", header=FALSE)
cnts_matrix <- as.matrix(cnts)
dimnames(cnts_matrix)[[2]] <- NULL

# Make a contour plot
contour(cnts_matrix)

# Transform counts matrix
cnts_matrix <- t(cnts_matrix[105:1,])
contour(cnts_matrix)

# Contour plot with logarithmic scale
contour(log(cnts_matrix+1))

# Using arguments in function
contour(log(cnts_matrix+1), xaxt="n", yaxt="n", nlevels=5, lwd=0.5)

# Color palettes for contours
bwpal <- colorRampPalette(c("white", "black"))
orangepal <- colorRampPalette(c("white", "#c75a00"))

# Contour with colors
contour(log(cnts_matrix+1), xaxt="n", yaxt="n", nlevels=5, col=bwpal(6))

# With rainbow palette
contour(log(cnts_matrix+1), xaxt="n", yaxt="n", nlevels=10, col=rainbow(12))

# Black-red-white palette for filled contours
redpal <- colorRampPalette(c("black", "red", "white"))

# Make the filled contour plot
filled.contour(log(cnts_matrix+1), color.palette=redpal, nlevels=5)
filled.contour(log(cnts_matrix+1), color.palette=redpal, nlevels=30)

# More color palettes
cyanpal <- colorRampPalette(c("black", "cyan", "white"))
purppal <- colorRampPalette(c("black", "purple", "white"))
bluepal <- colorRampPalette(c("black", "blue", "white"))

# Trying different palettes
filled.contour(log(cnts_matrix+1), color.palette=cyanpal, nlevels=30)
filled.contour(log(cnts_matrix+1), color.palette=purppal, nlevels=30)
filled.contour(log(cnts_matrix+1), color.palette=bluepal, nlevels=30)
filled.contour(log(cnts_matrix+1), color.palette=bwpal, nlevels=30)

