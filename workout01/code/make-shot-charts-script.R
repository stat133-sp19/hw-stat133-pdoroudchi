# Making Shot Charts of GSW Players
# description: we first use the shot data of each GSW player to make individual shot charts and export these as PDF files. We finally use the global shot data table that includes all five GSW players to create a facetted shot chart, which we then export in multiple formats.
# input(s): individual data frames (iguodala, draymond, thompson, durant, curry) and the global data frame (shots_data)
# output: individual shot chart PDFs, facetted shot chart in PDF and PNG format

# Andre Iguodala scatterplot

iguodala_scatterplot <- ggplot(data = iguodala) + geom_point(aes(x = x, y = y, color = shot_made_flag))
iguodala_scatterplot

# court image

court_file <- '../images/nba-court.jpg'

# create raste object

court_image <- rasterGrob(
  readJPEG(court_file),
  width = unit(1, 'npc'),
  height = unit(1, 'npc'))

# Iguodala shot chart with court background

iguodala_shot_chart <- ggplot(data = iguodala) + annotation_custom(court_image, -250, 250, -50, 420) + geom_point(aes(x = x, y = y, color = shot_made_flag)) + ylim(-50, 420) +
  ggtitle('Shot Chart: Andre Iguodala (2016 season)') + theme_minimal()

# saving Iguodala plot as PDF inside images folder

pdf(file = '../images/andre-iguodala-shot-chart.pdf',
    width = 6.5,
    height = 5)
iguodala_shot_chart
dev.off()

# Draymond Green shot chart with court background

draymond_shot_chart <- ggplot(data = draymond) + annotation_custom(court_image, -250, 250, -50, 420) + geom_point(aes(x = x, y = y, color = shot_made_flag)) + ylim(-50, 420) +
  ggtitle('Shot Chart: Draymond Green (2016 season)') + theme_minimal()

# saving Draymond plot as PDF inside images folder

pdf(file = '../images/draymond-green-shot-chart.pdf',
    width = 6.5,
    height = 5)
draymond_shot_chart
dev.off()

# Klay Thompson shot chart with court background

thompson_shot_chart <- ggplot(data = thompson) + annotation_custom(court_image, -250, 250, -50, 420) + geom_point(aes(x = x, y = y, color = shot_made_flag)) + ylim(-50, 420) +
  ggtitle('Shot Chart: Klay Thompson (2016 season)') + theme_minimal()

# saving Thompson plot as PDF inside images folder

pdf(file = '../images/klay-thompson-shot-chart.pdf',
    width = 6.5,
    height = 5)
thompson_shot_chart
dev.off()

# Kevin Durant shot chart with court background

durant_shot_chart <- ggplot(data = durant) + annotation_custom(court_image, -250, 250, -50, 420) + geom_point(aes(x = x, y = y, color = shot_made_flag)) + ylim(-50, 420) +
  ggtitle('Shot Chart: Kevin Durant (2016 season)') + theme_minimal()

# saving Durant plot as PDF inside images folder

pdf(file = '../images/kevin-durant-shot-chart.pdf',
    width = 6.5,
    height = 5)
durant_shot_chart
dev.off()

# Stephen Curry shot chart with court background

curry_shot_chart <- ggplot(data = curry) + annotation_custom(court_image, -250, 250, -50, 420) + geom_point(aes(x = x, y = y, color = shot_made_flag)) + ylim(-50, 420) +
  ggtitle('Shot Chart: Stephen Curry (2016 season)') + theme_minimal()

# saving Curry plot as PDF inside images folder

pdf(file = '../images/stephen-curry-shot-chart.pdf',
    width = 6.5,
    height = 5)
curry_shot_chart
dev.off()

# Facetted shot chart

gsw_shot_charts <- ggplot(data = shots_data) + annotation_custom(court_image, -250, 250, -50, 420) + geom_point(aes(x = x, y = y, color = shot_made_flag, shape = shot_made_flag)) + ylim(-50, 420) +
  ggtitle('Shot Charts: GSW (2016 season)') + theme_minimal() + facet_wrap(~ name) + theme(legend.position="top", legend.title = element_blank())

# saving gsw shot charts as PDF inside images folder

pdf(file = '../images/gsw-shot-charts.pdf',
    width = 8,
    height = 7)
gsw_shot_charts
dev.off()

# saving gsw shot charts as PNG inside images folder

png(filename = '../images/gsw-shot-charts.png',
    width = 8,
    height = 7,
    units = 'in',
    res = 300)
gsw_shot_charts
dev.off()
