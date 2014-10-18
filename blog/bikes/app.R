# Data which comes from the follwong R code.
# options(stringsAsFactors = FALSE)

# bike <- read.csv('2014-Q2-Trips-History-Data.csv')[, c(3, 6)]
# names(bike) <- c('inn', 'out')

#load('chord.RData')

#station <- unique(c(bike$inn, bike$out))


chord <- function(val) {
  
  
  start <- '<!DOCTYPE html>
  <meta charset="utf-8">
  <style>
  
  body {
  font: 10px sans-serif;
  }
  
  .chord path {
  fill-opacity: .67;
  stroke: #000;
  stroke-width: .5px;
  }
  
  </style>
  <body>
  <script src="http://d3js.org/d3.v3.min.js"></script>
  <script>
  
  // From http://mkweb.bcgsc.ca/circos/guide/tables/
  var matrix = ['
  
  
  mid <- '];
  
  var chord = d3.layout.chord()
  .padding(.05)
  .sortSubgroups(d3.descending)
  .matrix(matrix);
  
  var width = 960,
  height = 500,
  innerRadius = Math.min(width, height) * .41,
  outerRadius = innerRadius * 1.1;
  
  var fill = d3.scale.ordinal()
  .domain(d3.range('
  
  
  end <- ']);
  
  var svg = d3.select("body").append("svg")
  .attr("width", width)
  .attr("height", height)
  .append("g")
  .attr("transform", "translate(" + width / 2 + "," + height / 2 + ")");
  
  svg.append("g").selectAll("path")
  .data(chord.groups)
  .enter().append("path")
  .style("fill", function(d) { return fill(d.index); })
  .style("stroke", function(d) { return fill(d.index); })
  .attr("d", d3.svg.arc().innerRadius(innerRadius).outerRadius(outerRadius))
  .on("mouseover", fade(.1))
  .on("mouseout", fade(1));
  
  var ticks = svg.append("g").selectAll("g")
  .data(chord.groups)
  .enter().append("g").selectAll("g")
  .data(groupTicks)
  .enter().append("g")
  .attr("transform", function(d) {
  return "rotate(" + (d.angle * 180 / Math.PI - 90) + ")"
  + "translate(" + outerRadius + ",0)";
  });
  
  ticks.append("line")
  .attr("x1", 1)
  .attr("y1", 0)
  .attr("x2", 5)
  .attr("y2", 0)
  .style("stroke", "#000");
  
  ticks.append("text")
  .attr("x", 8)
  .attr("dy", ".35em")
  .attr("transform", function(d) { return d.angle > Math.PI ? "rotate(180)translate(-16)" : null; })
  .style("text-anchor", function(d) { return d.angle > Math.PI ? "end" : null; })
  .text(function(d) { return d.label; });
  
  svg.append("g")
  .attr("class", "chord")
  .selectAll("path")
  .data(chord.chords)
  .enter().append("path")
  .attr("d", d3.svg.chord().radius(innerRadius))
  .style("fill", function(d) { return fill(d.target.index); })
  .style("opacity", 1);
  
  // Returns an array of tick angles and labels, given a group.
  function groupTicks(d) {
  var k = (d.endAngle - d.startAngle) / d.value;
  return d3.range(0, d.value, 1000).map(function(v, i) {
  return {
  angle: v * k + d.startAngle,
  label: i % 5 ? null : v / 1000 + "k"
  };
  });
  }
  
  // Returns an event handler for fading a given chord group.
  function fade(opacity) {
  return function(g, i) {
  svg.selectAll(".chord path")
  .filter(function(d) { return d.source.index != i && d.target.index != i; })
  .transition()
  .style("opacity", opacity);
  };
  }
  
  </script>
</body>
</html>
  '
  
  
  colors <- c("#99FF99", "#6666FF", "#FFDD89", "#957244", "#F26223",
              '#660000', '#663300', '#6666FF', '#6699FF')
  html <- start
  tabl <- ''
  for(i in 1:dim(val)[1]) {
    tabl <- paste(tabl, '[ ', sep = '')
    for(j in 1:dim(val)[2]) {
      if (j == dim(val)[2]) {
        tabl <- paste(tabl, val[i, j], sep = '')
      } else {
        tabl <- paste(tabl, val[i, j], ', ', sep = '')
      }
    }
    if (i == dim(val)[1]) {
      tabl <- paste(tabl, ']\n', sep = '')
    } else {
      tabl <- paste(tabl, '],\n', sep = '')
    }
  }
  
  html <- paste(html, tabl, mid, sep = '')
  
  html <- paste(html, dim(val)[1], sep = '')
  
  html <- paste(html, '))
                .range([', sep = '')
  
  for(i in 1:dim(val)[1]) {
    if (i != dim(val)[1]) {
      html <- paste(html, '"', colors[i], '", ', sep = '')
    } else {
      html <- paste(html, '"', colors[i], '"', sep = '')
    }
  }
  
  cat(paste(html, end, sep = ''))
  
}

chord_plot <- function(val) {
  sink('tmp.html')
  chord(val)
  sink()
  suppressWarnings(html('tmp.html'))
}








#load('bike_stream.rda')
#station <- names(rev(sort(table(bike_stream$key))))


create_data_file <- function(st, file, d1 = min(bike_stream$date), 
                             d2 = min(bike_stream$date) + 500000) {
  
  dd <- bike_stream[bike_stream$key %in% st, ]
  
  dd <- dd[dd$date >= d1, ]
  dd <- dd[dd$date <= d2, ]
  
  # file 'www/data2.csv'
  write.csv(dd, file = file, row.names = FALSE)
  
}





stream <- function(file) {
  
  html <- paste('
  <!DOCTYPE html>
  <meta charset="utf-8">
  <style>
  
  body {
  font: 10px sans-serif;
  }
  
  .chart { 
  background: #fff;
  }
  
  p {
  font: 12px helvetica;
  }
  
  
  .axis path, .axis line {
  fill: none;
  stroke: #000;
  stroke-width: 2px;
  shape-rendering: crispEdges;
  }
  
  button {
  position: absolute;
  right: 50px;
  top: 10px;
  }
  
  </style>
  <body>
  <script src="http://d3js.org/d3.v3.min.js"></script>
  
  <div class="chart">
  </div>
  
  <script>
  
  
  
  chart("', file, '", "blue");
  
  var datearray = [];
  var colorrange = [];
  
  
  function chart(csvpath, color) {
  
  if (color == "blue") {
  colorrange = ["#045A8D", "#2B8CBE", "#74A9CF", "#A6BDDB", "#D0D1E6", "#F1EEF6"];
  }
  else if (color == "pink") {
  colorrange = ["#980043", "#DD1C77", "#DF65B0", "#C994C7", "#D4B9DA", "#F1EEF6"];
  }
  else if (color == "orange") {
  colorrange = ["#B30000", "#E34A33", "#FC8D59", "#FDBB84", "#FDD49E", "#FEF0D9"];
  }
  strokecolor = colorrange[0];
  
  var format = d3.time.format("%Y-%m-%d %H:%M:%S");
  
  var margin = {top: 20, right: 40, bottom: 30, left: 30};
  var width = document.body.clientWidth - margin.left - margin.right;
  var height = 400 - margin.top - margin.bottom;
  
  var tooltip = d3.select("body")
  .append("div")
  .attr("class", "remove")
  .style("position", "absolute")
  .style("z-index", "20")
  .style("visibility", "hidden")
  .style("top", "30px")
  .style("left", "55px");
  
  var x = d3.time.scale()
  .range([0, width]);
  
  var y = d3.scale.linear()
  .range([height-10, 0]);
  
  var z = d3.scale.ordinal()
  .range(colorrange);
  
  var xAxis = d3.svg.axis()
  .scale(x)
  .orient("bottom")
  .ticks(d3.time.weeks);
  
  var yAxis = d3.svg.axis()
  .scale(y);
  
  var yAxisr = d3.svg.axis()
  .scale(y);
  
  var stack = d3.layout.stack()
  .offset("silhouette")
  .values(function(d) { return d.values; })
  .x(function(d) { return d.date; })
  .y(function(d) { return d.value; });
  
  var nest = d3.nest()
  .key(function(d) { return d.key; });
  
  var area = d3.svg.area()
  .interpolate("cardinal")
  .x(function(d) { return x(d.date); })
  .y0(function(d) { return y(d.y0); })
  .y1(function(d) { return y(d.y0 + d.y); });
  
  var svg = d3.select(".chart").append("svg")
  .attr("width", width + margin.left + margin.right)
  .attr("height", height + margin.top + margin.bottom)
  .append("g")
  .attr("transform", "translate(" + margin.left + "," + margin.top + ")");
  
  var graph = d3.csv(csvpath, function(data) {
  data.forEach(function(d) {
  d.date = format.parse(d.date);
  d.value = +d.value;
  });
  
  var layers = stack(nest.entries(data));
  
  x.domain(d3.extent(data, function(d) { return d.date; }));
  y.domain([0, d3.max(data, function(d) { return d.y0 + d.y; })]);
  
  svg.selectAll(".layer")
  .data(layers)
  .enter().append("path")
  .attr("class", "layer")
  .attr("d", function(d) { return area(d.values); })
  .style("fill", function(d, i) { return z(i); });
  
  
  svg.append("g")
  .attr("class", "x axis")
  .attr("transform", "translate(0," + height + ")")
  .call(xAxis);
  
  svg.append("g")
  .attr("class", "y axis")
  .attr("transform", "translate(" + width + ", 0)")
  .call(yAxis.orient("right"));
  
  svg.append("g")
  .attr("class", "y axis")
  .call(yAxis.orient("left"));
  
  svg.selectAll(".layer")
  .attr("opacity", 1)
  .on("mouseover", function(d, i) {
  svg.selectAll(".layer").transition()
  .duration(250)
  .attr("opacity", function(d, j) {
  return j != i ? 0.6 : 1;
  })})
  
  .on("mousemove", function(d, i) {
  mousex = d3.mouse(this);
  mousex = mousex[0];
  var invertedx = x.invert(mousex);
  invertedx = invertedx.getMonth() + invertedx.getDate();
  var selected = (d.values);
  for (var k = 0; k < selected.length; k++) {
  datearray[k] = selected[k].date
  datearray[k] = datearray[k].getMonth() + datearray[k].getDate();
  }
  
  mousedate = datearray.indexOf(invertedx);
  pro = d.values[mousedate].value;
  
  d3.select(this)
  .classed("hover", true)
  .attr("stroke", strokecolor)
  .attr("stroke-width", "0.5px"), 
  tooltip.html( "<p>" + d.key + "<br>" + pro + "</p>" ).style("visibility", "visible");
  
  })
  .on("mouseout", function(d, i) {
  svg.selectAll(".layer")
  .transition()
  .duration(250)
  .attr("opacity", "1");
  d3.select(this)
  .classed("hover", false)
  .attr("stroke-width", "0px"), tooltip.html( "<p>" + d.key + "<br>" + pro + "</p>" ).style("visibility", "hidden");
  })
  
  var vertical = d3.select(".chart")
  .append("div")
  .attr("class", "remove")
  .style("position", "absolute")
  .style("z-index", "19")
  .style("width", "1px")
  .style("height", "380px")
  .style("top", "10px")
  .style("bottom", "30px")
  .style("left", "0px")
  .style("background", "#fff");
  
  d3.select(".chart")
  .on("mousemove", function(){  
  mousex = d3.mouse(this);
  mousex = mousex[0] + 5;
  vertical.style("left", mousex + "px" )})
  .on("mouseover", function(){  
  mousex = d3.mouse(this);
  mousex = mousex[0] + 5;
  vertical.style("left", mousex + "px")});
  });
  }
  </script>
  
  ', sep = '')
  
  cat(html)
  
}


stream_plot <- function(data) {
  sink('tmp2.html')
  stream(data)
  sink()
  suppressWarnings(html('tmp2.html'))
}





