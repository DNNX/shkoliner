// Based on http://bl.ocks.org/900762 by John Firebaugh
d3.json("dummy.json", function(faithful) {
  data = faithful;
  var margin = {top: 20, right: 10, bottom: 20, left: 10},
      w = 600 - margin.left - margin.right,
      h = 400 - margin.top - margin.bottom,
      numBins = 24,
      x = d3.scale.linear().domain([0, 24]).range([0, w]),
      xAng = d3.scale.linear().domain([0, 24]).range([0, 2*Math.PI]),
      bins = d3.layout.histogram().frequency(false).bins(x.ticks(numBins))(data),
      max = d3.max(bins, function(d) { return d.y; }),
      y = d3.scale.linear().domain([0, max]).range([0, h]),
      yRad = d3.scale.linear().domain([0, max]).range([0, h/2]),
      kde = science.stats.kde().sample(data);

  var vis = d3.select("body")
    .append("svg")
      .attr("width", w + margin.left + margin.right)
      .attr("height", h + margin.top + margin.bottom)
      .append("g")
        .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

  var barsEnter = vis.selectAll("g.bar")
      .data(bins)
    .enter();

  var bars = barsEnter.append("g")
    .attr("class", "bar")
    .attr("transform", function(d, i) {
      console.log(d)
      return "translate(" + x(d.x) + "," + (h - y(d.y)) + ")";
    });

  bars.append("rect")
      .attr("fill", "steelblue")
      .attr("width", function(d) { return x(d.dx); })
      .attr("height", function(d) { return y(d.y); });

  var radial = d3.svg.line()
      .x(function(d) { return w/2 + yRad(d[1]) * Math.sin(xAng(d[0])); })
      .y(function(d) { return h/2 - yRad(d[1]) * Math.cos(xAng(d[0])); });
  var line = d3.svg.line()
      .x(function(d) { return x(d[0]); })
      .y(function(d) { return h - y(d[1]); });

  vis.selectAll("circle")
      .data(y.ticks(10))
    .enter().append("circle")
      .attr("cx", w/2)
      .attr("cy", h/2)
      .attr("fill", "none")
      .attr("stroke", "gray")
      .attr("r", function(r) { return y(r)/2; });

  vis.selectAll("path.t")
      .data(d3.values(science.stats.bandwidth).slice(1,2))
    .enter().append("path")
      .classed("t", true)
      .attr("d", function(h) {
        return radial(kde.bandwidth(h)(d3.range(0, 24, .1)));
      });

  vis.selectAll("path.r")
      .data(d3.values(science.stats.bandwidth).slice(1,2))
    .enter().append("path")
      .classed("r", true)
      .attr("d", function(h) {
        return line(kde.bandwidth(h)(d3.range(0, 24, .1)));
      });

});
