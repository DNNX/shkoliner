// Based on http://bl.ocks.org/900762 by John Firebaugh
d3.json("dummy.json", function(faithful) {
  data = faithful;
  var w = 800,
      h = 400,
      x = d3.scale.linear().domain([0, 24]).range([0, w]),
      xAng = d3.scale.linear().domain([0, 24]).range([0, 2*Math.PI]),
      bins = d3.layout.histogram().frequency(false).bins(x.ticks(24))(data),
      max = d3.max(bins, function(d) { return d.y; }),
      y = d3.scale.linear().domain([0, 0.2]).range([0, h]),
      yRad = d3.scale.linear().domain([0, 0.15]).range([0, h/2]),
      kde = science.stats.kde().sample(data);

  var vis = d3.select("body")
    .append("svg")
      .attr("width", w)
      .attr("height", h);

  var bars = vis.selectAll("g.bar")
      .data(bins)
    .enter().append("g")
      .attr("class", "bar")
      .attr("transform", function(d, i) {
        return "translate(" + x(d.x) + "," + (h - y(d.y)) + ")";
      });

  bars.append("rect")
      .attr("fill", "steelblue")
      .attr("width", function(d) { return 35; })
      .attr("height", function(d) { return y(d.y); });

  var line = d3.svg.line()
      .x(function(d) { return w/2 + yRad(d[1]) * Math.sin(xAng(d[0])); })
      .y(function(d) { return h/2 - yRad(d[1]) * Math.cos(xAng(d[0])); });

  vis.selectAll("circle")
      .data([10, 20, 30, 40, 50, 60, 70, 80, 90, 100])
    .enter().append("circle")
      .attr("cx", w/2)
      .attr("cy", h/2)
      .attr("fill", "none")
      .attr("stroke", "gray")
      .attr("r", function(r) { return r; });

  vis.selectAll("path")
      .data(d3.values(science.stats.bandwidth).slice(1,2))
    .enter().append("path")
      .attr("d", function(h) {
        return line(kde.bandwidth(h)(d3.range(0, 24, .1)));
      });
});
