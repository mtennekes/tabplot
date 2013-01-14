TP = {};

TP.OutputBinding = new Shiny.OutputBinding();

$.extend(TP.OutputBinding, {
    find: function(scope) {
      return $(scope).find('.tableplot-output');
    },
    renderValue: function(el, data) {
    	var svg = $("svg", el);
    	svg.html("")
    	if (data){
    		TP.draw(svg.get(0), data);
    	} 
    }
});

Shiny.outputBindings.register(TP.OutputBinding, 'TP.OutputBinding');


TP.draw = function(el, data){
	var width = $(el).width();
	var height = $(el).height();

	if (!data){
		return;
	}

	console.log(data);

	var svg = d3.select(el);

	var columns = data.columns;
	var names = data.colNames;
	
	var bands = d3.scale.ordinal()
	   .domain(d3.range(columns.length))
	   .rangeBands([0, width], 0.2)
	   ;

	var bw = bands.rangeBand();

	var nms  = svg.selectAll("text.name")
	  .data(columns, function(col){return col.name});

	nms.exit().remove();
	
	nms.enter().append("text")
	  .attr({"class" :  "name"})
	  .text(function(d){ return d.name;})

	nms.attr({ x: function(_,col){return bands(col)}
		     , y: 10});
	
	var cols = svg.selectAll("g.column").data(columns, function(col){return col.name});

	cols.enter().append("g")
	  .attr({"class":"column"})
	  .append("rect")
	  .attr({"class": "background"
	        , x: 0, width: bw
	        , y: 15, height: height
	       })
	  .style("fill", "#F5F5F5")
	  ;

	cols.exit().remove();

	cols.attr({
		"transform" : function(d,i) { return "translate(" + bands(i) + ")" }
	})
	//cols.selectAll("rect.background")

	var bins = data.nBins;
	var binScale = d3.scale.ordinal()
	  .domain(d3.range(bins))
	  .rangeBands([15,height])

	cols.each(function(d,i){
		var g = d3.select(this);
	  	
	  	if (d.isnumeric){
		  	var x = d3.scale.linear()
		  	  .domain([0, d3.max(d.mean)])
		  	  .range([0,bw])
		  	  ;

			g.style({"fill" : "steelblue", "stroke-width": 0});
			var rect = g.selectAll("rect.bar").data(d3.zip(d.mean, d.compl));

			rect.enter()
			  .append("rect")
			  .attr({"class": "bar", title: function(m) {return m[0]}})
			  .style("fill-opacity", function(m){return m[1]/100;})
			  .on("mouseover", function(d){ d3.select(this).style({"stroke":"yellow", "stroke-width":1})})
			  .on("mouseout", function(d){d3.select(this).style({"stroke": null, "stroke-width": null})})
			  ;

			rect.exit().remove();
			
			rect = rect.transition();
			rect.attr({ x: x(0)
				      , width: function(m) { return x(m[0]) - x(0); }
				      , height: binScale.rangeBand()
				      , y: function(m, i){return binScale(i); }
		             })
		} else {
			var gfreq = g.selectAll("g.freq").data(d.freq);
			gfreq.enter().append("g")
			  .attr("class", "freq");

			gfreq.exit().remove();

			gfreq.attr({"transform" : function(_,bin) { return "translate(0," + binScale(bin) + ")" }})

			var x = d3.scale.linear()
		  	  .range([0,bw])
		  	  ;

		  	var rects = gfreq.selectAll("rect.freq").data(function(_, bin) {return d3.zip(d.x[bin], d.widths[bin]);});
			rects.enter().append("rect")
			  .attr({"class": "freq"});

			rects.exit().remove();

			//rects = rects.transition();
			rects.attr({ x: function(f) {return x(f[0])}
				      , width: function(f) { return x(f[1]); }
				      , height: binScale.rangeBand()
				      , y: 0
			}).style("fill", function(_, cat) {return d.palet[cat]})
		}
	})
	;
}