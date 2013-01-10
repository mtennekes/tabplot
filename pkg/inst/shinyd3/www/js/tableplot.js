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
	if (!data){
		return;
	}
	var svg = d3.select(el);
	var columns = data;
	
	var bands = d3.scale.ordinal()
	   .domain(d3.range(columns.length))
	   .rangeBands([0, 400])
	   ;

	var bw = bands.rangeBand();

	var cols = svg.selectAll("g.column").data(columns);

	cols.enter().append("g")
	  .attr({"class":"column"})
	  ;

	cols.exit().remove();

	cols.attr({
		"transform" : function(d,i) {console.log(bands(i));return "translate(" + bands(i) + ")" }
	})
	console.log(columns);
}
