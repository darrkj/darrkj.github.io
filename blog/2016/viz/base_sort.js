var ord = [17, 19, 14, 8, 2, 1, 9, 10, 12, 18, 16, 7, 6, 3, 11, 5, 0, 13, 15, 4]
var test = [];
var current = 0;
var first = 0;

function setup() {
	createCanvas(1040, 680);
  fill(0);
  noLoop();
}

function draw() {
	clear();
	// This will be one sort alg
	if (ord[current] > ord[current+1] & first == 1) {
    	tmp = ord[current]
    	ord[current] = ord[current+1]
    	ord[current+1] = tmp
    }
    for (var i = 0; i < ord.length; i++) {
    	tmp = ord[i] * 20 + 10;
    	rect(i * 50 + 10, 500 - tmp, 20, tmp)
  	}
  	// End of first sort alg.
  	first = 1
}




function mousePressed() {
	current = current + 1;
  	if (current == ord.length - 1) {
  		current = 0;
  	}
	redraw();
}
