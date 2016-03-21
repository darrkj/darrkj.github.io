var ord = [17, 19, 14, 8, 2, 1, 9, 10, 12, 18, 16, 7, 6, 3, 11, 5, 0, 13, 15, 4]
var first = 0;
var table;
var k = 3;
var kx = [];
var ky = [];
var Scale = 75;
var owner = [];
var df = [];
var avgx = [];
var avgy = [];
var cnt = [];

function setup() {
	createCanvas(740, 480);
	df[0] = []; df[1] = []; df[2] = [];
	//cycle through the table
  	for (var r = 0; r < table.getRowCount(); r++) {
    	for (var c = 0; c < table.getColumnCount(); c++) {
			df[0][r] = float(table.getString(r, 1));
			df[1][r] = float(table.getString(r, 3));
			df[2][r] = table.getString(r, 4);
		}
    }
    // Initialize all of the average counters at zero.
    for (var r = 0; r < k; r++) {
    	avgx[r] = 0; avgy[r] = 0; cnt[r] = 0;
    }
    noLoop();
}

function draw() {
	clear();
	
	if (first == 0) {
		for (var i = 0; i < k; i++) {
	    	kx = randStart(df[0], k);
	    	ky = randStart(df[1], k);
	  	}

		plotPoints();
	} else {
		plotPoints();
		for (var i = 0; i < df[1].length; i++) {
			if (owner[i] == 1) {
			  // use oner[i] to index avgx instead of writing it out everytime
				avgx[0] = avgx[0] + df[0][i];
				avgy[0] = avgy[0] + df[1][i];
				cnt[0] = cnt[0] + 1;
			} else if (owner[i] == 2) {
				avgx[1] = avgx[1] + df[0][i];
				avgy[1] = avgy[1] + df[1][i];
				cnt[1] = cnt[1] + 1;
			} else {
				avgx[2] = avgx[2] + df[0][i];
				avgy[2] = avgy[2] + df[1][i];
				cnt[2] = cnt[2] + 1;
			}
		}

		for (var r = 0; r < k; r++) {
    		kx[r] = avgx[r] / cnt[r];
    		ky[r] = avgy[r] / cnt[r];
    	}
	}

	fill(0, 255, 255);
	for (var i = 0; i < kx.length; i++) {
		ellipse(kx[i] * Scale, ky[i] * Scale, 15, 15);
	}
}

function preload() {
  // Table is csv and has a header for the columns labels
  table = loadTable("https://raw.githubusercontent.com/darrkj/darrkj.github.io/master/iris.csv", "csv", "header");
}

function plotPoints() {
	for (var i = 0; i < df[0].length; i++) {
		tmp1 = dist(df[0][i], df[1][i], kx[0], ky[0]);
	  	tmp2 = dist(df[0][i], df[1][i], kx[1], ky[1]);
	  	tmp3 = dist(df[0][i], df[1][i], kx[2], ky[2]);
	  	//text(min(tmp1, tmp2, tmp3), 10, 10);
	  	if (tmp1 == min(tmp1, tmp2, tmp3)) {
	    	fill(0, 255, 0);
	    	owner[i] = 1;
	  	} else if (tmp2 == min(tmp1, tmp2, tmp3)) {
	    	fill(255, 0, 0);
	    	owner[i] = 2;
	  	} else {
	  		fill(0, 0, 255);
	  		owner[i] = 3;
	  	}
	  	ellipse(df[0][i] * Scale, df[1][i] * Scale, 5, 5);
	}
}

function mousePressed() {
	if (first == 0)
		first = 1;
	redraw();
}

function randStart(a, k) {
	var x = [];
	var mxa = max(a);
	var mna = min(a);
	for (var i = 0; i < k; i++)
	  x[i] = random() * (mxa - mna) + mna;
	return x;
}
