/**
 *  original written by @maxwihlborg, adapted to server backend by johanmon@kth.se
 *  http://opensource.org/licenses/MIT MIT License.
*/


/**
 * Constants
 */
const WIDTH  = 700;
const HEIGHT = 600;

const PADDLE_WIDTH = 20
const PADDLE_HEIGHT = 100

const BALL_WIDTH = 20

const MOVE = 5 // pixle in each up/down move

const UpArrow   = 38;
const DownArrow = 40;


    /**
     * Scoreboard stuff
     *  -   0
     * | | 1 3 <= coresponding indexes, ie. for 1 is 
     *  -   2
     * | | 4 6    bar 3 and 6 visible => 0 001 001
     *  -   5
     */

const figures = [
    [true,   true,  false, true,   true,  true,  true ],
    [false,  false, false, true,   false, false, true ],
    [true,   false, true,  true,   true,  true,  false],
    [true,   false, true,  true,   false, true,  true ],
    [false,  true,  true,  true,   false, false, true ],
    [true,   true,  true,  false,  false, true,  true ],
    [true,   true,  true,  false,  true,  true,  true ],
    [true,   false, false, true,   false, false, true ],
    [true,   true,  true,  true,   true,  true,  true ],
    [true,   true,  true,  true,   false, true,  true ]
];

/* This array is used to describe the bars: 0..6 in the form 
 *  [x0, y0, x1, y1]
 */

const bars = [
    [0, 0, 1, 0],   //   --
    [0, 0, 0, 1],   // | 
    [0, 1, 1, 1],   //   --
    [1, 0, 1, 1],   //      |
    [0, 1, 0, 2],   // |
    [0, 2, 1, 2],   //   --
    [1, 1, 1, 2]    //      | 
];


/**
 * Game elements
 */
var canvas, ctx, keystate;

var uparrow, downarrow;

/**
 * WebSocket elements
 */
var socket;

/**
 * The player paddle
 * 
 * @type {Object}
 */

var player = {
    x: null,
    y: null,
    score: 0,

    width:  PADDLE_WIDTH,
    height: PADDLE_HEIGHT,

    /**
     * Draw the player paddle to the canvas
     */
    draw: function() {
	ctx.fillRect(this.x, this.y, this.width, this.height);
    }
};

/**
 * The opponent paddle
 * 
 * @type {Object}
 */
var opponent = {
    x: null,
    y: null,
    score: 0,

    width:  PADDLE_WIDTH,
    height: PADDLE_HEIGHT,
    
    /**
     * Draw the opponent paddle to the canvas
     */
    draw: function() {
	ctx.fillRect(this.x, this.y, this.width, this.height);
    }
};

/**	
 * The ball object
 * 
 * @type {Object}
 */
var ball = {
    x:   0,
    y:   0,
    side:  BALL_WIDTH,
    
    /**
     * Draw the ball to the canvas
     */
    draw: function() {
	if (this.x != 0) {
	    ctx.fillRect(this.x, this.y, this.side, this.side);
	}
    }
};

function requestFullscreen() {
    console.log(canvas);
    if (canvas.requestFullscreen) {
	canvas.requestFullscreen();
    } else if (canvas.msRequestFullscreen) {
	canvas.msRequestFullscreen();
    } else if (canvas.mozRequestFullScreen) {
	canvas.mozRequestFullScreen();
    } else if (canvas.webkitRequestFullscreen) {
	canvas.webkitRequestFullscreen();
    }
    window.onresize = function() {
	canvas.width = WIDTH = window.innerWidth;
	canvas.height = HEIGHT = window.innerHeight;
	init();
    }
}

/**
 * Starts the game
 */
function main() {
    // create, initiate and append game canvas
    canvas = document.createElement("canvas");
    canvas.width = WIDTH;
    canvas.height = HEIGHT;
    ctx = canvas.getContext("2d");
    document.body.appendChild(canvas);
    
    // keep track of keyboard presses
    document.addEventListener("keydown", function(evt) {
	if (evt.keyCode == UpArrow) {
	    uparrow =  true;
	}
	if (evt.keyCode == DownArrow) {
	    downarrow = true;
	}
	return false;
    });

    document.addEventListener("keyup", function(evt) {
	if (evt.keyCode == UpArrow) {
	    uparrow =  false;
	}
	if (evt.keyCode == DownArrow) {
	    downarrow = false;
	}
	return false;
    });

    
    init(); // initiate game objects

    // Hard coded IP number of server, should be given dynammic but
    // fine for now.
    socket = new WebSocket('ws://localhost:8080/demo', "pong");

    socket.binaryType = "arraybuffer";

    socket.onmessage =  function(event) {
	update(event.data);
    }

    socket.onopen =  function(event) {
	// we could alert the user
    }    

    var d = document.createElement("div");
    d.innerHTML = '<svg onclick="requestFullscreen()"width="20" height="16" opacity="0.5"><path d="M0 5v-5h5m10 0h5v5m0 6v5h-5m-10 0h-5v-5M6 6h8v4h-8z"style="fill:none;stroke:#000;stroke-width:4"></path></svg>';
    document.body.appendChild(d);

    // game loop function
    var loop = function() {				   
        input();
	draw();
	window.requestAnimationFrame(loop, canvas);
    };
    window.requestAnimationFrame(loop, canvas);
}


/**
 * Initatite game objects and set start positions
 */

function init() {
    player.x = player.width;
    player.y = (HEIGHT - player.height)/2;
    player.score = 0;
    
    opponent.x = WIDTH - (player.width + opponent.width);
    opponent.y = (HEIGHT - opponent.height)/2;
    opponent.score = 0;
}

/**
 * Decode message from server and update state
 */

function update(msg) {
    var code = new Uint8Array(msg)
    console.log(code)    

    switch (code[0]) {
    case 80: {   // P

	switch (code[1]) {
	case 85: {   // U 
	    console.log("player up");
	    player.y -= MOVE;
	    break;
	}
	case 68: {   // D
	    console.log("player down");
    	    player.y += MOVE;
	    break;
	}
	case 83: {   // S
	    console.log("player score " + code[2]);
    	    player.score = code[2];
	    break;
	}
	}
	break;
    }
    case 79: {    // O
	switch (code[1]) {
	case 85: {   // U 
	    console.log("opponent up");
	    opponent.y -= MOVE;
	    break;
	}
	case 68: {   // D
	    console.log("opponent down");
    	    opponent.y += MOVE;
	    break;
	}
	case 83: {   // S
	    console.log("opponent score: " + code[2]);
    	    opponent.score = code[2];
	    break;
	}
	}
	break;
    }
    case 66: {    // B
	var x = code[1]*255 + code[2]
	var y = code[3]*255 + code[4]
	console.log("ball : " + x + " x " + y)
	ball.x = x
	ball.y = y
	break;
    }
    }
    draw()
}


function input() {
    if(socket.readyState == 1) {
	if (uparrow) {
	    var msg = new Uint8Array(4);
	    msg[0] = 85; 
	    socket.send(msg, {binary: true});	
	}
	if (downarrow) {
	    var msg = new Uint8Array(4);
	    msg[0] = 68;   
	    socket.send(msg, {binary: true});	
	}
    }
}

				    
/**
 * Clear canvas and draw all game objects and net
 */
function draw() {
    ctx.fillRect(0, 0, WIDTH, HEIGHT);

    ctx.save();

    ctx.fillStyle = "#fff";
    

    ball.draw();
    player.draw();
    opponent.draw();

    // draw the net
    var w = 4;
    var x = (WIDTH - w)*0.5;
    var y = 0;  
    var step = HEIGHT/20; // how many net segments
    while (y < HEIGHT) {
	ctx.fillRect(x, y+step*0.25, w, step*0.5);
	y += step;
    }
    
    // draw the scores
    var w2 = WIDTH/2;
    drawNumber(player.score, w2-20, 20, true);
    drawNumber(opponent.score, w2+20, 20);

    ctx.restore();
}


function drawNumber(n, x, y, ralign) {
    var size = 32;
    var padding = 16;
    
    ctx.save();
    ctx.strokeStyle = "#fff";
    ctx.lineCap = "square";
    ctx.lineWidth = padding/2;

    n = n.toString(); // convert to string => possible to loop thru all digits

    if (ralign) { // if right aligned move x coord accordingly
	x -= (n.length*(padding+size)-padding);
    }
    ctx.translate(x, y);

    for (var i = 0; i < n.length; i++) {
	var fig = figures[parseInt(n[i])];
		
	ctx.beginPath();
	for (var j = 0; j < fig.length; j++) {
	    if (fig[j]) {
		var p = bars[j];
		ctx.moveTo(p[0]*size, p[1]*size);
		ctx.lineTo(p[2]*size, p[3]*size);
	    }
	}
	ctx.closePath();
	ctx.stroke();
	// fix anoying bug
	var p2 = padding/2,
	    p4 = padding/4;
	ctx.fillRect(size - p4, 2*size - p4, p2, p2);
	
	ctx.translate(size+padding, 0);
    }
    ctx.restore();
}


main();


