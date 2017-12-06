var state = {
    _objects: [],
    _scale: 1.0
};
var engine = {};

function update(tFrame) {
    // compute the time elapsed since the last update
    let time_elapsed = tFrame - state._time;

    // update the timestamp
    state._time = tFrame;

    // update all objects
    for (let o of state._objects) {
	o.update(time_elapsed);
    }
}

function draw() {
    // clear canvas
    state._ctx.clearRect(0, 0, state._ctx.canvas.width * (1 / state._scale),
			 state._ctx.canvas.height * (1 / state._scale));

    // draw all objects
    for (let o of state._objects) {
	if (o.draw) {
	    o.draw(state._ctx);
	}
    }
}

// deregister from the browser update loop
function stop() {
    window.cancelAnimationFrame(engine._stopCycle);
}

// The function to be called by the browser at every frame
function cycle(tFrame) {
    // re-register for the next frame
    engine._stopCycle = window.requestAnimationFrame(cycle);

    // update
    if (update(tFrame) < 0) {
	stop();
	return;
    }

    // draw
    draw();
}

// EXPORTS

exports._engine = engine;

exports.run = function(canvas) {
    state._canvas = canvas
    state._ctx = canvas.getContext('2d');
    // initialize state._time to the current time
    state._time = performance.now();
    // register the cycle function with the browser update loop
    engine._stopCycle = window.requestAnimationFrame(cycle);
};

exports.stop = stop;

exports.addObject = function(o) {
    state._objects.push(o);
};

exports.removeObject = function(o) {
    for (let i = 0; i < state._objects.length; i++) {
	// use the object's provided equals method
	if (o.equals(state._objects[i])) {
	    state._objects.splice(i, 1);
	}
    }
};

exports.getCtx = function() { return state._ctx; };

exports.scale = function(s) {
    state._scale *= s;
    state._canvas.width += 0; // reset canvas transform
    state._ctx.scale(state._scale, state._scale);
};

exports.getScale = function() { return state._scale; };
