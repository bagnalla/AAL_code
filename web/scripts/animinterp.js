var Puddi = require ('./puddi/puddi.js');
var Drawable = require('./puddi/puddidrawable.js');
var Objects = require('./arrayobjects.js');
var Cell = Objects.Cell;
var Variable = Objects.Variable;
var Array = Objects.Array;
var Vector = require('victor');
var Range = ace.require('ace/range').Range;

var StackFrame = function(parent, label = "") {
    Drawable.call(this, parent);
    this._label = label;
    this._variables = [];
};

StackFrame.prototype = Object.create(Drawable.prototype);
StackFrame.prototype.constructor = StackFrame;

StackFrame.prototype.addVariable = function(v) { this._variables.push(v); };
StackFrame.prototype.removeVariable = function(v) {
    for (let i = 0; i < this._variables.length; ) {
	if (this._variables[i].equals(v)) {
	    this._variables.splice(i, 1);
	    this.removeChildAt(i);
	}
	else {
	    i++;
	}
    }
};
StackFrame.prototype.getVariables = function() { return this._variables; };

StackFrame.prototype.getWidth = function() {
    let h = 0;
    for (let v of this._variables) {
	h += v.getWidth();
    }
    return h;
};

StackFrame.prototype.getHeight = function() {
    let h = 0;
    for (let v of this._variables) {
	h += v.getHeight();
    }
    return h;
};

StackFrame.prototype._getLabelWidth = function(ctx) {
    let w = 0;
    for (let v of this._variables) {
	let vw = v.getLabelWidth(ctx);
	if (vw > w) {
	    w = vw;
	}
    }
    return w;
};

StackFrame.prototype.setVariablePositions = function(ctx) {
    if (!this._variables.length) {
	return;
    }
    let labelWidth = this._getLabelWidth(ctx) + 9;
    let offset_y = this._variables[0].getHeight() / 2 +
	10 / 2 // get font height from ctx;
    for (let v of this._variables) {
	// let h = v.getHeight();
	v.setPosition(new Vector(labelWidth + 1 /* width of line */,
				 offset_y));
	offset_y += v.getHeight();
    }
    return labelWidth;
};

StackFrame.prototype._drawSelf = function(ctx) {
    let labelWidth = this.setVariablePositions(ctx);
    let textHeight = 10; // get from ctx font
    ctx.fillText(this._label, 1, 0);
    // let w = this.getWidth();
    let w = ctx.canvas.width * (1 / Puddi.getScale());
    let h = this.getHeight();
    ctx.fillStyle = "gray";
    ctx.fillRect(0, textHeight / 2, w, h);
    ctx.strokeRect(0, textHeight / 2, w, h);
    // ctx.strokeStyle = "#2f4f4f";
    // ctx.lineWidth = 2;
    // for (let v of this._variables) {
    // 	let pos = v.getPosition();
    // 	let vh = v.getHeight();
    // 	ctx.beginPath();
    // 	ctx.moveTo(0, pos.y + vh / 2);
    // 	ctx.lineTo(labelWidth, pos.y + vh / 2);
    // 	ctx.closePath();
    // 	ctx.stroke();
    // }
    
    ctx.fillStyle = "#bebebe";//"#d3d3d3";//"#696969";
    ctx.fillRect(0, textHeight / 2, labelWidth, this.getHeight());
};

let BASE_CELL_VELOCITY = 0.06;
// let BASE_CELL_COLOR_FADE_RATE = 0.05;

var AnimInterpreter = function(endCallback, statusCallback, canvas,
			       parent, aprog = []) {
    Drawable.call(this, parent);
    this.setProg(aprog);
    this._frame_stack = [];
    this.init();
    this._endCallback = endCallback;
    this._statusCallback = statusCallback;
    this._canvas = canvas;
};

AnimInterpreter.prototype = Object.create(Drawable.prototype);
AnimInterpreter.prototype.constructor = AnimInterpreter;

AnimInterpreter.prototype.setProg = function(aprog) {
    this._aprog = aprog; // array of commands
};

AnimInterpreter.prototype.reset = function() {
    this._pc = 0;
    this._autoplay_counter = 0;
    this._clear_frame_stack();
    this._frame_stack = [new StackFrame(this, "main")];
    this._var_map = []; // map anim_ids to (variable, frame) pairs
    this._setActiveLine(-1);
}

AnimInterpreter.prototype.init = function() {
    this._editor = ace.edit("editor");
    this.reset();
    this._autoplay = false;
    this._autoplay_period = 1000;
    this._fast_forward = false;
    this._cell_velocity = BASE_CELL_VELOCITY;
};

AnimInterpreter.prototype._createCell = function(parent, value) {
    let c = new Cell(parent, value);
    c.setVelocity(this._cell_velocity);
    return c;
}

AnimInterpreter.prototype._parseValue = function(parent, value) {
    console.log("parsing value: " + value);
    switch (value[0]) {
    case "AAtom":
	return this._createCell(parent, value[1]);
    case "AArray":
	let arr = new Array(parent);
	for (let v of value[1]) {
	    arr.addElement(this._parseValue(arr, v));
	    // addValueToArray(arr, v);
	}
	return arr;
    default:
	console.log("error in parseValue in animinterp.js");
	console.log(value[0]);
    }
};

AnimInterpreter.prototype._getCurrentFrame = function() {
    return this._frame_stack[this._frame_stack.length - 1];
};

function parseId(id) { return id; };

AnimInterpreter.prototype._create = function(id, ty, lbl) {
    let curFrame = this._getCurrentFrame();
    let v = new Variable(curFrame, lbl);
    curFrame.addVariable(v);
    this._var_map[id] = [v, curFrame];
    switch (ty) {
    case "ATVar":
	v.setElement(this._createCell(v, ""));
	break;
    case "ATArray":
	v.setElement(new Array(v));
	break;
    default:
	console.log("error in create in animinterp.js");
    }
};

AnimInterpreter.prototype._destroy = function(id) {
    let vid = parseId(id);
    let a = this._var_map[vid];
    let v = a[0];
    let frame = a[1];
    frame.removeVariable(v);
    delete this._var_map[vid];
};

AnimInterpreter.prototype._insert = function(id, index, value) {
    console.log("in insert");
    let vid = parseId(id);
    let v = this._var_map[vid][0];
    let arr = v.getElement(); // assume it's an array
    arr.insertElement(this._parseValue(arr, value), index);
};

AnimInterpreter.prototype._delete = function(id, index) {
    // console.log("delete");
    let vid = parseId(id);
    let v = this._var_map[vid][0];
    let arr = v.getElement(); // assume it's an array
    arr.deleteAt(index);
};

AnimInterpreter.prototype._assign = function(loc, value) {
    console.log("in assign");
    console.log(value);
    let id = loc[1];
    let vid = parseId(id);
    let v = this._var_map[vid][0];
    switch (loc[0]) {
    case "LVar":
	v.setElement(this._parseValue(v, value));
	break;
    case "LArrayCell":
	let index = loc[1];
	let arr = v.getElement(); // assume it's an array
	arr.assignAt(index, this._parseValue(arr, value));
	break;
    default:
	console.log("error in assign in animinterp.js");
    }
};

AnimInterpreter.prototype._swap = function(id, index1, index2) {
    let vid = parseId(id);
    let v = this._var_map[vid][0];
    let arr = v.getElement(); // assume it's an array
    arr.swap(index1, index2);
};

AnimInterpreter.prototype._clear = function(id) {
    let vid = parseId(id);
    let v = this._var_map[vid][0];
    let arr = v.getElement(); // assume it's an array
    arr.clear();
};

AnimInterpreter.prototype._interpInstr = function(instr) {
    console.log("interpreting " + instr[0]);
    console.log(instr);
    switch (instr[0]) {
    case "ICreate":
	var id = instr[1];
	var ty = instr[2];
	var lbl = instr[3];
	this._create(id, ty, lbl);
	break;
    case "IDestroy":
	id = instr[1];
	this._destroy(id);
	break;
    case "IInsert":
	id = instr[1];
	var index = instr[2];
	var value = instr[3];
	this._insert(id, index, value);
	break;
    case "IDelete":
	id = instr[1];
	index = instr[2];
	this._delete(id, index);
	break;
    case "IAssign":
	let loc = instr[1];
	value = instr[2];
	this._assign(loc, value);
	break;
    case "ISwap":
	id = instr[1];
	index1 = instr[2];
	index2 = instr[3];
	this._swap(id, index1, index2);
	break;
    case "IClear":
	id = instr[1];
	this._clear(id);
	break;
    default:
	console.log("unknown animation instruction");
    }
};

AnimInterpreter.prototype._addFrame = function(label) {
    this._frame_stack.push(new StackFrame(this, label));
};

AnimInterpreter.prototype._deleteFrame = function() {
    let frame = this._frame_stack.pop();
    this.removeChild(frame);
};

AnimInterpreter.prototype._clear_frame_stack = function() {
    while (this._frame_stack.length) {
	this._deleteFrame();
    }
};

AnimInterpreter.prototype._computeFrameStackHeight = function() {
    let h = 0;
    for (let frame of this._frame_stack) {
	h += frame.getHeight();
    }
    return h;
}

AnimInterpreter.prototype._setActiveLine = function(lnum) {
    if (this._activeLineMarker !== null) {
	this._editor.session.removeMarker(this._activeLineMarker);
    }
    if (lnum >= 0) {
	this._activeLineMarker =
	    this._editor.session.addMarker(new Range(lnum-1, 0, lnum-1, 1),
					   "lineMarker", "fullLine");
    }
    else {
	this._activeLineMarker = null;
    }
};

AnimInterpreter.prototype._lnum_of_com = function(com) {
    return com[1][1];
}

AnimInterpreter.prototype._interpCom = function(com) {
    console.log("interpreting " + com[0]);
    switch (com[0]) {
    case "CFrameBegin":
	var flabel = com[1][0];
	var lnum = com[1][1];
	this._addFrame(flabel);
	break;
    case "CFrameEnd":
	lnum = com[1];
	this._deleteFrame();
	break;
    case "CStep":
	let instrs = com[1][0];
	lnum = com[1][1];
	for (let instr of instrs) {
	    this._interpInstr(instr);
	}
	break;
    default:
	console.log("unknown animation command");
    }
};

AnimInterpreter.prototype.stepForward = function() {
    if (this._pc >= this._aprog.length) {
	return;
    }
    console.log("stepping. pc = " + this._pc);
    this._interpCom(this._aprog[this._pc]);
    this._pc++;

    if (this._pc == this._aprog.length) {
	this._setActiveLine(-1);
	if (this._endCallback) {
	    this._endCallback();
	}
    }
    else {
	let lnum = this._lnum_of_com(this._aprog[this._pc]);
	this._setActiveLine(lnum);
    }
};

AnimInterpreter.prototype.isDone = function() {
    return this._pc >= this._aprog.length;
}

AnimInterpreter.prototype._skipTo = function(i) {
    while (this._pc < i) {
	this.stepForward();
    }
};

AnimInterpreter.prototype.stepBack = function() {
    let pc = this._pc;
    if (!pc) { return; }
    this.reset();
    this._skipTo(pc - 1);
    for (let frame of this._frame_stack) {
	for (let v of frame.getVariables()) {
	    let arr = v.getElement();
	    if (arr.setPositions) {
		arr.setPositions();
	    }
	}
    }
};

AnimInterpreter.prototype.getPlaySpeed = function() {
    return this._play_speed;
};

AnimInterpreter.prototype.getPc = function() {
    return this._pc;
}

AnimInterpreter.prototype.getProgramLength = function() {
    return this._aprog.length();
}

AnimInterpreter.prototype._updateCellVelocity = function(v) {
    this._cell_velocity = v;

    // update all existing cells with new velocity
    for (let frame of this._frame_stack) {
	for (let v of frame.getVariables()) {
	    let el = v.getElement();
	    if (el.setCellVelocity) {
		el.setCellVelocity(this._cell_velocity);
	    }
	    else {
		el.setVelocity(this._cell_velocity);
	    }
	}
    }
}

AnimInterpreter.prototype.getAutoplayPeriod = function() {
    return this._autoplay_period;
};

AnimInterpreter.prototype.setAutoplayPeriod = function(p) {
    this._autoplay_period = p;
    this._updateCellVelocity(Math.min(1, (BASE_CELL_VELOCITY * 1000) / this._autoplay_period));
};

AnimInterpreter.prototype.getAutoplay = function() { return this._autoplay; };

AnimInterpreter.prototype.setAutoplay = function(a) {
    this._autoplay = a;
    if (a) {
	this._updateCellVelocity(Math.min(1, (BASE_CELL_VELOCITY * 1000) / this._autoplay_period));
    }
    else {
	this._updateCellVelocity(BASE_CELL_VELOCITY);
    }
};

AnimInterpreter.prototype._updateSelf = function(time_elapsed) {
    if (this._autoplay) {
	if (this._fast_forward) {
	    this.stepForward();
	}
	else {
	    this._autoplay_counter += time_elapsed;
	    if (this._autoplay_counter >= this._autoplay_period) {
		this._autoplay_counter = 0;
		this.stepForward();
	    }
	}
    }
    if (this._statusCallback) {
	this._statusCallback({ pc: this._pc });
    }
}

AnimInterpreter.prototype._drawSelf = function(ctx) {
    let frameStackHeight = this._computeFrameStackHeight();
    let actualHeight = frameStackHeight +
	this._frame_stack.length * 20; // from font height
    let overflow = Math.max(0, actualHeight -
			    this._canvas.height * (1 / Puddi.getScale()));
    offset_y = 10 - overflow; // font height from ctx
    for (let frame of this._frame_stack) {
	frame.setPosition(new Vector(0, offset_y));
	offset_y += frame.getHeight() + 20; // get font height from ctx
    }
};

// EXPORT
module.exports = AnimInterpreter;


