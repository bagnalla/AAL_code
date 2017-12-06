var Puddi = require('./puddi/puddi.js');
var Drawable = require('./puddi/puddidrawable.js');
var Vector = require('victor');

// CELL

var MIN_CELL_WIDTH = 25;
var MIN_CELL_HEIGHT = 25;
var DEFAULT_CELL_VELOCITY = 0.1;

function Cell(parent, value = "") {
    Drawable.call(this, parent);
    this.setVelocity(DEFAULT_CELL_VELOCITY);
    // this._width = MIN_CELL_WIDTH; // set by setValue
    this._height = MIN_CELL_HEIGHT;
    this._redStrength = 0.0;
    this._greenStrength = 0.0;
    this._blueStrength = 0.0;
    this.setValue(value);
}

Cell.prototype = Object.create(Drawable.prototype);
Cell.prototype.constructor = Cell;

Cell.prototype.getValue = function() { return this._value; };
Cell.prototype.getWidth = function() { return this._width; };
Cell.prototype.getHeight = function() { return this._height; };

Cell.prototype.setValue = function(v) {
    this._value = v;
    let ctx = Puddi.getCtx();
    let textWidth = ctx.measureText(this._value).width;
    this._width = Math.max(MIN_CELL_WIDTH, textWidth + 8);
};

Cell.prototype.flashRed = function() { this._redStrength = 1.0; };
Cell.prototype.flashGreen = function() { this._greenStrength = 1.0; };
Cell.prototype.flashBlue = function() { this._blueStrength = 1.0; };

Cell.prototype._updateSelf = function(time_elapsed) {
    // do stuff
    let fade = this._velocity / 50 * time_elapsed;
    this._redStrength -= fade;
    this._greenStrength -= fade;
    this._blueStrength -= fade;
    
    this.setColor("rgb(" + Math.floor(this._redStrength * 255) + ", " +
    		  Math.floor(this._greenStrength * 255) + ", " +
    		  Math.floor(this._blueStrength * 255) + ")");
    
    // console.log("cell position: " + this.getPosition());
    // console.log("cell target position: " + this.getTargetPosition());
};

Cell.prototype._drawSelf = function(ctx) {
    ctx.lineWidth = 2;
    let textWidth = ctx.measureText(this._value).width;
    let textHeight = 10; // get font size from ctx
    ctx.fillStyle = "white";
    ctx.fillRect(0, -this._height / 2, this._width, this._height);
    ctx.strokeRect(0, -this._height / 2, this._width, this._height);
    ctx.fillStyle = "black";
    ctx.fillText(this._value, this._width / 2 - textWidth / 2,
		 textHeight / 2.5);
};


// VARIABLE

// var MAX_LABEL_WIDTH = 50;

function Variable(parent, label = "") {
    Drawable.call(this, parent);
    this._label = label;
}

Variable.prototype = Object.create(Drawable.prototype);
Variable.prototype.constructor = Variable;

Variable.prototype.getLabel = function() { return this._label; };
Variable.prototype.getWidth = function() { return this._element.getWidth(); };
Variable.prototype.getHeight = function() { return this._element.getHeight(); };
Variable.prototype.getElement = function() { return this._element; };
// Variable.prototype.getMaxLabelWidth = function(ctx) { return MAX_LABEL_WIDTH; }
Variable.prototype.getLabelWidth = function(ctx) {
    return ctx.measureText(this._label).width;
};

Variable.prototype.setLabel = function(l) { this._label = l; };
// Variable.prototype.setValue = function(v) { this._cell.setValue(v); };
// Variable.prototype.clear = function() { this._cell.setValue(""); };
Variable.prototype.setElement = function(el) {
    if (this._element) {
	this.removeChild(this._element);
    }
    this._element = el;
    el.flashRed();
};

Variable.prototype._drawSelf = function(ctx) {
    // draw label
    //let labelWidth = ctx.measureText(this._label).width;
    ctx.fillText(this._label, 0 // need single cell width
		 - this.getLabelWidth(ctx) - 6, 2.5);
}


// ARRAY

function Array(parent) {
    Drawable.call(this, parent);
    this._elements = [];
}

Array.prototype = Object.create(Drawable.prototype);
Array.prototype.constructor = Array;

Array.prototype.getWidth = function() {
    let w = 0;
    for (let el of this._elements) {
	w += el.getWidth();
    }
    return Math.max(w, MIN_CELL_WIDTH);
};

Array.prototype.getHeight = function () {
    let maxHeight = 0;
    for (let el of this._elements) {
	let h = el.getHeight();
	if (h > maxHeight) {
	    maxHeight = h;
	}
    }
    return Math.max(maxHeight, MIN_CELL_HEIGHT);
}

Array.prototype.getElements = function() { return this._elements; };

Array.prototype._setElementPosition = function(el, i) {
    let w = 0;
    for (let j = 0; j < i; j++) {
	w += this._elements[j].getWidth();
    }
    el.setPosition(new Vector(w, 0));
    el.setTargetPosition(new Vector(w, 0));
}

Array.prototype._computeElementPositions = function() {
    positions = [];
    acc = 0;
    for (let el of this._elements) {
	positions.push(new Vector(acc, 0));
	acc += el.getWidth();
    }
    return positions;
}

Array.prototype._setAllElementPositions = function() {
    let positions = this._computeElementPositions();
    for (let i = 0; i < positions.length; i++) {
	this._elements[i].setPosition(positions[i]);
    }
}

Array.prototype._setAllElementTargetPositions = function() {
    let positions = this._computeElementPositions();
    for (let i = 0; i < positions.length; i++) {
	this._elements[i].setTargetPosition(positions[i]);
    }
}

// hard set the elements to their positions
Array.prototype.setPositions = function() {
    this._setAllElementPositions();
    this._setAllElementTargetPositions();
}

Array.prototype.addElement = function(o) {
    this._elements.push(o);
    // this.addChild(o); // the element adds itself
    this._setElementPosition(o, this._elements.length - 1);
    o.flashGreen();
};

Array.prototype.insertElement = function(o, i) {
    this._elements.splice(i, 0, o);
    this._setElementPosition(o, i);
    this._setAllElementTargetPositions();
    o.flashGreen();
};

Array.prototype.deleteAt = function(i) {
    this.removeChild(this._elements[i]);
    this._elements.splice(i, 1);
    this._setAllElementTargetPositions();
};

Array.prototype.assignAt = function(i, o) {
    this.removeChild(this._elements[i]);
    this._elements[i] = o;
    o.flashGreen();
};

Array.prototype.clear = function() {
    this.clearChildren();
    this._elements = [];
};

// need to create copies
// Array.prototype.copy = function(arr) {
//     this.clear();
//     for (let el of arr.getElements()) {
// 	this.addElement(el);
//     }
//     this._setAllElementPositions();
// };

Array.prototype.swap = function(index1, index2) {
    let el1 = this._elements[index1];
    let el2 = this._elements[index2];
    let tmp = el1.getTargetPosition();
    // swap graphically
    // el1.setTargetPosition(el2.getTargetPosition());
    // el2.setTargetPosition(tmp);
    // el1.flashBlue();
    // el2.flashBlue();
    // swap internally
    this._elements[index1] = el2;
    this._elements[index2] = el1;
    // swap graphically
    el1.flashBlue();
    el2.flashBlue();
    this._setAllElementTargetPositions();
}

Array.prototype.flashRed = function() {
    for (let el of this._elements) {
	el.flashRed();
    }
}
Array.prototype.flashGreen = function() {
    for (let el of this._elements) {
	el.flashGreen();
    }
}
Array.prototype.flashBlue = function() {
    for (let el of this._elements) {
	el.flashBlue();
    }
}

Array.prototype.setCellVelocity = function(v) {
    for (let el of this._elements) {
	if (el.setCellVelocity) {
	    el.setCellVelocity(v);
	}
	else {
	    el.setVelocity(v);
	}
    }
}

Array.prototype._drawSelf = function(ctx) {
    // console.log("array position: " + this.getPosition());
    // let labelWidth = ctx.measureText(this._label).width;
    // ctx.fillText(this._label, -labelWidth, 0);
}

module.exports = {
    Cell: Cell,
    Variable: Variable,
    Array: Array
};
