var sexp = require('sexp');

var Puddi = require('./puddi/puddi.js');
var PuddiDrawable = require('./puddi/puddidrawable.js');
var Vector = require('victor');

var Objects = require('./arrayobjects.js');
var Cell = Objects.Cell;
var Variable = Objects.Variable;
var AArray = Objects.Array;

var Interp = require('./animinterp.js');

var hotkeysEnabled = false;
var autoplaying = false;
var compiling = false;

var timeoutId = null;

var interpreter = null;

// small default screen size
let screen_width = 640;
let screen_height = 480;

function startEdit() {
    hotkeysEnabled = false;

    $("#canvas").css("visibility", "hidden");
    $("#status").css("visibility", "hidden");
    $("#editbutton").css("display", "none");
    $("#playbutton").css("display", "none");
    $("#stopbutton").css("display", "none");
    $("#stepbutton").css("display", "none");
    $("#backbutton").css("display", "none");
    $("#fasterbutton").css("display", "none");
    $("#slowerbutton").css("display", "none");
    $("#resetbutton").css("display", "none");
    $("#cancelbutton").css("display", "none");
    $("#stepinterval").css("display", "none");

    $("#gobutton").css("display", "inline");

    interpreter.reset();
    let editor = ace.edit("editor");
    editor.setReadOnly(false);

    $("#feedback").text("Press 'Go' to compile.");
}

function startCompile() {
    hotkeysEnabled = true;
    compiling = true;

    $("#gobutton").css("display", "none");
    $("#cancelbutton").css("display", "inline");
    $("#feedback").html("Compiling...<br>Note: if your program doesn't terminate this will run for 10 seconds before timing out and may use a lot of memory.<br>Press 'Cancel' to cancel compilation early and return to editing.");

    let editor = ace.edit("editor");
    editor.setReadOnly(true);
}

function stopCompile() {
    compiling = false;
}

function startAnimation(aprog) {
    let coms = aprog[2];
    interpreter.setProg(coms);
    $("#cancelbutton").css("display", "none");

    $("#canvas").css("visibility", "visible");
    $("#status").css("visibility", "visible");
    $("#editbutton").css("display", "inline");
    $("#playbutton").css("display", "inline");
    $("#stepbutton").css("display", "inline");
    $("#backbutton").css("display", "inline");
    $("#resetbutton").css("display", "inline");

    $("#status").text("Paused");

    $("#feedback").html("Ready.<br>Press 'Play' to start the animation or 'Step' to go one step at a time.<br>Press 'Edit' to end the animation and return to editing the program.");
}

function startAutoplay() {
    interpreter.setAutoplay(true);
    autoplaying = true;

    $("#stopbutton").css("display", "inline");
    $("#fasterbutton").css("display", "inline");
    $("#slowerbutton").css("display", "inline");
    $("#stepinterval").css("display", "inline");

    $("#playbutton").css("display", "none");
    $("#stepbutton").css("display", "none");
    $("#backbutton").css("display", "none");

    $("#status").text("Playing");
}

function stopAutoplay() {
    interpreter.setAutoplay(false);
    autoplaying = false;

    $("#stopbutton").css("display", "none");
    $("#fasterbutton").css("display", "none");
    $("#slowerbutton").css("display", "none");
    $("#stepinterval").css("display", "none");

    $("#playbutton").css("display", "inline");
    $("#stepbutton").css("display", "inline");
    $("#backbutton").css("display", "inline");

    $("#status").text("Paused");
}


function errorMsg(msg) {
    $("#feedback").html(msg);
}

function updateStepInterval() {
    $("#stepinterval").text("Step interval: " +
			    interpreter.getAutoplayPeriod()+ "ms");   
}

function cancelTimeout() {
    if (timeoutId !== null) {
	clearTimeout(timeoutId);
	timeoutId = null;
    }
}

function createWorker() {
    var worker = new Worker ("./scripts/aaljs.js");
    worker.onmessage = function (m) {
	if (typeof m.data == 'string') {
            console.log("" + m.data);
	} else {
            console.log ("[ASYNCH] back from " + m.data.fname);
            var handler = worker_handler[m.data.fname];
            handler (m.data.result);
	}
    }
    worker.onerror = function(event) {
	startEdit();
	cancelTimeout();
	$("#feedback").text("Compiler exception: " + event.message);
    };
    return worker;
}

var worker_handler = new Object ();
var worker = createWorker();

function ASYNCH (action_name, action_args, cont) {
    worker_handler[action_name] = cont;
    worker.postMessage ({fname: action_name, args: action_args});
    console.log ("[ASYNCH] " + action_name + " (" + action_args + ")");
}

function cancelWorker () {
    worker.terminate();
    worker = undefined;
    worker = createWorker();
}

function startTimeout() {
    timeoutId = setTimeout(function() {
	cancelWorker();
	startEdit();
	$("#feedback").text("Compilation timed out after 10 seconds.");
    }, 10000);
}

function compile () {
    let editor = ace.edit("editor");
    var txt = editor.getValue();
    startTimeout();
    ASYNCH ("compile", [txt], function (resp) {
	stopCompile();
	cancelTimeout();
	// console.log("response from compile: " + resp);
	let response = sexp(resp);
	switch (response[0]) {
	case "SuccessResponse":
	    let success = response[1];
	    let result = success[0];
	    let aprog = success[1];
	    startAnimation(aprog);
	    break;
	case "ErrorResponse":
	    let error = response[1];
	    startEdit();
	    errorMsg(error);
	    break;
	default:
	    startEdit();
	    errorMsg("unknown response from server");
	}
    })
}

var playEnabled = true;
function setPlayDisabled(b) {
    $("#playbutton").prop('disabled', b);
    $("#stepbutton").prop('disabled', b);
    playEnabled = !b;
}

var backEnabled = true;
function setBackDisabled(b) {
    $("#backbutton").prop('disabled', b);
    backEnabled = !b;
}

function increaseFontSize(editor) {
    editor.setOption("fontSize", editor.getOption("fontSize") + 1);
    editor.resize();
}

function decreaseFontSize(editor) {
    editor.setOption("fontSize",
		     Math.max(6, editor.getOption("fontSize") - 1));
    editor.resize();
}

function init() {
    interpreter = new Interp(programEndCallback, statusCallback,
			     // (status) => setBackDisabled(!status.pc),
			     document.getElementById('canvas'));
    
    $("#gobutton").click(function() {
	startCompile();
	compile();
	setPlayDisabled(false);	
    });

    $("#cancelbutton").click(function() {
	cancelTimeout();
	cancelWorker();
	stopCompile();
	startEdit();
    });

    $("#stepbutton").click(function() {
	interpreter.stepForward();
    });

    $("#editbutton").click(function() {
	stopAutoplay();
	startEdit();
    });

    $("#resetbutton").click(function() {
	interpreter.reset();
	setPlayDisabled(false);
    });

    $("#playbutton").click(function() {
	if (!interpreter.isDone()) {
	    startAutoplay();
	}
    });

    $("#stopbutton").click(function() {
	stopAutoplay();
    });

    $("#fasterbutton").click(function() {
	let cur_period = interpreter.getAutoplayPeriod();	    
	interpreter.setAutoplayPeriod(Math.max(1, cur_period - 50));
	updateStepInterval();
    });

    $("#slowerbutton").click(function() {
	let cur_period = interpreter.getAutoplayPeriod();
	if (cur_period === 1) {
	    interpreter.setAutoplayPeriod(50);
	}
	else {
	    interpreter.setAutoplayPeriod(cur_period + 50);
	}
	updateStepInterval();
    });

    $("#backbutton").click(function() {
	interpreter.stepBack();
	$("#status").text("Paused");
	setPlayDisabled(false);
    });

    updateStepInterval();
    
    let canvas = document.getElementById('canvas');
    let ctx = canvas.getContext('2d');
    // ctx.scale(2, 2); // not working .. ?
    Puddi.run(canvas);
    startEdit();

    // configure ace editor
    // editor.setTheme("ace/theme/twilight");
    let editor = ace.edit("editor");
    editor.setTheme("ace/theme/iplastic");
    editor.session.setMode("ace/mode/javascript");
    editor.session.setUseWorker(false); // disable errors/warnings
    // editor.setAutoScrollEditorIntoView(true);
    editor.commands.removeCommand(editor.commands.byName.showSettingsMenu)
    editor.setOption("fontSize", 14);
    editor.setOption("tabSize", 2);
    editor.setOption("showPrintMargin", false)
    // add commands for changing font size
    editor.commands.addCommand({
	name: 'fontSizeDecrease',
	bindKey: {win: 'Ctrl-,',  mac: 'Command-,'},
	exec: decreaseFontSize
    });
    editor.commands.addCommand({
	name: 'fontSizeIncrease',
	bindKey: {win: 'Ctrl-.',  mac: 'Command-.'},
	exec: increaseFontSize
    });

    $("#editorplusbutton").click(function() {
	increaseFontSize(editor);
    });

    $("#editorminusbutton").click(function() {
	decreaseFontSize(editor);
    });
}

function programEndCallback() {
    stopAutoplay();
    $("#status").text("Finished");
    setPlayDisabled(true);
}

function statusCallback(status) {
    setBackDisabled(!status.pc);
}

function rescale() {
    screen_width = window.innerWidth
	|| document.documentElement.clientWidth
	|| document.body.clientWidth;
    screen_height = window.innerHeight
	|| document.documentElement.clientHeight
	|| document.body.clientHeight;
    console.log("width: " + screen_width + ", height: " + screen_height);
    
    let w = screen_width - 75; // 25 margin on both sides
    let h = screen_height - 135; // vertical space available
    // give editor 80 columns or half the width if not enough space
    let editor_width = Math.min(545, w / 2);
    $("#editor").css("width", editor_width);
    $("#feedback").css("width", editor_width - 4); // minus left margin
    
    // give canvas the remaining width
    let canvas = document.getElementById('canvas');
    canvas.width = w - editor_width;
    $("#status").css("width", w - editor_width);

    // give canvas the max height possible and
    // editor 105 less than that
    canvas.height = h;
    $("#editor").css("height", h - 105);

    // refresh editor
    let editor = ace.edit("editor");
    editor.resize();
}

$("#canvasplusbutton").click(function() {
    Puddi.scale(1.1);
});

$("#canvasminusbutton").click(function() {
    Puddi.scale(0.9);
});

document.addEventListener('keydown', function(event) {
    if (!hotkeysEnabled) { return; }

    switch (event.keyCode) {
    case 37: // left
	break;
    case 38: // up
	break;
    case 39: // right
	break;
    case 40: // down
	break;
    case 66: // b
	document.getElementById("backbutton").click();w
	break;
    case 67: // c
	if (compiling) {
	    document.getElementById("cancelbutton").click();
	}
	break;
    case 69: // e
	document.getElementById("editbutton").click();
	break;
    case 70: // f
	document.getElementById("fasterbutton").click();
	break;
    // case 71: // g
    //  document.getElementById("gobutton").click();
    // 	break;
    case 80: // p
	document.getElementById("playbutton").click();
	break;
    case 82: // r
	document.getElementById("resetbutton").click();
	break;
    case 83: // s
	if (autoplaying) {
	    document.getElementById("stopbutton").click();
	}
	else {
	    document.getElementById("stepbutton").click();
	}
	break;
    case 87: // w
	document.getElementById("slowerbutton").click();
	break;
    default:
    }
    if(event.keyCode == 37) {
        console.log('Left was pressed');
    }
    else if(event.keyCode == 39) {
        console.log('Right was pressed');
    }
});

window.addEventListener('resize', function(event){
    rescale();
});

$(document).ready(function() {
    init();
    rescale();
});
