(function() {
	"use strict";

	var cropper = null;
	var cropperReady = false;
	var file = null;
	var fileName = null;
	var fileFormat = "png";
	var cameraMode = false;
	var autoEditAfterFileSelection = false;
	var suppressChooseClick = false;
	var maximumImageDimension = 4096;
	var minimumImageDimension = 320;
	var currentImageUrl = null;
	var resizeTimer = null;
	var encodedWidth = 0;
	var encodedHeight = 0;

	// Freehand drawing over the photo.
	// Strokes are kept in the photo's own pixels, before any crop or rotation, so they stay put
	// when the photo is cropped or rotated after drawing. They are uploaded as standard SVG markup
	// (posted as _mk) which the server draws over the uploaded image.
	var editorMode = "crop"; // crop, draw or text
	// Lines {colour: "#rrggbb", width: photo pixels, points: [[x, y], ...] in photo pixels}
	// and labels {type: "text", colour, text, x, y: centre in photo pixels, size: photo pixels, angle: radians from the photo's x axis}
	// in the order they were added, which is the order Undo removes them in
	var strokes = [];
	var activeStroke = null;
	var activePointerId = null;
	var pointersDown = {};
	var pointersDownCount = 0;
	var penColour = "#e53935";
	var view = null; // where the photo is drawn in draw and text modes - see layoutView()
	var renderPending = false;

	// Labels are text on a rounded block of colour, like Instagram's, in a form the Markup dialog's Excalidraw can draw the same:
	// regular Helvetica centred in a box whose corners are rounded by a quarter of its shorter side.
	var TEXT_FONT_FAMILY = "Helvetica, Arial, sans-serif";
	// Long labels wrap onto more lines, spaced as Excalidraw spaces Helvetica.
	var TEXT_BASELINE = 0.35; // a line's baseline sits this many font sizes below the line's centre, as Excalidraw's does
	var TEXT_LINE_HEIGHT = 1.15; // in font sizes
	var TEXT_WRAP_WIDTH = 20; // the widest line in font sizes, about 70% across a portrait photo
	var TEXT_PADDING = 0.45; // space either side of the text in font sizes
	var TEXT_BOX_MARGIN = 0.35; // the box's extra height above and below the lines, in font sizes
	var TEXT_BOX_OPACITY = 0.75; // the photo shows through the box a little; the text is solid
	var textMeasure = null; // a canvas context for measuring labels
	var selectedText = null; // the label colours apply to, and that a second tap retypes, in text mode
	var textDrag = null; // a label being moved, or a tap on the photo
	var textEntryOpen = false;
	var typingItem = null; // the label being typed, which shows on the photo as it is typed
	var typingIsNew = false;
	var typingOriginal = null; // its text and colour before, for putting back on cancel
	var cancelRequested = false; // the cross was pressed, so losing focus discards rather than places the label

	// Size the cropper to fit the frame at the aspect ratio (width / height) of the photo as shown
	function sizeCropperImage(image, aspect) {
		var frame = document.getElementById("cropperFrame");
		var imageContainer = document.getElementById("cropperImageContainer");
		var style = window.getComputedStyle(frame);
		var horizontalPadding = parseFloat(style.paddingLeft) + parseFloat(style.paddingRight);
		var verticalPadding = parseFloat(style.paddingTop) + parseFloat(style.paddingBottom);
		var frameWidth = frame.clientWidth - horizontalPadding;
		var frameHeight = frame.clientHeight - verticalPadding;
		if (! (frameWidth > 0) || ! (frameHeight > 0)) {
			return;
		}
		var imageWidth;
		var imageHeight;
		if ((frameWidth / frameHeight) > aspect) {
			imageWidth = Math.round(frameHeight * aspect);
			imageHeight = frameHeight;
		}
		else {
			imageWidth = frameWidth;
			imageHeight = Math.round(frameWidth / aspect);
		}
		imageContainer.style.width = imageWidth + "px";
		imageContainer.style.height = imageHeight + "px";
		image.style.width = imageWidth + "px";
		image.style.height = imageHeight + "px";
	}

	// restore puts back the editing so far when the cropper is rebuilt for a new window size:
	// {aspect: of the photo as shown, transform: {rotate, scaleX, scaleY}, crop: {x, y, width, height}}
	function initialiseCropper(imageUrl, restore) {
		currentImageUrl = imageUrl;
		var image = document.getElementById("cropperImage");
		if (cropper != null) {
			cropper.destroy();
			cropper = null;
		}
		cropperReady = false;
		view = null;
		image.onload = function() {
			window.requestAnimationFrame(function() {
				sizeCropperImage(image, restore ? restore.aspect : (image.naturalWidth / image.naturalHeight));
				cropper = new Cropper(image, {
					viewMode: 1,
					dragMode: "move",
					autoCropArea: 1,
					background: false,
					checkOrientation: true,
					responsive: true,
					// The defaults (200 x 100) are wider than a photo on a phone held sideways (0 also means the default)
					minContainerWidth: 1,
					minContainerHeight: 1,
					ready: function() {
						if (restore) {
							cropper.setData(restore.transform);
							fitCanvas();
							cropper.setData(restore.crop);
						}
						else {
							cropper.setCropBoxData(cropper.getCanvasData());
						}
						cropperReady = true;
						layoutView();
					},
					crop: function() {
						if (editorMode === "crop") {
							requestRender();
						}
					}
				});
			});
		};
		image.src = "";
		image.src = imageUrl;
	}

	// Show the whole photo, as big as the cropper allows
	function fitCanvas() {
		var container = cropper.getContainerData();
		var canvas = cropper.getCanvasData();
		var scale = Math.min(container.width / canvas.naturalWidth, container.height / canvas.naturalHeight);
		var width = canvas.naturalWidth * scale;
		var height = canvas.naturalHeight * scale;
		cropper.setCanvasData({left: (container.width - width) / 2, top: (container.height - height) / 2, width: width, height: height});
	}

	// Photo geometry from the cropper. "Box" coordinates are the rotated photo's bounding box in photo pixels,
	// which is the space the cropper's getData() crop rectangle is in.
	function photoGeometry() {
		var data = cropper.getData();
		var imageData = cropper.getImageData();
		var angle = (data.rotate || 0) * Math.PI / 180;
		var cos = Math.cos(angle);
		var sin = Math.sin(angle);
		var width = imageData.naturalWidth;
		var height = imageData.naturalHeight;
		return {
			data: data,
			angle: angle,
			cos: cos,
			sin: sin,
			scaleX: data.scaleX || 1,
			scaleY: data.scaleY || 1,
			width: width,
			height: height,
			boxWidth: Math.abs(width * cos) + Math.abs(height * sin),
			boxHeight: Math.abs(width * sin) + Math.abs(height * cos)
		};
	}

	function photoToBox(geometry, x, y) {
		var dx = (x - (geometry.width / 2)) * geometry.scaleX;
		var dy = (y - (geometry.height / 2)) * geometry.scaleY;
		return [(geometry.boxWidth / 2) + (dx * geometry.cos) - (dy * geometry.sin),
				(geometry.boxHeight / 2) + (dx * geometry.sin) + (dy * geometry.cos)];
	}

	function boxToPhoto(geometry, x, y) {
		var dx = x - (geometry.boxWidth / 2);
		var dy = y - (geometry.boxHeight / 2);
		return [(((dx * geometry.cos) + (dy * geometry.sin)) / geometry.scaleX) + (geometry.width / 2),
				(((dy * geometry.cos) - (dx * geometry.sin)) / geometry.scaleY) + (geometry.height / 2)];
	}

	function sizeCanvas(canvas, width, height, ratio) {
		var canvasWidth = Math.max(1, Math.round(width * ratio));
		var canvasHeight = Math.max(1, Math.round(height * ratio));
		if ((canvas.width !== canvasWidth) || (canvas.height !== canvasHeight)) {
			canvas.width = canvasWidth;
			canvas.height = canvasHeight;
		}
	}

	// Size the drawing layers to the frame and, in draw mode, fit the cropped photo into it
	function layoutView() {
		if ((cropper == null) || (! cropperReady)) {
			return;
		}
		var frame = document.getElementById("cropperFrame");
		var width = frame.clientWidth;
		var height = frame.clientHeight;
		var ratio = window.devicePixelRatio || 1;
		sizeCanvas(document.getElementById("scrawlInk"), width, height, ratio);

		var geometry = photoGeometry();
		view = {geometry: geometry, ratio: ratio};
		if (editorMode !== "crop") {
			var style = window.getComputedStyle(frame);
			var left = parseFloat(style.paddingLeft);
			var top = parseFloat(style.paddingTop);
			var availableWidth = width - left - parseFloat(style.paddingRight);
			var availableHeight = height - top - parseFloat(style.paddingBottom);
			var data = geometry.data;
			var scale = Math.min(availableWidth / data.width, availableHeight / data.height);
			var photoWidth = data.width * scale;
			var photoHeight = data.height * scale;
			left += (availableWidth - photoWidth) / 2;
			top += (availableHeight - photoHeight) / 2;
			view.scale = scale;
			view.originX = left - (data.x * scale);
			view.originY = top - (data.y * scale);
			view.clip = {left: left, top: top, width: photoWidth, height: photoHeight};
			var photo = document.getElementById("scrawlPhoto");
			sizeCanvas(photo, width, height, ratio);
			renderPhoto(photo);
		}
		updateCursor();
		requestRender();
	}

	// Draw mode shows just the cropped area, as big as it fits
	function renderPhoto(canvas) {
		var context = canvas.getContext("2d");
		var geometry = view.geometry;
		var clip = view.clip;
		context.setTransform(1, 0, 0, 1, 0, 0);
		context.clearRect(0, 0, canvas.width, canvas.height);
		context.setTransform(view.ratio, 0, 0, view.ratio, 0, 0);
		context.save();
		context.beginPath();
		context.rect(clip.left, clip.top, clip.width, clip.height);
		context.clip();
		context.translate(view.originX, view.originY);
		context.scale(view.scale, view.scale);
		context.translate(geometry.boxWidth / 2, geometry.boxHeight / 2);
		context.rotate(geometry.angle);
		context.scale(geometry.scaleX, geometry.scaleY);
		context.translate(-geometry.width / 2, -geometry.height / 2);
		context.imageSmoothingEnabled = true;
		context.imageSmoothingQuality = "high";
		context.drawImage(cropper.image, 0, 0, geometry.width, geometry.height);
		context.restore();
	}

	// Crop mode draws the strokes over the cropper, which the user can pan and zoom
	function cropView(ink) {
		var canvasData = cropper.getCanvasData();
		var cropBox = cropper.getCropBoxData();
		var containerRect = cropper.cropper.getBoundingClientRect();
		var inkRect = ink.getBoundingClientRect();
		var offsetX = containerRect.left - inkRect.left;
		var offsetY = containerRect.top - inkRect.top;
		var scale = canvasData.width / canvasData.naturalWidth;
		return {
			geometry: photoGeometry(),
			scale: scale,
			originX: offsetX + canvasData.left,
			originY: offsetY + canvasData.top,
			clip: {left: offsetX + cropBox.left, top: offsetY + cropBox.top, width: cropBox.width, height: cropBox.height}
		};
	}

	function requestRender() {
		if (! renderPending) {
			renderPending = true;
			window.requestAnimationFrame(render);
		}
	}

	function render() {
		renderPending = false;
		var ink = document.getElementById("scrawlInk");
		var context = ink.getContext("2d");
		context.setTransform(1, 0, 0, 1, 0, 0);
		context.clearRect(0, 0, ink.width, ink.height);
		if ((cropper == null) || (! cropperReady) || (view == null) || (strokes.length === 0)) {
			return;
		}
		var mapping = (editorMode !== "crop") ? view : cropView(ink);
		var clip = mapping.clip;
		context.setTransform(view.ratio, 0, 0, view.ratio, 0, 0);
		context.save();
		context.beginPath();
		context.rect(clip.left, clip.top, clip.width, clip.height);
		context.clip();
		context.lineCap = "round";
		context.lineJoin = "round";
		for (var i = 0, l = strokes.length; i < l; i++) {
			if (strokes[i].type === "text") {
				drawText(context, strokes[i], mapping);
			}
			else {
				drawStroke(context, strokes[i], mapping);
			}
		}
		context.restore();
	}

	// Black text on light colours and white on dark ones
	function contrastColour(colour) {
		var value = parseInt(colour.substring(1), 16);
		var luminance = ((0.299 * (value >> 16)) + (0.587 * ((value >> 8) & 255)) + (0.114 * (value & 255))) / 255;
		return (luminance > 0.6) ? "#000000" : "#ffffff";
	}

	// A label's lines, and its box's width, height and corner radius, for text of the given size.
	// Lines are measured at one size and scaled, so the label breaks in the same places on screen and in the upload.
	var TEXT_MEASURE_SIZE = 100;
	function labelBox(text, size) {
		if (textMeasure == null) {
			textMeasure = document.createElement("canvas").getContext("2d");
		}
		textMeasure.font = TEXT_MEASURE_SIZE + "px " + TEXT_FONT_FAMILY;
		var lines = wrapLabel(text, TEXT_WRAP_WIDTH * TEXT_MEASURE_SIZE);
		var widest = 0;
		for (var i = 0, l = lines.length; i < l; i++) {
			widest = Math.max(widest, textMeasure.measureText(lines[i]).width);
		}
		var width = ((widest / TEXT_MEASURE_SIZE) + (2 * TEXT_PADDING)) * size;
		var height = ((lines.length * TEXT_LINE_HEIGHT) + TEXT_BOX_MARGIN) * size;
		return {lines: lines, width: width, height: height, radius: Math.min(width, height) / 4};
	}

	// Breaks text into lines no wider than the given width (in textMeasure's font) at spaces,
	// and breaks a word that is too wide on its own between letters
	function wrapLabel(text, maximumWidth) {
		if (lineFits(text, maximumWidth)) {
			return [text];
		}
		var lines = [];
		var line = "";
		var words = text.trim().split(/\s+/);
		for (var i = 0, l = words.length; i < l; i++) {
			var word = words[i];
			var candidate = (line === "") ? word : (line + " " + word);
			if (lineFits(candidate, maximumWidth)) {
				line = candidate;
			}
			else {
				if (line !== "") {
					lines.push(line);
				}
				line = breakWord(word, maximumWidth, lines);
			}
		}
		if (line !== "") {
			lines.push(line);
		}
		return (lines.length > 0) ? lines : [text];
	}

	function lineFits(line, maximumWidth) {
		return textMeasure.measureText(line).width <= maximumWidth;
	}

	// Adds the full lines of a word too wide for a line to the lines given and returns the rest of the word
	function breakWord(word, maximumWidth, lines) {
		var line = "";
		for (var i = 0, l = word.length; i < l; i++) {
			if ((line !== "") && (! lineFits(line + word.charAt(i), maximumWidth))) {
				lines.push(line);
				line = "";
			}
			line += word.charAt(i);
		}
		return line;
	}

	// How far below the label's centre each line's baseline is, in font sizes
	function lineBaseline(index, count) {
		return ((index - ((count - 1) / 2)) * TEXT_LINE_HEIGHT) + TEXT_BASELINE;
	}

	// Draws a label and records where it is on screen for picking it up
	function drawText(context, item, mapping) {
		var box = photoToBox(mapping.geometry, item.x, item.y);
		var x = mapping.originX + (box[0] * mapping.scale);
		var y = mapping.originY + (box[1] * mapping.scale);
		var size = item.size * mapping.scale;
		var angle = mapping.geometry.angle + item.angle;
		var label = item.text || "|"; // a caret while nothing has been typed yet
		var tag = labelBox(label, size);
		var halfWidth = tag.width / 2;
		var halfHeight = tag.height / 2;
		context.save();
		context.translate(x, y);
		context.rotate(angle);
		context.beginPath();
		context.moveTo(-halfWidth + tag.radius, -halfHeight);
		context.arcTo(halfWidth, -halfHeight, halfWidth, halfHeight, tag.radius);
		context.arcTo(halfWidth, halfHeight, -halfWidth, halfHeight, tag.radius);
		context.arcTo(-halfWidth, halfHeight, -halfWidth, -halfHeight, tag.radius);
		context.arcTo(-halfWidth, -halfHeight, halfWidth, -halfHeight, tag.radius);
		context.closePath();
		context.fillStyle = item.colour;
		context.globalAlpha = TEXT_BOX_OPACITY;
		context.fill();
		context.globalAlpha = 1;
		context.font = size + "px " + TEXT_FONT_FAMILY;
		context.textAlign = "center";
		context.textBaseline = "alphabetic";
		context.fillStyle = contrastColour(item.colour);
		for (var i = 0, l = tag.lines.length; i < l; i++) {
			context.fillText(tag.lines[i], 0, size * lineBaseline(i, l));
		}
		item.hit = {x: x, y: y, angle: angle, halfWidth: halfWidth, halfHeight: halfHeight};
		if ((item === selectedText) && (editorMode === "text")) {
			// A dashed frame just outside the box
			var frameWidth = (halfWidth + 4) * 2;
			var frameHeight = (halfHeight + 4) * 2;
			context.lineWidth = 1.5;
			context.setLineDash([5, 4]);
			context.strokeStyle = "rgba(255, 255, 255, 0.95)";
			context.strokeRect(-halfWidth - 4, -halfHeight - 4, frameWidth, frameHeight);
			context.lineDashOffset = 4.5;
			context.strokeStyle = "rgba(0, 0, 0, 0.6)";
			context.strokeRect(-halfWidth - 4, -halfHeight - 4, frameWidth, frameHeight);
		}
		context.restore();
	}

	// The label under a pointer, topmost first
	function textAt(event, ink) {
		var rect = ink.getBoundingClientRect();
		var px = event.clientX - rect.left;
		var py = event.clientY - rect.top;
		for (var i = strokes.length - 1; i >= 0; i--) {
			var hit = strokes[i].hit;
			if ((strokes[i].type === "text") && hit) {
				var dx = px - hit.x;
				var dy = py - hit.y;
				var cos = Math.cos(hit.angle);
				var sin = Math.sin(hit.angle);
				if ((Math.abs((dx * cos) + (dy * sin)) <= hit.halfWidth) && (Math.abs((dy * cos) - (dx * sin)) <= hit.halfHeight)) {
					return strokes[i];
				}
			}
		}
		return null;
	}

	// Labels are 3.5% of the shorter side of the cropped photo, small enough for notes, rather than captions,
	// and they come out the same size whichever way a phone is held,
	// but no smaller than 10 or bigger than 40 CSS pixels on screen
	function textSize() {
		var clip = view.clip;
		var screenSize = Math.min(40, Math.max(10, 0.035 * Math.min(clip.width, clip.height)));
		return screenSize / view.scale;
	}

	function drawStroke(context, stroke, mapping) {
		var points = stroke.points;
		context.strokeStyle = stroke.colour;
		context.lineWidth = stroke.width * mapping.scale;
		context.beginPath();
		for (var i = 0, l = points.length; i < l; i++) {
			var box = photoToBox(mapping.geometry, points[i][0], points[i][1]);
			var x = mapping.originX + (box[0] * mapping.scale);
			var y = mapping.originY + (box[1] * mapping.scale);
			if (i === 0) {
				context.moveTo(x, y);
				if (l === 1) { // a dot - zero length lines are not drawn
					context.lineTo(x + 0.01, y);
				}
			}
			else {
				context.lineTo(x, y);
			}
		}
		context.stroke();
	}

	// The pen is about 1.5% of the shorter side of the cropped photo, and 4 to 10 CSS pixels on screen
	function penWidth() {
		var clip = view.clip;
		var screenWidth = Math.min(10, Math.max(4, 0.015 * Math.min(clip.width, clip.height)));
		return screenWidth / view.scale;
	}

	function inkPoint(event, ink) {
		var rect = ink.getBoundingClientRect();
		return boxToPhoto(view.geometry,
							(event.clientX - rect.left - view.originX) / view.scale,
							(event.clientY - rect.top - view.originY) / view.scale);
	}

	function onPointerDown(event) {
		if ((editorMode === "crop") || (view == null) || textEntryOpen) {
			return;
		}
		if ((event.pointerType === "mouse") && (event.button !== 0)) {
			return;
		}
		event.preventDefault();
		if (! pointersDown[event.pointerId]) {
			pointersDown[event.pointerId] = true;
			pointersDownCount++;
		}
		// A second finger is a pinch, not a stroke or a move
		if (pointersDownCount > 1) {
			cancelStroke();
			cancelTextDrag();
			return;
		}
		var ink = event.currentTarget;
		try {
			ink.setPointerCapture(event.pointerId);
		}
		catch (e) {
			// carry on without capture
		}
		activePointerId = event.pointerId;
		if (editorMode === "text") {
			// Select and pick up the label under the pointer; anywhere else on the photo deselects
			var item = textAt(event, ink);
			var point = inkPoint(event, ink);
			textDrag = {item: item, x: point[0], y: point[1], itemX: item ? item.x : 0, itemY: item ? item.y : 0,
						clientX: event.clientX, clientY: event.clientY, moved: false,
						wasSelected: (item != null) && (item === selectedText)};
			selectedText = item;
			requestRender();
			return;
		}
		activeStroke = {colour: penColour, width: penWidth(), points: [inkPoint(event, ink)]};
		strokes.push(activeStroke);
		updateUndo();
		requestRender();
	}

	function onPointerMove(event) {
		if ((editorMode === "text") && (view != null)) {
			moveText(event);
			return;
		}
		if ((activeStroke == null) || (event.pointerId !== activePointerId)) {
			return;
		}
		event.preventDefault();
		var ink = event.currentTarget;
		var events = (typeof event.getCoalescedEvents === "function") ? event.getCoalescedEvents() : null;
		if ((! events) || (events.length === 0)) {
			events = [event];
		}
		var points = activeStroke.points;
		var minimum = 1 / view.scale; // keep points at least a CSS pixel apart
		for (var i = 0, l = events.length; i < l; i++) {
			var point = inkPoint(events[i], ink);
			var last = points[points.length - 1];
			if ((Math.abs(point[0] - last[0]) + Math.abs(point[1] - last[1])) >= minimum) {
				points.push(point);
			}
		}
		requestRender();
	}

	function onPointerUp(event) {
		if (pointersDown[event.pointerId]) {
			delete pointersDown[event.pointerId];
			pointersDownCount--;
		}
		if (event.pointerId === activePointerId) {
			activeStroke = null;
			activePointerId = null;
			if (textDrag != null) {
				var drag = textDrag;
				textDrag = null;
				// Tapping the selected label again retypes it
				if ((! drag.moved) && drag.wasSelected && (event.type === "pointerup")) {
					openTextEntry(drag.item);
				}
			}
		}
	}

	// Moves the label being dragged, or shows which labels can be picked up under a mouse
	function moveText(event) {
		var ink = event.currentTarget;
		if ((textDrag == null) || (event.pointerId !== activePointerId)) {
			if (event.pointerType === "mouse") {
				ink.style.cursor = textAt(event, ink) ? "move" : "";
			}
			return;
		}
		event.preventDefault();
		if ((Math.abs(event.clientX - textDrag.clientX) + Math.abs(event.clientY - textDrag.clientY)) > 6) {
			textDrag.moved = true;
		}
		if (textDrag.moved && textDrag.item) {
			var point = inkPoint(event, ink);
			textDrag.item.x = textDrag.itemX + (point[0] - textDrag.x);
			textDrag.item.y = textDrag.itemY + (point[1] - textDrag.y);
			requestRender();
		}
	}

	function cancelTextDrag() {
		if (textDrag != null) {
			if (textDrag.item) {
				textDrag.item.x = textDrag.itemX;
				textDrag.item.y = textDrag.itemY;
			}
			textDrag = null;
			requestRender();
		}
	}

	// Typing happens in a box at the top of the editor, above where a phone's keyboard comes up,
	// and the label shows on the photo, selected, as it is typed. item is the label to retype, or null for a new one.
	function openTextEntry(item) {
		typingIsNew = (item == null);
		if (typingIsNew) {
			// In the upper third of the cropped photo, so it shows above a phone's keyboard; upright on screen
			var data = view.geometry.data;
			var position = boxToPhoto(view.geometry, data.x + (data.width / 2), data.y + (data.height / 3));
			item = {type: "text", colour: penColour, text: "", x: position[0], y: position[1], size: textSize(), angle: -view.geometry.angle};
			strokes.push(item);
		}
		typingItem = item;
		typingOriginal = {text: item.text, colour: item.colour};
		selectedText = item;
		textEntryOpen = true;
		var input = document.getElementById("scrawlText");
		input.value = item.text;
		document.getElementById("scrawlTextEntry").hidden = false;
		input.focus(); // straight away, while handling the tap, so iOS shows its keyboard
		updateUndo();
		requestRender();
	}

	// Closed before the box loses focus, so losing it does not place the label again
	function closeTextEntry() {
		textEntryOpen = false;
		typingItem = null;
		typingOriginal = null;
		document.getElementById("scrawlTextEntry").hidden = true;
		document.getElementById("scrawlText").blur();
	}

	// Places the label typed; an empty one is removed
	window.commitText = function() {
		if (! textEntryOpen) {
			return;
		}
		var item = typingItem;
		var text = document.getElementById("scrawlText").value.replace(/\s+/g, " ").trim();
		closeTextEntry();
		if (text) {
			item.text = text;
		}
		else {
			strokes.splice(strokes.indexOf(item), 1);
			selectedText = null;
		}
		updateUndo();
		requestRender();
	};

	// Puts the label back as it was, or removes a new one
	window.cancelText = function() {
		cancelRequested = false;
		if (! textEntryOpen) {
			return;
		}
		var item = typingItem;
		var original = typingOriginal;
		var isNew = typingIsNew;
		closeTextEntry();
		if (isNew) {
			strokes.splice(strokes.indexOf(item), 1);
			selectedText = null;
		}
		else {
			item.text = original.text;
			item.colour = original.colour;
		}
		updateUndo();
		requestRender();
	};

	function cancelStroke() {
		if (activeStroke != null) {
			var index = strokes.indexOf(activeStroke);
			if (index >= 0) {
				strokes.splice(index, 1);
			}
		}
		activeStroke = null;
		activePointerId = null;
		updateUndo();
		requestRender();
	}

	function resetDrawing() {
		if (textEntryOpen) {
			closeTextEntry();
		}
		selectedText = null;
		textDrag = null;
		strokes = [];
		activeStroke = null;
		activePointerId = null;
		pointersDown = {};
		pointersDownCount = 0;
		$("#_mk").val("");
		updateUndo();
		showEditorMode("crop");
	}

	function undo() {
		if ((activeStroke == null) && (textDrag == null)) {
			if (strokes.pop() === selectedText) {
				selectedText = null;
			}
			updateUndo();
			requestRender();
		}
	}

	function updateUndo() {
		$("#scrawlUndo").prop("disabled", strokes.length === 0).toggleClass("ui-state-disabled", strokes.length === 0);
	}

	// A dot of the pen's colour and size as the mouse cursor
	function updateCursor() {
		var ink = document.getElementById("scrawlInk");
		if ((editorMode !== "draw") || (view == null) || (! window.matchMedia("(pointer: fine)").matches)) {
			ink.style.cursor = "";
			return;
		}
		var diameter = Math.max(6, Math.round(penWidth() * view.scale));
		var size = diameter + 4;
		var centre = Math.round(size / 2);
		var svg = '<svg xmlns="http://www.w3.org/2000/svg" width="' + size + '" height="' + size + '">' +
					'<circle cx="' + (size / 2) + '" cy="' + (size / 2) + '" r="' + (diameter / 2) + '" fill="' + penColour + '" stroke="#fff" stroke-width="1.5"/>' +
					'</svg>';
		ink.style.cursor = 'url("data:image/svg+xml,' + encodeURIComponent(svg) + '") ' + centre + ' ' + centre + ', crosshair';
	}

	function showEditorMode(mode) {
		editorMode = mode;
		// Draw and text modes both show the cropped photo
		$(document.body).toggleClass("skyve-scrawl-drawing", mode !== "crop").toggleClass("skyve-scrawl-text", mode === "text");
		$("#textMode").toggleClass("scrawl-mode-selected", mode === "text").attr("aria-pressed", String(mode === "text"));
		// Not ui-state-active, which PrimeFaces removes from buttons on mouse up
		$("#cropMode").toggleClass("scrawl-mode-selected", mode === "crop").attr("aria-pressed", String(mode === "crop"));
		$("#drawMode").toggleClass("scrawl-mode-selected", mode === "draw").attr("aria-pressed", String(mode === "draw"));
	}

	// Text opens the typing box every time it is pressed, for another label
	window.setEditorMode = function(mode) {
		if ((cropper == null) || (! cropperReady)) {
			return;
		}
		if (textEntryOpen) {
			window.commitText();
		}
		if (mode !== editorMode) {
			activeStroke = null;
			activePointerId = null;
			textDrag = null;
			showEditorMode(mode);
			layoutView(); // synchronous, so a new label's size uses the new layout
		}
		if (mode === "text") {
			openTextEntry(null);
		}
	};

	window.undoStroke = undo;

	// The strokes as SVG over the uploaded image, which is width x height pixels
	function markupSvg(width, height) {
		if ((strokes.length === 0) || (cropper == null)) {
			return "";
		}
		var geometry = photoGeometry();
		var data = geometry.data;
		var scaleX = width / data.width;
		var scaleY = height / data.height;
		var round = function(value) {
			return Math.round(value * 10) / 10;
		};
		var svg = ['<svg xmlns="http://www.w3.org/2000/svg" width="', width, '" height="', height, '" viewBox="0 0 ', width, ' ', height, '">'];
		for (var i = 0, l = strokes.length; i < l; i++) {
			var stroke = strokes[i];
			if (stroke.type === "text") {
				svg.push(textSvg(stroke, geometry, scaleX, scaleY, round));
				continue;
			}
			var points = stroke.points;
			var d = [];
			for (var j = 0, m = points.length; j < m; j++) {
				var box = photoToBox(geometry, points[j][0], points[j][1]);
				var x = round((box[0] - data.x) * scaleX);
				var y = round((box[1] - data.y) * scaleY);
				d.push(((j === 0) ? "M" : "L") + x + " " + y);
				if (m === 1) { // a dot
					d.push("L" + round(x + 0.1) + " " + y);
				}
			}
			svg.push('<path d="', d.join(" "), '" fill="none" stroke="', stroke.colour, '" stroke-width="', round(stroke.width * scaleX),
						'" stroke-linecap="round" stroke-linejoin="round"/>');
		}
		svg.push("</svg>");
		return svg.join("");
	}

	// A label as its box then its text, with a tspan for each line of a long label.
	// The data attributes let the Markup dialog bring it back as editable Excalidraw text in a box.
	function textSvg(item, geometry, scaleX, scaleY, round) {
		var data = geometry.data;
		var box = photoToBox(geometry, item.x, item.y);
		var x = round((box[0] - data.x) * scaleX);
		var y = round((box[1] - data.y) * scaleY);
		var size = round(item.size * scaleX);
		var tag = labelBox(item.text, size);
		var degrees = round(((((geometry.angle + item.angle) * 180 / Math.PI) % 360) + 360) % 360);
		var transform = ((degrees === 0) || (degrees === 360)) ? "" : ' transform="rotate(' + degrees + ' ' + x + ' ' + y + ')"';
		var escape = function(line) {
			return line.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;");
		};
		var lines = tag.lines;
		var text = escape(lines[0]);
		if (lines.length > 1) {
			text = "";
			for (var i = 0, l = lines.length; i < l; i++) {
				text += '<tspan x="' + x + '" y="' + round(y + (size * lineBaseline(i, l))) + '">' + escape(lines[i]) + '</tspan>';
			}
		}
		return '<rect data-skyve-label="true" x="' + round(x - (tag.width / 2)) + '" y="' + round(y - (tag.height / 2)) +
				'" width="' + round(tag.width) + '" height="' + round(tag.height) + '" rx="' + round(tag.radius) + '"' + transform +
				' fill="' + item.colour + '" fill-opacity="' + TEXT_BOX_OPACITY + '"/>' +
				'<text data-skyve-text="true" x="' + x + '" y="' + round(y + (size * lineBaseline(0, lines.length))) + '"' + transform +
				' font-family="' + TEXT_FONT_FAMILY + '" font-size="' + size + '" text-anchor="middle" fill="' +
				contrastColour(item.colour) + '">' + text + '</text>';
	}

	// Nothing changed - no drawing, rotation or crop - so the original file can be uploaded as is
	function editorUnchanged() {
		if (strokes.length > 0) {
			return false;
		}
		var data = cropper.getData();
		var imageData = cropper.getImageData();
		return (((data.rotate || 0) % 360) === 0) &&
				((data.scaleX || 1) === 1) &&
				((data.scaleY || 1) === 1) &&
				(Math.abs(data.x) < 1) &&
				(Math.abs(data.y) < 1) &&
				(Math.abs(data.width - imageData.naturalWidth) < 1) &&
				(Math.abs(data.height - imageData.naturalHeight) < 1);
	}

	function canvasForMimeType(canvas, mimeType) {
		if (mimeType !== "image/jpeg") {
			return canvas;
		}
		var targetCanvas = document.createElement("canvas");
		targetCanvas.width = canvas.width;
		targetCanvas.height = canvas.height;
		var context = targetCanvas.getContext("2d");
		context.fillStyle = "#fff";
		context.fillRect(0, 0, targetCanvas.width, targetCanvas.height);
		context.drawImage(canvas, 0, 0);
		return targetCanvas;
	}

	function canvasToBlob(canvas, mimeType, quality) {
		var outputCanvas = canvasForMimeType(canvas, mimeType);
		return new Promise(function(resolve, reject) {
			outputCanvas.toBlob(function(blob) {
				if (blob == null) {
					reject(new Error("Could not encode cropped image"));
				}
				else {
					resolve(blob);
				}
			}, mimeType, quality);
		});
	}

	function createScaledCanvas(sourceCanvas, width, height) {
		var targetCanvas = document.createElement("canvas");
		targetCanvas.width = width;
		targetCanvas.height = height;
		var context = targetCanvas.getContext("2d");
		context.imageSmoothingEnabled = true;
		context.imageSmoothingQuality = "high";
		context.drawImage(sourceCanvas, 0, 0, width, height);
		return targetCanvas;
	}

	function scaleCanvas(sourceCanvas, scale) {
		var width = Math.max(1, Math.round(sourceCanvas.width * scale));
		var height = Math.max(1, Math.round(sourceCanvas.height * scale));
		return createScaledCanvas(sourceCanvas, width, height);
	}

	function scaleCanvasToMaximumDimension(sourceCanvas, maximumDimension) {
		var dimension = Math.max(sourceCanvas.width, sourceCanvas.height);
		if (! (dimension > maximumDimension)) {
			return sourceCanvas;
		}
		return scaleCanvas(sourceCanvas, maximumDimension / dimension);
	}

	function fileExtension(format) {
		return format === "jpeg" ? "jpg" : format;
	}

	function croppedFileName(format) {
		var baseName = file.name.replace(/\.[^/.]+$/, "");
		return baseName + ".cropped." + fileExtension(format);
	}

	function formatBytes(bytes) {
		if (bytes >= 1048576) {
			return (bytes / 1048576).toFixed(1) + "MB";
		}
		return Math.ceil(bytes / 1024) + "KB";
	}

	// Encodes the cropped photo under the upload limit and records its final size in encodedWidth/Height
	async function encodeCanvasForUpload(croppedCanvas) {
		var maximumUploadSizeInBytes = window.skyveMaximumUploadSizeInBytes || 0;
		var outputFormat = cameraMode ? "jpeg" : fileFormat;
		var mimeType = "image/" + outputFormat;
		var canvas = scaleCanvasToMaximumDimension(croppedCanvas, maximumImageDimension);
		var quality = outputFormat === "jpeg" ? 0.92 : 1;
		var blob = await canvasToBlob(canvas, mimeType, quality);

		if (blob.size > maximumUploadSizeInBytes) {
			outputFormat = "jpeg";
			mimeType = "image/jpeg";
			quality = 0.92;
			blob = await canvasToBlob(canvas, mimeType, quality);
		}

		while ((blob.size > maximumUploadSizeInBytes) && (quality > 0.55)) {
			quality = Math.max(0.55, quality - 0.07);
			blob = await canvasToBlob(canvas, mimeType, quality);
		}

		while ((blob.size > maximumUploadSizeInBytes) &&
				(Math.max(canvas.width, canvas.height) > minimumImageDimension)) {
			canvas = scaleCanvas(canvas, 0.85);
			quality = 0.82;
			blob = await canvasToBlob(canvas, mimeType, quality);
		}

		fileName = croppedFileName(outputFormat);
		encodedWidth = canvas.width;
		encodedHeight = canvas.height;
		return blob;
	}

	// While the cropped photo is encoded (a few seconds on a phone) hide the buttons and say so
	function setPreparing(preparing) {
		$("#crop, #cancel").prop("disabled", preparing);
		$("#cropperBar").toggle(! preparing);
		var note = $("#cropperPreparing");
		if (preparing && (note.length === 0)) {
			note = $('<div id="cropperPreparing" class="ui-widget" style="padding:8px;text-align:center">Preparing photo…</div>');
			$("#cropperUpload").append(note);
		}
		note.toggle(preparing);
	}

	// Hand the photo to the PrimeFaces file upload so it is validated, shows its progress bar,
	// and runs its update/oncomplete like any other upload. Any drawing goes with it in the _mk field.
	// The file is added through the jQuery File Upload "add" call that PrimeFaces 13 to 15 are built on,
	// with fileInput given as PrimeFaces 14 requires. A change event is not used, as WebKit then reports
	// the input's original file system entries, which no longer exist.
	function uploadFile(uploadedFile) {
		var widget = PrimeFaces.getWidgetById("upload");
		var input = document.getElementById("upload_input");
		if ((! widget) || (! input) || (typeof widget.jq.fileupload !== "function")) {
			uploadFileDirectly(uploadedFile);
			return;
		}

		widget.clear(); // drop the original photo so the 1 file limit counts only this one
		// clear() fades the original photo's row out; finish that now so it never shows next to this one
		widget.jq.find(".ui-fileupload-row").stop(true, true);
		if (typeof DataTransfer !== "undefined") { // PrimeFaces 14 validates the input's files too
			var transfer = new DataTransfer();
			transfer.items.add(uploadedFile);
			input.files = transfer.files;
		}
		widget.jq.fileupload("add", {files: [uploadedFile], fileInput: $(input)});

		// Back to the standard upload panel, which shows the file and its progress
		setPreparing(false);
		$(document.body).removeClass("skyve-image-editing");
		$("#cropperUpload").css({display: "none"});
		$("#cropperContainer").css({display: "none"});
		$("#upload").css({display: ""});

		// Upload once the widget has added the file (it is not added if it fails validation)
		var tries = 0;
		(function uploadWhenAdded() {
			if (widget.files.length > 0) {
				widget.upload();
			}
			else if (++tries < 20) {
				setTimeout(uploadWhenAdded, 50);
			}
		})();
	}

	// If the PrimeFaces file upload widget can't be used: post the photo the way it would
	function uploadFileDirectly(uploadedFile) {
		var formData = new FormData(document.getElementById("form"));
		formData.append("jakarta.faces.partial.ajax", "true");
		formData.append("jakarta.faces.partial.execute", "upload");
		formData.append("jakarta.faces.source", "upload");
		formData.append("jakarta.faces.partial.render", "messages");
		formData.append("upload", uploadedFile, uploadedFile.name);

		$.ajax({
			type: "POST",
			url: window.location.href,
			data: formData,
			processData: false,
			contentType: false
		}).done(function(data, status, xhr) {
			PrimeFaces.ajax.Response.handle(data, status, xhr);
		}).always(function() {
			setPreparing(false);
		});
	}

	window.cropAndUpload = async function() {
		if ((cropper == null) || (! cropperReady)) {
			return;
		}
		if (textEntryOpen) {
			window.commitText();
		}
		activeStroke = null;
		activePointerId = null;
		textDrag = null;
		setPreparing(true);
		// Let the browser paint the note before the encoding work starts
		await new Promise(function(resolve) { setTimeout(resolve, 0); });
		try {
			if ((! cameraMode) && editorUnchanged()) {
				$("#_mk").val("");
				uploadFile(file);
				return;
			}
			var croppedCanvas = cropper.getCroppedCanvas({
				imageSmoothingEnabled: true,
				imageSmoothingQuality: "high"
			});
			var blob = await encodeCanvasForUpload(croppedCanvas);
			if (blob.size > window.skyveMaximumUploadSizeInBytes) {
				setPreparing(false);
				alert("The cropped image is still too large to upload. Maximum size is " +
						formatBytes(window.skyveMaximumUploadSizeInBytes) + ". Encoded size is " + formatBytes(blob.size) + ".");
				return;
			}
			$("#_mk").val(markupSvg(encodedWidth, encodedHeight));
			uploadFile(new File([blob], fileName, {type: blob.type}));
		}
		catch (e) {
			setPreparing(false);
			alert(e.message || "Could not process cropped image");
		}
	};

	window.editImage = function() {
		fileFormat = "png";
		if (file.type === "image/jpeg") {
			fileFormat = "jpeg";
		}
		fileName = croppedFileName(fileFormat);
		resetDrawing();
		$(document.body).addClass("skyve-image-editing");
		$("#cropperUpload").css({display: "block"});
		$("#cropperContainer").css({display: "flex"});
		var reader = new FileReader();
		reader.onload = function(evt) {
			initialiseCropper(evt.target.result, null);
		};
		reader.readAsDataURL(file);
		$("#upload").css({display: "none"});
	};

	function chooseImage() {
		if (suppressChooseClick) {
			suppressChooseClick = false;
			return;
		}
		cameraMode = false;
		autoEditAfterFileSelection = false;
		$("#_af").val("generic");
		$("#upload_input").removeAttr("capture");
	}

	window.captureImage = function() {
		cameraMode = true;
		autoEditAfterFileSelection = true;
		suppressChooseClick = true;
		$("#_af").val("camera");
		$("#upload_input").attr("capture", "environment");
		$("#upload_input").trigger("click");
		setTimeout(function() {
			suppressChooseClick = false;
		}, 0);
	};

	// Rotate, then reshape the cropper to the rotated photo and crop to all of it
	window.rotateImage = function(degrees) {
		if ((cropper == null) || (! cropperReady)) {
			return;
		}
		cropper.rotate(degrees);
		var geometry = photoGeometry();
		sizeCropperImage(document.getElementById("cropperImage"), geometry.boxWidth / geometry.boxHeight);
		cropper.resize();
		fitCanvas();
		cropper.setCropBoxData(cropper.getCanvasData());
		requestRender();
	};

	function clearSelectedImage() {
		file = null;
		cameraMode = false;
		autoEditAfterFileSelection = false;
		$("#_af").val("generic");
		$("#_mk").val("");
		$("#upload_input").removeAttr("capture");
		$("#camera").show();
	}

	// Raster images chosen for an image field open straight in the editor
	function editable(selectedFile) {
		return (document.getElementById("cropperContainer").getAttribute("data-image-upload") === "true") &&
				(selectedFile.type.indexOf("image/") === 0) &&
				(selectedFile.type !== "image/svg+xml");
	}

	$(document).ready(function() {
		if (! document.getElementById("cropperContainer")) {
			return;
		}
		$("#camera").appendTo($("#upload .ui-fileupload-buttonbar")).show();
		$("#upload .ui-fileupload-choose").on("click", chooseImage);

		$("#upload_input").on("change", function() {
			if (this.files && this.files[0]) {
				file = this.files[0];
				$("#_mk").val("");
				if (autoEditAfterFileSelection || editable(file)) {
					autoEditAfterFileSelection = false;
					window.editImage();
				}
				else {
					$("#camera").hide();
				}

				setTimeout(function() {
					$(".ui-fileupload-cancel").on("click", function() {
						clearSelectedImage();
					});
				}, 500);
			}
		});

		var ink = document.getElementById("scrawlInk");
		if (ink) {
			ink.addEventListener("pointerdown", onPointerDown);
			ink.addEventListener("pointermove", onPointerMove);
			ink.addEventListener("pointerup", onPointerUp);
			ink.addEventListener("pointercancel", onPointerUp);
		}
		// In text mode a colour also applies to the selected label, or the one being typed
		$(".scrawl-colour").on("click", function() {
			penColour = this.getAttribute("data-colour");
			$(".scrawl-colour").attr("aria-checked", "false");
			this.setAttribute("aria-checked", "true");
			if ((editorMode === "text") && (selectedText != null)) {
				selectedText.colour = penColour;
				requestRender();
			}
			updateCursor();
		});

		// Pressing anything else places the label being typed as the box loses focus - a colour then applies to it.
		// The cross is pressed before that happens, so it notes that the label is to be discarded instead.
		$("#scrawlTextCancel").on("pointerdown mousedown", function() {
			if (textEntryOpen) {
				cancelRequested = true;
			}
		});

		// On touch screens PrimeFaces marks a tapped button hovered and nothing unmarks it, which can look like the selected mode
		if (window.matchMedia("(hover: none)").matches) {
			$("#cropperBar, .cropperControls, #scrawlTextEntry").on("click", ".ui-button", function() {
				var button = $(this);
				setTimeout(function() {
					button.removeClass("ui-state-hover ui-state-focus ui-state-active");
				}, 0);
			});
		}

		var textInput = document.getElementById("scrawlText");
		if (textInput) {
			// Enter (Done on a phone's keyboard) or leaving the box places the label; Escape abandons it
			textInput.addEventListener("keydown", function(event) {
				if (event.key === "Enter") {
					event.preventDefault();
					window.commitText();
				}
				else if (event.key === "Escape") {
					event.preventDefault();
					window.cancelText();
				}
			});
			textInput.addEventListener("blur", function() {
				if (cancelRequested) {
					window.cancelText();
				}
				else {
					window.commitText();
				}
			});
			textInput.addEventListener("input", function() {
				if (typingItem != null) {
					typingItem.text = textInput.value.replace(/\s+/g, " ");
					requestRender();
				}
			});
		}
		showEditorMode("crop");
		updateUndo();

		// Ctrl+Z or Cmd+Z undoes the last stroke while the editor is open
		$(document).on("keydown", function(event) {
			if ((event.ctrlKey || event.metaKey) && (! event.shiftKey) && (! event.altKey) && (! textEntryOpen) &&
					((event.key === "z") || (event.key === "Z")) &&
					($("#cropperContainer").css("display") !== "none")) {
				event.preventDefault();
				undo();
			}
		});

		// Rebuild the cropper for the new size, keeping the crop and rotation
		$(window).on("resize", function() {
			// A phone's keyboard coming up for typing a label is not a reason to rebuild
			if ((currentImageUrl == null) || ($("#cropperContainer").css("display") === "none") || textEntryOpen) {
				return;
			}
			clearTimeout(resizeTimer);
			resizeTimer = setTimeout(function() {
				var restore = null;
				if ((cropper != null) && cropperReady) {
					var geometry = photoGeometry();
					var data = geometry.data;
					restore = {
						aspect: geometry.boxWidth / geometry.boxHeight,
						transform: {rotate: data.rotate, scaleX: data.scaleX, scaleY: data.scaleY},
						crop: {x: data.x, y: data.y, width: data.width, height: data.height}
					};
				}
				initialiseCropper(currentImageUrl, restore);
			}, 200);
		});
	});
}());
