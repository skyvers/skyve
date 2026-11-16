(function() {
	var rootElement = document.getElementById('excalidraw-root');
	if (!rootElement) {
		return;
	}

	var imageWidth = Number(rootElement.getAttribute('data-image-width'));
	var imageHeight = Number(rootElement.getAttribute('data-image-height'));
	var backgroundUrl = rootElement.getAttribute('data-background-url');
	var forcedDark = rootElement.getAttribute('data-forced-dark') === 'true';
	var smartClient = rootElement.getAttribute('data-smart-client') === 'true';
	var darkModePreference = window.matchMedia('(prefers-color-scheme: dark)');
	var excalidrawAPI = null;

	function excalidrawTheme() {
		return ((! smartClient) && (forcedDark || darkModePreference.matches)) ? 'dark' : 'light';
	}

	// Fetch the background image and return its data URL and mime type.
	async function fetchBackgroundImage() {
		var response = await fetch(backgroundUrl, {credentials: 'same-origin'});
		if (!response.ok) {
			throw new Error('Background image fetch failed: ' + response.status);
		}
		var blob = await response.blob();
		return new Promise(function(resolve, reject) {
			var reader = new FileReader();
			reader.onloadend = function() {
				resolve({dataUrl: reader.result, mimeType: blob.type || 'image/jpeg'});
			};
			reader.onerror = reject;
			reader.readAsDataURL(blob);
		});
	}

	// Build a zero-opacity locked rectangle at (0,0,imageWidth,imageHeight).
	// This anchors the SVG export viewBox to the exact image dimensions so that
	// burnSvg() composites annotations onto the original photo at native resolution.
	function createBoundaryRect() {
		return {
			id: 'image-boundary-rect', type: 'rectangle',
			x: 0, y: 0, width: imageWidth, height: imageHeight,
			angle: 0, strokeColor: 'transparent', backgroundColor: 'transparent',
			fillStyle: 'solid', strokeWidth: 0, strokeStyle: 'solid',
			roughness: 0, opacity: 0, groupIds: [], frameId: null,
			roundness: null, seed: 1, version: 1, versionNonce: 1,
			isDeleted: false, boundElements: null, updated: 1, link: null, locked: true,
		};
	}

	var EXCALIDRAW_PAYLOAD = 'payload-type:application/vnd.excalidraw+json';
	var SVG_NAMESPACE = 'http://www.w3.org/2000/svg';
	// Excalidraw draws a freehand line with constant pressure about 6 times its stroke width wide:
	// perfect-freehand size 4.25 x strokeWidth, and a radius of sin(pi/4) x size either side.
	var FREEDRAW_WIDTH_RATIO = 4.25 * 2 * Math.SQRT1_2;
	var NUMBER = '-?(?:\\d+\\.?\\d*|\\.\\d+)(?:[eE][-+]?\\d+)?';
	var PATH_TOKEN = new RegExp('[A-Za-z]|' + NUMBER, 'g');
	var PATH_SEPARATORS = new RegExp('[A-Za-z]|' + NUMBER + '|[\\s,]', 'g');

	function randomInteger() {
		return Math.floor(Math.random() * 2147483647);
	}

	// The points of a path that is a single line: "M x y" then "L x y" (or bare pairs) in absolute coordinates.
	// Returns null for anything else (curves, relative commands, several sub-paths).
	function linePoints(d) {
		if ((! d) || (d.replace(PATH_SEPARATORS, '').length > 0)) {
			return null;
		}
		var tokens = d.match(PATH_TOKEN);
		if ((! tokens) || (tokens[0] !== 'M')) {
			return null;
		}
		var points = [];
		var i = 1;
		while (i < tokens.length) {
			if (tokens[i] === 'L') {
				i++;
				continue;
			}
			var x = Number(tokens[i]);
			var y = Number(tokens[i + 1]);
			if (! (isFinite(x) && isFinite(y))) {
				return null; // another command, or an odd number of coordinates
			}
			points.push([x, y]);
			i += 2;
		}
		return (points.length > 0) ? points : null;
	}

	// An Excalidraw freehand element through the points given in scene coordinates
	function freedrawElement(points, colour, lineWidth) {
		var originX = points[0][0];
		var originY = points[0][1];
		var relative = points.map(function(point) {
			return [point[0] - originX, point[1] - originY];
		});
		var xs = relative.map(function(point) { return point[0]; });
		var ys = relative.map(function(point) { return point[1]; });
		return {
			id: 'skyve-line-' + randomInteger(), type: 'freedraw',
			x: originX, y: originY,
			width: Math.max.apply(null, xs) - Math.min.apply(null, xs),
			height: Math.max.apply(null, ys) - Math.min.apply(null, ys),
			angle: 0, strokeColor: colour, backgroundColor: 'transparent',
			fillStyle: 'solid', strokeWidth: lineWidth / FREEDRAW_WIDTH_RATIO, strokeStyle: 'solid',
			roughness: 0, opacity: 100, groupIds: [], frameId: null,
			roundness: null, seed: randomInteger(), version: 1, versionNonce: randomInteger(),
			isDeleted: false, boundElements: null, updated: Date.now(), link: null, locked: false,
			points: relative,
			pressures: relative.map(function() { return 0.5; }),
			simulatePressure: false,
			lastCommittedPoint: relative[relative.length - 1],
		};
	}

	// Excalidraw elements for a label written by the upload page's editor (data-skyve-text),
	// which is centred on x and sits TEXT_BASELINE font sizes below its centre, optionally rotated about its centre.
	// When the text is on a box (data-skyve-label) it becomes the box's bound text, which Excalidraw centres the same way,
	// with a line for each tspan of a long label.
	// Returns null for any other text. The mapping is {originX, originY, scaleX, scaleY} from the SVG onto the image.
	var TEXT_BASELINE = 0.35;
	var BOUND_TEXT_PADDING = 5; // Excalidraw's space between a box and its text
	var ROTATE_ABOUT_POINT = /^\s*rotate\(\s*(-?[\d.]+)(?:[\s,]+-?[\d.]+[\s,]+-?[\d.]+)?\s*\)\s*$/;
	function labelElements(element, box, mapping) {
		var text = labelText(element);
		var fontSize = parseFloat(element.getAttribute('font-size'));
		var transform = element.getAttribute('transform');
		var rotation = transform ? ROTATE_ABOUT_POINT.exec(transform) : null;
		if ((! text) || (! (fontSize > 0)) || (transform && (! rotation)) || ((! box) && (text.indexOf('\n') >= 0))) {
			return null;
		}
		fontSize *= mapping.scaleY;
		var angle = rotation ? (parseFloat(rotation[1]) * Math.PI / 180) : 0;
		if (box) {
			return tagElements(text, fontSize, element.getAttribute('fill'), box, angle, mapping);
		}
		var centreX = (parseFloat(element.getAttribute('x')) - mapping.originX) * mapping.scaleX;
		var centreY = ((parseFloat(element.getAttribute('y')) - mapping.originY) * mapping.scaleY) - (fontSize * TEXT_BASELINE);
		var label = ExcalidrawLib.convertToExcalidrawElements([{
			type: 'text', x: centreX, y: centreY, text: text, fontSize: fontSize,
			fontFamily: 2, // Helvetica, as the upload page draws it
			strokeColor: element.getAttribute('fill') || '#1e1e1e',
		}])[0];
		// Put Excalidraw's baseline where the SVG's is. Excalidraw rotates about the middle of its text box, not the label's
		// centre, so shift the box by the difference that makes once rotated.
		var top = centreY + (fontSize * TEXT_BASELINE) - (label.baseline || (label.height * 0.8));
		var offset = (top + (label.height / 2)) - centreY;
		return [Object.assign({}, label, {
			x: centreX - (label.width / 2) - (offset * Math.sin(angle)),
			y: top + (offset * Math.cos(angle)) - offset,
			angle: angle,
		})];
	}

	// A label's text: its content, or its tspans' contents as lines. Null for anything else, such as nested elements.
	function labelText(element) {
		var children = element.children;
		if (children.length === 0) {
			return (element.textContent || '').trim();
		}
		var lines = [];
		for (var i = 0, l = children.length; i < l; i++) {
			if ((children[i].localName !== 'tspan') || (children[i].children.length > 0)) {
				return null;
			}
			lines.push((children[i].textContent || '').trim());
		}
		return lines.join('\n');
	}

	// A rounded box with the label's text bound inside, where the SVG's box is.
	// Corners rounded by a quarter of the shorter side are Excalidraw's proportional roundness.
	function tagElements(text, fontSize, textColour, box, angle, mapping) {
		var boxX = parseFloat(box.getAttribute('x')) || 0;
		var boxY = parseFloat(box.getAttribute('y')) || 0;
		var boxWidth = parseFloat(box.getAttribute('width'));
		var boxHeight = parseFloat(box.getAttribute('height'));
		var centreX = (boxX + (boxWidth / 2) - mapping.originX) * mapping.scaleX;
		var centreY = (boxY + (boxHeight / 2) - mapping.originY) * mapping.scaleY;
		// Excalidraw wraps text that does not fit, so make sure it does in Excalidraw's own measure
		var measured = ExcalidrawLib.convertToExcalidrawElements([{type: 'text', x: 0, y: 0, text: text, fontSize: fontSize, fontFamily: 2}])[0];
		var width = Math.max(boxWidth * mapping.scaleX, measured.width + (2 * BOUND_TEXT_PADDING) + 1);
		var height = Math.max(boxHeight * mapping.scaleY, measured.height + (2 * BOUND_TEXT_PADDING));
		// The box can be see-through while its text stays solid
		var boxOpacity = parseFloat(box.getAttribute('fill-opacity'));
		boxOpacity = (boxOpacity >= 0) && (boxOpacity <= 1) ? Math.round(boxOpacity * 100) : 100;
		var elements = ExcalidrawLib.convertToExcalidrawElements([{
			type: 'rectangle', x: centreX - (width / 2), y: centreY - (height / 2), width: width, height: height,
			strokeColor: 'transparent', backgroundColor: box.getAttribute('fill') || '#1e1e1e',
			fillStyle: 'solid', strokeWidth: 1, roughness: 0, roundness: {type: 2},
			label: {text: text, fontSize: fontSize, fontFamily: 2, strokeColor: textColour || '#ffffff'},
		}]);
		return elements.map(function(element) {
			return Object.assign({}, element, {angle: angle, opacity: (element.type === 'text') ? 100 : boxOpacity});
		});
	}

	// The box under a label from the upload page's editor: a plain rectangle immediately before the label's text
	function labelBox(element) {
		var next = element.nextElementSibling;
		return ((element.localName === 'rect') &&
				(element.getAttribute('data-skyve-label') === 'true') &&
				next && (next.localName === 'text') && (next.getAttribute('data-skyve-text') === 'true') &&
				(parseFloat(element.getAttribute('width')) > 0) &&
				(parseFloat(element.getAttribute('height')) > 0) &&
				((element.getAttribute('transform') || null) === (next.getAttribute('transform') || null))) ? element : null;
	}

	// Brings standard SVG markup (without an Excalidraw scene) into the editor.
	// Simple stroked lines and labels, such as those added when the image was uploaded, become editable lines and text.
	// Everything else is kept, unchanged, as one image over the photo that can be moved or deleted.
	function importStandardSvg(svgText) {
		var result = {elements: [], files: []};
		var svgDocument = new DOMParser().parseFromString(svgText, 'image/svg+xml');
		var root = svgDocument.documentElement;
		if ((! root) || (root.namespaceURI !== SVG_NAMESPACE) || (root.localName !== 'svg')) {
			return result;
		}

		// Map the SVG's coordinates onto the image
		var originX = 0;
		var originY = 0;
		var width = parseFloat(root.getAttribute('width')) || imageWidth;
		var height = parseFloat(root.getAttribute('height')) || imageHeight;
		var viewBox = (root.getAttribute('viewBox') || '').trim().split(/[\s,]+/).map(Number);
		if ((viewBox.length === 4) && (viewBox[2] > 0) && (viewBox[3] > 0)) {
			originX = viewBox[0];
			originY = viewBox[1];
			width = viewBox[2];
			height = viewBox[3];
		}
		var scaleX = imageWidth / width;
		var scaleY = imageHeight / height;

		var lines = [];
		var others = [];
		var drawing = false;
		var boxes = [];
		Array.prototype.forEach.call(root.children, function(element) {
			// Labels from the upload page's editor become editable text, in an editable box when they have one
			if ((element.localName === 'text') && (element.getAttribute('data-skyve-text') === 'true')) {
				var box = element.previousElementSibling;
				box = (box && (boxes.indexOf(box) >= 0)) ? box : null;
				var label = labelElements(element, box, {originX: originX, originY: originY, scaleX: scaleX, scaleY: scaleY});
				if (label) {
					lines = lines.concat(label);
					return;
				}
				if (box) {
					others.push(box);
					drawing = true;
				}
			}
			if (labelBox(element)) {
				boxes.push(element); // drawn with its text
				return;
			}
			var points = (element.localName === 'path') ? linePoints(element.getAttribute('d')) : null;
			var stroke = element.getAttribute('stroke');
			if (points &&
					(element.getAttribute('fill') === 'none') &&
					stroke && (stroke !== 'none') &&
					(! element.hasAttribute('transform')) &&
					(! element.hasAttribute('style'))) {
				var lineWidth = (parseFloat(element.getAttribute('stroke-width')) || 1) * scaleX;
				lines.push(freedrawElement(points.map(function(point) {
					return [(point[0] - originX) * scaleX, (point[1] - originY) * scaleY];
				}), stroke, lineWidth));
			}
			else if (element.localName !== 'metadata') {
				others.push(element);
				if (['defs', 'title', 'desc', 'style'].indexOf(element.localName) < 0) {
					drawing = true;
				}
			}
		});

		if (drawing) {
			var holder = root.cloneNode(false);
			others.forEach(function(element) {
				holder.appendChild(element.cloneNode(true));
			});
			var svg = new XMLSerializer().serializeToString(holder);
			var fileId = 'skyve-markup-' + randomInteger();
			result.files.push({
				id: fileId,
				dataURL: 'data:image/svg+xml;base64,' + btoa(unescape(encodeURIComponent(svg))),
				mimeType: 'image/svg+xml',
				created: Date.now(),
				lastRetrieved: Date.now(),
			});
			result.elements.push({
				id: fileId + '-element', type: 'image',
				x: 0, y: 0, width: imageWidth, height: imageHeight,
				angle: 0, strokeColor: 'transparent', backgroundColor: 'transparent',
				fillStyle: 'solid', strokeWidth: 0, strokeStyle: 'solid',
				roughness: 0, opacity: 100, groupIds: [], frameId: null,
				roundness: null, seed: randomInteger(), version: 1, versionNonce: randomInteger(),
				isDeleted: false, boundElements: null, updated: Date.now(), link: null,
				locked: false, fileId: fileId, status: 'saved', scale: [1, 1],
			});
		}
		result.elements = result.elements.concat(lines);
		return result;
	}

	// Called by Excalidraw when the imperative API is ready (excalidrawAPI prop callback).
	async function onExcalidrawReady(api) {
		excalidrawAPI = api;

		// Fetch the background image to add as a locked, non-exportable element.
		var bgData;
		try {
			bgData = await fetchBackgroundImage();
		} catch(e) {
			console.error('Could not load background image:', e);
			bgData = null;
		}

		// Restore existing annotations from the stored SVG, if any.
		// Excalidraw's own scene (embedded in a comment when it saved the markup) restores exactly;
		// any other standard SVG is imported.
		var annotationElements = [];
		var importedFiles = [];
		var existingSvg = document.getElementById('svg').value;
		if (existingSvg) {
			var restored = false;
			if (existingSvg.indexOf(EXCALIDRAW_PAYLOAD) >= 0) {
				try {
					var blob = new Blob([existingSvg], {type: 'image/svg+xml'});
					var sceneData = await ExcalidrawLib.loadFromBlob(blob, null, null);
					// Exclude bg and boundary rect -- both are re-created fresh below.
					annotationElements = (sceneData.elements || []).filter(function(element) {
						return element.id !== 'bg-image-element' && element.id !== 'image-boundary-rect';
					});
					importedFiles = Object.values(sceneData.files || {});
					restored = true;
				} catch(e) {
					console.warn('[skyve] Could not restore the Excalidraw scene from stored markup:', e);
				}
			}
			if (! restored) {
				try {
					var imported = importStandardSvg(existingSvg);
					annotationElements = ExcalidrawLib.restoreElements(imported.elements, null);
					importedFiles = imported.files;
				} catch(e) {
					console.warn('[skyve] Could not import stored markup:', e);
				}
			}
		}
		if (importedFiles.length > 0) {
			api.addFiles(importedFiles);
		}

		var elements = [createBoundaryRect()].concat(annotationElements);

		if (bgData) {
			// Add background image as a locked element positioned at scene origin.
			var bgFileId = 'bg-image';
			api.addFiles([{
				id: bgFileId,
				dataURL: bgData.dataUrl,
				mimeType: bgData.mimeType,
				created: 1,
				lastRetrieved: 1,
			}]);
			elements = [{
				id: 'bg-image-element', type: 'image',
				x: 0, y: 0, width: imageWidth, height: imageHeight,
				angle: 0, strokeColor: 'transparent', backgroundColor: 'transparent',
				fillStyle: 'solid', strokeWidth: 0, strokeStyle: 'solid',
				roughness: 0, opacity: 100, groupIds: [], frameId: null,
				roundness: null, seed: 2, version: 1, versionNonce: 2,
				isDeleted: false, boundElements: null, updated: 1, link: null,
				locked: true, fileId: bgFileId, status: 'saved', scale: [1, 1],
			}].concat(elements);
		}

		api.updateScene({
			elements: elements,
			appState: {viewBackgroundColor: 'transparent'},
		});
		api.scrollToContent(undefined, {fitToViewport: true});
	}

	// Exports annotation-only SVG (no background image), then triggers the server action.
	function applyMarkup() {
		if (!excalidrawAPI) {
			return;
		}

		var allElements = excalidrawAPI.getSceneElements();
		var exportElements = allElements.filter(function(element) {
			return element.id !== 'bg-image-element';
		});
		var exportFiles = Object.assign({}, excalidrawAPI.getFiles());
		delete exportFiles['bg-image'];

		// Excalidraw draws the elements relative to their bounds, so the image's origin is at minus the bounds' origin.
		// Lines drawn past the image's top or left edge put the bounds there, rather than at the boundary rectangle.
		var bounds = ExcalidrawLib.getCommonBounds(exportElements);
		ExcalidrawLib.exportToSvg({
			elements: exportElements,
			appState: {exportBackground: false, exportWithDarkMode: false, exportEmbedScene: true},
			files: exportFiles,
			exportPadding: 0,
		}).then(function(svgElement) {
			svgElement.setAttribute('width', imageWidth);
			svgElement.setAttribute('height', imageHeight);
			svgElement.setAttribute('viewBox', (-bounds[0]) + ' ' + (-bounds[1]) + ' ' + imageWidth + ' ' + imageHeight);
			document.getElementById('svg').value = new XMLSerializer().serializeToString(svgElement);
			document.getElementById('applySvgAction').click();
		});
	}

	var form = document.getElementById('form');
	form.addEventListener('submit', function(event) {
		event.preventDefault();
	});
	document.getElementById('markupApply').addEventListener('click', applyMarkup);

	var reactRoot = ReactDOM.createRoot(rootElement);
	reactRoot.render(React.createElement(ExcalidrawLib.Excalidraw, {
		excalidrawAPI: onExcalidrawReady,
		initialData: {
			appState: {
				viewBackgroundColor: 'transparent',
				theme: excalidrawTheme(),
				exportBackground: false,
				currentItemOpacity: 80,
			},
		},
	}));
}());
