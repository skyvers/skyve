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

		// Restore existing annotations from stored Excalidraw SVG, if any.
		var annotationElements = [];
		var existingSvg = document.getElementById('svg').value;
		if (existingSvg) {
			try {
				var blob = new Blob([existingSvg], {type: 'image/svg+xml'});
				var sceneData = await ExcalidrawLib.loadFromBlob(blob, null, null);
				// Exclude bg and boundary rect -- both are re-created fresh below.
				annotationElements = (sceneData.elements || []).filter(function(element) {
					return element.id !== 'bg-image-element' && element.id !== 'image-boundary-rect';
				});
			} catch(e) {
				// Not an Excalidraw SVG (e.g. legacy SVG-Edit markup) -- start fresh.
				console.warn('[skyve] Could not restore annotations from stored markup:', e);
			}
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

		ExcalidrawLib.exportToSvg({
			elements: exportElements,
			appState: {exportBackground: false, exportWithDarkMode: false, exportEmbedScene: true},
			files: exportFiles,
		}).then(function(svgElement) {
			svgElement.setAttribute('width', imageWidth);
			svgElement.setAttribute('height', imageHeight);
			svgElement.setAttribute('viewBox', '0 0 ' + imageWidth + ' ' + imageHeight);
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
