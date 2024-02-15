// Ever decreasing (in size) stack of reuseable modal windows for the application
isc.ClassFactory.defineClass("WindowStack");
isc.WindowStack.addClassProperties({
	// the amount to size the window in by
	_margin: 0,
	
	// stack of windows showing
	_stack: [],
	
	// unused pool of windows that can be used
	_unused: [],
	
	popup: function(fromRect, // [left, top, width, height]
						title, // the window title
						showCloseButton, // show to close button in the window title bar
						items, // the items to show in the window
						height, // height of window - can specify without width
						width, // width of window - can specify null as height to only set width
						closeButtonRefreshes) { // true to refresh opener on close button, or false otherwise
		isc.WindowStack._margin += 50;
		var result = isc.WindowStack._unused.pop();

		var sizeSet = width || height;
		var windowWidth = width ? width : (isc.Page.getWidth() - isc.WindowStack._margin);
		var windowHeight = height ? height : (isc.Page.getHeight() - isc.WindowStack._margin);
		if (result) {
			result.setTitle(title);
			result.sizeSet = sizeSet;
			if (items) {
				result.addItems(items);
			}
			result.setWidth(windowWidth);
			result.setHeight(windowHeight);
			result.closeClick = function() {
				isc.WindowStack.popoff(closeButtonRefreshes ? true : false);
			}
		}
		else {
			result = isc.Window.create({
				headerIconDefaults: {src: "../images/window/skyve_fav.png", width: 16, height: 16},
				autoCenter: true,
				isModal: true,
				showModalMask: true,
				showMinimizeButton: false,
				modalMaskOpacity: 25,
				canDragReposition: true,
				canDragResize: true,
				showShadow: true,
				shadowSoftness: 10,
				shadowOffset: 0,
				title: title,
				items: items,
				width: windowWidth,
				height: windowHeight,
				sizeSet: sizeSet,
				closeClick: function() {
					isc.WindowStack.popoff(closeButtonRefreshes ? true : false);
				}
			});
		}

		result.setShowCloseButton(showCloseButton);
		result._fromRect = fromRect; // ? fromRect : [isc.Page.getWidth() / 2 - 5, isc.Page.getHeight() / 2 - 5, 10, 10];
		isc.WindowStack._stack.push(result);
		result.show();
//		isc.WindowStack._animateOpen(result, [(isc.Page.getWidth() - windowWidth) / 2, (isc.Page.getHeight() - windowHeight) / 2, windowWidth, windowHeight]);
	},
	
	popoff: function(rerenderOpener) { // whether to call rerender() on the opener view or no
		var opener = isc.WindowStack.getOpener();

		var result = isc.WindowStack._stack.pop();
		if (result) {
			isc.WindowStack._animateClose(result);
			var items = result.items;
			if (items) {
				if (CKEDITOR && CKEDITOR.instances._CKEditor) {
					CKEDITOR.instances._CKEditor.destroy();
					items[0].destroy();
				}
				else {
					for (var i = 0, l = items.length; i < l; i++) {
						var item = items[i];
						if (item._vm) { // has a values manager - must be an edit view
							isc.BizUtil.relinquishEditView(item);
						}
						else if (isc.isA.BizListGrid(item)) { // pickList
							isc.BizUtil.relinquishPickList(item);
						}
					}
				}
				result.removeItems(items);
			}
	
			isc.WindowStack._unused.push(result);
			isc.WindowStack._margin -= 50;
			
			if (opener) {
				if (opener.resume) {
					opener.resume();
				}
				if (rerenderOpener && opener.rerender) {
					opener.rerender();
				}
			}
		}
	},
	
	// get the view in the window that opened the current window
	getOpener: function() {
		if (isc.WindowStack._stack.length <= 1) { // opener is harness current view
			return isc.BizUtil.getCurrentView();
		}
		return isc.WindowStack._stack[isc.WindowStack._stack.length - 2].items[0];
	},
	
	// called by the browser resize handler - from util2.js
	resize: function() {
		var windowWidth = isc.Page.getWidth() - 50;
		var windowHeight = isc.Page.getHeight() - 50;
		for (var i = 0; i < isc.WindowStack._stack.length; i++) {
			var window = isc.WindowStack._stack[i];
			if (window.sizeSet) {} else {
				window.setWidth(windowWidth);
				window.setHeight(windowHeight);
				windowWidth -= 50;
				windowHeight -=50;
			}
		}			
	},
	
	// wireframe animation stuff
	
	_wireframe: isc.Canvas.create({
		border: "3px ridge #E1E1E1", 
		backgroundColor: "whitesmoke",
		opacity: 75,
		autoDraw: false
	}),

//	_animateOpenDuration: 300,
	_animateClosedDuration: 400,
/*
	_animateOpen: function (window, toRect) {
		if (window._fromRect) {
			// initialize the wireframe at fromRect
		    isc.WindowStack._wireframe.setRect(window._fromRect);
		    isc.WindowStack._wireframe.show();
		    isc.WindowStack._wireframe.bringToFront();
		    // animate the wireframe to the final position/size of the window
		    isc.WindowStack._wireframe.animateRect(
	    		toRect[0], toRect[1], toRect[2], toRect[3],
		        // then hide wireframe and show window
		        function () {
		        	isc.WindowStack._wireframe.hide(); 
		            window.show();
		        },
		        isc.WindowStack._animateOpenDuration
		    );
		}
		else {
            window.show();
		}
	},
*/	
	_animateClose: function (window) {
		if (window._fromRect) {
			var toLeft, toTop, toWidth, toHeight;
		    toLeft = window.getLeft();
	        toTop = window.getTop();
	        toWidth = window.getVisibleWidth();
	        toHeight = window.getVisibleHeight();

	        // initialize the wireframe to the current window rect
			isc.WindowStack._wireframe.setRect([toLeft, toTop, toWidth, toHeight]);
			isc.WindowStack._wireframe.show();
			isc.WindowStack._wireframe.bringToFront();
		}
		
		window.hide();
		
		if (window._fromRect) {
			// animate the wireframe to the specified rect
			isc.WindowStack._wireframe.animateRect(
				window._fromRect[0], window._fromRect[1], window._fromRect[2], window._fromRect[3],
		        "isc.WindowStack._wireframe.hide();",
		        isc.WindowStack._animateClosedDuration
		    );
		}
	}
});
