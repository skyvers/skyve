// Ever decreasing (in size) stack of reuseable modal windows for the application
ClassFactory.defineClass("WindowStack");
WindowStack.addClassProperties({
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
						width) { // width of window - can specify null as height to only set width
		WindowStack._margin += 50;
		var result = WindowStack._unused.pop();

		var sizeSet = width || height;
		var windowWidth = width? width : (Page.getWidth() - WindowStack._margin);
		var windowHeight = height? height : (Page.getHeight() - WindowStack._margin);
		if (result) {
			result.setTitle(title);
			result.sizeSet = sizeSet;
			if (items) {
				result.addItems(items);
			}
			result.setWidth(windowWidth);
			result.setHeight(windowHeight);
		}
		else {
			result = isc.Window.create({
				headerIconDefaults: {src: "../images/window/BizHub16.png", width: 16, height: 16},
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
					WindowStack.popoff(false); // dont rerender the opener view
				}
			});
		}

		result.setShowCloseButton(showCloseButton);
		result._fromRect = fromRect; // ? fromRect : [Page.getWidth() / 2 - 5, Page.getHeight() / 2 - 5, 10, 10];
		WindowStack._stack.push(result);
		result.show();
//		WindowStack._animateOpen(result, [(Page.getWidth() - windowWidth) / 2, (Page.getHeight() - windowHeight) / 2, windowWidth, windowHeight]);
	},
	
	popoff: function(rerenderOpener) { // whether to call rerender() on the opener view or no
		var opener = WindowStack.getOpener();

		var result = WindowStack._stack.pop();
		if (result) {
			WindowStack._animateClose(result);
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
							BizUtil.relinquishEditView(item);
						}
						else if (isA.BizListGrid(item)) { // pickList
							BizUtil.relinquishPickList(item);
						}
					}
				}
				result.removeItems(items);
			}
	
			WindowStack._unused.push(result);
			WindowStack._margin -= 50;
			
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
		if (WindowStack._stack.length <= 1) { // opener is harness current view
			return BizUtil.getCurrentView();
		}
		return WindowStack._stack[WindowStack._stack.length - 2].items[0];
	},
	
	// called by the browser resize handler - from util2.js
	resize: function() {
		var windowWidth = Page.getWidth() - 50;
		var windowHeight = Page.getHeight() - 50;
		for (var i = 0; i < WindowStack._stack.length; i++) {
			var window = WindowStack._stack[i];
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
		    WindowStack._wireframe.setRect(window._fromRect);
		    WindowStack._wireframe.show();
		    WindowStack._wireframe.bringToFront();
		    // animate the wireframe to the final position/size of the window
		    WindowStack._wireframe.animateRect(
	    		toRect[0], toRect[1], toRect[2], toRect[3],
		        // then hide wireframe and show window
		        function () {
		        	WindowStack._wireframe.hide(); 
		            window.show();
		        },
		        WindowStack._animateOpenDuration
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
			WindowStack._wireframe.setRect([toLeft, toTop, toWidth, toHeight]);
			WindowStack._wireframe.show();
			WindowStack._wireframe.bringToFront();
		}
		
		window.hide();
		
		if (window._fromRect) {
			// animate the wireframe to the specified rect
			WindowStack._wireframe.animateRect(
				window._fromRect[0], window._fromRect[1], window._fromRect[2], window._fromRect[3],
		        "WindowStack._wireframe.hide();",
		        WindowStack._animateClosedDuration
		    );
		}
	}
});
