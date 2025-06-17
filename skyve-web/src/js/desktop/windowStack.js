/**
 * Implements the WindowStack UI component.
 */
isc.ClassFactory.defineClass("WindowStack");

isc.WindowStack.addClassProperties({
	_margin: 0, // The amount to size the window in by
	_stack: [], // Stack of windows showing
	_unused: [], // Unused pool of windows that can be used
	_index: 0, // Window number

	/**
	 * Displays a popup window.
	 * @param {Array} fromRect - the rectangle [left, top, width, height] from which the window animates.
	 * @param {string} title - the window title.
	 * @param {boolean} showCloseButton - whether to show the close button in the window title bar.
	 * @param {Array} items - the items to show in the window.
	 * @param {number} [height] - the height of the window. Can specify without width.
	 * @param {number} [width] - the width of the window. Can specify null as height to only set width.
	 * @param {boolean} [closeButtonRefreshes] - whether to refresh the opener on close button click.
	 */
	popup: function (
		fromRect,
		title,
		showCloseButton,
		items,
		height,
		width,
		closeButtonRefreshes,
	) {
		isc.WindowStack._margin += 50;
		let result = isc.WindowStack._unused.pop();

		const sizeSet = width || height;
		const windowWidth = width || isc.Page.getWidth() - isc.WindowStack._margin;
		const windowHeight =
			height || isc.Page.getHeight() - isc.WindowStack._margin;

		if (result) {
			result.setTitle(title);
			result.sizeSet = sizeSet;
			if (items) {
				result.addItems(items);
			}
			result.setWidth(windowWidth);
			result.setHeight(windowHeight);
			result.closeClick = () => {
				isc.WindowStack.popoff(closeButtonRefreshes);
			};
		} else {
			result = isc.Window.create({
				ID: 'Window' + isc.WindowStack._index++,
				headerIconDefaults: {
					src: "window/skyve_fav.png",
					width: 16,
					height: 16,
				},
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
				title,
				items,
				width: windowWidth,
				height: windowHeight,
				sizeSet,
				closeClick: function () {
					isc.WindowStack.popoff(closeButtonRefreshes);
				},
			});
		}

		result.setShowCloseButton(showCloseButton);
		result._fromRect = fromRect; // Store the fromRect for animation
		isc.WindowStack._stack.push(result);
		result.show();
	},

	/**
	 * Closes the topmost window in the stack.
	 * @param {boolean} [rerenderOpener] - whether to call rerender() on the opener view.
	 */
	popoff: function (rerenderOpener) {
		const opener = isc.WindowStack.getOpener();
		const result = isc.WindowStack._stack.pop();

		if (result) {
			isc.WindowStack._animateClose(result);
			const items = result.items;

			if (items) {
				if (CKEDITOR && CKEDITOR.instances._CKEditor) {
					CKEDITOR.instances._CKEditor.destroy();
					items[0].destroy();
				} else {
					items.forEach((item) => {
						if (item._vm) {
							// Has a values manager - must be an edit view
							isc.BizUtil.relinquishEditView(item);
						} else if (isc.isA.BizListGrid(item)) {
							// PickList
							isc.BizUtil.relinquishPickList(item);
						}
					});
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

	/**
	 * Gets the view in the window that opened the current window.
	 * @returns {Object} - the opener view.
	 */
	getOpener: function () {
		if (isc.WindowStack._stack.length <= 1) {
			// Opener is the harness current view
			return isc.BizUtil.getCurrentView();
		}
		return isc.WindowStack._stack[isc.WindowStack._stack.length - 2].items[0];
	},

	/**
	 * Resizes all windows in the stack to fit the current page dimensions.
	 */
	resize: function () {
		let windowWidth = isc.Page.getWidth() - 50;
		let windowHeight = isc.Page.getHeight() - 50;

		isc.WindowStack._stack.forEach((window) => {
			if (!window.sizeSet) {
				window.setWidth(windowWidth);
				window.setHeight(windowHeight);
				windowWidth -= 50;
				windowHeight -= 50;
			}
		});
	},

	/**
	 * Wireframe canvas used for animations.
	 * @type {Object}
	 */
	_wireframe: isc.Canvas.create({
		border: "3px ridge #E1E1E1",
		backgroundColor: "whitesmoke",
		opacity: 75,
		autoDraw: false,
	}),

	_animateClosedDuration: 400, // Duration for the close animation

	/** Animates the closing of a window.
	 * @param {Object} window - the window to animate.
	 */
	_animateClose: function (window) {
		if (window._fromRect) {
			const toLeft = window.getLeft();
			const toTop = window.getTop();
			const toWidth = window.getVisibleWidth();
			const toHeight = window.getVisibleHeight();

			// Initialize the wireframe to the current window rect
			isc.WindowStack._wireframe.setRect([toLeft, toTop, toWidth, toHeight]);
			isc.WindowStack._wireframe.show();
			isc.WindowStack._wireframe.bringToFront();
		}

		window.hide();

		if (window._fromRect) {
			// Animate the wireframe to the specified rect
			isc.WindowStack._wireframe.animateRect(
				window._fromRect[0],
				window._fromRect[1],
				window._fromRect[2],
				window._fromRect[3],
				"isc.WindowStack._wireframe.hide();",
				isc.WindowStack._animateClosedDuration,
			);
		}
	},
});
