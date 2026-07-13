SKYVE.PF = function() {
	// block multiple load attempts of google/leaflet maps JS libs.
	var loadingMap = false;
	
	var getSessionHistory = function() {
		var result = sessionStorage.sessionHistory;
		if (result) {
			result = JSON.parse(result);
		}
		else {
			result = [];
		}
		return result;
	};
	
	var contentOverlayIdsByBinding = {};
	var contentMarkupIdsByBinding = {};
	var contentMarkupCompanionIdsByBinding = {};
	var pageScrollLock = {
		count: 0,
		scrollX: 0,
		scrollY: 0,
		htmlOverflow: '',
		bodyOverflow: '',
		bodyPosition: '',
		bodyTop: '',
		bodyLeft: '',
		bodyWidth: '',
		bodyPaddingRight: ''
	};

	var getElement = function(element) {
		if (! element) {
			return null;
		}
		if (element.jquery) {
			return element[0] || null;
		}
		if (element.target) {
			return element.currentTarget || element.target;
		}
		if (typeof element === 'string') {
			var result = $(PrimeFaces.escapeClientId(element));
			if (result.length === 0) {
				result = $(element);
			}
			return result[0] || null;
		}
		return element;
	};

	var getWidgetElement = function(widgetVar) {
		var widget = window.PF ? window.PF(widgetVar) : null;
		return widget && widget.jq ? widget.jq[0] : null;
	};


	// Start of sticky header fixes
	// - keep filter inputs enabled
	// arrange across ultima/ecuador and editorial
	
	var dispatchWindowScroll = function() {
		if (typeof window.Event === 'function') {
			window.dispatchEvent(new Event('scroll'));
		}
		else {
			$(window).trigger('scroll');
		}
	};

	var alignStickyDataTableHeader = function(dataTable) {
		if ((! dataTable) || (! dataTable.jq) || (! dataTable.stickyContainer)) {
			return;
		}

		var sourceTable = dataTable.jq.children('.ui-datatable-tablewrapper').children('table');
		if (sourceTable.length === 0) {
			sourceTable = dataTable.jq.find('> .ui-datatable-tablewrapper table').first();
		}
		if (sourceTable.length === 0) {
			return;
		}

		var offset = sourceTable.offset();
		if (! offset) {
			return;
		}

		var width = sourceTable.outerWidth();
		dataTable.stickyContainer.css({
			left: offset.left + 'px',
			width: width + 'px'
		});
		dataTable.stickyContainer.children('table').css('width', width + 'px');
	};

	var refreshStickyDataTableHeader = function(dataTable) {
		alignStickyDataTableHeader(dataTable);
		dispatchWindowScroll();
		alignStickyDataTableHeader(dataTable);
	};

	var debounceStickyDataTableHeaderRefresh = function(dataTable, delay) {
		if (! dataTable) {
			return;
		}
		if (dataTable.skyveStickyRefreshTimeout) {
			window.clearTimeout(dataTable.skyveStickyRefreshTimeout);
		}
		dataTable.skyveStickyRefreshTimeout = window.setTimeout(function() {
			dataTable.skyveStickyRefreshTimeout = null;
			refreshStickyDataTableHeader(dataTable);
		}, delay);
	};

	var scheduleStickyDataTableHeaderRefresh = function(dataTable) {
		refreshStickyDataTableHeader(dataTable);
		window.setTimeout(function() {
			refreshStickyDataTableHeader(dataTable);
		}, 0);
		window.setTimeout(function() {
			refreshStickyDataTableHeader(dataTable);
		}, 50);
		window.setTimeout(function() {
			refreshStickyDataTableHeader(dataTable);
		}, 250);
	};

	var bindStickyDataTableHeaderRefresh = function(dataTable) {
		if ((! dataTable) || dataTable.skyveStickyResizePatch) {
			return;
		}

		var namespace = '.skyveStickyHeader-' + dataTable.id.replace(/\W/g, '_');
		var refresh = function() {
			refreshStickyDataTableHeader(dataTable);
			debounceStickyDataTableHeaderRefresh(dataTable, 80);
			window.setTimeout(function() {
				refreshStickyDataTableHeader(dataTable);
			}, 250);
		};

		$(window).on('resize' + namespace + ' orientationchange' + namespace, refresh);
		$(document).on('click' + namespace, '.layout-menu-button,.menu-button,#topbar-menu-button', function() {
			refreshStickyDataTableHeader(dataTable);
			window.setTimeout(function() {
				refreshStickyDataTableHeader(dataTable);
			}, 250);
		});

		dataTable.addDestroyListener(function() {
			if (dataTable.skyveStickyRefreshTimeout) {
				window.clearTimeout(dataTable.skyveStickyRefreshTimeout);
				dataTable.skyveStickyRefreshTimeout = null;
			}
			$(window).off(namespace);
			$(document).off(namespace);
		});
		dataTable.skyveStickyResizePatch = true;
	};

	var patchPrimeFacesStickyDataTableFilters = function() {
		var dataTablePrototype = window.PrimeFaces &&
									PrimeFaces.widget &&
									PrimeFaces.widget.DataTable &&
									PrimeFaces.widget.DataTable.prototype;
		if ((! dataTablePrototype) || dataTablePrototype.skyveStickyFilterPatch) {
			return;
		}

		var setupStickyHeader = dataTablePrototype.setupStickyHeader;
		if (typeof setupStickyHeader !== 'function') {
			return;
		}

		dataTablePrototype.setupStickyHeader = function() {
			setupStickyHeader.apply(this, arguments);

			// PrimeFaces keeps the enabled filter controls in the sticky header and disables
			// the cloned table header filters. Triggering the sticky scroll handler immediately
			// keeps the enabled header layered over the disabled clone before the user scrolls.
			bindStickyDataTableHeaderRefresh(this);
			scheduleStickyDataTableHeaderRefresh(this);
		};
		dataTablePrototype.skyveStickyFilterPatch = true;
	};

	patchPrimeFacesStickyDataTableFilters();
	$(patchPrimeFacesStickyDataTableFilters);

	// end of sticky header fixes
	
	var prepareMorph = function(source, target, options) {
		var sourceElement = getElement(source);
		var targetElement = getElement(target);
		if (! sourceElement || ! targetElement) {
			return null;
		}
		if (window.matchMedia && window.matchMedia('(prefers-reduced-motion: reduce)').matches) {
			return null;
		}

		var sourceRect = sourceElement.getBoundingClientRect();
		var targetRect = targetElement.getBoundingClientRect();
		if ((sourceRect.width <= 0) || (sourceRect.height <= 0) || (targetRect.width <= 0) || (targetRect.height <= 0)) {
			return null;
		}

		var settings = options || {};
		var originalTargetElement = targetElement;
		if (settings.ghost || targetElement.classList.contains('ui-dialog')) {
			var ghost = document.createElement('div');
			var targetStyle = window.getComputedStyle(targetElement);
			var targetZIndex = parseInt(targetStyle.zIndex, 10);
			var backgroundColor = targetStyle.backgroundColor;
			ghost.className = 'skyveMorphGhost';
			ghost.style.left = targetRect.left + 'px';
			ghost.style.top = targetRect.top + 'px';
			ghost.style.width = targetRect.width + 'px';
			ghost.style.height = targetRect.height + 'px';
			ghost.style.backgroundColor = (! backgroundColor || backgroundColor === 'rgba(0, 0, 0, 0)' || backgroundColor === 'transparent') ? '#fff' : backgroundColor;
			ghost.style.borderRadius = targetStyle.borderRadius;
			ghost.style.zIndex = isNaN(targetZIndex) ? '1000' : targetZIndex + 1;
			document.body.appendChild(ghost);
			targetElement = ghost;
			settings.endRadius = settings.endRadius || targetStyle.borderRadius;
		}

		var leftDistance = Math.min(Math.abs(sourceRect.left - targetRect.left), Math.abs(sourceRect.right - targetRect.left));
		var rightDistance = Math.min(Math.abs(sourceRect.left - targetRect.right), Math.abs(sourceRect.right - targetRect.right));
		var topDistance = Math.min(Math.abs(sourceRect.top - targetRect.top), Math.abs(sourceRect.bottom - targetRect.top));
		var bottomDistance = Math.min(Math.abs(sourceRect.top - targetRect.bottom), Math.abs(sourceRect.bottom - targetRect.bottom));
		var originX = leftDistance <= rightDistance ? 'left' : 'right';
		var originY = topDistance <= bottomDistance ? 'top' : 'bottom';
		var scaleX = Math.max(sourceRect.width / targetRect.width, 0.02);
		var scaleY = Math.max(sourceRect.height / targetRect.height, 0.02);
		var originOffsetX = originX === 'left' ? 0 : targetRect.width;
		var originOffsetY = originY === 'top' ? 0 : targetRect.height;
		var translateX = sourceRect.left - targetRect.left - (originOffsetX * (1 - scaleX));
		var translateY = sourceRect.top - targetRect.top - (originOffsetY * (1 - scaleY));

		targetElement.style.setProperty('--skyve-morph-origin', originX + ' ' + originY);
		targetElement.style.setProperty('--skyve-morph-x', translateX + 'px');
		targetElement.style.setProperty('--skyve-morph-y', translateY + 'px');
		targetElement.style.setProperty('--skyve-morph-scale-x', scaleX);
		targetElement.style.setProperty('--skyve-morph-scale-y', scaleY);
		targetElement.style.setProperty('--skyve-morph-start-radius', settings.startRadius || Math.round(Math.min(sourceRect.width, sourceRect.height) / 2) + 'px');
		if (settings.endRadius) {
			targetElement.style.setProperty('--skyve-morph-end-radius', settings.endRadius);
		}
		else {
			targetElement.style.removeProperty('--skyve-morph-end-radius');
		}
		return {
			element: targetElement,
			originalElement: originalTargetElement,
			settings: settings
		};
	};

	var morphFrom = function(source, target, options) {
		var prepared = prepareMorph(source, target, options);
		if (! prepared) {
			return;
		}
		var targetElement = prepared.element;
		var originalTargetElement = prepared.originalElement;
		var settings = prepared.settings;

		if (settings.hideTargetDuring && originalTargetElement !== targetElement) {
			originalTargetElement.style.visibility = 'hidden';
		}
		targetElement.classList.remove('skyveMorphOpening');
		void targetElement.offsetWidth;
		targetElement.classList.add('skyveMorphOpening');
		window.setTimeout(function() {
			targetElement.classList.remove('skyveMorphOpening');
			if (settings.hideTargetDuring && originalTargetElement !== targetElement) {
				originalTargetElement.style.visibility = '';
			}
			if (targetElement.classList.contains('skyveMorphGhost')) {
				targetElement.parentNode.removeChild(targetElement);
			}
		}, settings.duration || 360);
	};

	var morphWidgetFrom = function(source, widgetVar, options) {
		window.setTimeout(function() {
			morphFrom(source, getWidgetElement(widgetVar), options);
		}, 0);
	};

	var lockPageScroll = function() {
		var body = document.body;
		var documentElement = document.documentElement;
		if (! body || ! documentElement) {
			return;
		}
		if (pageScrollLock.count === 0) {
			pageScrollLock.scrollX = window.pageXOffset || documentElement.scrollLeft || body.scrollLeft || 0;
			pageScrollLock.scrollY = window.pageYOffset || documentElement.scrollTop || body.scrollTop || 0;
			pageScrollLock.htmlOverflow = documentElement.style.overflow;
			pageScrollLock.bodyOverflow = body.style.overflow;
			pageScrollLock.bodyPosition = body.style.position;
			pageScrollLock.bodyTop = body.style.top;
			pageScrollLock.bodyLeft = body.style.left;
			pageScrollLock.bodyWidth = body.style.width;
			pageScrollLock.bodyPaddingRight = body.style.paddingRight;

			var scrollbarWidth = window.innerWidth - documentElement.clientWidth;
			documentElement.style.overflow = 'hidden';
			body.style.overflow = 'hidden';
			body.style.position = 'fixed';
			body.style.top = (-pageScrollLock.scrollY) + 'px';
			body.style.left = (-pageScrollLock.scrollX) + 'px';
			body.style.width = '100%';
			if (scrollbarWidth > 0) {
				body.style.paddingRight = ((parseFloat(window.getComputedStyle(body).paddingRight) || 0) + scrollbarWidth) + 'px';
			}
		}
		pageScrollLock.count++;
	};

	var unlockPageScroll = function() {
		var body = document.body;
		var documentElement = document.documentElement;
		if (! body || ! documentElement || pageScrollLock.count === 0) {
			return;
		}
		pageScrollLock.count = Math.max(0, pageScrollLock.count - 1);
		if (pageScrollLock.count === 0) {
			documentElement.style.overflow = pageScrollLock.htmlOverflow;
			body.style.overflow = pageScrollLock.bodyOverflow;
			body.style.position = pageScrollLock.bodyPosition;
			body.style.top = pageScrollLock.bodyTop;
			body.style.left = pageScrollLock.bodyLeft;
			body.style.width = pageScrollLock.bodyWidth;
			body.style.paddingRight = pageScrollLock.bodyPaddingRight;
			window.scrollTo(pageScrollLock.scrollX, pageScrollLock.scrollY);
		}
	};

	var getContentActionMenu = function(source) {
		var menuId = source.id ? source.id.replace(/_button$/, '_menu') : null;
		var menu = menuId ? document.getElementById(menuId) : null;
		if (! menu && source.parentNode) {
			menu = $(source.parentNode).children('.skyveContentActionMenu')[0] || null;
		}
		return menu;
	};

	$(document).on('mousedown touchstart', '.skyveContentActionButton', function() {
		var menu = getContentActionMenu(this);
		$(this).data('skyveMenuWasVisible', !! (menu && $(menu).is(':visible')));
	});

	$(document).on('click', '.skyveContentActionButton', function() {
		if ($(this).data('skyveMenuWasVisible')) {
			return;
		}
		var source = this;
		window.setTimeout(function() {
			var menu = getContentActionMenu(source);
			if (menu && $(menu).is(':visible')) {
				morphFrom(source, menu, {duration: 360, endRadius: '4px', ghost: true, hideTargetDuring: true});
			}
		}, 0);
	});
	
	var getUrlParameter = function(url, name) {
		var match = new RegExp('[?&]' + name + '=([^&]*)').exec(url);
		return match ? decodeURIComponent(match[1].replace(/\+/g, ' ')) : null;
	};

	var unsanitiseBinding = function(binding) {
		return binding.replace(/\_(\d*)\_/g, '[$1]').replace(/\_/g, '.');
	};

	var getClientIdSelector = function(localId) {
		return '[id="' + localId + '"],[id$=":' + localId + '"]';
	};
	
	var getContentSelector = function(id, binding, suffix) {
		return getClientIdSelector(id ? id + '_' + binding + suffix : '_' + binding + suffix);
	};
	
	var getContentBindingCandidates = function(binding) {
		var result = [binding];
		var index = binding.lastIndexOf('_');
		if (index > -1 && index < (binding.length - 1)) {
			var suffix = binding.substring(index + 1);
			if (binding.charAt(0) === '_') {
				result.push('_' + suffix);
			}
			result.push(suffix);
		}
		return result;
	};

	var getContentElements = function(root, id, binding, suffix) {
		var bindings = getContentBindingCandidates(binding);
		var result = root.$();
		for (var i = 0, l = bindings.length; i < l; i++) {
			result = root.$(getContentSelector(id, bindings[i], suffix));
			if (result.length > 0) {
				return result;
			}
		}
		if (id) {
			for (var j = 0, m = bindings.length; j < m; j++) {
				result = root.$(getContentSelector(null, bindings[j], suffix));
				if (result.length > 0) {
					return result;
				}
			}
		}
		return result;
	};
	
	var setContentValue = function(root, id, binding, suffix, value) {
		getContentElements(root, id, binding, suffix).val(value).attr('value', value).trigger('change');
	};

	var setExactContentValue = function(root, id, binding, suffix, value) {
		var element = root.$(getContentSelector(id, binding, suffix));
		if (element.length > 0) {
			element.val(value).attr('value', value).trigger('change');
		}
	};
	
	var clearContentAnchor = function(root, id, binding) {
		getContentElements(root, id, binding, '_link').attr('href','javascript:void(0)').text('<Empty>').attr('onclick', 'return false').off('click.skyveContentClear').on('click.skyveContentClear', function() { return false; });
	};

	var showContentVideo = function(root, id, binding, url) {
		getContentElements(root, id, binding, '_video').each(function() {
			var container = root.$(this);
			var video = container.children('video');
			if (video.length > 0) {
				video.attr('src', url);
			}
			else {
				container.append('<video controls preload="metadata" style="width:100%;height:100%;object-fit:contain" src="' + url + '"></video>');
			}
		});
	};

	var showContentVideoPlaceholder = function(root, id, binding) {
		getContentElements(root, id, binding, '_video').each(function() {
			root.$(this).children('video').remove();
		});
	};

	var setContentImagePlaceholder = function(root, id, binding, empty) {
		var image = getContentElements(root, id, binding, '_image');
		image.toggleClass('skyveContentHidden', empty);
		image.parent().toggleClass('skyveContentEmpty', empty);
	};

	var setAutoContentVisibility = function(root, id, binding, mediaKind) {
		var link = getContentElements(root, id, binding, '_link');
		link.toggleClass('skyveContentHidden', !! mediaKind && mediaKind !== 'link');

		var image = getContentElements(root, id, binding, '_image');
		var showImage = mediaKind === 'image';
		image.toggleClass('skyveContentHidden', ! showImage);
		image.parent().toggleClass('skyveContentHidden', ! showImage);
		image.parent().toggleClass('skyveContentEmpty', ! showImage);

		var video = getContentElements(root, id, binding, '_video');
		var showVideo = mediaKind === 'video';
		video.toggleClass('skyveContentHidden', ! showVideo);
		video.parent().toggleClass('skyveContentHidden', ! showVideo);
	};

	var getContentMarkupItems = function(root, id, binding) {
		var result = id ? root.$('.skyveContentMarkupAction-' + id + '_' + binding) : root.$();
		if (result.length > 0) {
			return result;
		}
		return root.$('.skyveContentMarkupAction-' + binding);
	};

	var setContentMarkupVisibility = function(root, id, binding, mediaKind) {
		getContentMarkupItems(root, id, binding).toggleClass('skyveContentHidden', mediaKind !== 'image');
	};
		
	var getContentWidget = function(id, binding, suffix) {
		var widget = id ? window.PF(id + '_' + binding + suffix) : null;
		return widget || window.PF(binding + suffix);
	};

	// public
	return {
		getById: function(id) {
			return $(PrimeFaces.escapeClientId(id));
		},

		morphFrom: function(source, target, options) {
			morphFrom(source, target, options);
		},

		morphWidgetFrom: function(source, widgetVar, options) {
			morphWidgetFrom(source, widgetVar, options);
		},

		lockPageScroll: function() {
			lockPageScroll();
		},

		unlockPageScroll: function() {
			unlockPageScroll();
		},
		
		getByIdEndsWith: function(id) {
			return $('[id$="' + id + '"]');
		},
		
		contentOverlayOnShow: function(id, url, lockScroll) {
			if (lockScroll) {
				lockPageScroll();
			}
			var binding = getUrlParameter(url, '_n');
			if (binding) {
				contentOverlayIdsByBinding[binding] = id;
			}
			var iframe = SKYVE.PF.getById(id + '_overlayiframe');
			if (iframe.attr('src') !== url) {
				iframe.attr('src', url);
			}
		},
		
		contentOverlayOnHide: function(id, preserve, unlockScroll) {
			if (! preserve) {
				SKYVE.PF.getById(id + '_overlayiframe').attr('src','')
			}
			if (unlockScroll) {
				unlockPageScroll();
			}
		},
		
		afterContentUpload: function(binding, contentId, modoc, fileName, mediaKind, companionBinding) {
			var id = contentOverlayIdsByBinding[binding];
			setContentValue(window, id, binding, '_hidden', contentId);
			if (mediaKind && companionBinding) {
				setExactContentValue(window, id, companionBinding, '_hidden', mediaKind);
			}
			var url = 'content?_n=' + contentId + '&_doc=' + modoc + '&_b=' + unsanitiseBinding(binding);
			getContentElements(window, id, binding, '_link').off('click.skyveContentClear').attr('href', url).text('Content').attr('onclick', 'return true');
			getContentElements(window, id, binding, '_image').attr('src', url);
			setContentImagePlaceholder(window, id, binding, false);
			showContentVideo(window, id, binding, url);
			if (companionBinding) {
				setAutoContentVisibility(window, id, binding, mediaKind);
			}
			setContentMarkupVisibility(window, id, binding, mediaKind);
			var widget = getContentWidget(id, binding, 'Overlay');
			if (widget) {
				widget.hide();
			}
			SKYVE.PF.getById(id + '_overlayiframe').attr('src','');
			delete contentOverlayIdsByBinding[binding];
		},

		clearContentImage: function(binding, id, companionBinding) {
			setContentValue(window, id, binding, '_hidden', '');
			if (companionBinding) {
				setExactContentValue(window, id, companionBinding, '_hidden', '');
			}
			clearContentAnchor(window, id, binding);
			getContentElements(window, id, binding, '_image').attr('src','images/blank.gif');
			setContentImagePlaceholder(window, id, binding, true);
			showContentVideoPlaceholder(window, id, binding);
			if (companionBinding) {
				setAutoContentVisibility(window, id, binding, '');
			}
			setContentMarkupVisibility(window, id, binding, '');
		},
		
		clearContentLink: function(binding, id, companionBinding) {
			setContentValue(window, id, binding, '_hidden', '');
			if (companionBinding) {
				setExactContentValue(window, id, companionBinding, '_hidden', '');
			}
			clearContentAnchor(window, id, binding);
			showContentVideoPlaceholder(window, id, binding);
			if (companionBinding) {
				setAutoContentVisibility(window, id, binding, '');
			}
			setContentMarkupVisibility(window, id, binding, '');
		},

		clearContent: function(binding, id, companionBinding) {
			SKYVE.PF.clearContentLink(binding, id, companionBinding);
			getContentElements(window, id, binding, '_image').attr('src','images/blank.gif');
			setContentImagePlaceholder(window, id, binding, true);
		},

		contentMarkupOnShow: function(id, binding, url, companionBinding) {
			contentMarkupIdsByBinding[binding] = id;
			if (companionBinding) {
				contentMarkupCompanionIdsByBinding[binding] = companionBinding;
			}
			var finalUrl = url += '&_id=' + getContentElements(window, id, binding, '_hidden').val();
			SKYVE.PF.getById(id + '_markupiframe').attr('src', finalUrl);
		},

		contentMarkupOnHide: function(id) {
			SKYVE.PF.getById(id + '_markupiframe').attr('src','')
		},

		afterMarkupApply: function(binding, contentId, modoc, fileName) {
			var id = contentMarkupIdsByBinding[binding];
			setContentValue(window, id, binding, '_hidden', contentId);
			var companionBinding = contentMarkupCompanionIdsByBinding[binding];
			if (companionBinding) {
				setExactContentValue(window, id, companionBinding, '_hidden', 'image');
			}
			var url = 'content?_n=' + contentId + '&_doc=' + modoc + '&_b=' + unsanitiseBinding(binding);
			getContentElements(window, id, binding, '_link').off('click.skyveContentClear').attr('href', url).text('Content').attr('onclick', 'return true');
			getContentElements(window, id, binding, '_image').attr('src', url);
			setContentImagePlaceholder(window, id, binding, false);
			if (companionBinding) {
				setAutoContentVisibility(window, id, binding, 'image');
			}
			setContentMarkupVisibility(window, id, binding, 'image');
			var widget = getContentWidget(id, binding, 'Markup');
			if (widget) {
				widget.hide();
			}
			delete contentMarkupIdsByBinding[binding];
			delete contentMarkupCompanionIdsByBinding[binding];
		},

		tabChange: function(moduleName, documentName, id, index) {
			sessionStorage['tab_' + moduleName + '_' + documentName + '_' + id] = index;
			if (SKYVE.BizMap) {
				SKYVE.BizMap.resizeAll();
			}
			if (SKYVE.BizMapPicker) {
				SKYVE.BizMapPicker.resizeAll();
			}
		},
		
		getTextElement: function(id) {
			return SKYVE.PF.getById(id);
		},

		getTextValue: function(id) {
			return SKYVE.PF.getTextElement(id).val();
		},
		
		setTextValue: function(id, value) {
			SKYVE.PF.getTextElement(id).val(value);
		},

		getPasswordElement: function(id) {
			return SKYVE.PF.getById(id + 'password');
		},
		
		getPasswordValue: function(id) {
			return SKYVE.PF.getPasswordElement(id).val();
		},
		
		setPasswordValue: function(id, value) {
			SKYVE.PF.getPasswordElement(id).val(value);
		},

		// for selecting values and getting the selected value, use the PF SelectOneMenu API through widgetVar
		getComboElement: function(id) {
			return SKYVE.PF.getById(id);
		},

		// to perform a lookup, use the AutoComplete API through widgetVar
		getLookupElement: function(id) {
			return SKYVE.PF.getById(id);
		},
		
		getLookupValue: function(id) {
			return SKYVE.PF.getById(id + '_hinput').val();
		},
		
		setLookupValue: function(id, value) {
			SKYVE.PF.getById(id + '_hinput').val(value);
		},
		
		getLookupDescription: function(id) {
			return SKYVE.PF.getById(id + '_input').val();
		},
		
		setLookupDescription: function(id, value) {
			SKYVE.PF.getById(id + '_input').val(value);
		},
		
		getCheckboxElement: function(id) {
			return SKYVE.PF.getById(id);
		},
		
		getCheckboxValue: function(id) {
			var value = SKYVE.PF.getById(id + '_input').val();
			if (value == '0') {
				return null;
			}
			else if (value == '1') {
				return true;
			}
			else if (value == '2') {
				return false;
			}
			else {
				return SKYVE.PF.getById(id + '_input').is(":checked");
			}
		},
		
		setCheckboxValue: function(id, trueOrFalse) {
			SKYVE.PF.getById(id + '_input').prop('checked', trueOrFalse);

			var outerDiv = SKYVE.PF.getById(id);
			var innerDiv = outerDiv.find('.ui-chkbox-box');
			var innerSpan = innerDiv.find('.ui-chkbox-icon')
			if (trueOrFalse) {
				innerDiv.addClass('ui-state-active');
				innerSpan.addClass('ui-icon ui-icon-check')
			}
			else {
				innerDiv.removeClass('ui-state-active');
				innerSpan.removeClass('ui-icon ui-icon-check')
			}
		},

		// Called in the <head/> of PF pages when cold navigation to existing page.
		// ie Not from menu of conversation navigation
		establishHistory: function() {
			var sessionHistory = getSessionHistory();
			if (sessionHistory.length == 0) {
				var url = window.location.href;
				if (url.match(/.*[\?|\&]a\=e.*/) ||
						url.match(/.*[\?|\&]a\=e\&.*/)) {
					var referrer = document.referrer;
					if ((! referrer) || (referrer == '') || (referrer == url)) {
						referrer = SKYVE.Util.CONTEXT_URL;
					}
					sessionHistory.push(referrer);
					sessionStorage.sessionHistory = JSON.stringify(sessionHistory);
				}
			}
			//console.log('establish history ' + sessionStorage.sessionHistory);
		},
		
		// Called from the menu links to reset the history.
		startHistory: function(url) {
			sessionStorage.sessionHistory = '[]';
			window.location.assign(url);
			//console.log('start history ' + sessionStorage.sessionHistory);
		},
		
		// Called from New/Zoom In actions on DataGrids/ListGrids
		pushHistory: function(url) { // url is optional and browser wont change location if not defined
			var sessionHistory = getSessionHistory();
			sessionHistory.push(window.location.href);
			sessionStorage.sessionHistory = JSON.stringify(sessionHistory);
			if (url) {
				window.location.assign(url);
			}
			//console.log('push history ' + sessionStorage.sessionHistory);
		},
		
		// Called from OK, Delete and Cancel buttons on top-level edit views and
		// Zoom Out button on zoomed-in edit views.
		popHistory: function(redirect) {
			var sessionHistory = getSessionHistory();
			if (sessionHistory.length > 0) {
				var url = sessionHistory.pop();
				sessionStorage.sessionHistory = JSON.stringify(sessionHistory);
				if (redirect) {
					window.location.assign(url);
				}
			}
			//console.log('pop history ' + sessionStorage.sessionHistory);
		},
		
		// Called from Save button and from action buttons
		// when a bean was not persistent and becomes persistent from the button push.
		// It replaces the "new" URL (no i param) with an "edit" URL through the history stack.
		saveHistory: function(bizModule, bizDocument, bizId) {
			var sessionHistory = getSessionHistory();
			var historyChanged = false;
			for (var i = 0; i < sessionHistory.length; i++) {
				var url = sessionHistory[i];
				
				// NB can't just use indexOf() here - the entire URL parameter must match
				var tokens = url.split(/\?|\&/);
				var moduleParameterToken = 'm=' + bizModule;
				var documentParameterToken = 'd=' + bizDocument;
				var actionParameterFound = false;
				var moduleParameterFound = false;
				var documentParameterFound = false;
				var bizIdParameterFound = false;
				for (var j = 0; j < tokens.length; j++) {
					var token = tokens[j];
					if (token == 'a=e') {
						actionParameterFound = true;
					}
					else if (token == moduleParameterToken) {
						moduleParameterFound = true;
					}
					else if (token == documentParameterToken) {
						documentParameterFound = true;
					}
					else if (token.startsWith('i=')) {
						bizIdParameterFound = true;
					}
				}
				if (actionParameterFound && 
						moduleParameterFound && 
						documentParameterFound &&
						bizIdParameterFound) {
					url += '&i=' + bizId;
					sessionHistory[i] = url;
					historyChanged = true;
				}
			}
			if (historyChanged) {
				sessionStorage.sessionHistory = JSON.stringify(sessionHistory);
			}
			
			if (window.history) {
				if (history.replaceState) {
					var url = window.location.href + '&i=' + bizId;
					history.replaceState({}, '', url);
				}
			}
			//console.log('save history ' + sessionStorage.sessionHistory);
		},
		
        toggleFilters: function(dataTableId) {
            var hiddenClass = 'hiddenFilter';
            // test for element that ends with the dataTableId as it may be in a naming container
            var dataTable = $('[id$="' + dataTableId + '"]');
			if (dataTable != null) {
				var toggleClass = function() {
                    var filter = $(this);
                    if (filter.hasClass(hiddenClass)) {
                        filter.removeClass(hiddenClass);
                    } else {
                        filter.addClass(hiddenClass);
                    }
				};
				dataTable.find('.ui-filter-column').each(toggleClass);
				dataTable.find('.ui-column-customfilter').each(toggleClass);
				dispatchWindowScroll();
			}
		},
		
		onPushMessage: function(pushMessage) {
			var growls = [];

			for (var i = 0, l = pushMessage.length; i < l; i++) {
				var m = pushMessage[i];
				if (m.type == 'g') {
					growls.push({severity: m.severity, summary: m.message});
				}
				else if (m.type == 'm') {
					PrimeFaces.showMessageInDialog({severity: m.severity, summary: '', detail: m.message,escape: false});
				}
				else if (m.type == 'r') {
					pushRerender();
				}
				else if (m.type == 'j') {
					window[m.method](m.argument);
				}
			}
			
			if (growls.length > 0) {
				PrimeFaces.cw('Growl', 'pushGrowl', {
					id: 'pushGrowl', 
					widgetVar: 'pushGrowl',
					life: 6000, 
					sticky: false, 
					msgs: growls 
				});
			}
		},
		
		gmap: function(options) {
			if (loadingMap) {
				setTimeout(function() {SKYVE.PF.gmap(options)}, 100);
			}
			else if (window.google && window.google.maps && window.SKYVE.BizMapPicker) {
				if (options.queryName || options.modelName) {
					return SKYVE.BizMap.create(options);
				}
				return SKYVE.BizMapPicker.create(options);
			}
			else {
				loadingMap = true;

				SKYVE.Util.loadJS('wicket/wicket.js?v=' + SKYVE.Util.v, function() {
					SKYVE.Util.loadJS('wicket/wicket-gmap3.js?v=' + SKYVE.Util.v, function() {
						var url = 'https://maps.googleapis.com/maps/api/js?v=3&libraries=drawing';
						if (SKYVE.Util.googleMapsV3ApiKey) {
							url += '&key=' + SKYVE.Util.googleMapsV3ApiKey;
						}
						SKYVE.Util.loadJS(url, function() {
							SKYVE.Util.loadJS('skyve/prime/skyve-gmap-min.js?v=' + SKYVE.Util.v, function() {
								loadingMap = false;
								if (options.queryName || options.modelName) {
									return SKYVE.BizMap.create(options);
								}
								return SKYVE.BizMapPicker.create(options);
							});
						});
					});
				});
			}
		},
		
		leaflet: function(options) {
			if (loadingMap) {
				setTimeout(function() {SKYVE.PF.leaflet(options)}, 100);
			}
			else if (window.L && window.SKYVE.BizMapPicker) {
				if (options.queryName || options.modelName) {
					return SKYVE.BizMap.create(options);
				}
				return SKYVE.BizMapPicker.create(options);
			}
			else {
				loadingMap = true;

				SKYVE.Util.loadCSS('leaflet/leaflet.css?v=' + SKYVE.Util.v, function() {
					SKYVE.Util.loadJS('leaflet/leaflet.js?v=' + SKYVE.Util.v, function() {
						SKYVE.Util.loadJS('leaflet/Path.Drag.js?v=' + SKYVE.Util.v, function() {
							SKYVE.Util.loadJS('leaflet/Leaflet.Editable.js?v=' + SKYVE.Util.v, function() {
								SKYVE.Util.loadCSS('leaflet/MarkerCluster.css?v=' + SKYVE.Util.v, function() {
									SKYVE.Util.loadCSS('leaflet/MarkerCluster.Default.css?v=' + SKYVE.Util.v, function() {
										SKYVE.Util.loadJS('leaflet/leaflet.markercluster.js?v=' + SKYVE.Util.v, function() {
											SKYVE.Util.loadCSS('leaflet/leaflet.fullscreen.css?v=' + SKYVE.Util.v, function() {
												SKYVE.Util.loadJS('leaflet/Leaflet.fullscreen.min.js?v=' + SKYVE.Util.v, function() {
													SKYVE.Util.loadJS('skyve/prime/skyve-leaflet-min.js?v=' + SKYVE.Util.v, function() {
														loadingMap = false;
														if (options.queryName || options.modelName) {
															return SKYVE.BizMap.create(options);
														}
														return SKYVE.BizMapPicker.create(options);
													});
												});
											});
										});
									});
								});
							});
						});
					});
				});
			}
		},

		// This is called by PF charts to lose the 2:1 aspect ratio.
		chartExtender: function() {		
			if (! this.cfg.config.options) {
				this.cfg.config.options = {};
			}
			this.cfg.config.options.responsive = true;
			this.cfg.config.options.maintainAspectRatio = false;
		},
					
		sidebar: function(widgetId, width, breakpoint, floatingWidth, viewSuffix) {
			$(function() {
				var	win = $(window);
				var head = $('head');
				var body = $('body');
				var sidebar = $('#'+widgetId);
				var sidebarInner = sidebar.children('.inner');

				if ($('#sidebarStyle' + viewSuffix).length == 0) { // DNE
					$('<style id="sidebarStyle' + viewSuffix + '">.sidebar' + viewSuffix + '{background-color:white;font-size:0.9em;position:relative;width:' + width + ';flex-grow:0;flex-shrink:0;transition:margin-right 0.5s ease,box-shadow 0.5s ease;}' +
						'.sidebar' + viewSuffix + ' .toggle{text-decoration:none;transition:right 0.5s ease;-webkit-tap-highlight-color:rgba(255,255,255,0);border:0;display:none;position:absolute;top:calc(50% - 3.75em);right:calc(' + floatingWidth + 'px - 3em);width:4em;height:7.5em;line-height:6.25em;text-align:center;z-index:10000;}' + 
						'.sidebar' + viewSuffix + ' .toggle:before{font-family:FontAwesome;font-size:1.1rem;font-style:normal;font-weight:normal;text-transform:none !important;content:\'>\';height:inherit;right:0;line-height:inherit;position:absolute;text-indent:0;top:0;width:inherit;color:#7f888f;margin-right:1.5em;margin-top:-0.5em;z-index:1;}' + 
						'.sidebar' + viewSuffix + ' .toggle:after{background:rgba(222,225,226,0.75);border-radius:0.375em;content:\'\';height:3em;right:2em;position:absolute;top:1em;width:3em;}' +
						'.sidebar' + viewSuffix + '.inactive{margin-right:-' + floatingWidth + 'px;}.sidebar' + viewSuffix + '.inactive .toggle:before{content:\'<\';}' +
						'@media screen and (max-width:' + breakpoint + 'px){' +
							'.sidebar' + viewSuffix + '{box-shadow:0 0 5em 0 rgba(0,0,0,0.175);position:fixed;top:0;right:0;width:' + floatingWidth + 'px;height:100%;z-index:10000;}' +
							'.sidebar' + viewSuffix + '.inactive{box-shadow:none;}' +
							'.sidebar' + viewSuffix + '>.inner{position:absolute;top:0;right:0;width:100%;height:100%;overflow-x:hidden;overflow-y:auto;-webkit-overflow-scrolling:touch;}' +
							'.sidebar' + viewSuffix + ' .toggle{display:flex;}' +
						'}</style>').appendTo(head);
				}
				win.on('resize', function() {
					if (win.width() <= breakpoint) {
						sidebar.addClass('inactive');
					}
					else {
						sidebar.removeClass('inactive');
					}
				});
				win.trigger('resize'); // initial setting
				
				// Toggle.
				$('<a href="#' + widgetId + '" class="toggle"></a>')
					.appendTo(sidebar)
					.on('click', function(event) {
						event.preventDefault();
						event.stopPropagation();
						sidebar.toggleClass('inactive');
					});
					
				// Show the hidden sidebar once the transition is mostly over
				window.setTimeout(function() {
					sidebar.show();
				}, 100);
			});
		}
	};
}();
