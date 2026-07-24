(function() {
	'use strict';

	var previewUrlPrefix;
	var endPreviewUrl;
	var modeSwitchTimer;

	function size(styleClass) {
		var device = $('#device');
		var parent = device.parent();
		if (! styleClass) {
			styleClass = device.attr('class');
			if (styleClass) {
				styleClass = styleClass.split(' ')[0];
			}
		}
		if (styleClass === 'phone') {
			// height - 2 * margin top (20px) / screen height (800px) + 2 * shim height (60px)
			var verticalScale = (parent.height() - 40) / 920;
			// width - 2 * margin sides (20px) / (screen width (480px) + 2 * shim width (16px)
			var horizontalScale = (parent.width() - 40) / 512;
			device.css('transform', 'scale(' + Math.min(verticalScale, horizontalScale) + ') translate(-50%) rotate(0deg)');
		}
		else if (styleClass === 'tablet') {
			// height - 2 * margin top (20px) / screen height (1024px) + 2 * shim height (60px)
			var verticalScale = (parent.height() - 40) / 1144;
			// width - 2 * margin sides (20px) / (screen width (768px) + 2 * shim width (16px)
			var horizontalScale = (parent.width() - 40) / 800;
			device.css('transform', 'scale(' + Math.min(verticalScale, horizontalScale) + ') translate(-50%) rotate(0deg)');
		}
		else if (styleClass === 'laptop') {
			// height - 2 * margin top (20px) / (screen height (800px) + top bezel (18px) + lower shell and base (120px))
			var verticalScale = (parent.height() - 40) / 938;
			// width - 2 * margin sides (20px) / tapered base width (1540px)
			var horizontalScale = (parent.width() - 40) / 1540;
			device.css('transform', 'scale(' + Math.min(verticalScale, horizontalScale) + ') translate(-50%) rotate(0deg)');
		}
		else if (styleClass === 'desktop') {
			device.css('transform', 'scale(1) translate(0%) rotate(0deg)');
		}
	}

	function navigate(deviceType, userAgentType, message) {
		var device = $('#device');
		device.attr('class', deviceType);
		size(deviceType);
		device.addClass('mode-switching');
		window.clearTimeout(modeSwitchTimer);
		var iframe = $('#iframe');
		iframe.off('load.devicePreview').removeClass('powering-on').addClass('loading');
		iframe[0].getBoundingClientRect();
		var powerOn = function() {
			window.requestAnimationFrame(function() {
				iframe.addClass('powering-on').removeClass('loading');
				modeSwitchTimer = window.setTimeout(function() {
					device.removeClass('mode-switching');
				}, 350);
			});
		};
		iframe.one('load.devicePreview', powerOn);
		iframe.attr('src', previewUrlPrefix + userAgentType);
		if (message) {
			PF('previewMessages').renderMessage({severity: 'info', summary: message, detail: message});
		}
	}

	function switchTo(deviceType, label) {
		var userAgentType = deviceType;
		if ((deviceType !== 'phone') && (deviceType !== 'tablet') && (deviceType !== 'desktop')) {
			if (deviceType === 'laptop') {
				userAgentType = 'desktop';
			}
			else {
				return;
			}
		}
		navigate(deviceType, userAgentType, label ? label + ' selected' : null);
	}

	function bindSwitch(selector, deviceType, label) {
		$(selector).on('click.devicePreview', function(event) {
			event.preventDefault();
			switchTo(deviceType, label);
		});
	}

	$(function() {
		var device = $('#device');
		previewUrlPrefix = device.attr('data-preview-url-prefix');
		endPreviewUrl = device.attr('data-end-preview-url');

		bindSwitch('.ui-speeddial-action[id$="devicePreviewPhone"]', 'phone', 'Phone');
		bindSwitch('.ui-speeddial-action[id$="devicePreviewTablet"]', 'tablet', 'Tablet');
		bindSwitch('.ui-speeddial-action[id$="devicePreviewLaptop"]', 'laptop', 'Laptop');
		bindSwitch('.ui-speeddial-action[id$="devicePreviewDesktop"]', 'desktop', 'Desktop');
		$('.ui-speeddial-action[id$="devicePreviewEnd"]').on('click.devicePreview', function(event) {
			event.preventDefault();
			window.location.assign(endPreviewUrl);
		});

		switchTo('phone');
		$(window).on('resize.devicePreview', function() {
			size();
		});
	});
})();
