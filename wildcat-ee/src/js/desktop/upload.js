// Global BizHub object
if (typeof BizHub == "undefined" || ! BizHub) {
    var BizHub = {};
}

BizHub.upload = function() {
	// private methods
	
	var setImageOrLink = function(document, modoc, dataGroup, user, binding, value) {
		var imageOrLink = document.getElementById(binding + '_link');
		if (imageOrLink != null) { // it's a link
			var href = imageOrLink.href;
			var index = href.indexOf('?_n=') + 4;
			href = href.substring(0, index);
			if (value != null) {
				href += value;
			}
			href += '&_doc=' + modoc;
			href += '&_dg=' + dataGroup;
			href += '&_u=' + user;
			href += '&_b=' + binding;
			imageOrLink.href = href;
		}
		else { // it's not a link
			imageOrLink = document.getElementById(binding + '_image');
			if (imageOrLink != null) { // it's an image
				var src = imageOrLink.src;
				if (value != null) {
					var index = src.indexOf('?_n=');
					if (index < 0) { // not found
						src = 'content?_n=';
					}
					else { // found
						index += 4;
						src = src.substring(0, index);
					}
					src += value;
					src += '&_doc=' + modoc;
					src += '&_b=' + binding;
				}
				else {
					src = 'images/blank.gif';
				}
							
				imageOrLink.src = src;
			}
		}
	};
	
	// public methods
	
	return {
		submitProgress: function(url) {
			var fname = document.getElementById('myFile').value;
			if (fname != '') {
				var progressFrame = window.parent.document.getElementById('progress');
				progressFrame.src = url + '?first&uplMonitor=' + encodeURIComponent(fname);
			}
			else {
				alert('Please select a file to upload.');
				return false; // stop post event
			}
		},
	
		populateParentWidgetAndClose: function(modoc, dataGroup, user, binding, fileName) {
			if (top.WindowStack) {
				var vm = top.WindowStack.getOpener()._vm;
				// use regex replace instead of replaceAll() as there are no isomorphic scripts loaded here
				vm.setValue(binding.replace(/\./g,'_'), fileName);

				top.WindowStack.popoff(false);
			}
			else if (window.parent) {
				var parentWindowDocument = window.parent.opener.document;
				// populate hidden field
				var parentWidget = parentWindowDocument.getElementById(binding);
				if (parentWidget == null) {
					alert("Cannot populate upload hidden with content UUID");
					return;
				}
				parentWidget.value = fileName; // set the hidden value
					
				setImageOrLink(parentWindowDocument, modoc, dataGroup, user, binding, fileName);
				
				window.parent.close();
			}
		},
		
		clear: function(modoc, dataGroup, user, binding) {
			var hidden = document.getElementById(binding);
			if (hidden == null) {
				alert("Cannot clear upload hidden = " + binding);
				return;
			}
			hidden.value = '';
			
			setImageOrLink(window.document, modoc, dataGroup, user, binding, null);
		}
	};
}();
