WILDCAT = function() {
	// set some JQuery mobile settings up front
	$.mobile.defaultPageTransition = 'none';
	$.mobile.defaultDialogTransition = 'none';
	$.mobile.useFastClick = true;

	// public
	return {
		// hide the disabled text field and show an empty autoComplete field
		switchToAutoComplete: function(buttonElement) {
			var button = $(buttonElement);
			button.css('display', 'none');
			var text = button.prev();
			text.css('display', 'none');
			var autoComplete = text.prev();
			autoComplete.css('display', 'block');
			var autoCompleteHidden = $(PrimeFaces.escapeClientId(autoComplete.attr('id')) + '_hinput');
			autoCompleteHidden.val('');
			var autoCompleteText = $(PrimeFaces.escapeClientId(autoComplete.attr('id')) + '_input');
			autoCompleteText.val('');
			autoCompleteText.focus();
			return false;
		}
	};
}();
