/**
 * Styles an output HTML element based on text based on the specified input element 
 * defined by the smart client variable and the binding.
 * 
 * @param scVar The smart client variable assigned to the form containing the input textArea, e.g. 'v1'
 * @param binding The binding of the input textArea, e.g. 'template'
 * @param languageMimeType The syntax highlighting language mime type, e.g. 'text/html'
 * 
 * @see https://codemirror.net/LICENSE
 */
 function styleOutput(scVar, binding, languageMimeType) {
	SKYVE.Util.loadCSS('pages/css/cm/codemirror.css?v=' + SKYVE.Util.v, function() {
		SKYVE.Util.loadCSS('pages/css/cm/base16-dark.css?v=' + SKYVE.Util.v, function() {
			SKYVE.Util.loadJS('pages/js/cm/codemirror.js?v=' + SKYVE.Util.v, function() {
				SKYVE.Util.loadJS('pages/js/cm/css/css.js?v=' + SKYVE.Util.v, function() {
					SKYVE.Util.loadJS('pages/js/cm/htmlmixed/htmlmixed.js?v=' + SKYVE.Util.v, function() {
						SKYVE.Util.loadJS('pages/js/cm/sql/sql.js?v=' + SKYVE.Util.v, function() {
							SKYVE.Util.loadJS('pages/js/cm/xml/xml.js?v=' + SKYVE.Util.v, function() {
								var templates = document.getElementsByName(binding);
								var templateElement = undefined;

								if(templates && templates.length > 0 
										&& "textarea" === templates[0].nodeName.toLowerCase()) {
									templateElement = templates[0];
								}
								
								if(templateElement) {
									// console.log('templateElement', templateElement.id);
									templateElement.setAttribute("autocapitalize", "off");
									templateElement.setAttribute("spellcheck", "false");

									// codemirror configuration
									var editor = CodeMirror.fromTextArea(templateElement, {
										mode: languageMimeType,
										lineNumbers: true,
										theme: "base16-dark"
									});
									editor.setSize(600, null);

									editor.on('change', () => {
										editor.save();
									});
								}
							});
						});
					});
				});
			});
		});
	});
}