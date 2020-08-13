/**
 * Styles an output HTML element based on text based on the specified input element 
 * defined by the smart client variable and the binding.
 * 
 * @param scVar The smart client variable assigned to the form containing the input textArea, e.g. 'v1'
 * @param binding The binding of the input textArea, e.g. 'template'
 * @param outputElementSelector The CSS selector to the output element, e.g. '#output'
 * 
 * @see https://www.horuskol.net/blog/2020-03-05/live-code-highlighting-in-the-browser-with-vanilla-javascript/
 * @see https://github.com/highlightjs/highlight.js/blob/master/LICENSE
 */
function styleOutput(scVar, binding, outputElementSelector) {
	SKYVE.Util.loadCSS('pages/css/report-style.css?v=' + SKYVE.Util.v, function() {
		SKYVE.Util.loadCSS('pages/css/highlight-9.18.1-vs2015.css?v=' + SKYVE.Util.v, function() {
			SKYVE.Util.loadJS('pages/js/highlight.pack-9.18.1.js?v=' + SKYVE.Util.v, function() {
				var templateId = null;
				if(scVar.items) {
					for(var i=0, l=scVar.items.length; i < l; i++) {
						var item = scVar.items[i];
						if(item.name === binding) {
							// grab the SC id for this field
							// console.log('item', item);
							if(item['$14y']) {
								templateId = item['$14y'];
							}
							break;
						}
					}
				}
				
				if(templateId !== null) {
					var el = document.querySelector('#'+templateId);
					// console.log('#'+templateId, el);
					if(el) {
						el.className = el.className + " input";
						el.setAttribute("aria-controls", "code-highlighter");
						el.setAttribute("autocapitalize", "off");
						el.setAttribute("spellcheck", "false");
						
						const codeInput = el;
						const codeOutput = document.querySelector(outputElementSelector);
						
						// initialise the highlighted output with whatever is in the input
						codeOutput.textContent = codeInput.value;
						hljs.highlightBlock(codeOutput);
						
						codeInput.addEventListener('input', (event) => {
						  codeOutput.textContent = codeInput.value;
						  hljs.highlightBlock(codeOutput);
						});
						
						codeInput.addEventListener('scroll', (event) => {
						  codeOutput.scrollTop = codeInput.scrollTop;
						  codeOutput.scrollLeft = codeInput.scrollLeft;
						});
					}
				}
			});
		});
	});
}