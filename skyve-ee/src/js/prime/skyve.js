SKYVE = function() {
	// public
	return {
		getById: function(id) {
			return $(PrimeFaces.escapeClientId(id));
		},
		
		contentOverlayOnShow: function(id, url) {
			SKYVE.getById(id + '_iframe').attr('src', url);
		},
		
		contentOverlayOnHide: function(id) {
			SKYVE.getById(id + '_iframe').attr('src','')
		},
		
		afterContentUpload: function(binding, contentId, modoc, fileName) {
			top.$('[id$="_' + binding + '"]').val(contentId);
			var url = 'content?_n=' + contentId + '&_doc=' + modoc + '&_b=' + binding + '&_ctim=' + new Date().getTime();
			top.$('[id$="_' + binding + '_link"]').attr('href', url).text(fileName);
			top.$('[id$="_' + binding + '_image"]').attr('src', url);
			top.PF(binding + 'Overlay').hide();
		},
		
		clearContentImage: function(binding) {
			$('[id$="_' + binding + '"]').val('');
			$('[id$="_' + binding + '_image"]').attr('src','images/blank.gif');
		},
		
		clearContentLink: function(binding) {
			$('[id$="_' + binding + '"]').val('');
			$('[id$="_' + binding + '_link"]').attr('href','javascript:void(0)').text('<Empty>');
		},
		
		getTextElement: function(id) {
			return SKYVE.getById(id);
		},

		getTextValue: function(id) {
			return SKYVE.getTextElement(id).val();
		},
		
		setTextValue: function(id, value) {
			SKYVE.getTextElement(id).val(value);
		},

		getPasswordElement: function(id) {
			return SKYVE.getById(id + 'password');
		},
		
		getPasswordValue: function(id) {
			return SKYVE.getPasswordElement(id).val();
		},
		
		setPasswordValue: function(id, value) {
			SKYVE.getPasswordElement(id).val(value);
		},

		// for selecting values and getting the selected value, use the PF SelectOneMenu API through widgetVar
		getComboElement: function(id) {
			return SKYVE.getById(id);
		},

		// to perform a lookup, use the AutoComplete API through widgetVar
		getLookupElement: function(id) {
			return SKYVE.getById(id);
		},
		
		getLookupValue: function(id) {
			return SKYVE.getById(id + '_hinput').val();
		},
		
		setLookupValue: function(id, value) {
			SKYVE.getById(id + '_hinput').val(value);
		},
		
		getLookupDescription: function(id) {
			return SKYVE.getById(id + '_input').val();
		},
		
		setLookupDescription: function(id, value) {
			SKYVE.getById(id + '_input').val(value);
		},
		
		getCheckboxElement: function(id) {
			return SKYVE.getById(id);
		},
		
		getCheckboxValue: function(id) {
			var value = SKYVE.getById(id + '_input').val();
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
				return SKYVE.getById(id + '_input').is(":checked");
			}
		},
		
		setCheckboxValue: function(id, trueOrFalse) {
			SKYVE.getById(id + '_input').prop('checked', trueOrFalse);

			var outerDiv = SKYVE.getById(id);
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

        toggleFilters: function(dataTableId) {
            var hiddenClass = 'hiddenFilter';
			var dataTable = $('#' + dataTableId);
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
			}
		}
	};
}();
