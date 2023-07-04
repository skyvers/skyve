package org.skyve.impl.web.service.smartclient;

import java.util.List;

import org.skyve.domain.Bean;
import org.skyve.impl.metadata.view.widget.bound.input.InputWidget;
import org.skyve.impl.metadata.view.widget.bound.input.LookupDescription;
import org.skyve.impl.metadata.view.widget.bound.input.Radio;
import org.skyve.impl.metadata.view.widget.bound.input.TextField;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.util.BeanValidator;
import org.skyve.util.OWASP;
import org.skyve.util.Util;

public class SmartClientFieldDefinition extends SmartClientDataGridFieldDefinition {
	private String helpText;

	protected SmartClientFieldDefinition(User user,
											Customer customer, 
											Module module, 
											Document document, 
											InputWidget widget,
											boolean runtime) {
		super(user, customer, module, document, widget, null, runtime);
		Attribute attribute = target.getAttribute();

		if (attribute != null) {
			helpText = attribute.getLocalisedDescription();
		}
		
		// Use a drop down for grids but in the edit view, use the text field as specified
		if (widget instanceof TextField) {
			editorType = null; // is set to "select" in the SmartClientAttributeDefinition
			valueMap = null; // ensure there is no value map or SC will create a combo anyway
		}
		// Use a drop down for grids but in the edit view, use the radio group as specified
		else if (widget instanceof Radio) {
			editorType = null; // is set to "select" in the SmartClientDataGridFieldDefinition
		}
		// Use a combo box for grids but in the edit view, use the lookup description as specified
		else if (widget instanceof LookupDescription) {
			editorType = null; // is set to "comboBox" in the SmartClientDataGridFieldDefinition
		}
	}

	public String getHelpText() {
		return helpText;
	}

	public void setHelpText(String helpText) {
		this.helpText = helpText;
	}

	@Override
    public String toJavascript() {
        StringBuilder result = new StringBuilder(128);

        result.append("name:'");
        result.append(name);
        result.append("',title:'");
        result.append(OWASP.escapeJsString(title));
        result.append("',type:'");
        result.append(type);
        if (editorType != null) {
            result.append("',editorType:'").append(editorType);
        }
        if (length != null) {
            result.append("',length:").append(length);
        }
        else {
            result.append('\'');
        }
        if (valueMap != null) {
            result.append(",valueMap:").append(valueMap);
        }
        if (required) {
        	result.append(",bizRequired:true,requiredMessage:'");
        	result.append(OWASP.escapeJsString(Util.i18n(BeanValidator.VALIDATION_REQUIRED_KEY, title))).append('\'');
        }
        else {
            if ("select".equals(type)) {
                result.append(",allowEmptyValue:true");
            }
        }
        if (mask != null) {
			result.append(",mask:'").append(mask).append("',maskSaveLiterals:true");
        }
        if (textBoxStyle != null) {
        	 // trailing space coz SC adds more classes
        	result.append(",textBoxStyle:'").append(textBoxStyle).append(" '");
        }
		if (validation != null) {
			result.append(",validators:[").append(validation).append(']');
		}
		
		// This alignment here is text alignment, not the alignment within the form item which is handled in SmartClientViewRenderer.renderFormItem().
		if (align != null) {
			result.append(",textAlign:'").append(align.toAlignmentString()).append('\'');
		}

	    if (helpText != null) {
			result.append(",icons:[{src:'icons/help.png',tabIndex:-1,showOver:true,neverDisable:true,prompt:'");
			result.append(OWASP.escapeJsString(helpText, false, true));
			result.append("',click:function(){isc.say(this.prompt, null, {title:'").append(OWASP.escapeJsString(title)).append("'})}}]");
		}

	    if (lookup != null) {
            result.append(",optionDataSource:'").append(lookup.getOptionDataSource());
            result.append("',valueField:'").append(Bean.DOCUMENT_ID);
            result.append("',displayField:'").append(lookup.getDisplayField());
            result.append("',pickListFields:[");
            List<String> pickListFields = lookup.getPickListFields();
            for (String pickListField : pickListFields) {
                result.append("{name:'").append(pickListField).append("'},");
            }
            if (! pickListFields.isEmpty()) {
                result.setLength(result.length() - 1); // remove last pick list field comma
            }
            result.append(']');

            List<String> filterFields = lookup.getFilterFields();
            if (! filterFields.isEmpty()) {
            	result.append(",filterFields:[");
	            for (String filterField : filterFields) {
	                result.append('\'').append(filterField).append("',");
	            }
                result.setLength(result.length() - 1); // remove last pick list field comma
                result.append(']');
            }
        }

        return result.toString();
	}
}
