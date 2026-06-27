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
import org.skyve.util.Util;

/**
 * SmartClient form field definition.
 * This is used for both edit and grid fields, with some differences in editorType and valueMap handling as described in the constructor.
 */
public class SmartClientFieldDefinition extends SmartClientDataGridFieldDefinition {
	private String helpText;
	private boolean escapeTitle = true;
	private boolean escapeRequiredMessage = true;
	private boolean escapeHelp = true;

	/**
	 * Builds a SmartClient form-field definition from widget and document metadata.
	 *
	 * @param user active user
	 * @param customer active customer metadata
	 * @param module module containing the target document
	 * @param document target document metadata
	 * @param widget source widget metadata
	 * @param runtime whether runtime domain values should be resolved
	 * @param uxui active UX/UI profile name
	 */
	protected SmartClientFieldDefinition(User user,
											Customer customer, 
											Module module, 
											Document document, 
											InputWidget widget,
											boolean runtime,
											String uxui) {
		super(user, customer, module, document, widget, null, false, runtime, true, uxui);
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

	/**
	 * Returns the help text displayed as an inline help icon prompt.
	 *
	 * @return help text, or {@code null}
	 */
	public String getHelpText() {
		return helpText;
	}

	/**
	 * Sets the help text displayed as an inline help icon prompt.
	 *
	 * @param helpText help text
	 */
	public void setHelpText(String helpText) {
		this.helpText = helpText;
	}

	/**
	 * Sets whether the field title should be escaped before rendering.
	 *
	 * @param escapeTitle {@code true} to escape at the renderer boundary; {@code false} to allow trusted title markup
	 */
	@Override
	public void setEscapeTitle(boolean escapeTitle) {
		this.escapeTitle = escapeTitle;
	}

	/**
	 * Sets whether the required-message text should be escaped before rendering.
	 *
	 * @param escapeRequiredMessage {@code true} to escape at the renderer boundary; {@code false} to allow trusted message markup
	 */
	public void setEscapeRequiredMessage(boolean escapeRequiredMessage) {
		this.escapeRequiredMessage = escapeRequiredMessage;
	}

	/**
	 * Sets whether the help text should be escaped before rendering.
	 *
	 * @param escapeHelp {@code true} to escape at the renderer boundary; {@code false} to allow trusted help markup
	 */
	public void setEscapeHelp(boolean escapeHelp) {
		this.escapeHelp = escapeHelp;
	}

	/**
	 * Produces the SmartClient JavaScript field definition payload for this form field.
	 *
	 * @return SmartClient JavaScript field definition payload
	 */
	@Override
    @SuppressWarnings("java:S3776") // Complexity OK
    public String toJavascript() {
        StringBuilder result = new StringBuilder(128);

        result.append("name:'");
        result.append(name);
        result.append("',title:'");
        String ultimateTitle = SmartClientViewRenderer.escapeSmartClientText(title, escapeTitle);
        if (required) {
        	ultimateTitle += " *";
        }
        result.append(ultimateTitle);
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
            result.append(",valueMap:").append(getValueMapAsString());
        }
        if (required) {
        	result.append(",bizRequired:true,requiredMessage:'");
        	String ultimateRequiredMessage;
        	boolean ultimateEscapeRequiredMessage;
        	if (requiredMessage == null) {
        		ultimateRequiredMessage = Util.nullSafeI18n(BeanValidator.VALIDATION_REQUIRED_KEY, title);
        		ultimateEscapeRequiredMessage = true;
        	}
        	else {
        		ultimateRequiredMessage = requiredMessage;
        		ultimateEscapeRequiredMessage = escapeRequiredMessage;
        	}
    		result.append(SmartClientViewRenderer.escapeSmartClientText(ultimateRequiredMessage, ultimateEscapeRequiredMessage));
        	result.append('\'');
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
			// NB in SC textAlign affects the alignment of a checkbox in its item, so exclude for checkboxes
			if (! "checkbox".equals(type)) {
            	result.append(",textAlign:'").append(align.toTextAlignmentString()).append('\'');
            }
		}

	    if (helpText != null) {
			result.append(",icons:[{src:'icons/help.png',tabIndex:-1,showOver:true,neverDisable:true,prompt:'");
			result.append(SmartClientViewRenderer.escapeSmartClientText(helpText, escapeHelp));
			result.append("',click:function(){isc.say(this.prompt, null, {title:'").append(SmartClientViewRenderer.escapeSmartClientText(title, escapeTitle)).append("'})}}]");
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
