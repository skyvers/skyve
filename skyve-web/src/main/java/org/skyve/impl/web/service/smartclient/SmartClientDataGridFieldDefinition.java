package org.skyve.impl.web.service.smartclient;

import org.skyve.impl.metadata.model.document.field.Field;
import org.skyve.impl.metadata.view.widget.bound.input.HTML;
import org.skyve.impl.metadata.view.widget.bound.input.InputWidget;
import org.skyve.impl.metadata.view.widget.bound.input.LookupDescription;
import org.skyve.impl.metadata.view.widget.bound.input.RichText;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Relation;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.util.BeanValidator;
import org.skyve.util.OWASP;
import org.skyve.util.Util;

/**
 * SmartClient data-grid field definition.
 */
public class SmartClientDataGridFieldDefinition extends SmartClientAttributeDefinition {
    protected boolean editable;
    protected String defaultValueJavascriptExpression;

	/**
	 * Builds a SmartClient data-grid field definition from widget and document metadata.
	 *
	 * @param user active user used for localisation and metadata rules
	 * @param customer active customer metadata
	 * @param module module containing the target document
	 * @param document target document metadata
	 * @param widget source widget metadata
	 * @param dataGridBindingOverride optional binding override, or {@code null}
	 * @param hasFormatter whether a display formatter is applied
	 * @param runtime whether runtime domain values should be resolved
	 * @param isField whether the definition is generated for a field context
	 * @param uxui active UX/UI profile name
	 */
    protected SmartClientDataGridFieldDefinition(User user,
			    									Customer customer, 
			                                        Module module, 
			                                        Document document, 
			                                        InputWidget widget,
			                                        String dataGridBindingOverride,
			                                        boolean hasFormatter,
			                                        boolean runtime,
			                                        boolean isField,
			                                        String uxui) {
		super(user,
				customer,
				module,
				document,
				(dataGridBindingOverride == null) ? widget.getBinding() : dataGridBindingOverride,
				null,
				runtime,
				false,
				isField,
				uxui);
		setHasDisplayField(hasFormatter);
		
		// for datagrids, ensure that enum types are text so that valueMaps don't have to be set all the time.
		if ("enum".equals(type)) {
			type = "text";
		}
        Attribute attribute = target.getAttribute();

        if (attribute instanceof Field field) {
            // determine the defaultValue expression for the list grid
        	defaultValueJavascriptExpression = field.getDefaultValue();
        	if (defaultValueJavascriptExpression != null) {
        		AttributeType attributeType = attribute.getAttributeType();
				if (AttributeType.date.equals(attributeType) || 
						AttributeType.dateTime.equals(attributeType) || 
						AttributeType.time.equals(attributeType) || 
						AttributeType.timestamp.equals(attributeType)) {
					defaultValueJavascriptExpression = new StringBuilder(128).append("isc.DateUtil.parseSchemaDate('").append(defaultValueJavascriptExpression).append("')").toString();
				}
				else if (! (AttributeType.bool.equals(attributeType) || 
								AttributeType.integer.equals(attributeType) ||
								AttributeType.longInteger.equals(attributeType))) {
					defaultValueJavascriptExpression = new StringBuilder(128).append('\'').append(defaultValueJavascriptExpression).append('\'').toString();
				}
        	}
        }
        
        if ((attribute instanceof Relation relation) && (widget instanceof LookupDescription lookupDescription)) { // widget could be a combo for instance
        	editorType = "comboBox";
        	lookup = new SmartClientLookupDefinition(dataGridBindingOverride != null,
        												user,
        												customer,
        												module,
        												document,
        												relation,
        												lookupDescription,
        												runtime,
        												uxui);
        }

        // By default a SmartClientDataGridDefinition sets memo fields to a text area.
		if ((attribute != null) && AttributeType.memo.equals(attribute.getAttributeType())) {
			if ((widget instanceof RichText) ||
					(widget instanceof HTML)) {
				editorType = null;
			}
		}
    }

	/**
	 * Indicates whether the field can be edited in the SmartClient grid.
	 *
	 * @return {@code true} when the field can be edited
	 */
	public boolean getEditable() {
		return editable;
	}

	/**
	 * Sets whether the field can be edited in the SmartClient grid.
	 *
	 * @param editable whether the field can be edited
	 */
	public void setEditable(boolean editable) {
		this.editable = editable;
	}

	/**
	 * Produces the SmartClient JavaScript field definition payload for this data-grid field.
	 *
	 * @return SmartClient JavaScript field definition payload
	 */
	public String toJavascript() {
		StringBuilder result = new StringBuilder(128);

		result.append("name:'");
		result.append(name);
		result.append("',title:'");
		result.append(OWASP.escapeJsString(title));
		result.append("',type:'");
		result.append(type).append('\'');
		if (defaultValueJavascriptExpression != null) {
			result.append(",defaultValue:").append(defaultValueJavascriptExpression);
		}
		if (editorType != null) {
			result.append(",editorType:'").append(editorType).append('\'');
		}
		appendEditorProperties(result, true, null, null);
		if (required) {
			result.append(",bizRequired:true,requiredMessage:'");
			if (requiredMessage == null) {
				result.append(OWASP.escapeJsString(Util.nullSafeI18n(BeanValidator.VALIDATION_REQUIRED_KEY, title)));
			}
			else {
				result.append(OWASP.escapeJsString(requiredMessage));
			}
			result.append('\'');
		}
		if (valueMap != null) {
			result.append(",valueMap:").append(getValueMapAsString());
		}
		if (align != null) {
			result.append(",align:'").append(align.toTextAlignmentString()).append('\'');
		}
		if (length != null) {
			result.append(",length:").append(length);
		}
		if (! editable) {
			result.append(",canEdit:false");
		}
		if (pixelWidth != null) {
			result.append(",width:").append(pixelWidth);
		}
		if (escape) {
			result.append(",escapeHTML:true");
		}
//TODO fix this
//result.append(",changed:'alert(item.grid.getSelectedRecord().bizId)'");

		return result.toString();
	}
}
