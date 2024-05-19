package org.skyve.impl.web.service.smartclient;

import java.util.List;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.types.Decimal;
import org.skyve.domain.types.converters.Converter;
import org.skyve.domain.types.converters.Format.TextCase;
import org.skyve.domain.types.converters.decimal.Decimal10TwoDecimalPlaces;
import org.skyve.domain.types.converters.decimal.Decimal2Integer;
import org.skyve.domain.types.converters.decimal.Decimal2IntegerPercentage;
import org.skyve.domain.types.converters.decimal.Decimal2OneDecimalPlace;
import org.skyve.domain.types.converters.decimal.Decimal5Integer;
import org.skyve.domain.types.converters.decimal.Decimal5IntegerPercentage;
import org.skyve.domain.types.converters.decimal.Decimal5OneDecimalPlace;
import org.skyve.domain.types.converters.decimal.Decimal5TimeDuration;
import org.skyve.domain.types.converters.decimal.Decimal5TwoDecimalPlaces;
import org.skyve.domain.types.converters.decimal.Decimal5TwoDecimalPlacesPercentage;
import org.skyve.domain.types.converters.decimal.currency.Decimal2DollarsAndCents;
import org.skyve.domain.types.converters.decimal.currency.Decimal2DollarsAndCentsAbsolute;
import org.skyve.domain.types.converters.decimal.currency.Decimal5DollarsAndCents;
import org.skyve.domain.types.converters.integer.IntegerSeparator;
import org.skyve.domain.types.converters.integer.LongIntegerSeparator;
import org.skyve.domain.types.converters.integer.SimplePercentage;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.model.document.field.ConvertableField;
import org.skyve.impl.metadata.model.document.field.Date;
import org.skyve.impl.metadata.model.document.field.DateTime;
import org.skyve.impl.metadata.model.document.field.Decimal10;
import org.skyve.impl.metadata.model.document.field.Decimal2;
import org.skyve.impl.metadata.model.document.field.Decimal5;
import org.skyve.impl.metadata.model.document.field.LengthField;
import org.skyve.impl.metadata.model.document.field.LongInteger;
import org.skyve.impl.metadata.model.document.field.Text;
import org.skyve.impl.metadata.model.document.field.TextFormat;
import org.skyve.impl.metadata.model.document.field.Time;
import org.skyve.impl.metadata.model.document.field.Timestamp;
import org.skyve.impl.metadata.model.document.field.validator.DateValidator;
import org.skyve.impl.metadata.model.document.field.validator.DecimalValidator;
import org.skyve.impl.metadata.model.document.field.validator.IntegerValidator;
import org.skyve.impl.metadata.model.document.field.validator.LongValidator;
import org.skyve.impl.metadata.model.document.field.validator.TextValidator;
import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.impl.metadata.view.widget.bound.input.CheckBox;
import org.skyve.impl.metadata.view.widget.bound.input.InputWidget;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.controller.Customisations;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.metadata.model.document.Collection;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.DomainType;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.util.Binder;
import org.skyve.util.Binder.TargetMetaData;
import org.skyve.util.OWASP;

public class SmartClientAttributeDefinition {
    protected SmartClientLookupDefinition lookup;
	protected String name;
	protected String title;
	protected String type = "text";
	protected String editorType;
	protected String filterEditorType;
	protected boolean hasDisplayField = false;
	protected Integer length;
	protected String mask;
	protected String textBoxStyle;
	protected String validation;
	protected String valueMap;
	protected boolean required = false;
	protected boolean triStateCheckBox = false;
    protected boolean escape = true;
    protected Integer pixelWidth;
    protected HorizontalAlignment align;
	protected TargetMetaData target;
	
	protected SmartClientAttributeDefinition(User user,
												Customer customer, 
												Module module,
												Document document,
												String binding,
												String name,
												boolean runtime,
												boolean isQueryColumn,
												String uxui) {
		this.name = (name != null) ? name : BindUtil.sanitiseBinding(binding);
		title = this.name;

		Document bindingDocument = null;
		Attribute bindingAttribute = null;
		if (binding != null) {
			this.target = BindUtil.getMetaDataForBinding(customer, 
															module, 
															document, 
															binding);
			bindingDocument = target.getDocument();
			bindingAttribute = target.getAttribute();

			if (binding.endsWith(Bean.BIZ_KEY)) {
				if (bindingDocument != null) {
					title = bindingDocument.getLocalisedSingularAlias();
				}
				else {
					title = DocumentImpl.getBizKeyAttribute().getLocalisedDisplayName();
				}
				align = HorizontalAlignment.left;
			}
			else if (binding.endsWith(Bean.ORDINAL_NAME)) {
				title = DocumentImpl.getBizOrdinalAttribute().getLocalisedDisplayName();
				align = HorizontalAlignment.right;
			}
		}
		
		if ((bindingDocument != null) && (bindingAttribute != null)) {
			AttributeType attributeType = bindingAttribute.getAttributeType();

			title = bindingAttribute.getLocalisedDisplayName();
			required = bindingAttribute.isRequired();

			// set the default alignment and pixelWidth
			Customisations customisations = CORE.getCustomisations();
			align = customisations.determineDefaultTextAlignment(uxui, attributeType);
			pixelWidth = customisations.determineDefaultColumnWidth(uxui, attributeType);

			DomainType domainType = bindingAttribute.getDomainType();
			if (domainType != null) {
				// constant domain types
				if (DomainType.constant.equals(domainType)) {
					valueMap = getConstantDomainValueMapString(customer, bindingDocument, bindingAttribute, runtime);
				}
				else {
					// if this is an enumeration on a query column defn, ensure the filter has all values
					if (isQueryColumn && bindingAttribute.getAttributeType() == AttributeType.enumeration) {
						valueMap = getConstantDomainValueMapString(customer, bindingDocument, bindingAttribute, runtime);
					}
					// this valueMap will be replaced in client logic but this defn ensures that the
					// select widget doesn't try to use the form's data source to get values when opened
					else {
						valueMap = "[' ']";
					}
					
					if (domainType == DomainType.variant || domainType == DomainType.dynamic) {
						hasDisplayField = true;
					}
				}
				type = "enum";
			}
			else {
				DateValidator dateValidator = null;
				DecimalValidator decimalValidator = null;
				IntegerValidator integerValidator = null;
				LongValidator longValidator = null;

				if (bindingAttribute instanceof Text) {
					Text text = (Text) bindingAttribute;
					setMaskAndStyle(text);
					TextValidator validator = text.getValidator();
					if (validator != null) {
						@SuppressWarnings("unchecked")
						Converter<String> converter = (Converter<String>) text.getConverterForCustomer(customer); 
						String regex = validator.getRegularExpression();
						if (regex != null) {
							StringBuilder sb = new StringBuilder(128);
							// NB don't use processString for regular expression as \n could be a valid part of the expression and needs to remain
							sb.append("{expression:'").append(regex.replace("\\", "\\\\").replace("'", "\\'"));
							sb.append("',type:'regexp',errorMessage:'");
							sb.append(OWASP.escapeJsString(validator.constructMessage(user, title, converter)));
							sb.append("'}");
							validation = sb.toString();
						}
					}
				}
				else if (bindingAttribute instanceof Date) {
					dateValidator = ((Date) bindingAttribute).getValidator();
				}
				else if (bindingAttribute instanceof DateTime) {
					dateValidator = ((DateTime) bindingAttribute).getValidator();
				}
				else if (bindingAttribute instanceof Time) {
					dateValidator = ((Time) bindingAttribute).getValidator();
				}
				else if (bindingAttribute instanceof Timestamp) {
					dateValidator = ((Timestamp) bindingAttribute).getValidator();
				}
				else if (bindingAttribute instanceof Decimal2) {
					decimalValidator = ((Decimal2) bindingAttribute).getValidator();
				}
				else if (bindingAttribute instanceof Decimal5) {
					decimalValidator = ((Decimal5) bindingAttribute).getValidator();
				}
				else if (bindingAttribute instanceof Decimal10) {
					decimalValidator = ((Decimal10) bindingAttribute).getValidator();
				}
				else if (bindingAttribute instanceof org.skyve.impl.metadata.model.document.field.Integer) {
					integerValidator = ((org.skyve.impl.metadata.model.document.field.Integer) bindingAttribute).getValidator();
				}
				else if (bindingAttribute instanceof LongInteger) {
					longValidator = ((LongInteger) bindingAttribute).getValidator();
				}
				
				try {
					if (dateValidator != null) {
						@SuppressWarnings("unchecked")
						Converter<java.util.Date> converter = (Converter<java.util.Date>) ((ConvertableField) bindingAttribute).getConverterForCustomer(customer); 
						StringBuilder sb = new StringBuilder(128);
						sb.append('{');
						java.util.Date min = dateValidator.getMin();
						if (min != null) {
							sb.append("min:isc.DateUtil.parseSchemaDate('").append(Binder.convert(java.util.Date.class, min)).append("'),");
						}
						java.util.Date max = dateValidator.getMax();
						if (max != null) {
							sb.append("max:isc.DateUtil.parseSchemaDate('").append(Binder.convert(java.util.Date.class, max)).append("'),");
						}
						sb.append("type:'dateRange',errorMessage:'");
						sb.append(OWASP.escapeJsString(dateValidator.constructMessage(user, title, converter)));
						sb.append("'}");

						validation = sb.toString();
					}
					else if (decimalValidator != null) {
						@SuppressWarnings("unchecked")
						Converter<Decimal> converter = (Converter<Decimal>) ((ConvertableField) bindingAttribute).getConverterForCustomer(customer); 
						StringBuilder sb = new StringBuilder(128);
						sb.append('{');
						Decimal min = decimalValidator.getMin();
						if (min != null) {
							sb.append("min:").append(min).append(',');
						}
						Decimal max = decimalValidator.getMax();
						if (max != null) {
							sb.append("max:").append(max).append(',');
						}
						sb.append("type:'floatRange',errorMessage:'");
						sb.append(OWASP.escapeJsString(decimalValidator.constructMessage(user, title, converter)));
						sb.append("'}");

						Integer precision = decimalValidator.getPrecision();
						if (precision != null) {
							sb.append(",{precision:").append(precision).append(",roundToPrecision:true,type:'floatPrecision',errorMessage:'");
							sb.append(OWASP.escapeJsString(decimalValidator.constructMessage(user, title, converter)));
							sb.append("'}");
						}
						validation = sb.toString();
					}
					else if (integerValidator != null) {
						@SuppressWarnings("unchecked")
						Converter<Integer> converter = (Converter<Integer>) ((ConvertableField) bindingAttribute).getConverterForCustomer(customer); 
						StringBuilder sb = new StringBuilder(128);
						sb.append('{');
						Integer min = integerValidator.getMin();
						if (min != null) {
							sb.append("min:").append(min).append(',');
						}
						Integer max = integerValidator.getMax();
						if (max != null) {
							sb.append("max:").append(max).append(',');
						}
						sb.append("type:'integerRange',errorMessage:'");
						sb.append(OWASP.escapeJsString(integerValidator.constructMessage(user, title, converter)));
						sb.append("'}");

						validation = sb.toString();
					}
					else if (longValidator != null) {
						@SuppressWarnings("unchecked")
						Converter<Long> converter = (Converter<Long>) ((ConvertableField) bindingAttribute).getConverterForCustomer(customer); 
						StringBuilder sb = new StringBuilder(128);
						sb.append('{');
						Long min = longValidator.getMin();
						if (min != null) {
							sb.append("min:").append(min).append(',');
						}
						Long max = longValidator.getMax();
						if (max != null) {
							sb.append("max:").append(max).append(',');
						}
						sb.append("type:'integerRange',errorMessage:'");
						sb.append(OWASP.escapeJsString(longValidator.constructMessage(user, title, converter)));
						sb.append("'}");

						validation = sb.toString();
					}
				}
				catch (Exception e) {
					throw new MetaDataException("Could not convert min/max values to javascript validator.", e);
				}
			}
			
			Converter<?> converter = null;
			if (bindingAttribute instanceof LengthField) {
				LengthField field = (LengthField) bindingAttribute;
				length = Integer.valueOf(field.getLength());
			}
			else if (bindingAttribute instanceof ConvertableField) {
				ConvertableField field = (ConvertableField) bindingAttribute;
				converter = field.getConverterForCustomer(customer);
			}
			else if (bindingAttribute instanceof Collection) {
				this.name = Bean.DOCUMENT_ID;
				type = "enum";
			}

			switch (attributeType) {
			case text:
				break; // already assigned at initialisation
			case memo:
				type = "text";
				editorType = "TextAreaItem";
				break;
			case markup:
				type = "richText";
				filterEditorType = "text";
				break;
			case bool:
				type = "boolean";
				InputWidget diw = bindingAttribute.getDefaultInputWidget();
				if (diw instanceof CheckBox) {
					triStateCheckBox = (! Boolean.FALSE.equals(((CheckBox) diw).getTriState()));
				}
				break;
			case colour:
				type = "text";
				editorType = "ColorItem";
				break;
			case decimal2:
				type = "bizDecimal2";
				if (converter instanceof Decimal2DollarsAndCents) {
					type = "bizDollarsAndCents";
				}
				else if (converter instanceof Decimal2DollarsAndCentsAbsolute) {
					type = "bizDollarsAndCents";
				}
				else if (converter instanceof Decimal2Integer) {
					type = "bizDecimal0";
				}
				else if (converter instanceof Decimal2IntegerPercentage) {
					type = "bizIntegerPercentage";
				}
				else if (converter instanceof Decimal2OneDecimalPlace) {
					type = "bizDecimal1";
				}
				break;
			case decimal5:
				type = "bizDecimal5";
				if (converter instanceof Decimal5DollarsAndCents) {
					type = "bizDollarsAndCents";
				}
				else if (converter instanceof Decimal5Integer) {
					type = "bizDecimal0";
				}
				else if (converter instanceof Decimal5IntegerPercentage) {
					type = "bizIntegerPercentage";
				}
				else if (converter instanceof Decimal5OneDecimalPlace) {
					type = "bizDecimal1";
				}
				else if (converter instanceof Decimal5TimeDuration) {
					type = "bizTimeDuration";
				}
				else if (converter instanceof Decimal5TwoDecimalPlacesPercentage) {
					type = "bizTwoDecimalPlacesPercentage";
				}
				else if (converter instanceof Decimal5TwoDecimalPlaces) {
					type = "bizDecimal2";
				}
				break;
			case decimal10:
				type = "bizDecimal10";
				if (converter instanceof Decimal10TwoDecimalPlaces) {
					type = "bizDecimal2";
				}
				break;
			case enumeration:
				type = "enum";
				editorType = "select";
				break;
			case integer:
			case longInteger:
				type = "integer";
				if (converter instanceof SimplePercentage) {
					type = "bizIntegerPercentage";
				}
				else if(converter instanceof IntegerSeparator){
					type = "bizIntegerSeparator";
				}
				else if(converter instanceof LongIntegerSeparator){
					type = "bizIntegerSeparator";
				}
				break;
			case date:
			case dateTime:
			case timestamp:
				type = (converter == null) ? "DD_MMM_YYYY" : converter.getClass().getSimpleName();
				break;
			case time:
				type = (converter == null) ? "HH24_MI" : converter.getClass().getSimpleName();
				break;
			case content:
			case image:
				// nothing yet
				break;
			case geometry:
				type = "geometry";
				break;
			case association:
				type = "enum";
				break;
			case collection:
				// nothing yet
				break;
			default:
			}
		}
	}

	public TargetMetaData getTarget() {
	    return target;
	}
	
	public String getEditorType() {
		return editorType;
	}

	public void setEditorType(String editorType) {
		this.editorType = editorType;
	}

    public SmartClientLookupDefinition getLookup() {
        return lookup;
    }

	public Integer getLength() {
		return length;
	}

	public void setLength(Integer length) {
		this.length = length;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public boolean isRequired() {
		return required;
	}

	public void setRequired(boolean required) {
		this.required = required;
	}

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	public String getType() {
		return type;
	}

	public void setType(String type) {
		this.type = type;
	}

	public boolean isHasDisplayField() {
		return hasDisplayField;
	}
	
	public void setHasDisplayField(boolean hasDisplayField) {
		this.hasDisplayField = hasDisplayField;
	}
	
	public boolean isEscape() {
		return escape;
	}

	public void setEscape(boolean escape) {
		this.escape = escape;
	}

	public String getValueMap() {
		return valueMap;
	}
	
	void setMask(String mask) {
		this.mask = mask;
	}

	void setTextBoxStyle(String textBoxStyle) {
		this.textBoxStyle = textBoxStyle;
	}
	
	public Integer getPixelWidth() {
		return pixelWidth;
	}

	public void setPixelWidth(Integer pixelWidth) {
		this.pixelWidth = pixelWidth;
	}

	public HorizontalAlignment getAlign() {
		return align;
	}

	public void setAlign(HorizontalAlignment align) {
		this.align = align;
	}

	/**
	 * My spec is
	 * A - alphanumeric
	 * # - digit
	 * L - letter
	 * 
	 * SC spec is 
	 * Character	Description
	 * 0	Digit (0 through 9) or plus [+] or minus [-] signs
	 * 9	Digit or space
	 * #	Digit
	 * L	Letter (A through Z)
	 * ?	Letter (A through Z) or space
	 * A	Letter or digit
	 * a	Letter or digit
	 * C	Any character or space
	 * <	Causes all characters that follow to be converted to lowercase
	 * >	Causes all characters that follow to be converted to uppercase
	 * 
	 * This method escapes anything that should be literal and then 
	 * converts the expression taking into consideration the case setting.
	 * 
	 * @param text
	 * @return
	 */
	public void setMaskAndStyle(Text text) {
		String result = null;
		
		TextFormat format = text.getFormat();
		if (format != null) {
			result = format.getMask();
			if (result != null) {
				// first escape characters with meaning
				result = result.replace("0", "\\0");
				result = result.replace("9", "\\9");
				result = result.replace("?", "\\?");
				result = result.replace("a", "\\a");
				result = result.replace("C", "\\C");
				result = result.replace("<", "\\<");
				result = result.replace(">", "\\>");
			}
			
			TextCase textCase = format.getCase();
			if (TextCase.capital.equals(textCase)) {
				if (result == null) {
					setTextBoxStyle("textItem bizhubTextCapital");
				}
				else {
					int lIndex = result.indexOf('L');
					if (lIndex >= 0) {
						result = new StringBuilder(result).replace(lIndex, lIndex + 1, ">L<").toString();
					}
				}
			}
			else if (TextCase.lower.equals(textCase)) {
				if (result == null) {
					setTextBoxStyle("textItem bizhubTextLower");
				}
				else {
					result = '<' + result;
				}
			}
			else if (TextCase.upper.equals(textCase)) {
				if (result == null) {
					setTextBoxStyle("textItem bizhubTextUpper");
				}
				else {
					result = '>' + result;
				}
			}
		}
		setMask(result);
	}
	
	// cannot get the filter row to either
	// 1) be a combo box in either the editor or the filter editor
	//		result.append("',editorType:'ComboBoxItem");
	//		the search string is not sent in the request unless
	//		result.append(",filterFields:['bizId', 'bizKey']");
	//		but including this has unexpected results also
	// 2) get the filter row to be a text field bound to the description binding of the field
	//		this hangs the UI when trying to display the list
	void appendEditorProperties(StringBuilder result,
									boolean forDataGrid,
									Integer pixelHeight,
									String emptyThumbnailRelativeFile) {
		if (lookup == null) {
			if (triStateCheckBox) {
				result.append(",editorProperties:{allowEmptyValue:true}");
			}
			else if ((! required) && ("select".equals(type) || "enum".equals(type))) {
				result.append(",editorProperties:{allowEmptyValue:true}");
			}
			else if ("geometry".equals(type)) {
            	result.append(",formatCellValue:function(v){return isc.GeometryItem.format(v)}");
			}
			else if ("image".equals(type)) { // content thumbnail column
				String wpx = (pixelWidth == null) ? 
								((pixelHeight == null) ? "64" : pixelHeight.toString()) :
								pixelWidth.toString();
				String hpx = (pixelHeight == null) ? 
								((pixelWidth == null) ? "64" : pixelWidth.toString()) : 
								pixelHeight.toString();
				result.append(",formatCellValue:function(v,rec,row,col){if(v){var u='content?_n='+v+'");
            	result.append("&_doc='+rec.bizModule+'.'+rec.bizDocument+'&_b=").append(BindUtil.unsanitiseBinding(name));
        		result.append("';return '<a href=\"'+u+'\" target=\"_blank\"><img src=\"'+u+'");
        		result.append("&_w=").append(wpx).append("&_h=").append(hpx);
        		result.append("\" style=\"width:").append(wpx).append("px;height:").append(hpx);
        		result.append("px:object-fit:contain\"/></a>'}if(rec && rec.bizId){return '");
        		if (emptyThumbnailRelativeFile == null) {
        			result.append("'}");
        		}
        		else {
        			result.append("<img src=\"resources?_n=").append(emptyThumbnailRelativeFile);
	            	result.append("&_doc='+rec.bizModule+'.'+rec.bizDocument+");
            		result.append("'&_w=").append(wpx).append("&_h=").append(hpx);
        			result.append("\"'}");
        		}
        		result.append("return ''}");
			}
			else if ("link".equals(type)) {
    			result.append(",formatCellValue:function(v,rec,row,col){return (v ? '<a href=\"content?_n='+v+'");
            	result.append("&_doc='+rec.bizModule+'.'+rec.bizDocument+'&_b=").append(BindUtil.unsanitiseBinding(name));
            	result.append("\" target=\"_blank\">Content</a>' : '')}");
			}
			else {
				if ((mask != null) || (textBoxStyle != null)) {
					result.append(",editorProperties:{");
					if (mask != null) {
						result.append("mask:'").append(mask).append("',maskSaveLiterals:true");
						if (textBoxStyle != null) {
							result.append(',');
						}
					}
					if (textBoxStyle != null) {
						 // trailing space coz SC adds more classes
						result.append("textBoxStyle:'").append(textBoxStyle).append(" '");
					}
					result.append('}');
				}
				if (validation != null) {
					result.append(",validators:[").append(validation).append(']');
				}
			}
			if (hasDisplayField) { // select or enum or text can have display fields
				result.append(",displayField:'_display_").append(name).append('\'');
			}
		}
		else {
        	boolean bindingToDataGrid = lookup.isBindingToDataGrid();

			result.append(",editorProperties:{optionDataSource:'").append(lookup.getOptionDataSource());
			result.append("',valueField:'").append(Bean.DOCUMENT_ID);
        	result.append("',displayField:'").append(lookup.getDisplayField());
        	result.append("',fetchMissingValues:false,autoFetchData:false,selectOnFocus:true,completeOnTab:true,textMatchStyle:'substring'");
        	if (! required) {
        		result.append(",allowEmptyValue:true");
        	}
        	
        	// defaultDynamicValue and value map processing
			result.append(",defaultDynamicValue:'if(this.grid){var r=this.grid.getRecord(this.rowNum);var v=(r?r.");
			if (! bindingToDataGrid) {
			    result.append(name);
			}
			else {
				result.append(Bean.DOCUMENT_ID);
			}
			result.append(":null);if(v){var vm=this.valueMap;vm[v]=r.");
			if (! bindingToDataGrid) {
				result.append(name).append('_');
			}
			result.append(lookup.getDisplayField());
			result.append(";return v;}}',valueMap:{}}");

            // changed
            if (forDataGrid) {
//            	result.append(",changed:'if(this.grid){var r=this.grid.getRecord(this.rowNum);if(r){r.");
//	            String referenceBinding = bindingToDataGrid ? Bean.DOCUMENT_ID : name;
//            	result.append(referenceBinding);
//	            result.append("=this.getValue();r.");
//	            if (! bindingToDataGrid) {
//	                result.append(name).append('_');
//	            }
//	            result.append(lookup.getDisplayField());
//	            result.append("=this.getDisplayValue();return r.").append(referenceBinding).append(";}}'");
            }
            else {
            	result.append(",filterEditorProperties:{optionDataSource:'").append(lookup.getOptionDataSource());
	        	result.append("',valueField:'").append(Bean.DOCUMENT_ID);
	        	result.append("',displayField:'").append(lookup.getDisplayField());
	        	result.append("',fetchMissingValues:false,autoFetchData:false,selectOnFocus:true,completeOnTab:true,textMatchStyle:'substring'}");
            }
            
            result.append(",sortByDisplayField:true,valueField:'");
            if (! bindingToDataGrid) {
                result.append(name);
            }
            else {
            	result.append(Bean.DOCUMENT_ID);
            }
        	result.append("',displayField:'");
        	if (! bindingToDataGrid) {
        		result.append(name).append('_');
        	}
        	result.append(lookup.getDisplayField());
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
    }

	private static String getConstantDomainValueMapString(Customer customer,
															Document document,
															Attribute attribute,
															boolean runtime) {
		List<DomainValue> values = ((DocumentImpl) document).getDomainValues((CustomerImpl) customer, 
																				DomainType.constant, 
																				attribute, 
																				null,
																				runtime);
		
		StringBuilder sb = new StringBuilder(64);
		sb.append('{');
		for (DomainValue value : values) {
			sb.append('\'').append(OWASP.escapeJsString(value.getCode())).append("':'");
			sb.append(OWASP.escapeJsString(value.getLocalisedDescription())).append("',");
		}
		if (values.isEmpty()) { // no values
			sb.append('}');
		}
		else {
			sb.setCharAt(sb.length() - 1, '}'); // replace last comma
		}

		return sb.toString();
	}
}
