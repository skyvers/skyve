package org.skyve.impl.generate;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.types.Decimal;
import org.skyve.domain.types.converters.Converter;
import org.skyve.domain.types.converters.Format.TextCase;
import org.skyve.domain.types.converters.decimal.Decimal2Integer;
import org.skyve.domain.types.converters.decimal.Decimal2IntegerPercentage;
import org.skyve.domain.types.converters.decimal.Decimal2OneDecimalPlace;
import org.skyve.domain.types.converters.decimal.Decimal2TwoDecimalPlacesPercentage;
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
import org.skyve.impl.metadata.model.document.field.Field;
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
import org.skyve.impl.metadata.repository.module.MetaDataQueryContentColumnMetaData.DisplayType;
import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.impl.metadata.view.widget.bound.input.CheckBox;
import org.skyve.impl.metadata.view.widget.bound.input.HTML;
import org.skyve.impl.metadata.view.widget.bound.input.InputWidget;
import org.skyve.impl.metadata.view.widget.bound.input.Lookup;
import org.skyve.impl.metadata.view.widget.bound.input.LookupDescription;
import org.skyve.impl.metadata.view.widget.bound.input.Radio;
import org.skyve.impl.metadata.view.widget.bound.input.RichText;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.document.Association;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.metadata.model.document.Collection;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.DomainType;
import org.skyve.metadata.model.document.Reference;
import org.skyve.metadata.model.document.Relation;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.module.query.MetaDataQueryProjectedColumn;
import org.skyve.metadata.module.query.MetaDataQueryColumn;
import org.skyve.metadata.module.query.MetaDataQueryContentColumn;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.metadata.view.widget.bound.FilterParameter;
import org.skyve.metadata.view.widget.bound.Parameter;
import org.skyve.util.Binder;
import org.skyve.util.Binder.TargetMetaData;
import org.skyve.util.Util;

public class SmartClientGenerateUtils {
	private SmartClientGenerateUtils() {
		// nothing to do
	}

    public static class SmartClientLookupDefinition {
    	private boolean bindingToDataGrid;
    	// The data source for the drop down box
    	private String optionDataSource;
        private String displayField;
        // List Grid fields for option data source
        private List<String> pickListFields = new ArrayList<>();
        // Filter fields for option data source
        private List<String> filterFields = new ArrayList<>();
        private MetaDataQueryDefinition query;
        private boolean canCreate = true;
        private boolean canUpdate = true;
        
		SmartClientLookupDefinition(boolean bindingToDataGrid,
        								User user,
        								Customer customer,
        								Module module,
        								Document document,
        								Relation relation,
        								LookupDescription lookup,
        								boolean runtime) {
            this.bindingToDataGrid = bindingToDataGrid;
            String queryName = (lookup == null) ? null : lookup.getQuery();
            // Use reference query name if none provided in lookup
            if ((queryName == null) && (relation instanceof Reference)) {
            	queryName = ((Reference) relation).getQueryName();
            }
			// Use the default query if none is defined, else get the named query.
            if (queryName == null) {
            	query = module.getDocumentDefaultQuery(customer, relation.getDocumentName());
            	queryName = query.getName();
            }
            else {
            	query = module.getMetaDataQuery(queryName);
            }
            
            StringBuilder sb = new StringBuilder(128);
            sb.append(module.getName()).append('_').append(queryName).append('_');
            sb.append(document.getName()).append('_').append(relation.getName());
            optionDataSource = sb.toString();

            String descriptionBinding = (lookup == null) ? null : lookup.getDescriptionBinding();
            displayField = (descriptionBinding == null) ? 
            					Bean.BIZ_KEY : 
        						BindUtil.sanitiseBinding(descriptionBinding);

            Document queryDocument = module.getDocument(customer, query.getDocumentName());
            
            if (user != null) {
	            canCreate = user.canCreateDocument(queryDocument);
	            canUpdate = user.canUpdateDocument(queryDocument);
            }
            
            Set<String> dropDownColumns = (lookup == null) ? null : lookup.getDropDownColumns();
            if ((dropDownColumns == null) || dropDownColumns.isEmpty()) {
            	pickListFields.add(displayField);
            }
            else {
                for (MetaDataQueryColumn column : query.getColumns()) {
                	String alias = column.getName();
                	if (alias == null) {
                		alias = column.getBinding();
                	}
                	if (dropDownColumns.contains(alias)) {
                		if ((column instanceof MetaDataQueryProjectedColumn) &&
                				((MetaDataQueryProjectedColumn) column).isProjected()) {
                            SmartClientQueryColumnDefinition def = SmartClientGenerateUtils.getQueryColumn(user,
                        																					customer, 
																	                                        module,
																	                                        queryDocument,
																	                                        column,
																	                                        runtime);

                        	pickListFields.add(def.getName());
                        	// only add fields that can use the substring operator
                        	if (def.getHasTextFilterOperators()) {
                        		filterFields.add(def.getName());
                        	}
                		}
                	}
                }
            }
        }
        
		public String getDisplayField() {
            return displayField;
        }

        public void setDisplayField(String displayField) {
            this.displayField = displayField;
        }

        public String getOptionDataSource() {
            return optionDataSource;
        }

        public void setOptionDataSource(String optionDataSource) {
            this.optionDataSource = optionDataSource;
        }

        public List<String> getPickListFields() {
            return pickListFields;
        }

        public List<String> getFilterFields() {
            return filterFields;
        }

        public MetaDataQueryDefinition getQuery() {
            return query;
        }

        public void setQuery(MetaDataQueryDefinition query) {
            this.query = query;
        }

		public boolean isBindingToDataGrid() {
			return bindingToDataGrid;
		}

		public void setBindingToDataGrid(boolean bindingToDataGrid) {
			this.bindingToDataGrid = bindingToDataGrid;
		}

		public boolean getCanCreate() {
			return canCreate;
		}

		public void setCanCreate(boolean canCreate) {
			this.canCreate = canCreate;
		}

		public boolean getCanUpdate() {
			return canUpdate;
		}

		public void setCanUpdate(boolean canUpdate) {
			this.canUpdate = canUpdate;
		}
    }

	protected static class SmartClientAttributeDefinition {
        protected SmartClientLookupDefinition lookup;
        protected Locale locale;
		protected String name;
		protected String title;
		protected String type = "text";
		protected String editorType;
		protected String filterEditorType;
		protected Integer length;
		protected String mask;
		protected String textBoxStyle;
		protected String validation;
		protected String valueMap;
		protected boolean required = false;
		protected boolean triStateCheckBox = false;
		protected TargetMetaData target;
		
		@SuppressWarnings("synthetic-access")
		protected SmartClientAttributeDefinition(User user,
													Customer customer, 
													Module module,
													Document document,
													Locale locale,
													String binding,
													String name,
													boolean runtime) {
			this.locale = locale;
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
						title = bindingDocument.getSingularAlias();
					}
					else {
						title = DocumentImpl.getBizKeyAttribute().getDisplayName();
					}
				}
				else if (binding.endsWith(Bean.ORDINAL_NAME)) {
					title = DocumentImpl.getBizOrdinalAttribute().getDisplayName();
				}
			}
			
			if ((bindingDocument != null) && (bindingAttribute != null)) {
				title = bindingAttribute.getDisplayName();
				required = bindingAttribute.isRequired();

				DomainType domainType = bindingAttribute.getDomainType();
				if (domainType != null) {
					// constant domain types
					if (DomainType.constant.equals(domainType)) {
						valueMap = getConstantDomainValueMapString(customer, bindingDocument, bindingAttribute, locale, runtime);
					}
					else { // variant or dynamic
						// this valueMap will be replaced in client logic but this defn ensures that the
						// select widget doesn't try to use the form's data source to get values when opened
						valueMap = "[' ']"; 
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
						determineMaskAndStyle(text, this);
						TextValidator validator = text.getValidator();
						if (validator != null) {
							@SuppressWarnings("unchecked")
							Converter<String> converter = (Converter<String>) text.getConverterForCustomer(customer); 
							StringBuilder sb = new StringBuilder(128);
							// NB don't use processString for regular expression as \n could be a valid part of the expression and needs to remain
							sb.append("{expression:'").append(validator.getRegularExpression().replace("\\", "\\\\").replace("'", "\\'"));
							sb.append("',type:'regexp',errorMessage:'");
							sb.append(processString(validator.constructMessage(user, title, converter)));
							sb.append("'}");
							validation = sb.toString();
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
								sb.append("min:Date.parseSchemaDate('").append(Binder.convert(java.util.Date.class, min)).append("'),");
							}
							java.util.Date max = dateValidator.getMax();
							if (max != null) {
								sb.append("max:Date.parseSchemaDate('").append(Binder.convert(java.util.Date.class, max)).append("'),");
							}
							sb.append("type:'dateRange',errorMessage:'");
							sb.append(processString(dateValidator.constructMessage(user, title, converter)));
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
							sb.append(processString(decimalValidator.constructMessage(user, title, converter)));
							sb.append("'}");

							Integer precision = decimalValidator.getPrecision();
							if (precision != null) {
								sb.append(",{precision:").append(precision).append(",roundToPrecision:true,type:'floatPrecision',errorMessage:'");
								sb.append(processString(decimalValidator.constructMessage(user, title, converter)));
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
							sb.append(processString(integerValidator.constructMessage(user, title, converter)));
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
							sb.append(processString(longValidator.constructMessage(user, title, converter)));
							sb.append("'}");

							validation = sb.toString();
						}
					}
					catch (Exception e) {
						throw new MetaDataException("Could not convert min/max values to javascript validator.", e);
					}
				}
				
				AttributeType attributeType = bindingAttribute.getAttributeType();
				Converter<?> converter = null;
				if (bindingAttribute instanceof LengthField) {
					LengthField field = (LengthField) bindingAttribute;
					length = new Integer(field.getLength());
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
					else if (converter instanceof Decimal2TwoDecimalPlacesPercentage) {
						type = "bizTwoDecimalPlacesPercentage";
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
					type = "bizUpload";
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

		public String getValueMap() {
			return valueMap;
		}
		
		void setMask(String mask) {
			this.mask = mask;
		}

		void setTextBoxStyle(String textBoxStyle) {
			this.textBoxStyle = textBoxStyle;
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
										Integer pixelWidth,
										Integer pixelHeight,
										String emptyThumbnailRelativeFile) {
			if (lookup == null) {
				if (triStateCheckBox) {
					result.append(",editorProperties:{allowEmptyValue:true}");
				}
				if ((! required) && ("select".equals(type) || "enum".equals(type))) {
					result.append(",editorProperties:{allowEmptyValue:true}");
				}
				else if ("geometry".equals(type)) {
	            	result.append(",formatCellValue:function(v){return isc.GeometryItem.format(v)}");
				}
				else if ("image".equals(type)) { // content thumbnail column
	            	result.append(",formatCellValue:function(v,rec,row,col){if(v){var u='content?_n='+v+'");
	            	result.append("&_doc='+rec.bizModule+'.'+rec.bizDocument+'&_b=").append(BindUtil.unsanitiseBinding(name));
            		result.append("';return '<a href=\"'+u+'\" target=\"_blank\"><img src=\"'+u+'");
            		result.append("&_w=").append((pixelWidth == null) ? 
													((pixelHeight == null) ? "64" : pixelHeight.toString()) :
													pixelWidth.toString());
            		result.append("&_h=").append((pixelHeight == null) ? 
													((pixelWidth == null) ? "64" : pixelWidth.toString()) : 
													pixelHeight.toString());
            		result.append("\"/></a>'}if(rec && rec.bizId){return '");
            		if (emptyThumbnailRelativeFile == null) {
            			result.append("'}");
            		}
            		else {
            			result.append("<img src=\"resources?_n=").append(emptyThumbnailRelativeFile);
    	            	result.append("&_doc='+rec.bizModule+'.'+rec.bizDocument+");
                		result.append("'&_w=").append((pixelWidth == null) ? 
    													((pixelHeight == null) ? "64" : pixelHeight.toString()) :
    													pixelWidth.toString());
                		result.append("&_h=").append((pixelHeight == null) ? 
    													((pixelWidth == null) ? "64" : pixelWidth.toString()) : 
    													pixelHeight.toString());
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
//	            	result.append(",changed:'if(this.grid){var r=this.grid.getRecord(this.rowNum);if(r){r.");
//		            String referenceBinding = bindingToDataGrid ? Bean.DOCUMENT_ID : name;
//	            	result.append(referenceBinding);
//		            result.append("=this.getValue();r.");
//		            if (! bindingToDataGrid) {
//		                result.append(name).append('_');
//		            }
//		            result.append(lookup.getDisplayField());
//		            result.append("=this.getDisplayValue();return r.").append(referenceBinding).append(";}}'");
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
	}

	public static class SmartClientDataGridFieldDefinition extends SmartClientAttributeDefinition {
        protected HorizontalAlignment align;
        protected boolean editable;
        protected Integer pixelWidth;
        protected String defaultValueJavascriptExpression;

        SmartClientDataGridFieldDefinition(User user,
        									Customer customer, 
                                            Module module, 
                                            Document document, 
                                            InputWidget widget,
                                            String dataGridBindingOverride,
                                            boolean runtime) {
            super(user,
        			customer,
                    module, 
                    document, 
                    user.getLocale(),
                    (dataGridBindingOverride == null) ? widget.getBinding() : dataGridBindingOverride,
            		null,
            		runtime);
            // for datagrids, ensure that enum types are text so that valueMaps don't have to be set all the time.
			if ("enum".equals(type)) {
				type = "text";
			}
            Attribute attribute = target.getAttribute();

            if (attribute instanceof Field) {
                // determine the defaultValue expression for the list grid
            	defaultValueJavascriptExpression = ((Field) attribute).getDefaultValue();
            	if (defaultValueJavascriptExpression != null) {
            		AttributeType attributeType = attribute.getAttributeType();
    				if (AttributeType.date.equals(attributeType) || 
    						AttributeType.dateTime.equals(attributeType) || 
    						AttributeType.time.equals(attributeType) || 
    						AttributeType.timestamp.equals(attributeType)) {
    					defaultValueJavascriptExpression = new StringBuilder(128).append("Date.parseSchemaDate('").append(defaultValueJavascriptExpression).append("')").toString();
    				}
    				else if (! (AttributeType.bool.equals(attributeType) || 
									AttributeType.integer.equals(attributeType) ||
									AttributeType.longInteger.equals(attributeType))) {
    					defaultValueJavascriptExpression = new StringBuilder(128).append('\'').append(defaultValueJavascriptExpression).append('\'').toString();
    				}
            	}
            }
            
            if ((attribute instanceof Relation) && (widget instanceof LookupDescription)) { // widget could be a combo for instance
            	editorType = "comboBox";
            	lookup = new SmartClientLookupDefinition(dataGridBindingOverride != null,
            												user,
            												customer,
            												module,
            												document,
            												(Relation) attribute,
            												(LookupDescription) widget,
            												runtime);
            }

            // By default a SmartClientDataGridDefinition sets memo fields to a text area.
			if ((attribute != null) && AttributeType.memo.equals(attribute.getAttributeType())) {
				if ((widget instanceof RichText) ||
						(widget instanceof HTML)) {
					editorType = null;
				}
			}

			// set the default alignment
			if (attribute != null) {
				AttributeType attributeType = attribute.getAttributeType();
				align = determineDefaultColumnAlignment(attributeType);
				pixelWidth = determineDefaultColumnWidth(attributeType);
			}
        }

        public HorizontalAlignment getAlign() {
			return align;
		}

		public void setAlign(HorizontalAlignment align) {
			this.align = align;
		}

		public boolean getEditable() {
			return editable;
		}

		public void setEditable(boolean editable) {
			this.editable = editable;
		}
		
		public Integer getPixelWidth() {
			return pixelWidth;
		}

		public void setPixelWidth(Integer pixelWidth) {
			this.pixelWidth = pixelWidth;
		}

		public String toJavascript() {
            StringBuilder result = new StringBuilder(128);

            result.append("name:'");
            result.append(name);
            result.append("',title:'");
            result.append(processString(Util.i18n(title, locale)));
            result.append("',type:'");
            result.append(type).append('\'');
            if (defaultValueJavascriptExpression != null) {
				result.append(",defaultValue:").append(defaultValueJavascriptExpression);
            }
            if (editorType != null) {
                result.append(",editorType:'").append(editorType).append('\'');
            }
            appendEditorProperties(result, true, pixelWidth, null, null);
            if (required) {
            	result.append(",bizRequired:true,requiredMessage:'").append(processString(Util.i18n(title, locale))).append(' ');
            	result.append(processString(Util.i18n("is required", locale))).append(".'");
            }
            if (valueMap != null) {
                result.append(",valueMap:").append(valueMap);
            }
            if (align != null) {
            	result.append(",align:'").append(align.toAlignmentString()).append('\'');
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
// TODO fix this
//result.append(",changed:'alert(item.grid.getSelectedRecord().bizId)'");
            
            return result.toString();
        }
    }
	
	public static class SmartClientFieldDefinition extends SmartClientDataGridFieldDefinition {
		private HorizontalAlignment textAlign;
		private String helpText;

		SmartClientFieldDefinition(User user,
									Customer customer, 
									Module module, 
									Document document, 
									InputWidget widget,
									boolean runtime) {
			super(user, customer, module, document, widget, null, runtime);
			Attribute attribute = target.getAttribute();
			if (attribute != null) {
				helpText = attribute.getDescription();
				if (AttributeType.time.equals(attribute.getAttributeType())) {
					textAlign = HorizontalAlignment.right;
				}
			}
			// Use a drop down for grids but in the edit view, use the radio group as specified
			if (widget instanceof Radio) {
				editorType = null; // is set to "select" in the SmartClientDataGridFieldDefinition
			}
			// Use a combo box for grids but in the edit view, use the lookup description as specified
			if (widget instanceof LookupDescription) {
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
            result.append(processString(Util.i18n(title, locale)));
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
            	result.append(",bizRequired:true,requiredMessage:'").append(processString(Util.i18n(title, locale))).append(' ');
            	result.append(processString(Util.i18n("is required", locale))).append(".'");
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
			
			if (textAlign != null) {
				result.append(",textAlign:'").append(textAlign.toAlignmentString()).append('\'');
			}
			
		    if (helpText != null) {
				result.append(",icons:[{src:'icons/help.png',tabIndex:-1,showOver:true,neverDisable:true,prompt:'");
				result.append(processString(Util.i18n(helpText, locale), false, true));
				result.append("',click:function(){isc.say(this.prompt, null, {title:'").append(processString(Util.i18n(title, locale))).append("'})}}]");
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

	public static class SmartClientQueryColumnDefinition extends SmartClientAttributeDefinition {
		private boolean canFilter = true;
		private boolean canSave = true;
		private boolean detail = false;
		private boolean canSortClientOnly = false;
		private boolean onlyEqualsFilterOperators = false;
		private boolean hasTextFilterOperators = false;
		protected Integer pixelWidth;
		protected Integer pixelHeight;
		protected String emptyThumbnailRelativeFile;
		protected HorizontalAlignment align;
		
		SmartClientQueryColumnDefinition(User user,
											Customer customer, 
											Module module, 
											Document document, 
											MetaDataQueryColumn column,
											boolean runtime) {
			super(user,
					customer, 
					module,
					document,
					(user == null) ? null : user.getLocale(),
					column.getBinding(),
					column.getName(),
					runtime);
			String displayName = column.getDisplayName();
			if (displayName != null) {
				title = displayName;
			}
			align = column.getAlignment();
			pixelWidth = column.getPixelWidth();

			Attribute attribute = (target != null) ? target.getAttribute() : null;
			if (attribute != null) {
				DomainType domainType = attribute.getDomainType();
				if (domainType != null) {
						onlyEqualsFilterOperators = true;
					
					if (DomainType.variant.equals(domainType) || 
							DomainType.dynamic.equals(domainType)) {
						canSave = false;
					}
				}
				else {
					AttributeType attributeType = attribute.getAttributeType();
					hasTextFilterOperators = AttributeType.text.equals(attributeType) ||
												AttributeType.memo.equals(attributeType) || 
												AttributeType.markup.equals(attributeType) ||
												AttributeType.colour.equals(attributeType);
					if (attribute instanceof Text) {
						Text text = (Text) attribute;
						determineMaskAndStyle(text, this);
					}
					if (align == null) {
						align = determineDefaultColumnAlignment(attributeType);
					}
					if (pixelWidth == null) {
						pixelWidth = determineDefaultColumnWidth(attributeType);
					}
				}
				if (attribute instanceof Association) {
					String targetDocumentName = ((Association) attribute).getDocumentName();
					Document targetDocument = module.getDocument(customer, targetDocumentName);
					Persistent targetPersistent = targetDocument.getPersistent();
					if (targetPersistent.getName() != null) { // this is a persistent target document - not a mapped document
						type = "text";
						editorType = "comboBox";
						lookup = new SmartClientLookupDefinition(false,
																	user,
																	customer,
																	module,
																	document,
																	(Relation) attribute,
																	null,
																	runtime);
					}
				}
			}

			detail = column.isHidden();
			if (column instanceof MetaDataQueryProjectedColumn) {
				MetaDataQueryProjectedColumn projectedColumn = (MetaDataQueryProjectedColumn) column;
				canFilter = projectedColumn.isFilterable();
				canSortClientOnly = (! projectedColumn.isSortable());
				canSave = canSave && projectedColumn.isEditable();
			}
			else if (column instanceof MetaDataQueryContentColumn) {
				MetaDataQueryContentColumn contentColumn = (MetaDataQueryContentColumn) column;
				canFilter = false;
				canSortClientOnly = false;
				canSave = false;
				if (DisplayType.thumbnail.equals(contentColumn.getDisplay())) {
					type = "image";
				}
				else {
					type = "link";
				}
				pixelWidth = contentColumn.getPixelWidth();
				pixelHeight = contentColumn.getPixelHeight();
				emptyThumbnailRelativeFile = contentColumn.getEmptyThumbnailRelativeFile();
			}
		}

		public boolean isCanFilter() {
			return canFilter;
		}

		public void setCanFilter(boolean canFilter) {
			this.canFilter = canFilter;
		}

		public boolean isCanSave() {
			return canSave;
		}

		public void setCanSave(boolean canSave) {
			this.canSave = canSave;
		}

		public boolean isCanSortClientOnly() {
			return canSortClientOnly;
		}

		public void setCanSortClientOnly(boolean canSortClientOnly) {
			this.canSortClientOnly = canSortClientOnly;
		}

		public boolean isDetail() {
			return detail;
		}

		public void setDetail(boolean detail) {
			this.detail = detail;
		}

        public HorizontalAlignment getAlign() {
			return align;
		}

		public void setAlign(HorizontalAlignment align) {
			this.align = align;
		}
	
		public Integer getPixelWidth() {
			return pixelWidth;
		}

		public void setPixelWidth(Integer pixelWidth) {
			this.pixelWidth = pixelWidth;
		}

		public Integer getPixelHeight() {
			return pixelHeight;
		}

		public void setPixelHeight(Integer pixelHeight) {
			this.pixelHeight = pixelHeight;
		}

		public String getEmptyThumbnailRelativeFile() {
			return emptyThumbnailRelativeFile;
		}

		public void setEmptyThumbnailRelativeFile(String emptyThumbnailRelativeFile) {
			this.emptyThumbnailRelativeFile = emptyThumbnailRelativeFile;
		}

		public String getMask() {
			return mask;
		}

		public boolean getHasTextFilterOperators() {
			return hasTextFilterOperators;
		}
		
		public String toJavascript() {
			StringBuilder result = new StringBuilder(64);

			result.append("name:'");
			result.append(name);
			result.append("',title:'");
			result.append(processString(Util.i18n(title, locale)));
			result.append("',type:'");
			result.append(type);
			if (editorType != null) {
				result.append("',editorType:'").append(editorType);
			}
			if (filterEditorType != null) {
				result.append("',filterEditorType:'").append(filterEditorType);
			}
			if (length != null) {
				result.append("',length:").append(length);
			}
			else {
				result.append('\'');
			}
			appendEditorProperties(result, false, pixelWidth, pixelHeight, emptyThumbnailRelativeFile);
			if (valueMap != null) {
				result.append(",valueMap:").append(valueMap);
			}
			if (required) {
            	result.append(",bizRequired:true,requiredMessage:'").append(processString(Util.i18n(title, locale))).append(' ');
            	result.append(processString(Util.i18n("is required", locale))).append(".'");
			}
			if (! canFilter) {
				result.append(",canFilter:false");
			}
			if (! canSave) {
				result.append(",canSave:false");
			}
			if (detail) {
				result.append(",detail:true");
			}
			if (canSortClientOnly) {
				// TODO should use the listgridcolumn to set sorting off
				result.append(",canSortClientOnly:true");
			}
            if (align != null) {
            	result.append(",align:'").append(align.toAlignmentString()).append('\'');
            }
            if (pixelWidth != null) {
            	result.append(",width:").append("image".equals(type) ? pixelWidth.intValue() + 8 : pixelWidth.intValue());
            }
            else if ("image".equals(type)) {
            	if (pixelHeight != null) {
                	result.append(",width:").append(pixelHeight.intValue() + 8);
            	}
            	else {
            		result.append(",width:72"); // 64 + 8
            	}
            }
			if (onlyEqualsFilterOperators) {
				result.append(",validOperators:['equals','notEqual','isNull','notNull']");
			}
			else if ("geometry".equals(type)) {
				result.append(",validOperators:isc.GeometryItem.validOperators");
			}

			return result.toString();
		}
	}

	public static HorizontalAlignment determineDefaultColumnAlignment(AttributeType attributeType) {
		if (AttributeType.date.equals(attributeType) || 
				AttributeType.dateTime.equals(attributeType) ||
				AttributeType.time.equals(attributeType) || 
				AttributeType.timestamp.equals(attributeType) ||
				AttributeType.decimal2.equals(attributeType) || 
				AttributeType.decimal5.equals(attributeType) ||
				AttributeType.decimal10.equals(attributeType) || 
				AttributeType.integer.equals(attributeType) ||
				AttributeType.longInteger.equals(attributeType)) {
			return HorizontalAlignment.right;
		}
		if (AttributeType.bool.equals(attributeType) || 
				AttributeType.content.equals(attributeType)) {
			return HorizontalAlignment.centre;
		}
		return HorizontalAlignment.left;
	}
	
	public static Integer determineDefaultColumnWidth(AttributeType attributeType) {
		if (AttributeType.date.equals(attributeType)) {
			return Integer.valueOf(100);
		}
		if (AttributeType.dateTime.equals(attributeType)) {
			return Integer.valueOf(125);
		}
		if (AttributeType.time.equals(attributeType)) {
			return Integer.valueOf(75);
		}
		if (AttributeType.timestamp.equals(attributeType)) {
			return Integer.valueOf(125);
		}
		if (AttributeType.bool.equals(attributeType)) {
			return Integer.valueOf(75);
		}

		return null;
	}
	
	private static String getConstantDomainValueMapString(Customer customer,
															Document document,
															Attribute attribute,
															Locale locale,
															boolean runtime) {
		List<DomainValue> values = ((DocumentImpl) document).getDomainValues((CustomerImpl) customer, 
																				DomainType.constant, 
																				attribute, 
																				null,
																				runtime);
		
		StringBuilder sb = new StringBuilder(64);
		sb.append('{');
		for (DomainValue value : values) {
			sb.append('\'').append(value.getCode()).append("':'");
			sb.append(processString(Util.i18n(value.getDescription(), locale))).append("',");
		}
		if (values.isEmpty()) { // no values
			sb.append('}');
		}
		else {
			sb.setCharAt(sb.length() - 1, '}'); // replace last comma
		}

		return sb.toString();
	}

	public static Map<String, String> getConstantDomainValueMap(User user,
																	Document document,
																	Attribute attribute,
																	boolean runtime) {
		Locale locale = user.getLocale();
		List<DomainValue> values = ((DocumentImpl) document).getDomainValues((CustomerImpl) user.getCustomer(), 
																				DomainType.constant, 
																				attribute, 
																				null,
																				runtime);
		Map<String, String> result = new TreeMap<>(); 
		for (DomainValue value : values) {
			result.put(value.getCode(), processString(Util.i18n(value.getDescription(), locale)));
		}
		
		return result;
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
	static void determineMaskAndStyle(Text text, SmartClientAttributeDefinition def) {
		String mask = null;
		
		TextFormat format = text.getFormat();
		if (format != null) {
			mask = format.getMask();
			if (mask != null) {
				// first escape characters with meaning
				mask = mask.replace("0", "\\0");
				mask = mask.replace("9", "\\9");
				mask = mask.replace("?", "\\?");
				mask = mask.replace("a", "\\a");
				mask = mask.replace("C", "\\C");
				mask = mask.replace("<", "\\<");
				mask = mask.replace(">", "\\>");
			}
			
			TextCase textCase = format.getCase();
			if (TextCase.capital.equals(textCase)) {
				if (mask == null) {
					def.setTextBoxStyle("textItem bizhubTextCapital");
				}
				else {
					int lIndex = mask.indexOf('L');
					if (lIndex >= 0) {
						mask = new StringBuilder(mask).replace(lIndex, lIndex + 1, ">L<").toString();
					}
				}
			}
			else if (TextCase.lower.equals(textCase)) {
				if (mask == null) {
					def.setTextBoxStyle("textItem bizhubTextLower");
				}
				else {
					mask = '<' + mask;
				}
			}
			else if (TextCase.upper.equals(textCase)) {
				if (mask == null) {
					def.setTextBoxStyle("textItem bizhubTextUpper");
				}
				else {
					mask = '>' + mask;
				}
			}
		}
		def.setMask(mask);
	}
	
	public static SmartClientQueryColumnDefinition getQueryColumn(User user,
																	Customer customer,
																	Module module,
																	Document document,
																	MetaDataQueryColumn column,
																	boolean runtime) {
		return new SmartClientQueryColumnDefinition(user, customer, module, document, column, runtime);
	}

	/**
	 * Get the smart client field definition given the widget/binding.
	 * If bindingOverride is defined, it will be used to determine the field to use.
	 * bindingOverride is used when a Datagrid has a lookupDescription which has no binding.
	 * That is, the lookupDescription is for the entire dataGrid entity.
	 * 
	 * @param customer
	 * @param module
	 * @param document
	 * @param widget	The widget metadata to use to define the smart client form field
	 * @param bindingOverride	If defined, specifies a different binding to use.
	 * @return
	 */
	public static SmartClientFieldDefinition getField(User user,
														Customer customer, 
														Module module, 
														Document document, 
														InputWidget widget,
														boolean runtime) {
		return new SmartClientFieldDefinition(user, customer, module, document, widget, runtime);
	}
	
    public static SmartClientDataGridFieldDefinition getDataGridField(User user,
    																	Customer customer, 
                                                                        Module module, 
                                                                        Document document, 
                                                                        InputWidget widget,
                                                                        String dataGridBinding,
                                                                        boolean runtime) {
    	return new SmartClientDataGridFieldDefinition(user, customer, module, document, widget, dataGridBinding, runtime);
    }

    /**
     * Appends a data source definition from a document list model.
     * @param user
     * @param customer
     * @param owningModule
     * @param owningDocument
     * @param modelName
     * @param config	Whether to create a partial config data source defn for the menu items
     * @param toAppendTo	definition is appended to this
     * @param visitedQueryNames
     * @return	The ID for the query definition generated.
     */
	public static String appendDataSourceDefinition(User user,
														Customer customer,
														Module owningModule,
														Document owningDocument,
														String modelName,
														boolean config,
														StringBuilder toAppendTo,
														Set<String> visitedQueryNames) {
		ListModel<Bean> model = CORE.getRepository().getListModel(customer, owningDocument, modelName, true);
		Document drivingDocument = model.getDrivingDocument();
		Module drivingDocumentModule = customer.getModule(drivingDocument.getOwningModuleName());

		return appendDataSourceDefinition(user, 
											customer,
											owningModule.getName(),
											owningDocument,
											drivingDocumentModule,
											drivingDocument,
											null,
											modelName,
											model.getDescription(),
											model.getColumns(),
											null, 
											null, 
											config, 
											toAppendTo, 
											visitedQueryNames);
	}
	
	/**
     * Appends a data source definition from a module query.
     * @param customer
     * @param query
     * @param dataSourceIDOverride	ID of created data source if mandated
     * @param hiddenBindings	Extra bindings to include in the data source - not mandatory
     * @param config	Whether to create a partial config data source defn for the menu items
     * @param toAppendTo	definition is appended to this
     * @param visitedQueryNames
     * @return	The ID for the query definition generated.
     */
	public static String appendDataSourceDefinition(User user,
														Customer customer,
														MetaDataQueryDefinition query,
														String dataSourceIDOverride,
														Lookup forLookup,
														boolean config,
														StringBuilder toAppendTo,
														Set<String> visitedQueryNames) {
		String documentName = query.getDocumentName();
		Module documentModule = query.getDocumentModule(customer);
		Module owningModule = query.getOwningModule();
		Document drivingDocument = documentModule.getDocument(customer, documentName);
		return appendDataSourceDefinition(user, 
											customer, 
											owningModule.getName(), 
											drivingDocument,
											documentModule, 
											drivingDocument, 
											query.getName(), 
											null, 
											query.getDescription(),
											query.getColumns(),
											dataSourceIDOverride, 
											forLookup, 
											config, 
											toAppendTo, 
											visitedQueryNames);
	}
	
	@SuppressWarnings("null")
	private static String appendDataSourceDefinition(User user,
														Customer customer,
														String owningModuleName,
														Document owningDocument,
														Module drivingDocumentModule,
														Document drivingDocument,
														String queryName,
														String modelName,
														String description,
														List<MetaDataQueryColumn> columns,
														String dataSourceIDOverride,
														Lookup forLookup,
														// indicates that this is for configuration in the harness page
														boolean config,
														StringBuilder toAppendTo,
														Set<String> visitedQueryNames) {
		Locale locale = user.getLocale();
		
		// dataSourceId -> defn
		Map<String, String> childDataSources = new TreeMap<>();
		
		String drivingDocumentName = drivingDocument.getName();
		String dataSourceId = null;
		if (dataSourceIDOverride != null) {
			dataSourceId = dataSourceIDOverride;
		}
		else if (queryName != null) {
			dataSourceId = new StringBuilder(32).append(owningModuleName).append('_').append(queryName).toString();
		}
		else if (modelName != null) {
			// NB 4 tokens, not 3
			dataSourceId = new StringBuilder(32).append(owningModuleName).append('_').append(owningDocument.getName()).append("__").append(modelName).toString();
		}
		if (visitedQueryNames == null) {
			toAppendTo.append("if(window.").append(dataSourceId);
			toAppendTo.append("){}else{isc.RestDataSource.create({dataFormat:'json',dataURL:'smartlist',");
			toAppendTo.append("operationBindings:[{operationType:'fetch',dataProtocol:'postParams'},");
			toAppendTo.append("{operationType:'update',dataProtocol:'postParams'},");
			toAppendTo.append("{operationType:'add',dataProtocol:'postParams'},");
			toAppendTo.append("{operationType:'remove',dataProtocol:'postParams'}],");
		}
		else {
			if (visitedQueryNames.contains(dataSourceId)) {
				return dataSourceId;
			}
			toAppendTo.append('{');
		}
		toAppendTo.append("ID:'").append(dataSourceId);
		toAppendTo.append("',modoc:'");
		toAppendTo.append(drivingDocumentModule.getName());
		toAppendTo.append('.');
		toAppendTo.append(drivingDocumentName);
		String icon32 = drivingDocument.getIcon32x32RelativeFileName();
		if (icon32 != null) {
			toAppendTo.append("',icon:'").append(icon32);
		}
		else {
			String icon = drivingDocument.getIconStyleClass();
			if (icon != null) {
				toAppendTo.append("',fontIcon:'").append(icon);
			}
		}
		if (! config) {
			// ensure all filtering is server-side
			// this enables the summary row to always stay in sync and
			// lookups to drop down with the same criteria but load from the server
			// NB _drop is set to true in bizLookupDescription.showPicker() JS.
			toAppendTo.append("',compareCriteria:function(newCriteria,oldCriteria,requestProperties,policy){if(this._drop){return -1;}else{return this.Super('compareCriteria',arguments)}}");
			toAppendTo.append(",_drop:false");
			toAppendTo.append(",transformResponse:function(dsResponse,dsRequest,data){this._drop=false;return this.Super('transformResponse',arguments)}");
			toAppendTo.append(",criteriaPolicy:'dropOnChange");
		}
		toAppendTo.append("',canCreate:").append(user.canCreateDocument(drivingDocument));
		toAppendTo.append(",canUpdate:").append(user.canUpdateDocument(drivingDocument));
		toAppendTo.append(",canDelete:").append(user.canDeleteDocument(drivingDocument));
		toAppendTo.append(",title:'");
		toAppendTo.append(processString(Util.i18n(description, locale)));
		toAppendTo.append("',fields:[");

		if (! config) {
			toAppendTo.append("{name:'bizTagged',title:'");
			toAppendTo.append(processString(Util.i18n("Tag", locale), false, true));
			toAppendTo.append("',type:'boolean',validOperators:['equals']},");
			toAppendTo.append("{name:'bizFlagComment',title:'");
			toAppendTo.append(processString(Util.i18n("Flag", locale), false, true));
			toAppendTo.append("'},"); //,length:1024} long length makes filter builder use a text area
		}
		
		if (drivingDocumentName.equals(drivingDocument.getParentDocumentName())) { // hierarchical
			toAppendTo.append("{name:'bizParentId',title:'Parent ID',type:'text',hidden:true,foreignKey:'");
			toAppendTo.append(dataSourceId).append(".bizId'},");
		}
		
		List<String> hiddenBindingsList = new ArrayList<>();
		if (forLookup instanceof LookupDescription) {
			hiddenBindingsList.add(((LookupDescription) forLookup).getDescriptionBinding());
		}
		if (forLookup != null) {
			List<FilterParameter> parameters = forLookup.getParameters();
			if (parameters != null) {
				for (Parameter parameter : parameters) {
					hiddenBindingsList.add(parameter.getName());
				}
			}
		}
		
		for (MetaDataQueryColumn column : columns) {
			if ((column instanceof MetaDataQueryProjectedColumn) && 
					(! ((MetaDataQueryProjectedColumn) column).isProjected())) {
				continue;
			}

			SmartClientQueryColumnDefinition def = getQueryColumn(user, customer, drivingDocumentModule, drivingDocument, column, true);
			toAppendTo.append('{').append(def.toJavascript()).append("},");
			SmartClientLookupDefinition lookup = def.getLookup();
			if (lookup != null) {
				StringBuilder childDataSourceDefinition = new StringBuilder(512);
				String childDataSourceId = appendDataSourceDefinition(user,
																		customer,
																		lookup.getQuery(),
																		lookup.getOptionDataSource(),
																		null,
																		config,
																		childDataSourceDefinition,
																		visitedQueryNames);
				childDataSources.put(childDataSourceId, childDataSourceDefinition.toString());
			}
			
			if (hiddenBindingsList != null) {
				hiddenBindingsList.remove(column.getBinding());
			}
		}
		
		if (! config) {
			// for filtering
			toAppendTo.append("{name: 'operator', type: 'text', hidden: true},");
			toAppendTo.append("{name: 'criteria', type: 'text', hidden: true},");

			// standard for all rows
			toAppendTo.append("{name: 'bizId', primaryKey: true, hidden: true},");
			toAppendTo.append("{name:'bizLock', hidden: true},");
		}
		
		for (String hiddenBinding : hiddenBindingsList) {
			toAppendTo.append("{name:'").append(BindUtil.sanitiseBinding(hiddenBinding));
			toAppendTo.append("',type:'text',hidden:true},");
		}

		if (toAppendTo.charAt(toAppendTo.length() - 1) == ',') { // if we have a comma then we at least have a field in the data source
			toAppendTo.setLength(toAppendTo.length() - 1); // remove the last field comma
		}
		toAppendTo.append("]");
		if (visitedQueryNames == null) {
			toAppendTo.append("});}\n");
		}
		else {
			toAppendTo.append("},\n");
			visitedQueryNames.add(dataSourceId);
		}
		
		// Add any child datasources found
		for (String childDataSourceDefinition : childDataSources.values()) {
			toAppendTo.append(childDataSourceDefinition).append('\n');
		}
		
		return dataSourceId;
	}
	
	public static String processString(String value) {
		return processString(value, true, true);
	}
	
	public static String processString(String value, boolean escapeDoubleQuotes, boolean escapeNewLines) {
		if (value == null) {
			return null;
		}

		String result = value.replace("\\", "\\\\").replace("'", "\\'");
		if (escapeDoubleQuotes) {
			result = result.replace("\"", "&quot;");
		}
		if (escapeNewLines) {
			result = result.replace("\n", "<br/>");
		}
		else {
			result = result.replace("\n", "");
		}
		return result;
	}
}
