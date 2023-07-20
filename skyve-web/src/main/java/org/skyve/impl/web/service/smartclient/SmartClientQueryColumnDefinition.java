package org.skyve.impl.web.service.smartclient;

import org.skyve.impl.metadata.model.document.field.Text;
import org.skyve.impl.metadata.repository.module.MetaDataQueryContentColumnMetaData.DisplayType;
import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.Association;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.DomainType;
import org.skyve.metadata.model.document.Relation;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryColumn;
import org.skyve.metadata.module.query.MetaDataQueryContentColumn;
import org.skyve.metadata.module.query.MetaDataQueryProjectedColumn;
import org.skyve.metadata.user.User;
import org.skyve.util.BeanValidator;
import org.skyve.util.OWASP;
import org.skyve.util.Util;

public class SmartClientQueryColumnDefinition extends SmartClientAttributeDefinition {
	private boolean canFilter = true;
	private boolean canSave = true;
	private boolean detail = false;
	private boolean canSortClientOnly = false;
	private String sortByField;
	private boolean onlyEqualsFilterOperators = false;
	private boolean onlyContainsFilterOperator = false;
	private boolean hasTextFilterOperators = false;
	protected Integer pixelHeight;
	protected String emptyThumbnailRelativeFile;
	
	protected SmartClientQueryColumnDefinition(User user,
												Customer customer, 
												Module module, 
												Document document, 
												MetaDataQueryColumn column,
												boolean runtime,
												String uxui) {
		super(user,
				customer, 
				module,
				document,
				column.getBinding(),
				column.getName(),
				runtime,
				true,
				uxui);
		String displayName = column.getLocalisedDisplayName();
		if (displayName != null) {
			title = displayName;
		}
		HorizontalAlignment columnAlignment = column.getAlignment();
		if (columnAlignment != null) {
			align = columnAlignment;
		}
		Integer columnPixelWidth = column.getPixelWidth();
		if (columnPixelWidth != null) {
			pixelWidth = columnPixelWidth;
		}
		escape = column.isEscape();

		Attribute attribute = (target != null) ? target.getAttribute() : null;
		if (attribute != null) {
			AttributeType attributeType = attribute.getAttributeType();
			DomainType domainType = attribute.getDomainType();
			if (domainType != null) {
				// Constant domains have a drop down as a filter
				if ((attributeType == AttributeType.enumeration) || (domainType == DomainType.constant)) {
					onlyEqualsFilterOperators = true;
				}
				// Variant and Dynamic domains use a text field
				else {
					canSave = false; // can't edit as it is the code anyway
					// reset to a text field as it was set to enum in SmartClientAttribute super constructor call
					valueMap = null; 
					type = "text";
					filterEditorType = "text";
					hasTextFilterOperators = true;
					sortByField = name; // sort by the code field, not _display_*
					// Dynamic domain values can't be filtered
					if (domainType == DomainType.dynamic) {
						canFilter = false;
					}
					// Variant domain values only has contain operator
					else {
						onlyContainsFilterOperator = true;
					}
				}
			}
			else {
				hasTextFilterOperators = AttributeType.text.equals(attributeType) ||
											AttributeType.memo.equals(attributeType) || 
											AttributeType.markup.equals(attributeType) ||
											AttributeType.colour.equals(attributeType);
				if (attribute instanceof Text) {
					Text text = (Text) attribute;
					setMaskAndStyle(text);
				}
				// Bindings directly to an association with no domain values
				// work similarly to a lookupDescription with editable="false"
				if (attribute instanceof Association) {
					String targetDocumentName = ((Association) attribute).getDocumentName();
					Document targetDocument = module.getDocument(customer, targetDocumentName);
					if (targetDocument.isPersistable()) { // this is a persistent target document - not a mapped document
						type = "text";
						editorType = "comboBox";
						lookup = new SmartClientLookupDefinition(false,
																	user,
																	customer,
																	module,
																	document,
																	(Relation) attribute,
																	null,
																	runtime,
																	uxui);
						onlyEqualsFilterOperators = true;
					}
				}
			}
		}

		detail = column.isHidden();
		if (column instanceof MetaDataQueryProjectedColumn) {
			MetaDataQueryProjectedColumn projectedColumn = (MetaDataQueryProjectedColumn) column;
			canFilter = canFilter && projectedColumn.isFilterable();
			canSortClientOnly = (! projectedColumn.isSortable());
			canSave = canSave && projectedColumn.isEditable();
		}
		else if (column instanceof MetaDataQueryContentColumn) {
			MetaDataQueryContentColumn contentColumn = (MetaDataQueryContentColumn) column;
			canFilter = false;
			canSortClientOnly = false;
			canSave = false;
			pixelWidth = contentColumn.getPixelWidth();
			pixelHeight = contentColumn.getPixelHeight();
			emptyThumbnailRelativeFile = contentColumn.getEmptyThumbnailRelativeFile();
			if (DisplayType.thumbnail.equals(contentColumn.getDisplay())) {
				type = "image";
				if (pixelHeight == null) {
					pixelHeight = Integer.valueOf(64);
				}
			}
			else {
				type = "link";
			}
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
		result.append(OWASP.escapeJsString(title));
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
		appendEditorProperties(result, false, pixelHeight, emptyThumbnailRelativeFile);
		if (valueMap != null) {
			result.append(",valueMap:").append(valueMap);
		}
		if (required) {
        	result.append(",bizRequired:true,requiredMessage:'");
        	result.append(OWASP.escapeJsString(Util.i18n(BeanValidator.VALIDATION_REQUIRED_KEY, title))).append('\'');
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
		if (sortByField != null) {
			result.append(",sortByField:'").append(sortByField).append('\'');
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
        if (escape) {
        	result.append(",escapeHTML:true");
        }
		if (onlyEqualsFilterOperators) {
			result.append(",validOperators:['equals','notEqual','isNull','notNull']");
		}
		else if (onlyContainsFilterOperator) {
			result.append(",validOperators:['iContains','isNull','notNull']");
		}
		else if ("geometry".equals(type)) {
			result.append(",validOperators:isc.GeometryItem.validOperators");
		}

		return result.toString();
	}
}
