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
import org.skyve.util.Util;

/**
 * Encapsulates SmartClient query-column metadata for list-grid and repeater rendering.
 *
 * <p>Instances enrich the base attribute definition with query-specific contract data
 * such as filtering, sorting, detail-only rendering, and thumbnail handling.
 */
public class SmartClientQueryColumnDefinition extends SmartClientAttributeDefinition {
	private static final String IMAGE_TYPE = "image";
	private static final String TEXT_TYPE = "text";

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
	
	/**
	 * Builds a SmartClient query-column definition from metadata query column details.
	 *
	 * @param user active user
	 * @param customer active customer metadata
	 * @param module module containing the query/document metadata
	 * @param document driving document metadata
	 * @param column query column metadata
	 * @param runtime whether runtime domain values should be resolved
	 * @param uxui active UX/UI profile name
	 */
	@SuppressWarnings("java:S3776") // Complexity OK
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
				false,
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

		// Set up for formatted columns
		if (column instanceof MetaDataQueryProjectedColumn projectedColumn) {
			if ((projectedColumn.getFormatterName() != null) || (projectedColumn.getCustomFormatterName() != null)) {
				setHasDisplayField(true);
				sortByField = name; // sort by the code field, not _display_*
			}
		}
		
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
					type = TEXT_TYPE;
					filterEditorType = TEXT_TYPE;
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
				if (attribute instanceof Text text) {
					setMaskAndStyle(text);
				}
				// Bindings directly to an association with no domain values
				// work similarly to a lookupDescription with editable="false"
				if (attribute instanceof Association association) {
					String targetDocumentName = association.getDocumentName();
					Document targetDocument = module.getDocument(customer, targetDocumentName);
					if (targetDocument.isPersistable()) { // this is a persistent target document - not a mapped document
						type = TEXT_TYPE;
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
		if (column instanceof MetaDataQueryProjectedColumn projectedColumn) {
			canFilter = canFilter && projectedColumn.isFilterable();
			canSortClientOnly = (! projectedColumn.isSortable());
			canSave = canSave && projectedColumn.isEditable();
		}
		else if (column instanceof MetaDataQueryContentColumn contentColumn) {
			canFilter = false;
			canSortClientOnly = false;
			canSave = false;
			pixelWidth = contentColumn.getPixelWidth();
			pixelHeight = contentColumn.getPixelHeight();
			emptyThumbnailRelativeFile = contentColumn.getEmptyThumbnailRelativeFile();
			if (DisplayType.thumbnail.equals(contentColumn.getDisplay())) {
				type = IMAGE_TYPE;
				if (pixelHeight == null) {
					pixelHeight = Integer.valueOf(64);
				}
			}
			else {
				type = "link";
			}
		}
	}

	/**
	 * Indicates whether this query column can be filtered in the SmartClient list view.
	 *
	 * @return {@code true} when filtering is enabled
	 */
	public boolean isCanFilter() {
		return canFilter;
	}

	/**
	 * Sets whether this query column can be filtered in the SmartClient list view.
	 *
	 * @param canFilter whether filtering is enabled
	 */
	public void setCanFilter(boolean canFilter) {
		this.canFilter = canFilter;
	}

	/**
	 * Indicates whether this query column can be persisted from inline edits.
	 *
	 * @return {@code true} when inline saves are enabled
	 */
	public boolean isCanSave() {
		return canSave;
	}

	/**
	 * Sets whether this query column can be persisted from inline edits.
	 *
	 * @param canSave whether inline saves are enabled
	 */
	public void setCanSave(boolean canSave) {
		this.canSave = canSave;
	}

	/**
	 * Indicates whether this query column is sortable only on the client.
	 *
	 * @return {@code true} when only client-side sorting is available
	 */
	public boolean isCanSortClientOnly() {
		return canSortClientOnly;
	}

	/**
	 * Sets whether this query column is sortable only on the client.
	 *
	 * @param canSortClientOnly whether only client-side sorting is available
	 */
	public void setCanSortClientOnly(boolean canSortClientOnly) {
		this.canSortClientOnly = canSortClientOnly;
	}

	/**
	 * Indicates whether this query column is marked as detail-only.
	 *
	 * @return {@code true} when the column is detail-only
	 */
	public boolean isDetail() {
		return detail;
	}

	/**
	 * Sets whether this query column is marked as detail-only.
	 *
	 * @param detail whether the column is detail-only
	 */
	public void setDetail(boolean detail) {
		this.detail = detail;
	}

	/**
	 * Returns the configured pixel height for rendered content values.
	 *
	 * @return configured pixel height, or {@code null}
	 */
	public Integer getPixelHeight() {
		return pixelHeight;
	}

	/**
	 * Sets the configured pixel height for rendered content values.
	 *
	 * @param pixelHeight configured pixel height
	 */
	public void setPixelHeight(Integer pixelHeight) {
		this.pixelHeight = pixelHeight;
	}

	/**
	 * Returns the relative file used when thumbnail content is empty.
	 *
	 * @return relative file path for empty thumbnails, or {@code null}
	 */
	public String getEmptyThumbnailRelativeFile() {
		return emptyThumbnailRelativeFile;
	}

	/**
	 * Sets the relative file used when thumbnail content is empty.
	 *
	 * @param emptyThumbnailRelativeFile relative file path for empty thumbnails
	 */
	public void setEmptyThumbnailRelativeFile(String emptyThumbnailRelativeFile) {
		this.emptyThumbnailRelativeFile = emptyThumbnailRelativeFile;
	}

	/**
	 * Returns the mask expression applied to filter/editor behaviour for this column.
	 *
	 * @return mask expression, or {@code null}
	 */
	public String getMask() {
		return mask;
	}

	/**
	 * Indicates whether text-oriented filter operators are available for this column.
	 *
	 * @return {@code true} when text filter operators are available
	 */
	public boolean getHasTextFilterOperators() {
		return hasTextFilterOperators;
	}
	
	/**
	 * Produces the SmartClient JavaScript field definition payload for this query column.
	 *
	 * @return SmartClient JavaScript field definition payload
	 */
	@SuppressWarnings("java:S3776") // Complexity OK
	public String toJavascript() {
		StringBuilder result = new StringBuilder(64);

		result.append("name:'");
		result.append(name);
		result.append("',title:'");
		result.append(SmartClientViewRenderer.escapeSmartClientText(title, true));
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
            result.append(",valueMap:")
                  .append(getValueMapAsString());
        }

		if (required) {
			result.append(",bizRequired:true,requiredMessage:'");
			if (requiredMessage == null) {
				result.append(SmartClientViewRenderer.escapeSmartClientText(Util.nullSafeI18n(BeanValidator.VALIDATION_REQUIRED_KEY, title), true));
			}
			else {
				result.append(SmartClientViewRenderer.escapeSmartClientText(requiredMessage, true));
			}
			result.append('\'');
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
        	result.append(",align:'").append(align.toTextAlignmentString()).append('\'');
        }
        if (pixelWidth != null) {
			result.append(",width:").append(IMAGE_TYPE.equals(type) ? pixelWidth.intValue() + 8 : pixelWidth.intValue());
        }
        else if (IMAGE_TYPE.equals(type)) {
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
