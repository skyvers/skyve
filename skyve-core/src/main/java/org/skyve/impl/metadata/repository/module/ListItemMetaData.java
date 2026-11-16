package org.skyve.impl.metadata.repository.module;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;

import jakarta.annotation.Nullable;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * JAXB-annotated descriptor for a {@code <list>} menu item in a module menu.
 *
 * <p>Navigates to a list/grid view for the named document and query.
 * Extends {@link ItemMetaData}.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 *
 * @see ItemMetaData
 */
@XmlType(name = "list", namespace = XMLMetaData.MODULE_NAMESPACE)
@XmlRootElement(name = "list", namespace = XMLMetaData.MODULE_NAMESPACE)
public class ListItemMetaData extends ItemMetaData {
	private static final long serialVersionUID = 6623610207302009047L;

	private String documentName;
	private String queryName;
	private String modelName;
	private Boolean autoPopulate;
	
	public @Nullable String getDocumentName() {
		return documentName;
	}
	@XmlAttribute(name = "document")
	public void setDocumentName(@Nullable String documentName) {
		this.documentName = UtilImpl.processStringValue(documentName);
	}

	public @Nullable String getQueryName() {
		return queryName;
	}
	@XmlAttribute(name = "query")
	public void setQueryName(@Nullable String queryName) {
		this.queryName = UtilImpl.processStringValue(queryName);
	}

	public @Nullable String getModelName() {
		return modelName;
	}
	@XmlAttribute(name = "model")
	public void setModelName(@Nullable String modelName) {
		this.modelName = UtilImpl.processStringValue(modelName);
	}

	public @Nullable Boolean getAutoPopulate() {
		return autoPopulate;
	}
	@XmlAttribute(name = "autoPopulate")
	public void setAutoPopulate(@Nullable Boolean autoPopulate) {
		this.autoPopulate = autoPopulate;
	}
}
