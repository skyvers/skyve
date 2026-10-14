package org.skyve.impl.metadata.repository.module;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * JAXB-annotated descriptor for a {@code <calendar>} menu item in a module menu.
 *
 * <p>Navigates to a calendar view for the named document and query,
 * optionally restricted to specific UX/UI scopes.  Extends {@link ItemMetaData}.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 *
 * @see ItemMetaData
 */
@XmlType(name = "calendar", namespace = XMLMetaData.MODULE_NAMESPACE)
@XmlRootElement(name = "calendar", namespace = XMLMetaData.MODULE_NAMESPACE)
public class CalendarItemMetaData extends ItemMetaData {
	private static final long serialVersionUID = -6472812043264486704L;

	private String documentName;
	private String queryName;
	private String modelName;
	private String startBinding;
	private String endBinding;
	
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

	public @Nonnull String getStartBinding() {
		return startBinding;
	}
	@XmlAttribute(required = true)
	public void setStartBinding(@Nonnull String startBinding) {
		this.startBinding = UtilImpl.processStringValue(startBinding);
	}

	public @Nonnull String getEndBinding() {
		return endBinding;
	}
	@XmlAttribute(required = true)
	public void setEndBinding(@Nonnull String endBinding) {
		this.endBinding = UtilImpl.processStringValue(endBinding);
	}
}
