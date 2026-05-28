package org.skyve.impl.web.service;

import java.util.EnumMap;
import java.util.Map;

import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.view.View.ViewType;

/**
 * Encapsulates all information about a document.
 * Document - the Repository Document
 * Views - the Repository views.
 * 
 * @author Mike
 */
public final class ClientDocument {
	private Document document;
	private Map<ViewType, ViewImpl> views = new EnumMap<>(ViewType.class);

	/**
	 * Returns the document metadata represented by this client-document payload.
	 *
	 * @return document metadata
	 */
	public Document getDocument() {
		return document;
	}

	/**
	 * Sets the document metadata represented by this client-document payload.
	 *
	 * @param document document metadata
	 */
	public void setDocument(Document document) {
		this.document = document;
	}

	/**
	 * Returns available views keyed by view type.
	 *
	 * @return mutable map of view definitions
	 */
	public Map<ViewType, ViewImpl> getViews() {
		return views;
	}
}
