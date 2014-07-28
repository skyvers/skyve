package org.skyve.wildcat.web.service;

import java.util.HashMap;
import java.util.Map;

import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.wildcat.metadata.view.ViewImpl;

/**
 * Encapsulates all information about a document.
 * Document - the Repository Document
 * Views - the Repository views.
 * 
 * @author Mike
 */
public final class ClientDocument 
{
	private Document document;
	private Map<ViewType, ViewImpl> views = new HashMap<>(2);

	public Document getDocument() {
		return document;
	}
	public void setDocument(Document document) {
		this.document = document;
	}
	public Map<ViewType, ViewImpl> getViews() {
		return views;
	}
}
