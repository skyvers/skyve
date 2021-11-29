package org.skyve.metadata.module.fluent;

import org.skyve.impl.metadata.repository.module.ListItem;

public class FluentListItem extends FluentMenuItem<FluentListItem> {
	private ListItem item = new ListItem();
	
	public FluentListItem() {
		// nothing to see
	}
	
	public FluentListItem(org.skyve.impl.metadata.module.menu.ListItem item) {
		super(item);
		documentName(item.getDocumentName());
		queryName(item.getQueryName());
		modelName(item.getModelName());
		autoPopulate(item.isAutoPopulate());
	}

	public FluentListItem documentName(String documentName) {
		item.setDocumentName(documentName);
		return this;
	}
	
	public FluentListItem queryName(String queryName) {
		item.setQueryName(queryName);
		return this;
	}

	public FluentListItem modelName(String modelName) {
		item.setModelName(modelName);
		return this;
	}
	
	public FluentListItem autoPopulate(boolean autoPopulate) {
		item.setAutoPopulate(autoPopulate ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	@Override
	public ListItem get() {
		return item;
	}
}
