package org.skyve.metadata.module.fluent;

import org.skyve.impl.metadata.repository.module.ListItem;

public class FluentListItem extends FluentMenuItem<FluentListItem> {
	private ListItem item = null;
	
	public FluentListItem() {
		item = new ListItem();
	}

	public FluentListItem(ListItem item) {
		this.item = item;
	}

	public FluentListItem from(@SuppressWarnings("hiding") org.skyve.impl.metadata.module.menu.ListItem item) {
		super.from(item);
		documentName(item.getDocumentName());
		queryName(item.getQueryName());
		modelName(item.getModelName());
		autoPopulate(item.isAutoPopulate());
		return this;
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
