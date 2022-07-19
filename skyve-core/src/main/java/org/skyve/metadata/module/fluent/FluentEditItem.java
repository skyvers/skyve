package org.skyve.metadata.module.fluent;

import org.skyve.impl.metadata.repository.module.EditItemMetaData;

public class FluentEditItem extends FluentMenuItem<FluentEditItem> {
	private EditItemMetaData item = null;
	
	public FluentEditItem() {
		item = new EditItemMetaData();
	}

	public FluentEditItem(EditItemMetaData item) {
		this.item = item;
	}

	public FluentEditItem from(@SuppressWarnings("hiding") org.skyve.impl.metadata.module.menu.EditItem item) {
		super.from(item);
		documentName(item.getDocumentName());
		return this;
	}
	
	public FluentEditItem documentName(String documentName) {
		item.setDocumentName(documentName);
		return this;
	}
	
	@Override
	public EditItemMetaData get() {
		return item;
	}
}
