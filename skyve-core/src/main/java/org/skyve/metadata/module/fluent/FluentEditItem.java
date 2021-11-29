package org.skyve.metadata.module.fluent;

import org.skyve.impl.metadata.repository.module.EditItem;

public class FluentEditItem extends FluentMenuItem<FluentEditItem> {
	private EditItem item = new EditItem();
	
	public FluentEditItem() {
		// nothing to see
	}

	public FluentEditItem(org.skyve.impl.metadata.module.menu.EditItem item) {
		super(item);
		documentName(item.getDocumentName());
	}
	
	public FluentEditItem documentName(String documentName) {
		item.setDocumentName(documentName);
		return this;
	}
	
	@Override
	public EditItem get() {
		return item;
	}
}
