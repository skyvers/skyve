package org.skyve.metadata.module.fluent;

import org.skyve.impl.metadata.repository.module.TreeItem;

public class FluentTreeItem extends FluentMenuItem<FluentTreeItem> {
	private TreeItem item = new TreeItem();
	
	public FluentTreeItem() {
		// nothing to see
	}

	public FluentTreeItem(org.skyve.impl.metadata.module.menu.TreeItem item) {
		super(item);
		documentName(item.getDocumentName());
		queryName(item.getQueryName());
		modelName(item.getModelName());
		autoPopulate(item.isAutoPopulate());
	}
	
	public FluentTreeItem documentName(String documentName) {
		item.setDocumentName(documentName);
		return this;
	}
	
	public FluentTreeItem queryName(String queryName) {
		item.setQueryName(queryName);
		return this;
	}

	public FluentTreeItem modelName(String modelName) {
		item.setModelName(modelName);
		return this;
	}
	
	public FluentTreeItem autoPopulate(boolean autoPopulate) {
		item.setAutoPopulate(autoPopulate ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	@Override
	public TreeItem get() {
		return item;
	}
}
