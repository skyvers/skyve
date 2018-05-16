package org.skyve.impl.metadata.module.menu;

public class ListItem extends AbstractDocumentOrQueryOrModelMenuItem {
	private static final long serialVersionUID = -7998788239200957754L;
	
	private boolean autoPopulate = true;

	public boolean isAutoPopulate() {
		return autoPopulate;
	}

	public void setAutoPopulate(boolean autoPopulate) {
		this.autoPopulate = autoPopulate;
	}
}
