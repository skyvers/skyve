package org.skyve.impl.metadata.module.menu;

public class CalendarItem extends AbstractDocumentOrQueryOrModelMenuItem {
	private static final long serialVersionUID = -7998788239200957754L;

	private String startBinding;
	private String endBinding;

	public String getStartBinding() {
		return startBinding;
	}
	public void setStartBinding(String startBinding) {
		this.startBinding = startBinding;
	}

	public String getEndBinding() {
		return endBinding;
	}
	public void setEndBinding(String endBinding) {
		this.endBinding = endBinding;
	}
}
