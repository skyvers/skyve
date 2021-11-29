package org.skyve.metadata.module.fluent;

import org.skyve.impl.metadata.repository.module.CalendarItem;

public class FluentCalendarItem extends FluentMenuItem<FluentCalendarItem> {
	private CalendarItem item = new CalendarItem();
	
	public FluentCalendarItem() {
		// nothing to see
	}

	public FluentCalendarItem(org.skyve.impl.metadata.module.menu.CalendarItem item) {
		super();
		documentName(item.getDocumentName());
		queryName(item.getQueryName());
		modelName(item.getModelName());
		startBinding(item.getStartBinding());
		endBinding(item.getEndBinding());
	}
	
	public FluentCalendarItem documentName(String documentName) {
		item.setDocumentName(documentName);
		return this;
	}
	
	public FluentCalendarItem queryName(String queryName) {
		item.setQueryName(queryName);
		return this;
	}

	public FluentCalendarItem modelName(String modelName) {
		item.setModelName(modelName);
		return this;
	}
	
	public FluentCalendarItem startBinding(String startBinding) {
		item.setStartBinding(startBinding);
		return this;
	}
	
	public FluentCalendarItem endBinding(String endBinding) {
		item.setEndBinding(endBinding);
		return this;
	}

	@Override
	public CalendarItem get() {
		return item;
	}
}
