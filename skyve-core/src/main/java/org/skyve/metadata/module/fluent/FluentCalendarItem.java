package org.skyve.metadata.module.fluent;

import org.skyve.impl.metadata.repository.module.CalendarItemMetaData;

/**
 * Builds calendar menu item metadata.
 */
public class FluentCalendarItem extends FluentMenuItem<FluentCalendarItem> {
	private CalendarItemMetaData item = null;
	
	/**
	 * Creates a fluent wrapper with a new metadata instance.
	 */
	public FluentCalendarItem() {
		item = new CalendarItemMetaData();
	}

	/**
	 * Creates a fluent wrapper around an existing metadata instance.
	 *
	 * @param item The metadata to mutate.
	 */
	public FluentCalendarItem(CalendarItemMetaData item) {
		this.item = item;
	}

	/**
	 * Copies calendar menu item state from an existing menu item.
	 *
	 * @param item The source calendar menu item.
	 * @return this fluent instance.
	 */
	public FluentCalendarItem from(@SuppressWarnings("hiding") org.skyve.impl.metadata.module.menu.CalendarItem item) {
		super.from(item);
		documentName(item.getDocumentName());
		queryName(item.getQueryName());
		modelName(item.getModelName());
		startBinding(item.getStartBinding());
		endBinding(item.getEndBinding());
		return this;
	}
	
	/**
	 * Sets the target document for the calendar item.
	 *
	 * @param documentName The document name.
	 * @return this fluent instance.
	 */
	public FluentCalendarItem documentName(String documentName) {
		item.setDocumentName(documentName);
		return this;
	}
	
	/**
	 * Sets the query used to load calendar entries.
	 *
	 * @param queryName The query name.
	 * @return this fluent instance.
	 */
	public FluentCalendarItem queryName(String queryName) {
		item.setQueryName(queryName);
		return this;
	}

	/**
	 * Sets the model used by the calendar item.
	 *
	 * @param modelName The model name.
	 * @return this fluent instance.
	 */
	public FluentCalendarItem modelName(String modelName) {
		item.setModelName(modelName);
		return this;
	}
	
	/**
	 * Sets the binding used for each event start value.
	 *
	 * @param startBinding The start binding expression.
	 * @return this fluent instance.
	 */
	public FluentCalendarItem startBinding(String startBinding) {
		item.setStartBinding(startBinding);
		return this;
	}
	
	/**
	 * Sets the binding used for each event end value.
	 *
	 * @param endBinding The end binding expression.
	 * @return this fluent instance.
	 */
	public FluentCalendarItem endBinding(String endBinding) {
		item.setEndBinding(endBinding);
		return this;
	}

	/**
	 * Returns the mutable metadata backing this fluent wrapper.
	 *
	 * @return The calendar item metadata instance.
	 */
	@Override
	public CalendarItemMetaData get() {
		return item;
	}
}
