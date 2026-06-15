package org.skyve.impl.metadata.module.menu;

/**
 * Menu item that opens the calendar view of a document or query.
 *
 * <p>Extends {@link AbstractDocumentOrQueryOrModelMenuItem} with a
 * {@code startBinding} and optional {@code endBinding} that map to date/time
 * attributes on the target document, defining the event interval displayed in the
 * calendar grid.
 *
 * <p>Threading: not thread-safe.  Instances are populated during metadata loading
 * and are read-only once placed in the repository cache.
 *
 * @see AbstractDocumentOrQueryOrModelMenuItem
 * @see org.skyve.metadata.module.menu.CalendarItem
 */
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
