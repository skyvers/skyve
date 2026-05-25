package org.skyve.metadata.view;

/**
 * Mixin interface for view elements that carry an explicit editable override.
 *
 * <p>When {@link #getEditable()} returns {@code Boolean.FALSE} the element is
 * rendered read-only regardless of document-level permissions. {@code null}
 * means the element inherits editability from the containing context.
 *
 * @see Disableable
 */
public interface Editable {
	/**
	 * Returns the explicit editable flag, or {@code null} to inherit from context.
	 *
	 * @return {@code Boolean.TRUE} to force editable, {@code Boolean.FALSE} to force
	 *         read-only, or {@code null} to inherit
	 */
	public Boolean getEditable();
	public void setEditable(Boolean editable);
}
