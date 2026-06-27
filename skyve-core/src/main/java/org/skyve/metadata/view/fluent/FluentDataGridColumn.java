package org.skyve.metadata.view.fluent;


import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridColumn;

/**
 * Provides shared fluent mutation for tabular column metadata.
 *
 * @param <T>
 *            the concrete fluent builder type
 */
public abstract class FluentDataGridColumn<T extends FluentDataGridColumn<T>> {
	/**
	 * Creates a base data-grid column builder.
	 */
	protected FluentDataGridColumn() {
		// nothing to see
	}

	/**
	 * Copies common column state into this builder.
	 *
	 * @param column
	 *            the source column metadata
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	protected T from(DataGridColumn column) {
		title(column.getTitle());
		escapeTitle(column.getEscapeTitle());
		alignment(column.getAlignment());
		Integer i = column.getPixelWidth();
		if (i != null) {
			pixelWidth(i.intValue());
		}
		return (T) this;
	}
	
	@SuppressWarnings("unchecked")
	/**
	 * Sets the rendered column title.
	 *
	 * @param title
	 *            the column title
	 * @return this builder
	 */
	public T title(String title) {
		get().setTitle(title);
		return (T) this;
	}

	/**
	 * Sets whether the column title should be escaped before rendering.
	 *
	 * @param escapeTitle {@code false} to allow trusted markup; {@code true} to escape at the renderer boundary
	 * @return this builder
	 */
	public T escapeTitle(boolean escapeTitle) {
		return escapeTitle(escapeTitle ? Boolean.TRUE : Boolean.FALSE);
	}

	/**
	 * Sets whether the column title should be escaped before rendering.
	 *
	 * @param escapeTitle {@code Boolean.FALSE} to allow trusted markup; {@code null} or {@code Boolean.TRUE} to escape at the renderer boundary
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public T escapeTitle(Boolean escapeTitle) {
		get().setEscapeTitle(escapeTitle);
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	/**
	 * Sets the horizontal alignment for rendered cell content.
	 *
	 * @param alignment
	 *            the cell-content alignment
	 * @return this builder
	 */
	public T alignment(HorizontalAlignment alignment) {
		get().setAlignment(alignment);
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	/**
	 * Sets the fixed pixel width for the column.
	 *
	 * @param pixelWidth
	 *            the column width in pixels
	 * @return this builder
	 */
	public T pixelWidth(int pixelWidth) {
		get().setPixelWidth(Integer.valueOf(pixelWidth));
		return (T) this;
	}

	/**
	 * Returns the underlying mutable metadata.
	 *
	 * @return the wrapped column metadata
	 */
	public abstract DataGridColumn get();
}
