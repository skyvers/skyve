package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.container.form.FormColumn;

/**
 * Fluent builder for {@link FormColumn} metadata.
 */
public class FluentFormColumn {
	private FormColumn column = null;
	
	/**
	 * Creates a builder backed by a new {@link FormColumn} metadata instance.
	 */
	public FluentFormColumn() {
		column = new FormColumn();
	}
	
	/**
	 * Creates a builder backed by the supplied column metadata instance.
	 *
	 * @param column
	 *            the column metadata to mutate
	 */
	public FluentFormColumn(FormColumn column) {
		this.column = column;
	}
	
	/**
	 * Copies all configured width values from the supplied column metadata.
	 *
	 * @param column
	 *            the source column metadata
	 * @return this builder
	 */
	public FluentFormColumn from(@SuppressWarnings("hiding") FormColumn column) {
		Integer i = column.getPixelWidth();
		if (i != null) {
			pixelWidth(i.intValue());
		}
		i = column.getResponsiveWidth();
		if (i != null) {
			responsiveWidth(i.intValue());
		}
		i = column.getSm();
		if (i != null) {
			sm(i.intValue());
		}
		i = column.getMd();
		if (i != null) {
			md(i.intValue());
		}
		i = column.getLg();
		if (i != null) {
			lg(i.intValue());
		}
		i = column.getXl();
		if (i != null) {
			xl(i.intValue());
		}
		i = column.getPercentageWidth();
		if (i != null) {
			percentageWidth(i.intValue());
		}
		return this;
	}
	
	/**
	 * Sets the absolute column width in pixels.
	 *
	 * @param pixelWidth
	 *            the column width in pixels
	 * @return this builder
	 */
	public FluentFormColumn pixelWidth(int pixelWidth) {
		column.setPixelWidth(Integer.valueOf(pixelWidth));
		return this;
	}

	/**
	 * Sets the responsive width value for this column.
	 *
	 * @param responsiveWidth
	 *            the responsive width value
	 * @return this builder
	 */
	public FluentFormColumn responsiveWidth(int responsiveWidth) {
		column.setResponsiveWidth(Integer.valueOf(responsiveWidth));
		return this;
	}

	/**
	 * Sets the small breakpoint width for this column.
	 *
	 * @param sm
	 *            the small breakpoint width
	 * @return this builder
	 */
	public FluentFormColumn sm(int sm) {
		column.setSm(Integer.valueOf(sm));
		return this;
	}
	
	/**
	 * Sets the medium breakpoint width for this column.
	 *
	 * @param md
	 *            the medium breakpoint width
	 * @return this builder
	 */
	public FluentFormColumn md(int md) {
		column.setMd(Integer.valueOf(md));
		return this;
	}
	
	/**
	 * Sets the large breakpoint width for this column.
	 *
	 * @param lg
	 *            the large breakpoint width
	 * @return this builder
	 */
	public FluentFormColumn lg(int lg) {
		column.setLg(Integer.valueOf(lg));
		return this;
	}

	/**
	 * Sets the extra-large breakpoint width for this column.
	 *
	 * @param xl
	 *            the extra-large breakpoint width
	 * @return this builder
	 */
	public FluentFormColumn xl(int xl) {
		column.setXl(Integer.valueOf(xl));
		return this;
	}

	/**
	 * Sets column width as a percentage.
	 *
	 * @param percentageWidth
	 *            the width percentage
	 * @return this builder
	 */
	public FluentFormColumn percentageWidth(int percentageWidth) {
		column.setPercentageWidth(Integer.valueOf(percentageWidth));
		return this;
	}

	/**
	 * Returns the backing metadata instance.
	 *
	 * @return the mutable column metadata
	 */
	public FormColumn get() {
		return column;
	}
}
