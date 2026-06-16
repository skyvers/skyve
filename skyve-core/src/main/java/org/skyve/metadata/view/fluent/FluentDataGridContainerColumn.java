package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.Blurb;
import org.skyve.impl.metadata.view.widget.DynamicImage;
import org.skyve.impl.metadata.view.widget.Link;
import org.skyve.impl.metadata.view.widget.StaticImage;
import org.skyve.impl.metadata.view.widget.bound.Label;
import org.skyve.impl.metadata.view.widget.bound.input.ContentUpload;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridContainerColumn;
import org.skyve.metadata.MetaData;

import jakarta.annotation.Nonnull;

/**
 * Builds container-style {@link DataGridContainerColumn} metadata.
 */
public class FluentDataGridContainerColumn extends FluentDataGridColumn<FluentDataGridContainerColumn> {
	private DataGridContainerColumn column = null;
	
	/**
	 * Creates a builder backed by a new {@link DataGridContainerColumn}.
	 */
	public FluentDataGridContainerColumn() {
		column = new DataGridContainerColumn();
	}

	/**
	 * Creates a builder backed by the supplied {@link DataGridContainerColumn}.
	 *
	 * @param column
	 *            the metadata instance to mutate
	 */
	public FluentDataGridContainerColumn(DataGridContainerColumn column) {
		this.column = column;
	}

	/**
	 * Copies all supported widget references from runtime metadata.
	 *
	 * @param column
	 *            the source metadata to copy
	 * @return this builder
	 * @throws IllegalStateException
	 *             if an unsupported widget type is encountered
	 */
	public FluentDataGridContainerColumn from(@SuppressWarnings("hiding") DataGridContainerColumn column) {
		super.from(column);

		for (MetaData widget : column.getWidgets()) {
			if (widget instanceof Link link) {
				addWidget(new FluentLink().from(link));
			}
			else if (widget instanceof ContentUpload content) {
				addWidget(new FluentContentUpload().from(content));
			}
			else if (widget instanceof StaticImage image) {
				addWidget(new FluentStaticImage().from(image));
			}
			else if (widget instanceof DynamicImage image) {
				addWidget(new FluentDynamicImage().from(image));
			}
			else if (widget instanceof Blurb blurb) {
				addWidget(new FluentBlurb().from(blurb));
			}
			else if (widget instanceof Label label) {
				addWidget(new FluentLabel().from(label));
			}
			else {
				throw new IllegalStateException(widget + " is not catered for");
			}
		}
		
		return this;
	}

	/**
	 * Adds a link widget to the container column.
	 *
	 * @param link
	 *            the link builder whose widget is appended
	 * @return this builder
	 */
	public FluentDataGridContainerColumn addWidget(FluentLink link) {
		column.getWidgets().add(link.get());
		return this;
	}

	/**
	 * Adds a {@link org.skyve.impl.metadata.view.widget.bound.input.ContentUpload} widget to this container column.
	 *
	 * @param content the managed-content builder whose widget is appended
	 * @return this builder
	 */
	public @Nonnull FluentDataGridContainerColumn addWidget(@Nonnull FluentContentUpload content) {
		column.getWidgets().add(content.get());
		return this;
	}

	/**
	 * Adds a {@link org.skyve.impl.metadata.view.widget.StaticImage} widget to this container column.
	 *
	 * @param image the static image builder whose widget is appended
	 * @return this builder
	 */
	public FluentDataGridContainerColumn addWidget(FluentStaticImage image) {
		column.getWidgets().add(image.get());
		return this;
	}

	/**
	 * Adds a {@link org.skyve.impl.metadata.view.widget.DynamicImage} widget to this container column.
	 *
	 * @param image the dynamic image builder whose widget is appended
	 * @return this builder
	 */
	public FluentDataGridContainerColumn addWidget(FluentDynamicImage image) {
		column.getWidgets().add(image.get());
		return this;
	}

	/**
	 * Adds a {@link org.skyve.impl.metadata.view.widget.Blurb} widget to this container column.
	 *
	 * @param blurb the blurb builder whose widget is appended
	 * @return this builder
	 */
	public FluentDataGridContainerColumn addWidget(FluentBlurb blurb) {
		column.getWidgets().add(blurb.get());
		return this;
	}

	/**
	 * Adds a {@link org.skyve.impl.metadata.view.widget.bound.Label} widget to this container column.
	 *
	 * @param label the label builder whose widget is appended
	 * @return this builder
	 */
	public FluentDataGridContainerColumn addWidget(FluentLabel label) {
		column.getWidgets().add(label.get());
		return this;
	}

	/**
	 * Returns the underlying mutable metadata.
	 *
	 * @return the wrapped container column metadata
	 */
	@Override
	public DataGridContainerColumn get() {
		return column;
	}
}
