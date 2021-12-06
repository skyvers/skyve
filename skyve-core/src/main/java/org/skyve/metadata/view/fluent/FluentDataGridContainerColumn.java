package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.Blurb;
import org.skyve.impl.metadata.view.widget.DynamicImage;
import org.skyve.impl.metadata.view.widget.Link;
import org.skyve.impl.metadata.view.widget.StaticImage;
import org.skyve.impl.metadata.view.widget.bound.Label;
import org.skyve.impl.metadata.view.widget.bound.input.ContentImage;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridContainerColumn;
import org.skyve.metadata.MetaData;

public class FluentDataGridContainerColumn extends FluentDataGridColumn<FluentDataGridContainerColumn> {
	private DataGridContainerColumn column = null;
	
	public FluentDataGridContainerColumn() {
		column = new DataGridContainerColumn();
	}

	public FluentDataGridContainerColumn(DataGridContainerColumn column) {
		this.column = column;
	}

	public FluentDataGridContainerColumn from(@SuppressWarnings("hiding") DataGridContainerColumn column) {
		super.from(column);

		for (MetaData widget : column.getWidgets()) {
			if (widget instanceof Link) {
				addWidget(new FluentLink().from((Link) widget));
			}
			else if (widget instanceof ContentImage) {
				addWidget(new FluentContentImage().from((ContentImage) widget));
			}
			else if (widget instanceof StaticImage) {
				addWidget(new FluentStaticImage().from((StaticImage) widget));
			}
			else if (widget instanceof DynamicImage) {
				addWidget(new FluentDynamicImage().from((DynamicImage) widget));
			}
			else if (widget instanceof Blurb) {
				addWidget(new FluentBlurb().from((Blurb) widget));
			}
			else if (widget instanceof Label) {
				addWidget(new FluentLabel().from((Label) widget));
			}
			else {
				throw new IllegalStateException(widget + " is not catered for");
			}
		}
		
		return this;
	}

	public FluentDataGridContainerColumn addWidget(FluentLink link) {
		column.getWidgets().add(link.get());
		return this;
	}

	public FluentDataGridContainerColumn addWidget(FluentContentImage image) {
		column.getWidgets().add(image.get());
		return this;
	}

	public FluentDataGridContainerColumn addWidget(FluentStaticImage image) {
		column.getWidgets().add(image.get());
		return this;
	}

	public FluentDataGridContainerColumn addWidget(FluentDynamicImage image) {
		column.getWidgets().add(image.get());
		return this;
	}

	public FluentDataGridContainerColumn addWidget(FluentBlurb blurb) {
		column.getWidgets().add(blurb.get());
		return this;
	}

	public FluentDataGridContainerColumn addWidget(FluentLabel label) {
		column.getWidgets().add(label.get());
		return this;
	}

	@Override
	public DataGridContainerColumn get() {
		return column;
	}
}
