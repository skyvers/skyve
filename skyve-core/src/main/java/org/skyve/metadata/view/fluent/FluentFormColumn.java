package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.container.form.FormColumn;

public class FluentFormColumn {
	private FormColumn column = null;
	
	public FluentFormColumn() {
		column = new FormColumn();
	}
	
	public FluentFormColumn(FormColumn column) {
		this.column = column;
	}
	
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
	
	public FluentFormColumn pixelWidth(int pixelWidth) {
		column.setPixelWidth(Integer.valueOf(pixelWidth));
		return this;
	}

	public FluentFormColumn responsiveWidth(int responsiveWidth) {
		column.setResponsiveWidth(Integer.valueOf(responsiveWidth));
		return this;
	}

	public FluentFormColumn sm(int sm) {
		column.setSm(Integer.valueOf(sm));
		return this;
	}
	
	public FluentFormColumn md(int md) {
		column.setMd(Integer.valueOf(md));
		return this;
	}
	
	public FluentFormColumn lg(int lg) {
		column.setLg(Integer.valueOf(lg));
		return this;
	}

	public FluentFormColumn xl(int xl) {
		column.setXl(Integer.valueOf(xl));
		return this;
	}

	public FluentFormColumn percentageWidth(int percentageWidth) {
		column.setPercentageWidth(Integer.valueOf(percentageWidth));
		return this;
	}

	public FormColumn get() {
		return column;
	}
}
