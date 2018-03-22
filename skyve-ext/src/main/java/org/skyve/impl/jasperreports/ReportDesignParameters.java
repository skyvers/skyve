package org.skyve.impl.jasperreports;

import java.util.ArrayList;
import java.util.List;

import org.skyve.metadata.model.Attribute;
import org.skyve.report.ReportFormat;

public final class ReportDesignParameters {
	public static enum ReportStyle 
	{
		tabular, columnar
	}

	public static enum ColumnAlignment
	{
		left, center, right
	}
	
	public static class ReportColumn {
		private String name;
		private String title;
		private String type;
		private int line;
		private int width;
		private ColumnAlignment alignment;
		private Attribute.AttributeType attributeType;

		public String getName() {
			return name;
		}
		public void setName(String name) {
			this.name = name;
		}
		public String getTitle() {
			return title;
		}
		public void setTitle(String title) {
			this.title = title;
		}
		public String getType() {
			return type;
		}
		public void setType(String type) {
			this.type = type;
		}
		public int getLine() {
			return line;
		}
		public void setLine(int line) {
			this.line = line;
		}
		public int getWidth() {
			return width;
		}
		public void setWidth(int width) {
			this.width = width;
		}
		public ColumnAlignment getAlignment() {
			return alignment;
		}
		public void setAlignment(ColumnAlignment alignment) {
			this.alignment = alignment;
		}
		public Attribute.AttributeType getAttributeType() {
			return attributeType;
		}
		public void setAttributeType(Attribute.AttributeType attributeType) {
			this.attributeType = attributeType;
		}
	}

	private ReportFormat reportFormat;
	private boolean pretty;
	private boolean paginated;
	private ReportStyle reportStyle;
	// No [format] as all it does is pick the width and height
	private int pageWidth;
	private int pageHeight;
	private int topMargin;
	private int bottomMargin;
	private int leftMargin;
	private int rightMargin;
	private List<ReportColumn> columns = new ArrayList<>();	
	private List<String> groupColumns = new ArrayList<>();

	public ReportFormat getReportFormat() {
		return reportFormat;
	}
	public void setReportFormat(ReportFormat reportFormat) {
		this.reportFormat = reportFormat;
	}
	public boolean isPaginated() {
		return paginated;
	}
	public void setPaginated(boolean paginated) {
		this.paginated = paginated;
	}
	public ReportStyle getReportStyle() {
		return reportStyle;
	}
	public boolean isPretty() {
		return pretty;
	}
	public void setPretty(boolean pretty) {
		this.pretty = pretty;
	}
	public void setReportStyle(ReportStyle reportStyle) {
		this.reportStyle = reportStyle;
	}
	public int getPageWidth() {
		return pageWidth;
	}
	public void setPageWidth(int pageWidth) {
		this.pageWidth = pageWidth;
	}
	public int getPageHeight() {
		return pageHeight;
	}
	public void setPageHeight(int pageHeight) {
		this.pageHeight = pageHeight;
	}
	public int getTopMargin() {
		return topMargin;
	}
	public void setTopMargin(int topMargin) {
		this.topMargin = topMargin;
	}
	public int getBottomMargin() {
		return bottomMargin;
	}
	public void setBottomMargin(int bottomMargin) {
		this.bottomMargin = bottomMargin;
	}
	public int getLeftMargin() {
		return leftMargin;
	}
	public void setLeftMargin(int leftMargin) {
		this.leftMargin = leftMargin;
	}
	public int getRightMargin() {
		return rightMargin;
	}
	public void setRightMargin(int rightMargin) {
		this.rightMargin = rightMargin;
	}
	public List<ReportColumn> getColumns() {
		return columns;
	}
	public List<String> getGroupColumns() {
		return groupColumns;
	}
}
