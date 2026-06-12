package org.skyve.impl.report.jasperreports;

import java.util.ArrayList;
import java.util.List;

import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.report.ReportFormat;

/**
 * Captures layout, grouping, and export options used when generating ad hoc report designs.
 */
public final class ReportDesignParameters {
	/**
	 * Supported high-level layout styles for generated reports.
	 */
	@SuppressWarnings("java:S115") // Enum names are report design option codes.
	public static enum ReportStyle {
		/** Row-oriented table layout. */
		tabular, columnar
	}

	/**
	 * Horizontal alignment options for report column values.
	 */
	@SuppressWarnings("java:S115") // Enum names are report design option codes.
	public static enum ColumnAlignment {
		/** Left-align text/content. */
		left, center, right
	}
	
	/**
	 * Defines a single report column's display and formatting metadata.
	 */
	public static class ReportColumn {
		private String name;
		private String title;
		private int line;
		private int width;
		private ColumnAlignment alignment;
		private AttributeType attributeType;
		private String formatPattern;
		
		/**
		 * Returns the source binding name of this column.
		 *
		 * @return The column binding name.
		 */
		public String getName() {
			return name;
		}

		/**
		 * Sets the source binding name of this column.
		 *
		 * @param name The column binding name.
		 */
		public void setName(String name) {
			this.name = name;
		}
		
		/**
		 * Returns the display title shown for this column.
		 *
		 * @return The display title.
		 */
		public String getTitle() {
			return title;
		}
		
		/**
		 * Sets the display title shown for this column.
		 *
		 * @param title The display title.
		 */
		public void setTitle(String title) {
			this.title = title;
		}
		
		/**
		 * Returns the logical line index used for grouped/stacked column rendering.
		 *
		 * @return The line index.
		 */
		public int getLine() {
			return line;
		}
		
		/**
		 * Sets the logical line index used for grouped/stacked column rendering.
		 *
		 * @param line The line index.
		 */
		public void setLine(int line) {
			this.line = line;
		}
		
		/**
		 * Returns the configured column width.
		 *
		 * @return The column width.
		 */
		public int getWidth() {
			return width;
		}
		
		/**
		 * Sets the configured column width.
		 *
		 * @param width The column width.
		 */
		public void setWidth(int width) {
			this.width = width;
		}
		
		/**
		 * Returns the horizontal alignment for this column.
		 *
		 * @return The configured alignment.
		 */
		public ColumnAlignment getAlignment() {
			return alignment;
		}
		
		/**
		 * Sets the horizontal alignment for this column.
		 *
		 * @param alignment The alignment to apply.
		 */
		public void setAlignment(ColumnAlignment alignment) {
			this.alignment = alignment;
		}
		
		/**
		 * Returns the underlying attribute type represented by this column.
		 *
		 * @return The attribute type.
		 */
		public AttributeType getAttributeType() {
			return attributeType;
		}
		
		/**
		 * Sets the underlying attribute type represented by this column.
		 *
		 * @param attributeType The attribute type.
		 */
		public void setAttributeType(AttributeType attributeType) {
			this.attributeType = attributeType;
		}
		
		/**
		 * Returns the format pattern used when rendering this column.
		 *
		 * @return The format pattern, or {@code null}.
		 */
		public String getFormatPattern() {
			return formatPattern;
		}
		
		/**
		 * Sets the format pattern used when rendering this column.
		 *
		 * @param formatPattern The format pattern.
		 */
		public void setFormatPattern(String formatPattern) {
			this.formatPattern = formatPattern;
		}
	}

	private ReportFormat reportFormat;
	private boolean pretty;
	private boolean paginated;
	private boolean showSummary;
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
	/**
	 * Whether or not the customer's logo should be included in the report's title band.
	 */
	private boolean includeCustomerLogo = true;

	/**
	 * Returns the output report format.
	 *
	 * @return The report format.
	 */
	public ReportFormat getReportFormat() {
		return reportFormat;
	}
	
	/**
	 * Sets the output report format.
	 *
	 * @param reportFormat The report format.
	 */
	public void setReportFormat(ReportFormat reportFormat) {
		this.reportFormat = reportFormat;
	}
	
	/**
	 * Indicates whether report output should be paginated.
	 *
	 * @return {@code true} when pagination is enabled.
	 */
	public boolean isPaginated() {
		return paginated;
	}
	
	/**
	 * Sets whether report output should be paginated.
	 *
	 * @param paginated {@code true} to enable pagination.
	 */
	public void setPaginated(boolean paginated) {
		this.paginated = paginated;
	}
	
	/**
	 * Returns the configured report layout style.
	 *
	 * @return The report style.
	 */
	public ReportStyle getReportStyle() {
		return reportStyle;
	}
	
	/**
	 * Indicates whether summary rows/sections should be shown.
	 *
	 * @return {@code true} when summaries are enabled.
	 */
	public boolean isShowSummary() {
		return showSummary;
	}
	
	/**
	 * Sets whether summary rows/sections should be shown.
	 *
	 * @param showSummary {@code true} to enable summaries.
	 */
	public void setShowSummary(boolean showSummary) {
		this.showSummary = showSummary;
	}
	
	/**
	 * Indicates whether the report should apply "pretty" rendering options.
	 *
	 * @return {@code true} when pretty rendering is enabled.
	 */
	public boolean isPretty() {
		return pretty;
	}
	
	/**
	 * Sets whether the report should apply "pretty" rendering options.
	 *
	 * @param pretty {@code true} to enable pretty rendering.
	 */
	public void setPretty(boolean pretty) {
		this.pretty = pretty;
	}
	
	/**
	 * Sets the configured report layout style.
	 *
	 * @param reportStyle The report style.
	 */
	public void setReportStyle(ReportStyle reportStyle) {
		this.reportStyle = reportStyle;
	}
	
	/**
	 * Returns the page width used for layout generation.
	 *
	 * @return The page width.
	 */
	public int getPageWidth() {
		return pageWidth;
	}
	
	/**
	 * Sets the page width used for layout generation.
	 *
	 * @param pageWidth The page width.
	 */
	public void setPageWidth(int pageWidth) {
		this.pageWidth = pageWidth;
	}
	
	/**
	 * Returns the page height used for layout generation.
	 *
	 * @return The page height.
	 */
	public int getPageHeight() {
		return pageHeight;
	}
	
	/**
	 * Sets the page height used for layout generation.
	 *
	 * @param pageHeight The page height.
	 */
	public void setPageHeight(int pageHeight) {
		this.pageHeight = pageHeight;
	}
	
	/**
	 * Returns the top page margin.
	 *
	 * @return The top margin.
	 */
	public int getTopMargin() {
		return topMargin;
	}
	
	/**
	 * Sets the top page margin.
	 *
	 * @param topMargin The top margin.
	 */
	public void setTopMargin(int topMargin) {
		this.topMargin = topMargin;
	}
	
	/**
	 * Returns the bottom page margin.
	 *
	 * @return The bottom margin.
	 */
	public int getBottomMargin() {
		return bottomMargin;
	}
	
	/**
	 * Sets the bottom page margin.
	 *
	 * @param bottomMargin The bottom margin.
	 */
	public void setBottomMargin(int bottomMargin) {
		this.bottomMargin = bottomMargin;
	}
	
	/**
	 * Returns the left page margin.
	 *
	 * @return The left margin.
	 */
	public int getLeftMargin() {
		return leftMargin;
	}
	
	/**
	 * Sets the left page margin.
	 *
	 * @param leftMargin The left margin.
	 */
	public void setLeftMargin(int leftMargin) {
		this.leftMargin = leftMargin;
	}
	
	/**
	 * Returns the right page margin.
	 *
	 * @return The right margin.
	 */
	public int getRightMargin() {
		return rightMargin;
	}
	
	/**
	 * Sets the right page margin.
	 *
	 * @param rightMargin The right margin.
	 */
	public void setRightMargin(int rightMargin) {
		this.rightMargin = rightMargin;
	}
	
	/**
	 * Returns the mutable list of report columns.
	 *
	 * @return The report column definitions.
	 */
	public List<ReportColumn> getColumns() {
		return columns;
	}
	
	/**
	 * Returns the mutable list of grouping column names.
	 *
	 * @return The grouping column names.
	 */
	public List<String> getGroupColumns() {
		return groupColumns;
	}
	
	/**
	 * Indicates whether the customer logo should be rendered in the title band.
	 *
	 * @return {@code true} when the logo should be included.
	 */
	public boolean isIncludeCustomerLogo() {
		return includeCustomerLogo;
	}
	
	/**
	 * Sets whether the customer logo should be rendered in the title band.
	 *
	 * @param includeCustomerLogo {@code true} to include the logo.
	 */
	public void setIncludeCustomerLogo(boolean includeCustomerLogo) {
		this.includeCustomerLogo = includeCustomerLogo;
	}
}
