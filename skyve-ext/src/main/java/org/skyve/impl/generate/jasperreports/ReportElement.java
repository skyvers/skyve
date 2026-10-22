package org.skyve.impl.generate.jasperreports;

import org.apache.commons.lang3.StringUtils;
import org.skyve.domain.types.Decimal2;
import org.skyve.impl.metadata.view.HorizontalAlignment;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Describes a single positioned element (text field, static text, image) within
 * a JasperReports band during programmatic {@code .jrxml} generation.
 */
public class ReportElement {
	@SuppressWarnings("java:S115") // Enum names are JasperReports/view element names.
	public enum ElementType {
		staticText, 
		textField, 
		checkBox, 
		combo, 
		colourPicker,
		contentImage, 
		dynamicImage,
		geometry,
		staticImage, 
		line, 
		radio,
		richTextField,
		html,
		subreport, 
		slider,
		spinner,
		border
	}

	@SuppressWarnings("java:S115") // Enum names are JasperReports alignment names.
	public enum ElementAlignment {
		left, right, center;

		@Override
		public @Nonnull String toString() {
			return StringUtils.capitalize(super.toString());
		}
		/**
		 * handle skyve spelling of "centre" vs jasper spelling of "center" 
		 * 
		 * @param ha
		 * @return
		 */
		public static @Nullable ElementAlignment fromHorizontalAlignment(@Nullable HorizontalAlignment ha) {
			if (ha != null) {
				return ElementAlignment.valueOf(ha.toTextAlignmentString());
			}
			return null;
		}
	}

	@SuppressWarnings("java:S115") // Enum names are JasperReports evaluation-time names.
	public enum EvaluationTime {
		now, report, page, column, band, auto;
		
		@Override
		public @Nonnull String toString() {
			return StringUtils.capitalize(super.toString());
		}
	}

	/**
	 * Name
	 **/
	private String name;
	/**
	 * Type
	 **/
	private ElementType elementType;
	/**
	 * Top
	 **/
	private Integer elementTop;
	/**
	 * Left
	 **/
	private Integer elementLeft;
	/**
	 * Width
	 **/
	private Integer elementWidth;
	/**
	 * Height
	 **/
	private Integer elementHeight;
	/**
	 * Border
	 **/
	private Boolean elementBorder;
	/**
	 * Value
	 **/
	private String elementValue;
	/**
	 * Alignment
	 **/
	private ElementAlignment elementAlignment;
	/**
	 * Font Name
	 **/
	private String elementFontName;
	/**
	 * Font Size
	 **/
	private Integer elementFontSize;
	/**
	 * Evaluation Time
	 **/
	private EvaluationTime evaluationTime;
	/**
	 * Collection Document Name
	 **/
	private String collectionDocumentName;
	/**
	 * Top Padding
	 **/
	private Integer topPadding;
	/**
	 * Left Padding
	 **/
	private Integer leftPadding;
	/**
	 * Bottom Padding
	 **/
	private Integer bottomPadding;
	/**
	 * Right Padding
	 **/
	private Integer rightPadding;
	/**
	 * Border Colour
	 **/
	private String borderColour;
	/**
	 * Border Line Pen (stroke thickness)
	 **/
	private Decimal2 borderLineWidth;
	/**
	 * Border Top
	 **/
	private Boolean borderTop;
	/**
	 * Border Left
	 **/
	private Boolean borderLeft;
	/**
	 * Border Bottom
	 **/
	private Boolean borderBottom;
	/**
	 * Border Right
	 **/
	private Boolean borderRight;
	/**
	 * Dynamic Flow
	 * <br/>
	 * <p>
	 * <b>Dynamic Flow</b>
	 * </p>
	 * <p>
	 * Allow (as far as possible) page sections, bands and fields to flow as much as required
	 * to show all data.
	 * </p>
	 **/
	private Boolean dynamicFlow;
	/**
	 * Bold
	 **/
	private Boolean elementBold;
	/**
	 * Italic
	 **/
	private Boolean elementItalic;
	/**
	 * Invisible Condition Name
	 **/
	private String invisibleConditionName;
	/**
	 * Pixel Width
	 **/
	private Integer pixelWidth;
	/**
	 * Percentage Width
	 **/
	private Integer percentageWidth;
	/**
	 * Responsive Width
	 **/
	private Integer responsiveWidth;
	/**
	 * Row
	 **/
	private Integer row;
	/**
	 * Colour
	 **/
	private String elementForeColour;
	private String elementBackColour;

	private ReportBand parent;
	private Integer ordinal;
	
	private String reportFileName;
	
	private ReportField field;

	public @Nullable Integer getOrdinal() {
		return ordinal;
	}

	public void setOrdinal(@Nullable Integer ordinal) {
		this.ordinal = ordinal;
	}

	public @Nullable String getName() {
		return name;
	}

	public void setName(@Nullable String name) {
		this.name = name;
	}

	public @Nonnull ElementType getElementType() {
		return elementType;
	}

	public void setElementType(@Nonnull ElementType elementType) {
		this.elementType = elementType;
	}

	public @Nullable Integer getElementTop() {
		return elementTop;
	}

	public void setElementTop(@Nullable Integer elementTop) {
		this.elementTop = elementTop;
	}

	public @Nullable Integer getElementLeft() {
		return elementLeft;
	}

	public void setElementLeft(@Nullable Integer elementLeft) {
		this.elementLeft = elementLeft;
	}

	public @Nullable Integer getElementWidth() {
		return elementWidth;
	}

	public void setElementWidth(@Nullable Integer elementWidth) {
		this.elementWidth = elementWidth;
	}

	public @Nullable Integer getElementHeight() {
		return elementHeight;
	}

	public void setElementHeight(@Nullable Integer elementHeight) {
		this.elementHeight = elementHeight;
	}

	public @Nullable Boolean getElementBorder() {
		return elementBorder;
	}

	public void setElementBorder(@Nullable Boolean elementBorder) {
		this.elementBorder = elementBorder;
	}

	public @Nullable String getElementValue() {
		return elementValue;
	}

	public void setElementValue(@Nullable String elementValue) {
		this.elementValue = elementValue;
	}

	public @Nullable ElementAlignment getElementAlignment() {
		return elementAlignment;
	}

	public void setElementAlignment(@Nullable ElementAlignment elementAlignment) {
		this.elementAlignment = elementAlignment;
	}

	public @Nullable String getElementFontName() {
		return elementFontName;
	}

	public void setElementFontName(@Nullable String elementFontName) {
		this.elementFontName = elementFontName;
	}

	public @Nullable Integer getElementFontSize() {
		return elementFontSize;
	}

	public void setElementFontSize(@Nullable Integer elementFontSize) {
		this.elementFontSize = elementFontSize;
	}

	public @Nullable EvaluationTime getEvaluationTime() {
		return evaluationTime;
	}

	public void setEvaluationTime(@Nullable EvaluationTime evaluationTime) {
		this.evaluationTime = evaluationTime;
	}

	public @Nullable String getCollectionDocumentName() {
		return collectionDocumentName;
	}

	public void setCollectionDocumentName(@Nullable String collectionDocumentName) {
		this.collectionDocumentName = collectionDocumentName;
	}

	public @Nullable Integer getTopPadding() {
		return topPadding;
	}

	public void setTopPadding(@Nullable Integer topPadding) {
		this.topPadding = topPadding;
	}

	public @Nullable Integer getLeftPadding() {
		return leftPadding;
	}

	public void setLeftPadding(@Nullable Integer leftPadding) {
		this.leftPadding = leftPadding;
	}

	public @Nullable Integer getBottomPadding() {
		return bottomPadding;
	}

	public void setBottomPadding(@Nullable Integer bottomPadding) {
		this.bottomPadding = bottomPadding;
	}

	public @Nullable Integer getRightPadding() {
		return rightPadding;
	}

	public void setRightPadding(@Nullable Integer rightPadding) {
		this.rightPadding = rightPadding;
	}

	public @Nullable String getBorderColour() {
		return borderColour;
	}

	public void setBorderColour(@Nullable String borderColour) {
		this.borderColour = borderColour;
	}

	public @Nullable Decimal2 getBorderLineWidth() {
		return borderLineWidth;
	}

	public void setBorderLineWidth(@Nullable Decimal2 borderLineWidth) {
		this.borderLineWidth = borderLineWidth;
	}

	public @Nullable Boolean getBorderTop() {
		return borderTop;
	}

	public void setBorderTop(@Nullable Boolean borderTop) {
		this.borderTop = borderTop;
	}

	public @Nullable Boolean getBorderLeft() {
		return borderLeft;
	}

	public void setBorderLeft(@Nullable Boolean borderLeft) {
		this.borderLeft = borderLeft;
	}

	public @Nullable Boolean getBorderBottom() {
		return borderBottom;
	}

	public void setBorderBottom(@Nullable Boolean borderBottom) {
		this.borderBottom = borderBottom;
	}

	public @Nullable Boolean getBorderRight() {
		return borderRight;
	}

	public void setBorderRight(@Nullable Boolean borderRight) {
		this.borderRight = borderRight;
	}

	public @Nullable Boolean getDynamicFlow() {
		return dynamicFlow;
	}

	public void setDynamicFlow(@Nullable Boolean dynamicFlow) {
		this.dynamicFlow = dynamicFlow;
	}

	public @Nullable Boolean getElementBold() {
		return elementBold;
	}

	public void setElementBold(@Nullable Boolean elementBold) {
		this.elementBold = elementBold;
	}

	public @Nullable Boolean getElementItalic() {
		return elementItalic;
	}

	public void setElementItalic(@Nullable Boolean elementItalic) {
		this.elementItalic = elementItalic;
	}

	public @Nullable String getInvisibleConditionName() {
		return invisibleConditionName;
	}

	public void setInvisibleConditionName(@Nullable String invisibleConditionName) {
		this.invisibleConditionName = invisibleConditionName;
	}

	public @Nullable Integer getPixelWidth() {
		return pixelWidth;
	}

	public void setPixelWidth(@Nullable Integer pixelWidth) {
		this.pixelWidth = pixelWidth;
	}

	public @Nullable Integer getPercentageWidth() {
		return percentageWidth;
	}

	public void setPercentageWidth(@Nullable Integer percentageWidth) {
		this.percentageWidth = percentageWidth;
	}

	public @Nullable Integer getResponsiveWidth() {
		return responsiveWidth;
	}

	public void setResponsiveWidth(@Nullable Integer responsiveWidth) {
		this.responsiveWidth = responsiveWidth;
	}

	public @Nullable Integer getRow() {
		return row;
	}

	public void setRow(@Nullable Integer row) {
		this.row = row;
	}

	public @Nullable String getElementForeColour() {
		return elementForeColour;
	}

	public void setElementForeColour(@Nullable String elementForeColour) {
		this.elementForeColour = elementForeColour;
	}

	public @Nullable String getElementBackColour() {
		return elementBackColour;
	}

	public void setElementBackColour(@Nullable String elementBackColour) {
		this.elementBackColour = elementBackColour;
	}

	public @Nonnull String getJrxml() {
		return Renderer.renderElement(this);
	}

	public @Nullable ReportBand getParent() {
		return parent;
	}

	public void setParent(@Nullable ReportBand parent) {
		this.parent = parent;
	}

	/**
	 * Returns the band this element has been added to.
	 *
	 * @return The parent band.
	 * @throws IllegalStateException If the element has not been added to a band.
	 */
	@Nonnull ReportBand getNullSafeParent() {
		final ReportBand result = parent;
		if (result == null) {
			throw new IllegalStateException("Report element " + name + " has not been added to a band");
		}
		return result;
	}

	public @Nullable String getReportFileName() {
		return reportFileName;
	}

	public void setReportFileName(@Nullable String reportFileName) {
		this.reportFileName = reportFileName;
	}
	public @Nullable ReportField getField() {
		return field;
	}

	public void setField(@Nullable ReportField field) {
		this.field = field;
	}




	/**
	 * Basic constructor for report element
	 * 
	 * @param type
	 * @param name
	 * @param valueExpression
	 * @param fontName
	 * @param fontSize
	 * @param top
	 * @param left
	 * @param width
	 * @param height
	 * @param border
	 * @param alignment
	 * @param bold
	 * @param italic
	 * @param printWhenExpression
	 */
	@SuppressWarnings("java:S107") // Long parameter list preserves the existing framework/API contract.
	public ReportElement(@Nonnull ElementType type, @Nullable String name, @Nullable String valueExpression, @Nullable String fontName, @Nullable Integer fontSize, @Nullable Integer top, @Nullable Integer left,
			@Nullable Integer width, @Nullable Integer height, @Nullable Boolean border, @Nullable ElementAlignment alignment, @Nullable Boolean bold, @Nullable Boolean italic, @Nullable String invisibleConditionName) {

		this.setElementType(type);

		this.setName(name);
		this.setElementValue(valueExpression);
		this.setInvisibleConditionName(invisibleConditionName);

		// font name
		this.setElementFontName(fontName);

		// font size
		this.setElementFontSize(fontSize);

		// position
		this.setElementTop(top);
		this.setElementLeft(left);
		this.setElementWidth(width);

		// height
		this.setElementHeight(height);

		// border
		this.setElementBorder(border);

		// font style
		this.setElementBold(bold);
		this.setElementItalic(italic);

		// alignment
		if (alignment == null) {
			this.setElementAlignment(ElementAlignment.left);
		} else {
			this.setElementAlignment(alignment);
		}
	}

	/**
	 * Convenience constructor to use default values
	 * 
	 * @param type
	 * @param name
	 * @param valueExpression
	 * @param top
	 * @param left
	 * @param width
	 * @param invisibleConditionName
	 */
	public ReportElement(@Nonnull ElementType type, @Nullable String name, @Nullable String valueExpression, @Nullable Integer top, @Nullable Integer left, @Nullable Integer width, @Nullable String invisibleConditionName) {
		this(type, name, valueExpression, null, null, top, left, width, null, null, null, Boolean.FALSE,
				Boolean.FALSE, invisibleConditionName);
	}

}
