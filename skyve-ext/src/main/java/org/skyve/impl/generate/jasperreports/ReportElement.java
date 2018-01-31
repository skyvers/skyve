package org.skyve.impl.generate.jasperreports;

import org.apache.commons.lang3.StringUtils;
import org.skyve.domain.types.Decimal2;
import org.skyve.impl.metadata.view.HorizontalAlignment;

public class ReportElement {
	public static enum ElementType {
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

	public static enum ElementAlignment {
		left, right, center;

		@Override
		public String toString() {
			return StringUtils.capitalize(super.toString());
		}
		/**
		 * handle skyve spelling of "centre" vs jasper spelling of "center" 
		 * 
		 * @param ha
		 * @return
		 */
		public static ElementAlignment fromHorizontalAlignment(HorizontalAlignment ha) {
			ElementAlignment itemAlignment = null;
			if (ha != null) {
				switch (ha) {
				case centre:
					itemAlignment = ReportElement.ElementAlignment.center; // note Jasper spelling
					break;
				case right:
					itemAlignment = ReportElement.ElementAlignment.right;
					break;
				case left:
					itemAlignment = ReportElement.ElementAlignment.left;
					break;
				default:
					break;
				}
			}
			return itemAlignment;
		}
	}

	public static enum EvaluationTime {
		now, report, page, column, band, auto;
		
		@Override
		public String toString() {
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

	public Integer getOrdinal() {
		return ordinal;
	}

	public void setOrdinal(Integer ordinal) {
		this.ordinal = ordinal;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public ElementType getElementType() {
		return elementType;
	}

	public void setElementType(ElementType elementType) {
		this.elementType = elementType;
	}

	public Integer getElementTop() {
		return elementTop;
	}

	public void setElementTop(Integer elementTop) {
		this.elementTop = elementTop;
	}

	public Integer getElementLeft() {
		return elementLeft;
	}

	public void setElementLeft(Integer elementLeft) {
		this.elementLeft = elementLeft;
	}

	public Integer getElementWidth() {
		return elementWidth;
	}

	public void setElementWidth(Integer elementWidth) {
		this.elementWidth = elementWidth;
	}

	public Integer getElementHeight() {
		return elementHeight;
	}

	public void setElementHeight(Integer elementHeight) {
		this.elementHeight = elementHeight;
	}

	public Boolean getElementBorder() {
		return elementBorder;
	}

	public void setElementBorder(Boolean elementBorder) {
		this.elementBorder = elementBorder;
	}

	public String getElementValue() {
		return elementValue;
	}

	public void setElementValue(String elementValue) {
		this.elementValue = elementValue;
	}

	public ElementAlignment getElementAlignment() {
		return elementAlignment;
	}

	public void setElementAlignment(ElementAlignment elementAlignment) {
		this.elementAlignment = elementAlignment;
	}

	public String getElementFontName() {
		return elementFontName;
	}

	public void setElementFontName(String elementFontName) {
		this.elementFontName = elementFontName;
	}

	public Integer getElementFontSize() {
		return elementFontSize;
	}

	public void setElementFontSize(Integer elementFontSize) {
		this.elementFontSize = elementFontSize;
	}

	public EvaluationTime getEvaluationTime() {
		return evaluationTime;
	}

	public void setEvaluationTime(EvaluationTime evaluationTime) {
		this.evaluationTime = evaluationTime;
	}

	public String getCollectionDocumentName() {
		return collectionDocumentName;
	}

	public void setCollectionDocumentName(String collectionDocumentName) {
		this.collectionDocumentName = collectionDocumentName;
	}

	public Integer getTopPadding() {
		return topPadding;
	}

	public void setTopPadding(Integer topPadding) {
		this.topPadding = topPadding;
	}

	public Integer getLeftPadding() {
		return leftPadding;
	}

	public void setLeftPadding(Integer leftPadding) {
		this.leftPadding = leftPadding;
	}

	public Integer getBottomPadding() {
		return bottomPadding;
	}

	public void setBottomPadding(Integer bottomPadding) {
		this.bottomPadding = bottomPadding;
	}

	public Integer getRightPadding() {
		return rightPadding;
	}

	public void setRightPadding(Integer rightPadding) {
		this.rightPadding = rightPadding;
	}

	public String getBorderColour() {
		return borderColour;
	}

	public void setBorderColour(String borderColour) {
		this.borderColour = borderColour;
	}

	public Decimal2 getBorderLineWidth() {
		return borderLineWidth;
	}

	public void setBorderLineWidth(Decimal2 borderLineWidth) {
		this.borderLineWidth = borderLineWidth;
	}

	public Boolean getBorderTop() {
		return borderTop;
	}

	public void setBorderTop(Boolean borderTop) {
		this.borderTop = borderTop;
	}

	public Boolean getBorderLeft() {
		return borderLeft;
	}

	public void setBorderLeft(Boolean borderLeft) {
		this.borderLeft = borderLeft;
	}

	public Boolean getBorderBottom() {
		return borderBottom;
	}

	public void setBorderBottom(Boolean borderBottom) {
		this.borderBottom = borderBottom;
	}

	public Boolean getBorderRight() {
		return borderRight;
	}

	public void setBorderRight(Boolean borderRight) {
		this.borderRight = borderRight;
	}

	public Boolean getDynamicFlow() {
		return dynamicFlow;
	}

	public void setDynamicFlow(Boolean dynamicFlow) {
		this.dynamicFlow = dynamicFlow;
	}

	public Boolean getElementBold() {
		return elementBold;
	}

	public void setElementBold(Boolean elementBold) {
		this.elementBold = elementBold;
	}

	public Boolean getElementItalic() {
		return elementItalic;
	}

	public void setElementItalic(Boolean elementItalic) {
		this.elementItalic = elementItalic;
	}

	public String getInvisibleConditionName() {
		return invisibleConditionName;
	}

	public void setInvisibleConditionName(String invisibleConditionName) {
		this.invisibleConditionName = invisibleConditionName;
	}

	public Integer getPixelWidth() {
		return pixelWidth;
	}

	public void setPixelWidth(Integer pixelWidth) {
		this.pixelWidth = pixelWidth;
	}

	public Integer getPercentageWidth() {
		return percentageWidth;
	}

	public void setPercentageWidth(Integer percentageWidth) {
		this.percentageWidth = percentageWidth;
	}

	public Integer getResponsiveWidth() {
		return responsiveWidth;
	}

	public void setResponsiveWidth(Integer responsiveWidth) {
		this.responsiveWidth = responsiveWidth;
	}

	public Integer getRow() {
		return row;
	}

	public void setRow(Integer row) {
		this.row = row;
	}

	public String getElementForeColour() {
		return elementForeColour;
	}

	public void setElementForeColour(String elementForeColour) {
		this.elementForeColour = elementForeColour;
	}

	public String getElementBackColour() {
		return elementBackColour;
	}

	public void setElementBackColour(String elementBackColour) {
		this.elementBackColour = elementBackColour;
	}

	public String getJrxml() {
		return Renderer.renderElement(this);
	}

	public ReportBand getParent() {
		return parent;
	}

	public void setParent(ReportBand parent) {
		this.parent = parent;
	}
	
	public String getReportFileName() {
		return reportFileName;
	}

	public void setReportFileName(String reportFileName) {
		this.reportFileName = reportFileName;
	}
	public ReportField getField() {
		return field;
	}

	public void setField(ReportField field) {
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
	public ReportElement(ReportElement.ElementType type, String name, String valueExpression, String fontName, Integer fontSize, Integer top, Integer left,
			Integer width, Integer height, Boolean border, ReportElement.ElementAlignment alignment, Boolean bold, Boolean italic, String invisibleConditionName) {

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
			this.setElementAlignment(ReportElement.ElementAlignment.left);
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
	public ReportElement(ReportElement.ElementType type, String name, String valueExpression, Integer top, Integer left, Integer width, String invisibleConditionName) {
		this(type, name, valueExpression, null, null, top, left, width, null, null, null, false, false, invisibleConditionName);
	}

}
