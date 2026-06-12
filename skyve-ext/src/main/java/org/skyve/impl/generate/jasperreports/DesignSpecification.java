package org.skyve.impl.generate.jasperreports;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.skyve.CORE;
import org.skyve.domain.types.Decimal2;
import org.skyve.domain.types.Decimal5;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Collection.CollectionType;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;

/**
 * Holds the design parameters used to programmatically generate a JasperReports
 * report design ({@code .jrxml}) from Skyve module/document metadata.
 *
 * <p>Consumed by {@link ReportDesignGenerator} subclasses to determine layout,
 * field placement, grouping, and column widths.
 */
public class DesignSpecification {
	private int alias = 'a';
	private Map<String, String> joinAlias = null;
	private Map<String, String> joins = null;
	
	@SuppressWarnings("java:S115") // Enum names are report generation option codes.
	public enum Mode {
		sql,
		bean
	}

	@SuppressWarnings("java:S115") // Enum names are report generation option codes.
	public enum DefinitionSource {
		document,
		view,
		query,
		list
	}

// TODO do I need to Capitalise the toString().
	@SuppressWarnings("java:S115") // Enum names are report generation option codes.
	public enum ReportType {
		report,
		subreport;
	}

// TODO do I need to Capitalise the toString().
	@SuppressWarnings("java:S115") // Enum names are report generation option codes.
	public enum Orientation {
		portrait,
		landscape
	}

	private String language;
	/**
	 * Report Name
	 **/
	private String name;
	/**
	 * Mode
	 **/
	private Mode mode;
	/**
	 * Definition Source
	 **/
	private DefinitionSource definitionSource;
	/**
	 * Type
	 **/
	private ReportType reportType;
	/**
	 * Module
	 * <br/>
	 * Report Bean Module.
	 **/
	private String moduleName;
	/**
	 * Document
	 * <br/>
	 * Report Bean Document.
	 **/
	private String documentName;
    /**
     * Report Query Name.
     **/
    private String queryName;
	/**
	 * If a view report, this is the uxui to create the report for
	 */
	private String uxui;
	/**
	 * Path to metadata repository
	 * <br/>
	 * <p>The path to the place where you want jrxml files created.</p>
			<p>Normally this will be the path to the folder where you modules are defined - e.g. C:\workspace\projectName\src\skyve\</p>
	 **/
	private String repositoryPath;
	/**
	 * Save to Document Package
	 **/
	private Boolean saveToDocumentPackage;
	/**
	 * Orientation
	 **/
	private Orientation orientation;
	/**
	 * Width
	 **/
	private Integer width;
	/**
	 * Height
	 **/
	private Integer height;
	/**
	 * Left Margin
	 **/
	private Integer leftMargin;
	/**
	 * Right Margin
	 **/
	private Integer rightMargin;
	/**
	 * Top Margin
	 **/
	private Integer topMargin;
	/**
	 * Bottom Margin
	 **/
	private Integer bottomMargin;
	/**
	 * Column Width
	 **/
	private Integer columnWidth;
	/**
	 * Font Name
	 * <br/>
	 * Font not working? Check that you've included a font extension jar for your font in the classpath
	 **/
	private String defaultFontName;
	/**
	 * Font Size (Title)
	 **/
	private Integer titleFontSize;
	/**
	 * Font Size (Detail)
	 **/
	private Integer defaultFontSize;
	/**
	 * Render Labels as TextFields
	 * <br/>
	 * This option will allow labels to flow and stretch like text fields
	 **/
	private Boolean renderLabelAsTextFields;
	/**
	 * Default Line Colour
	 **/
	private String defaultLineColour;
	/**
	 * Default Line Pen Stroke thickness
	 **/
	private Decimal2 defaultLineWidth;
	/**
	 * Default Border
	 **/
	private Boolean defaultBorder;
	/**
	 * Default Border Top
	 **/
	private Boolean defaultBorderTop;
	/**
	 * Default Border Left
	 **/
	private Boolean defaultBorderLeft;
	/**
	 * Default Border Bottom
	 **/
	private Boolean defaultBorderBottom;
	/**
	 * Default Border Right
	 **/
	private Boolean defaultBorderRight;
	/**
	 * Default Element Height
	 **/
	private Integer defaultElementHeight;
	/**
	 * Include Page Numbers
	 **/
	private Boolean includePageNumbers;
	/**
	 * Cell Top Padding
	 **/
	private Integer defaultCellTopPadding;
	/**
	 * Cell Left Padding
	 **/
	private Integer defaultCellLeftPadding;
	/**
	 * Cell Bottom Padding
	 **/
	private Integer defaultCellBottomPadding;
	/**
	 * Cell Right Padding
	 **/
	private Integer defaultCellRightPadding;
	/**
	 * Dynamic Flow
	 * <br/>
	 * <p><b>Dynamic Flow</b></p>
			<p>Allow (as far as possible) page sections, bands and fields to flow as much as required 
			to show all data.</p>
	 **/
	private Boolean dynamicFlow;
	/**
	 * Split Type
	 **/
	private ReportBand.SplitType bandSplitType;
	/**
	 * Bold Labels
	 * <br/>
	 * Bold not working? Check that you've included a font extension jar for your font in the classpath
	 **/
	private Boolean boldLabels;
	/**
	 * Checkbox FontName
	 * <br/>
	 * This is the font for checkboxes - ensure the font will be on your classpath
	 **/
	private String checkBoxFontName;
	/**
	 * Checkbox Display Expression
	 * <br/>
	 * This is the expression to use to represent checkboxes
	 **/
	private String checkBoxDisplayExpression;
	/**
	 * Pixel to TWIP
	 * <br/>
	 * For pixel specified item widths, what TWIP size to use.
	 **/
	private Decimal5 pixelToTwip;
	/**
	 * Border Top
	 **/
	private Boolean sectionBorderTop;
	/**
	 * Border Left
	 **/
	private Boolean sectionBorderLeft;
	/**
	 * Border Right
	 **/
	private Boolean sectionBorderRight;
	/**
	 * Border Bottom
	 **/
	private Boolean sectionBorderBottom;
	/**
	 * Title Border Top
	 **/
	private Boolean sectionTitleBorderTop;
	/**
	 * Title Border Left
	 **/
	private Boolean sectionTitleBorderLeft;
	/**
	 * Title Border Right
	 **/
	private Boolean sectionTitleBorderRight;
	/**
	 * Title Border Bottom
	 **/
	private Boolean sectionTitleBorderBottom;
	/**
	 * Title Foreground
	 **/
	private String sectionTitleForeground;
	/**
	 * Title Background
	 **/
	private String sectionTitleBackground;
	/**
	 * Parameters
	 **/
	private List<ReportParameter> parameters = new ArrayList<>();
	/**
	 * Fields
	 **/
	private List<ReportField> fields = new ArrayList<>();
	/**
	 * Variables
	 **/
	private List<ReportVariable> variables = new ArrayList<>();
	/**
	 * Bands
	 **/
	private List<ReportBand> bands = new ArrayList<>();
	/**
	 * Sub Reports
	 **/
	private List<DesignSpecification> subReports = new ArrayList<>();
	/**
	 * Field
	 * <br/>
	 * The field on which this subreport is based.
	 **/
	private ReportField field = null;
	/**
	 * Collection Type
	 **/
	private CollectionType collectionType;
	/**
	 * Parent Report Persistent Name
	 **/
	private String parentReportPersistentName;
	/**
	 * Transform horizontal containers to vertical
	 **/
	private Boolean verticalise;
	/**
	 * Whether or not the customer's logo should be included in the report's title band.
	 */
	private boolean includeCustomerLogo = true;
	/**
	 * Label Alignment
	 **/
	private ReportElement.ElementAlignment labelAlignmentOverride;

	/**
	 * Returns the configured name.
	 * @return The configured value.
	 */
	public String getName() {
		return name;
	}
	
	/**
	 * Sets the configured name.
	 * @param name The name value.
	 */
	public void setName(String name) {
		this.name = name;
	}
	
	/**
	 * Returns the configured mode.
	 * @return The configured value.
	 */
	public Mode getMode() {
		return mode;
	}
	
	/**
	 * Sets the configured mode.
	 * @param mode The mode value.
	 */
	public void setMode(Mode mode) {
		this.mode = mode;
	}
	
	/**
	 * Returns the configured definitionSource.
	 * @return The configured value.
	 */
	public DefinitionSource getDefinitionSource() {
		return definitionSource;
	}
	
	/**
	 * Sets the configured definitionSource.
	 * @param definitionSource The definitionSource value.
	 */
	public void setDefinitionSource(DefinitionSource definitionSource) {
		this.definitionSource = definitionSource;
	}
	
	/**
	 * Returns the configured reportType.
	 * @return The configured value.
	 */
	public ReportType getReportType() {
		return reportType;
	}
	
	/**
	 * Sets the configured reportType.
	 * @param reportType The reportType value.
	 */
	public void setReportType(ReportType reportType) {
		this.reportType = reportType;
	}
	
	/**
	 * Sets the configured moduleName.
	 * @param moduleName The moduleName value.
	 */
	public void setModuleName(String moduleName) {
		this.moduleName = moduleName;
	}
	
	/**
	 * Sets the configured documentName.
	 * @param documentName The documentName value.
	 */
	public void setDocumentName(String documentName) {
		this.documentName = documentName;
	}
   
	/**
     * Returns the configured queryName.
     * @return The configured value.
     */
    public String getQueryName() {
        return queryName;
    }
    
    /**
     * Sets the configured queryName.
     * @param queryName The queryName value.
     */
    public void setQueryName(String queryName) {
        this.queryName = queryName;
    }
    /**
     * Returns the configured uxui.
     * @return The configured value.
     */
    public String getUxui() {
		return uxui;
	}
	
    /**
	 * Sets the configured uxui.
	 * @param uxui The uxui value.
	 */
	public void setUxui(String uxui) {
		this.uxui = uxui;
	}
	
	/**
	 * Returns the configured repositoryPath.
	 * @return The configured value.
	 */
	public String getRepositoryPath() {
		return repositoryPath;
	}
	
	/**
	 * Sets the configured repositoryPath.
	 * @param repositoryPath The repositoryPath value.
	 */
	public void setRepositoryPath(String repositoryPath) {
		this.repositoryPath = repositoryPath;
	}
	
	/**
	 * Returns the configured saveToDocumentPackage.
	 * @return The configured value.
	 */
	public Boolean getSaveToDocumentPackage() {
		return saveToDocumentPackage;
	}
	
	/**
	 * Sets the configured saveToDocumentPackage.
	 * @param saveToDocumentPackage The saveToDocumentPackage value.
	 */
	public void setSaveToDocumentPackage(Boolean saveToDocumentPackage) {
		this.saveToDocumentPackage = saveToDocumentPackage;
	}
	
	/**
	 * Returns the configured orientation.
	 * @return The configured value.
	 */
	public Orientation getOrientation() {
		return orientation;
	}
	
	/**
	 * Sets the configured orientation.
	 * @param orientation The orientation value.
	 */
	public void setOrientation(Orientation orientation) {
		this.orientation = orientation;
	}
	
	/**
	 * Returns the configured width.
	 * @return The configured value.
	 */
	public Integer getWidth() {
		return width;
	}
	
	/**
	 * Sets the configured width.
	 * @param width The width value.
	 */
	public void setWidth(Integer width) {
		this.width = width;
	}
	
	/**
	 * Returns the configured height.
	 * @return The configured value.
	 */
	public Integer getHeight() {
		return height;
	}
	
	/**
	 * Sets the configured height.
	 * @param height The height value.
	 */
	public void setHeight(Integer height) {
		this.height = height;
	}
	
	/**
	 * Returns the configured leftMargin.
	 * @return The configured value.
	 */
	public Integer getLeftMargin() {
		return leftMargin;
	}
	
	/**
	 * Sets the configured leftMargin.
	 * @param leftMargin The leftMargin value.
	 */
	public void setLeftMargin(Integer leftMargin) {
		this.leftMargin = leftMargin;
	}
	
	/**
	 * Returns the configured rightMargin.
	 * @return The configured value.
	 */
	public Integer getRightMargin() {
		return rightMargin;
	}
	
	/**
	 * Sets the configured rightMargin.
	 * @param rightMargin The rightMargin value.
	 */
	public void setRightMargin(Integer rightMargin) {
		this.rightMargin = rightMargin;
	}
	
	/**
	 * Returns the configured topMargin.
	 * @return The configured value.
	 */
	public Integer getTopMargin() {
		return topMargin;
	}
	
	/**
	 * Sets the configured topMargin.
	 * @param topMargin The topMargin value.
	 */
	public void setTopMargin(Integer topMargin) {
		this.topMargin = topMargin;
	}
	
	/**
	 * Returns the configured bottomMargin.
	 * @return The configured value.
	 */
	public Integer getBottomMargin() {
		return bottomMargin;
	}
	
	/**
	 * Sets the configured bottomMargin.
	 * @param bottomMargin The bottomMargin value.
	 */
	public void setBottomMargin(Integer bottomMargin) {
		this.bottomMargin = bottomMargin;
	}
	
	/**
	 * Returns the configured columnWidth.
	 * @return The configured value.
	 */
	public Integer getColumnWidth() {
		return columnWidth;
	}

	/**
	 * Sets the configured columnWidth.
	 * @param columnWidth The columnWidth value.
	 */
	public void setColumnWidth(Integer columnWidth) {
		this.columnWidth = columnWidth;
	}
	
	/**
	 * Returns the configured defaultFontName.
	 * @return The configured value.
	 */
	public String getDefaultFontName() {
		return defaultFontName;
	}
	
	/**
	 * Sets the configured defaultFontName.
	 * @param defaultFontName The defaultFontName value.
	 */
	public void setDefaultFontName(String defaultFontName) {
		this.defaultFontName = defaultFontName;
	}
	
	/**
	 * Returns the configured titleFontSize.
	 * @return The configured value.
	 */
	public Integer getTitleFontSize() {
		return titleFontSize;
	}
	
	/**
	 * Sets the configured titleFontSize.
	 * @param titleFontSize The titleFontSize value.
	 */
	public void setTitleFontSize(Integer titleFontSize) {
		this.titleFontSize = titleFontSize;
	}
	
	/**
	 * Returns the configured defaultFontSize.
	 * @return The configured value.
	 */
	public Integer getDefaultFontSize() {
		return defaultFontSize;
	}
	
	/**
	 * Sets the configured defaultFontSize.
	 * @param defaultFontSize The defaultFontSize value.
	 */
	public void setDefaultFontSize(Integer defaultFontSize) {
		this.defaultFontSize = defaultFontSize;
	}
	
	/**
	 * Returns the configured renderLabelAsTextFields.
	 * @return The configured value.
	 */
	public Boolean getRenderLabelAsTextFields() {
		return renderLabelAsTextFields;
	}
	
	/**
	 * Sets the configured renderLabelAsTextFields.
	 * @param renderLabelAsTextFields The renderLabelAsTextFields value.
	 */
	public void setRenderLabelAsTextFields(Boolean renderLabelAsTextFields) {
		this.renderLabelAsTextFields = renderLabelAsTextFields;
	}
	
	/**
	 * Returns the configured defaultBorder.
	 * @return The configured value.
	 */
	public Boolean getDefaultBorder() {
		return defaultBorder;
	}
	
	/**
	 * Sets the configured defaultBorder.
	 * @param defaultBorder The defaultBorder value.
	 */
	public void setDefaultBorder(Boolean defaultBorder) {
		this.defaultBorder = defaultBorder;
	}
	
	/**
	 * Returns the configured defaultBorderTop.
	 * @return The configured value.
	 */
	public Boolean getDefaultBorderTop() {
		return defaultBorderTop;
	}
	
	/**
	 * Sets the configured defaultBorderTop.
	 * @param defaultBorderTop The defaultBorderTop value.
	 */
	public void setDefaultBorderTop(Boolean defaultBorderTop) {
		this.defaultBorderTop = defaultBorderTop;
	}
	
	/**
	 * Returns the configured defaultBorderLeft.
	 * @return The configured value.
	 */
	public Boolean getDefaultBorderLeft() {
		return defaultBorderLeft;
	}
	
	/**
	 * Sets the configured defaultBorderLeft.
	 * @param defaultBorderLeft The defaultBorderLeft value.
	 */
	public void setDefaultBorderLeft(Boolean defaultBorderLeft) {
		this.defaultBorderLeft = defaultBorderLeft;
	}
	
	/**
	 * Returns the configured defaultBorderBottom.
	 * @return The configured value.
	 */
	public Boolean getDefaultBorderBottom() {
		return defaultBorderBottom;
	}
	
	/**
	 * Sets the configured defaultBorderBottom.
	 * @param defaultBorderBottom The defaultBorderBottom value.
	 */
	public void setDefaultBorderBottom(Boolean defaultBorderBottom) {
		this.defaultBorderBottom = defaultBorderBottom;
	}
	
	/**
	 * Returns the configured defaultBorderRight.
	 * @return The configured value.
	 */
	public Boolean getDefaultBorderRight() {
		return defaultBorderRight;
	}
	/**
	 * Sets the configured defaultBorderRight.
	 * @param defaultBorderRight The defaultBorderRight value.
	 */
	public void setDefaultBorderRight(Boolean defaultBorderRight) {
		this.defaultBorderRight = defaultBorderRight;
	}
	
	/**
	 * Returns the configured defaultLineColour.
	 * @return The configured value.
	 */
	public String getDefaultLineColour() {
		return defaultLineColour;
	}
	
	/**
	 * Sets the configured defaultLineColour.
	 * @param defaultLineColour The defaultLineColour value.
	 */
	public void setDefaultLineColour(String defaultLineColour) {
		this.defaultLineColour = defaultLineColour;
	}
	
	/**
	 * Returns the configured defaultLineWidth.
	 * @return The configured value.
	 */
	public Decimal2 getDefaultLineWidth() {
		return defaultLineWidth;
	}
	
	/**
	 * Sets the configured defaultLineWidth.
	 * @param defaultLineWidth The defaultLineWidth value.
	 */
	public void setDefaultLineWidth(Decimal2 defaultLineWidth) {
		this.defaultLineWidth = defaultLineWidth;
	}
	
	/**
	 * Returns the configured defaultElementHeight.
	 * @return The configured value.
	 */
	public Integer getDefaultElementHeight() {
		return defaultElementHeight;
	}
	
	/**
	 * Sets the configured defaultElementHeight.
	 * @param defaultElementHeight The defaultElementHeight value.
	 */
	public void setDefaultElementHeight(Integer defaultElementHeight) {
		this.defaultElementHeight = defaultElementHeight;
	}
	
	/**
	 * Returns the configured includePageNumbers.
	 * @return The configured value.
	 */
	public Boolean getIncludePageNumbers() {
		return includePageNumbers;
	}
	
	/**
	 * Sets the configured includePageNumbers.
	 * @param includePageNumbers The includePageNumbers value.
	 */
	public void setIncludePageNumbers(Boolean includePageNumbers) {
		this.includePageNumbers = includePageNumbers;
	}
	
	/**
	 * Returns the configured defaultCellTopPadding.
	 * @return The configured value.
	 */
	public Integer getDefaultCellTopPadding() {
		return defaultCellTopPadding;
	}
	
	/**
	 * Sets the configured defaultCellTopPadding.
	 * @param defaultCellTopPadding The defaultCellTopPadding value.
	 */
	public void setDefaultCellTopPadding(Integer defaultCellTopPadding) {
		this.defaultCellTopPadding = defaultCellTopPadding;
	}
	
	/**
	 * Returns the configured defaultCellLeftPadding.
	 * @return The configured value.
	 */
	public Integer getDefaultCellLeftPadding() {
		return defaultCellLeftPadding;
	}
	
	/**
	 * Sets the configured defaultCellLeftPadding.
	 * @param defaultCellLeftPadding The defaultCellLeftPadding value.
	 */
	public void setDefaultCellLeftPadding(Integer defaultCellLeftPadding) {
		this.defaultCellLeftPadding = defaultCellLeftPadding;
	}
	
	/**
	 * Returns the configured defaultCellBottomPadding.
	 * @return The configured value.
	 */
	public Integer getDefaultCellBottomPadding() {
		return defaultCellBottomPadding;
	}
	
	/**
	 * Sets the configured defaultCellBottomPadding.
	 * @param defaultCellBottomPadding The defaultCellBottomPadding value.
	 */
	public void setDefaultCellBottomPadding(Integer defaultCellBottomPadding) {
		this.defaultCellBottomPadding = defaultCellBottomPadding;
	}
	
	/**
	 * Returns the configured defaultCellRightPadding.
	 * @return The configured value.
	 */
	public Integer getDefaultCellRightPadding() {
		return defaultCellRightPadding;
	}
	
	/**
	 * Sets the configured defaultCellRightPadding.
	 * @param defaultCellRightPadding The defaultCellRightPadding value.
	 */
	public void setDefaultCellRightPadding(Integer defaultCellRightPadding) {
		this.defaultCellRightPadding = defaultCellRightPadding;
	}
	
	/**
	 * Returns the configured dynamicFlow.
	 * @return The configured value.
	 */
	public Boolean getDynamicFlow() {
		return dynamicFlow;
	}
	
	/**
	 * Sets the configured dynamicFlow.
	 * @param dynamicFlow The dynamicFlow value.
	 */
	public void setDynamicFlow(Boolean dynamicFlow) {
		this.dynamicFlow = dynamicFlow;
	}
	
	/**
	 * Returns the configured bandSplitType.
	 * @return The configured value.
	 */
	public ReportBand.SplitType getBandSplitType() {
		return bandSplitType;
	}
	
	/**
	 * Sets the configured bandSplitType.
	 * @param bandSplitType The bandSplitType value.
	 */
	public void setBandSplitType(ReportBand.SplitType bandSplitType) {
		this.bandSplitType = bandSplitType;
	}
	
	/**
	 * Returns the configured boldLabels.
	 * @return The configured value.
	 */
	public Boolean getBoldLabels() {
		return boldLabels;
	}
	
	/**
	 * Sets the configured boldLabels.
	 * @param boldLabels The boldLabels value.
	 */
	public void setBoldLabels(Boolean boldLabels) {
		this.boldLabels = boldLabels;
	}
	
	/**
	 * Returns the configured checkBoxFontName.
	 * @return The configured value.
	 */
	public String getCheckBoxFontName() {
		return checkBoxFontName;
	}
	
	/**
	 * Sets the configured checkBoxFontName.
	 * @param checkBoxFontName The checkBoxFontName value.
	 */
	public void setCheckBoxFontName(String checkBoxFontName) {
		this.checkBoxFontName = checkBoxFontName;
	}
	
	/**
	 * Returns the configured checkBoxDisplayExpression.
	 * @return The configured value.
	 */
	public String getCheckBoxDisplayExpression() {
		return checkBoxDisplayExpression;
	}
	
	/**
	 * Sets the configured checkBoxDisplayExpression.
	 * @param checkBoxDisplayExpression The checkBoxDisplayExpression value.
	 */
	public void setCheckBoxDisplayExpression(String checkBoxDisplayExpression) {
		this.checkBoxDisplayExpression = checkBoxDisplayExpression;
	}
	
	/**
	 * Returns the configured pixelToTwip.
	 * @return The configured value.
	 */
	public Decimal5 getPixelToTwip() {
		return pixelToTwip;
	}
	
	/**
	 * Sets the configured pixelToTwip.
	 * @param pixelToTwip The pixelToTwip value.
	 */
	public void setPixelToTwip(Decimal5 pixelToTwip) {
		this.pixelToTwip = pixelToTwip;
	}
	
	/**
	 * Returns the configured sectionBorderTop.
	 * @return The configured value.
	 */
	public Boolean getSectionBorderTop() {
		return sectionBorderTop;
	}
	
	/**
	 * Sets the configured sectionBorderTop.
	 * @param sectionBorderTop The sectionBorderTop value.
	 */
	public void setSectionBorderTop(Boolean sectionBorderTop) {
		this.sectionBorderTop = sectionBorderTop;
	}
	
	/**
	 * Returns the configured sectionBorderLeft.
	 * @return The configured value.
	 */
	public Boolean getSectionBorderLeft() {
		return sectionBorderLeft;
	}
	
	/**
	 * Sets the configured sectionBorderLeft.
	 * @param sectionBorderLeft The sectionBorderLeft value.
	 */
	public void setSectionBorderLeft(Boolean sectionBorderLeft) {
		this.sectionBorderLeft = sectionBorderLeft;
	}
	
	/**
	 * Returns the configured sectionBorderRight.
	 * @return The configured value.
	 */
	public Boolean getSectionBorderRight() {
		return sectionBorderRight;
	}
	
	/**
	 * Sets the configured sectionBorderRight.
	 * @param sectionBorderRight The sectionBorderRight value.
	 */
	public void setSectionBorderRight(Boolean sectionBorderRight) {
		this.sectionBorderRight = sectionBorderRight;
	}
	
	/**
	 * Returns the configured sectionBorderBottom.
	 * @return The configured value.
	 */
	public Boolean getSectionBorderBottom() {
		return sectionBorderBottom;
	}
	
	/**
	 * Sets the configured sectionBorderBottom.
	 * @param sectionBorderBottom The sectionBorderBottom value.
	 */
	public void setSectionBorderBottom(Boolean sectionBorderBottom) {
		this.sectionBorderBottom = sectionBorderBottom;
	}
	
	/**
	 * Returns the configured sectionTitleBorderTop.
	 * @return The configured value.
	 */
	public Boolean getSectionTitleBorderTop() {
		return sectionTitleBorderTop;
	}
	
	/**
	 * Sets the configured sectionTitleBorderTop.
	 * @param sectionTitleBorderTop The sectionTitleBorderTop value.
	 */
	public void setSectionTitleBorderTop(Boolean sectionTitleBorderTop) {
		this.sectionTitleBorderTop = sectionTitleBorderTop;
	}
	
	/**
	 * Returns the configured sectionTitleBorderLeft.
	 * @return The configured value.
	 */
	public Boolean getSectionTitleBorderLeft() {
		return sectionTitleBorderLeft;
	}
	
	/**
	 * Sets the configured sectionTitleBorderLeft.
	 * @param sectionTitleBorderLeft The sectionTitleBorderLeft value.
	 */
	public void setSectionTitleBorderLeft(Boolean sectionTitleBorderLeft) {
		this.sectionTitleBorderLeft = sectionTitleBorderLeft;
	}
	
	/**
	 * Returns the configured sectionTitleBorderRight.
	 * @return The configured value.
	 */
	public Boolean getSectionTitleBorderRight() {
		return sectionTitleBorderRight;
	}
	
	/**
	 * Sets the configured sectionTitleBorderRight.
	 * @param sectionTitleBorderRight The sectionTitleBorderRight value.
	 */
	public void setSectionTitleBorderRight(Boolean sectionTitleBorderRight) {
		this.sectionTitleBorderRight = sectionTitleBorderRight;
	}
	
	/**
	 * Returns the configured sectionTitleBorderBottom.
	 * @return The configured value.
	 */
	public Boolean getSectionTitleBorderBottom() {
		return sectionTitleBorderBottom;
	}
	
	/**
	 * Sets the configured sectionTitleBorderBottom.
	 * @param sectionTitleBorderBottom The sectionTitleBorderBottom value.
	 */
	public void setSectionTitleBorderBottom(Boolean sectionTitleBorderBottom) {
		this.sectionTitleBorderBottom = sectionTitleBorderBottom;
	}
	
	/**
	 * Returns the configured sectionTitleForeground.
	 * @return The configured value.
	 */
	public String getSectionTitleForeground() {
		return sectionTitleForeground;
	}
	
	/**
	 * Sets the configured sectionTitleForeground.
	 * @param sectionTitleForeground The sectionTitleForeground value.
	 */
	public void setSectionTitleForeground(String sectionTitleForeground) {
		this.sectionTitleForeground = sectionTitleForeground;
	}
	
	/**
	 * Returns the configured sectionTitleBackground.
	 * @return The configured value.
	 */
	public String getSectionTitleBackground() {
		return sectionTitleBackground;
	}
	
	/**
	 * Sets the configured sectionTitleBackground.
	 * @param sectionTitleBackground The sectionTitleBackground value.
	 */
	public void setSectionTitleBackground(String sectionTitleBackground) {
		this.sectionTitleBackground = sectionTitleBackground;
	}
	
	/**
	 * Returns the configured parameters.
	 * @return The configured value.
	 */
	public List<ReportParameter> getParameters() {
		return parameters;
	}

	/**
	 * Sets the configured parameters.
	 * @param parameters The parameters value.
	 */
	public void setParameters(List<ReportParameter> parameters) {
		this.parameters = parameters;
	}

	/**
	 * Returns the configured fields.
	 * @return The configured value.
	 */
	public List<ReportField> getFields() {
		return fields;
	}

	/**
	 * Sets the configured fields.
	 * @param fields The fields value.
	 */
	public void setFields(List<ReportField> fields) {
		this.fields = fields;
	}

	/**
	 * Returns the configured variables.
	 * @return The configured value.
	 */
	public List<ReportVariable> getVariables() {
		return variables;
	}

	/**
	 * Sets the configured variables.
	 * @param variables The variables value.
	 */
	public void setVariables(List<ReportVariable> variables) {
		this.variables = variables;
	}

	/**
	 * Returns the configured bands.
	 * @return The configured value.
	 */
	public List<ReportBand> getBands() {
		return bands;
	}

	/**
	 * Sets the configured bands.
	 * @param bands The bands value.
	 */
	public void setBands(List<ReportBand> bands) {
		this.bands = bands;
	}

	/**
	 * Returns the configured subReports.
	 * @return The configured value.
	 */
	public List<DesignSpecification> getSubReports() {
		return subReports;
	}

	/**
	 * Sets the configured subReports.
	 * @param subReports The subReports value.
	 */
	public void setSubReports(List<DesignSpecification> subReports) {
		this.subReports = subReports;
	}

	/**
	 * Returns the configured field.
	 * @return The configured value.
	 */
	public ReportField getField() {
		return field;
	}

	/**
	 * Sets the configured field.
	 * @param field The field value.
	 */
	public void setField(ReportField field) {
		this.field = field;
	}

	/**
	 * Returns the configured collectionType.
	 * @return The configured value.
	 */
	public CollectionType getCollectionType() {
		return collectionType;
	}

	/**
	 * Sets the configured collectionType.
	 * @param collectionType The collectionType value.
	 */
	public void setCollectionType(CollectionType collectionType) {
		this.collectionType = collectionType;
	}

	/**
	 * Returns the configured parentReportPersistentName.
	 * @return The configured value.
	 */
	public String getParentReportPersistentName() {
		return parentReportPersistentName;
	}

	/**
	 * Sets the configured parentReportPersistentName.
	 * @param parentReportPersistentName The parentReportPersistentName value.
	 */
	public void setParentReportPersistentName(String parentReportPersistentName) {
		this.parentReportPersistentName = parentReportPersistentName;
	}

	/**
	 * Returns the configured verticalise.
	 * @return The configured value.
	 */
	public Boolean getVerticalise() {
		return verticalise;
	}

	/**
	 * Sets the configured verticalise.
	 * @param verticalise The verticalise value.
	 */
	public void setVerticalise(Boolean verticalise) {
		this.verticalise = verticalise;
	}

	/**
	 * Returns the configured labelAlignmentOverride.
	 * @return The configured value.
	 */
	public ReportElement.ElementAlignment getLabelAlignmentOverride() {
		return labelAlignmentOverride;
	}

	/**
	 * Sets the configured labelAlignmentOverride.
	 * @param labelAlignmentOverride The labelAlignmentOverride value.
	 */
	public void setLabelAlignmentOverride(ReportElement.ElementAlignment labelAlignmentOverride) {
		this.labelAlignmentOverride = labelAlignmentOverride;
	}

	/**
	 * Returns the configured moduleName.
	 * @return The configured value.
	 */
	public String getModuleName() {
		return moduleName;
	}

	/**
	 * Returns the configured documentName.
	 * @return The configured value.
	 */
	public String getDocumentName() {
		return documentName;
	}

	/**
	 * Returns the configured alias.
	 * @return The configured value.
	 */
	public int getAlias() {
		return alias;
	}

	/**
	 * Sets the configured alias.
	 * @param alias The alias value.
	 */
	public void setAlias(int alias) {
		this.alias = alias;
	}

	/**
	 * Returns the configured joinAlias.
	 * @return The configured value.
	 */
	public Map<String, String> getJoinAlias() {
		return joinAlias;
	}

	/**
	 * Sets the configured joinAlias.
	 * @param joinAlias The joinAlias value.
	 */
	public void setJoinAlias(Map<String, String> joinAlias) {
		this.joinAlias = joinAlias;
	}

	/**
	 * Returns the configured joins.
	 * @return The configured value.
	 */
	public Map<String, String> getJoins() {
		return joins;
	}

	/**
	 * Sets the configured joins.
	 * @param joins The joins value.
	 */
	public void setJoins(Map<String, String> joins) {
		this.joins = joins;
	}
	
	/**
	 * Registers a generated SQL join and its corresponding table alias.
	 *
	 * @param documentName The logical join key (document or join path).
	 * @param alias The table alias assigned to the join.
	 * @param join The SQL join fragment.
	 */
	public void addJoin(@SuppressWarnings("hiding") String documentName, @SuppressWarnings("hiding") String alias, String join){
		if(joins==null){
			joins = new HashMap<>();
			joinAlias = new HashMap<>();
		}
		joins.put(documentName, join);
		joinAlias.put(join, alias);
	}

	/**
	 * Indicates whether the title band should include the customer logo.
	 *
	 * @return {@code true} when customer branding should be rendered.
	 */
	public boolean isIncludeCustomerLogo() {
		return includeCustomerLogo;
	}

	/**
	 * Sets whether the title band should include the customer logo.
	 *
	 * @param includeCustomerLogo The branding flag.
	 */
	public void setIncludeCustomerLogo(boolean includeCustomerLogo) {
		this.includeCustomerLogo = includeCustomerLogo;
	}

	/**
	 * Creates a design with default report settings and A4 portrait metrics.
	 *
	 * <p>Side effects: initialises mutable collections and baseline rendering
	 * options before calling {@link #resetDesign()}.
	 */
	public DesignSpecification(){
		
		fields = new ArrayList<>();
		bands = new ArrayList<>();
		
		setReportType(ReportType.report);
		
		setDefaultElementHeight(Integer.valueOf(20));
		setOrientation(Orientation.portrait);

		setLeftMargin(Integer.valueOf(30));
		setRightMargin(Integer.valueOf(30));
		setTopMargin(Integer.valueOf(42));
		setBottomMargin(Integer.valueOf(30));
		setMode(Mode.bean);
		setDefaultFontName("SansSerif");
		setDefaultFontSize(Integer.valueOf(12));
		setTitleFontSize(Integer.valueOf(16));
		setDefinitionSource(DefinitionSource.view);
		setDynamicFlow(Boolean.TRUE);
		setRenderLabelAsTextFields(Boolean.TRUE);
		setBandSplitType(ReportBand.SplitType.prevent);
		
		setDefaultCellTopPadding(Integer.valueOf(2));
		setDefaultCellLeftPadding(Integer.valueOf(2));
		setDefaultCellBottomPadding(Integer.valueOf(2));
		setDefaultCellRightPadding(Integer.valueOf(2));
		
		setDefaultBorder(Boolean.FALSE);
		setDefaultLineWidth(new Decimal2(1.0));

		setDefaultBorderTop(Boolean.FALSE);
		setDefaultBorderLeft(Boolean.FALSE);
		setDefaultBorderBottom(Boolean.FALSE);
		setDefaultBorderRight(Boolean.FALSE);
		setBoldLabels(Boolean.TRUE);
		
		setPixelToTwip(Decimal5.ONE);
		
		//checkbox defaults
		setCheckBoxDisplayExpression("(Boolean.TRUE.equals(<fieldName>)?\"Yes\":\"No\")");
		setCheckBoxFontName(null);
		
		resetDesign();
	}
	
	/**
	 * Recalculates page dimensions and column width from orientation and margins.
	 */
	public void resetDesign() {
		if (Orientation.portrait.equals(getOrientation())) {
			setWidth(Integer.valueOf(595));
			setColumnWidth(Integer.valueOf(595 - getLeftMargin().intValue() - getRightMargin().intValue()));
			setHeight(Integer.valueOf(842));
		} else {
			setWidth(Integer.valueOf(842));
			setColumnWidth(Integer.valueOf(842 - getLeftMargin().intValue() - getRightMargin().intValue()));
			setHeight(Integer.valueOf(595));
		}
	}

	/**
	 * Resolves the configured module from the current customer context.
	 *
	 * @return The resolved module metadata.
	 */
	public Module getModule() {
		final Customer customer = CORE.getCustomer();
		return customer.getModule(getModuleName());
	}

	/**
	 * Resolves the configured document from the current customer context.
	 *
	 * @return The resolved document metadata.
	 */
	public Document getDocument() {
		final Customer customer = CORE.getCustomer();
		return getModule().getDocument(customer, getDocumentName());
	}

	/**
	 * Returns the configured optional Jasper expression language override.
	 *
	 * @return The configured language, or {@code null} to use Jasper defaults.
	 */
	public String getLanguage() {
		return language;
	}

	/**
	 * Sets the configured language.
	 * @param language The language value.
	 */
	public void setLanguage(String language) {
		this.language = language;
	}
}
