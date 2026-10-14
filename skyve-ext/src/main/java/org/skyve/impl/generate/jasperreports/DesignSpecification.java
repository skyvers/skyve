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

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

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
	public @Nullable String getName() {
		return name;
	}
	
	/**
	 * Sets the configured name.
	 * @param name The name value.
	 */
	public void setName(@Nullable String name) {
		this.name = name;
	}
	
	/**
	 * Returns the configured mode.
	 * @return The configured value.
	 */
	public @Nullable Mode getMode() {
		return mode;
	}
	
	/**
	 * Sets the configured mode.
	 * @param mode The mode value.
	 */
	public void setMode(@Nullable Mode mode) {
		this.mode = mode;
	}
	
	/**
	 * Returns the configured definitionSource.
	 * @return The configured value.
	 */
	public @Nullable DefinitionSource getDefinitionSource() {
		return definitionSource;
	}
	
	/**
	 * Sets the configured definitionSource.
	 * @param definitionSource The definitionSource value.
	 */
	public void setDefinitionSource(@Nullable DefinitionSource definitionSource) {
		this.definitionSource = definitionSource;
	}
	
	/**
	 * Returns the configured reportType.
	 * @return The configured value.
	 */
	public @Nullable ReportType getReportType() {
		return reportType;
	}
	
	/**
	 * Sets the configured reportType.
	 * @param reportType The reportType value.
	 */
	public void setReportType(@Nullable ReportType reportType) {
		this.reportType = reportType;
	}
	
	/**
	 * Sets the configured moduleName.
	 * @param moduleName The moduleName value.
	 */
	public void setModuleName(@Nullable String moduleName) {
		this.moduleName = moduleName;
	}
	
	/**
	 * Sets the configured documentName.
	 * @param documentName The documentName value.
	 */
	public void setDocumentName(@Nullable String documentName) {
		this.documentName = documentName;
	}
   
	/**
     * Returns the configured queryName.
     * @return The configured value.
     */
    public @Nullable String getQueryName() {
        return queryName;
    }
    
    /**
     * Sets the configured queryName.
     * @param queryName The queryName value.
     */
    public void setQueryName(@Nullable String queryName) {
        this.queryName = queryName;
    }
    /**
     * Returns the configured uxui.
     * @return The configured value.
     */
    public @Nullable String getUxui() {
		return uxui;
	}
	
    /**
	 * Sets the configured uxui.
	 * @param uxui The uxui value.
	 */
	public void setUxui(@Nullable String uxui) {
		this.uxui = uxui;
	}
	
	/**
	 * Returns the configured repositoryPath.
	 * @return The configured value.
	 */
	public @Nullable String getRepositoryPath() {
		return repositoryPath;
	}
	
	/**
	 * Sets the configured repositoryPath.
	 * @param repositoryPath The repositoryPath value.
	 */
	public void setRepositoryPath(@Nullable String repositoryPath) {
		this.repositoryPath = repositoryPath;
	}
	
	/**
	 * Returns the configured saveToDocumentPackage.
	 * @return The configured value.
	 */
	public @Nullable Boolean getSaveToDocumentPackage() {
		return saveToDocumentPackage;
	}
	
	/**
	 * Sets the configured saveToDocumentPackage.
	 * @param saveToDocumentPackage The saveToDocumentPackage value.
	 */
	public void setSaveToDocumentPackage(@Nullable Boolean saveToDocumentPackage) {
		this.saveToDocumentPackage = saveToDocumentPackage;
	}
	
	/**
	 * Returns the configured orientation.
	 * @return The configured value.
	 */
	public @Nullable Orientation getOrientation() {
		return orientation;
	}
	
	/**
	 * Sets the configured orientation.
	 * @param orientation The orientation value.
	 */
	public void setOrientation(@Nullable Orientation orientation) {
		this.orientation = orientation;
	}
	
	/**
	 * Returns the configured width.
	 * @return The configured value.
	 */
	public @Nullable Integer getWidth() {
		return width;
	}
	
	/**
	 * Sets the configured width.
	 * @param width The width value.
	 */
	public void setWidth(@Nullable Integer width) {
		this.width = width;
	}
	
	/**
	 * Returns the configured height.
	 * @return The configured value.
	 */
	public @Nullable Integer getHeight() {
		return height;
	}
	
	/**
	 * Sets the configured height.
	 * @param height The height value.
	 */
	public void setHeight(@Nullable Integer height) {
		this.height = height;
	}
	
	/**
	 * Returns the configured leftMargin.
	 * @return The configured value.
	 */
	public @Nullable Integer getLeftMargin() {
		return leftMargin;
	}
	
	/**
	 * Sets the configured leftMargin.
	 * @param leftMargin The leftMargin value.
	 */
	public void setLeftMargin(@Nullable Integer leftMargin) {
		this.leftMargin = leftMargin;
	}
	
	/**
	 * Returns the configured rightMargin.
	 * @return The configured value.
	 */
	public @Nullable Integer getRightMargin() {
		return rightMargin;
	}
	
	/**
	 * Sets the configured rightMargin.
	 * @param rightMargin The rightMargin value.
	 */
	public void setRightMargin(@Nullable Integer rightMargin) {
		this.rightMargin = rightMargin;
	}
	
	/**
	 * Returns the configured topMargin.
	 * @return The configured value.
	 */
	public @Nullable Integer getTopMargin() {
		return topMargin;
	}
	
	/**
	 * Sets the configured topMargin.
	 * @param topMargin The topMargin value.
	 */
	public void setTopMargin(@Nullable Integer topMargin) {
		this.topMargin = topMargin;
	}
	
	/**
	 * Returns the configured bottomMargin.
	 * @return The configured value.
	 */
	public @Nullable Integer getBottomMargin() {
		return bottomMargin;
	}
	
	/**
	 * Sets the configured bottomMargin.
	 * @param bottomMargin The bottomMargin value.
	 */
	public void setBottomMargin(@Nullable Integer bottomMargin) {
		this.bottomMargin = bottomMargin;
	}
	
	/**
	 * Returns the configured columnWidth.
	 * @return The configured value.
	 */
	public @Nullable Integer getColumnWidth() {
		return columnWidth;
	}

	/**
	 * Sets the configured columnWidth.
	 * @param columnWidth The columnWidth value.
	 */
	public void setColumnWidth(@Nullable Integer columnWidth) {
		this.columnWidth = columnWidth;
	}
	
	/**
	 * Returns the configured defaultFontName.
	 * @return The configured value.
	 */
	public @Nullable String getDefaultFontName() {
		return defaultFontName;
	}
	
	/**
	 * Sets the configured defaultFontName.
	 * @param defaultFontName The defaultFontName value.
	 */
	public void setDefaultFontName(@Nullable String defaultFontName) {
		this.defaultFontName = defaultFontName;
	}
	
	/**
	 * Returns the configured titleFontSize.
	 * @return The configured value.
	 */
	public @Nullable Integer getTitleFontSize() {
		return titleFontSize;
	}
	
	/**
	 * Sets the configured titleFontSize.
	 * @param titleFontSize The titleFontSize value.
	 */
	public void setTitleFontSize(@Nullable Integer titleFontSize) {
		this.titleFontSize = titleFontSize;
	}
	
	/**
	 * Returns the configured defaultFontSize.
	 * @return The configured value.
	 */
	public @Nullable Integer getDefaultFontSize() {
		return defaultFontSize;
	}
	
	/**
	 * Sets the configured defaultFontSize.
	 * @param defaultFontSize The defaultFontSize value.
	 */
	public void setDefaultFontSize(@Nullable Integer defaultFontSize) {
		this.defaultFontSize = defaultFontSize;
	}
	
	/**
	 * Returns the configured renderLabelAsTextFields.
	 * @return The configured value.
	 */
	public @Nullable Boolean getRenderLabelAsTextFields() {
		return renderLabelAsTextFields;
	}
	
	/**
	 * Sets the configured renderLabelAsTextFields.
	 * @param renderLabelAsTextFields The renderLabelAsTextFields value.
	 */
	public void setRenderLabelAsTextFields(@Nullable Boolean renderLabelAsTextFields) {
		this.renderLabelAsTextFields = renderLabelAsTextFields;
	}
	
	/**
	 * Returns the configured defaultBorder.
	 * @return The configured value.
	 */
	public @Nullable Boolean getDefaultBorder() {
		return defaultBorder;
	}
	
	/**
	 * Sets the configured defaultBorder.
	 * @param defaultBorder The defaultBorder value.
	 */
	public void setDefaultBorder(@Nullable Boolean defaultBorder) {
		this.defaultBorder = defaultBorder;
	}
	
	/**
	 * Returns the configured defaultBorderTop.
	 * @return The configured value.
	 */
	public @Nullable Boolean getDefaultBorderTop() {
		return defaultBorderTop;
	}
	
	/**
	 * Sets the configured defaultBorderTop.
	 * @param defaultBorderTop The defaultBorderTop value.
	 */
	public void setDefaultBorderTop(@Nullable Boolean defaultBorderTop) {
		this.defaultBorderTop = defaultBorderTop;
	}
	
	/**
	 * Returns the configured defaultBorderLeft.
	 * @return The configured value.
	 */
	public @Nullable Boolean getDefaultBorderLeft() {
		return defaultBorderLeft;
	}
	
	/**
	 * Sets the configured defaultBorderLeft.
	 * @param defaultBorderLeft The defaultBorderLeft value.
	 */
	public void setDefaultBorderLeft(@Nullable Boolean defaultBorderLeft) {
		this.defaultBorderLeft = defaultBorderLeft;
	}
	
	/**
	 * Returns the configured defaultBorderBottom.
	 * @return The configured value.
	 */
	public @Nullable Boolean getDefaultBorderBottom() {
		return defaultBorderBottom;
	}
	
	/**
	 * Sets the configured defaultBorderBottom.
	 * @param defaultBorderBottom The defaultBorderBottom value.
	 */
	public void setDefaultBorderBottom(@Nullable Boolean defaultBorderBottom) {
		this.defaultBorderBottom = defaultBorderBottom;
	}
	
	/**
	 * Returns the configured defaultBorderRight.
	 * @return The configured value.
	 */
	public @Nullable Boolean getDefaultBorderRight() {
		return defaultBorderRight;
	}
	/**
	 * Sets the configured defaultBorderRight.
	 * @param defaultBorderRight The defaultBorderRight value.
	 */
	public void setDefaultBorderRight(@Nullable Boolean defaultBorderRight) {
		this.defaultBorderRight = defaultBorderRight;
	}
	
	/**
	 * Returns the configured defaultLineColour.
	 * @return The configured value.
	 */
	public @Nullable String getDefaultLineColour() {
		return defaultLineColour;
	}
	
	/**
	 * Sets the configured defaultLineColour.
	 * @param defaultLineColour The defaultLineColour value.
	 */
	public void setDefaultLineColour(@Nullable String defaultLineColour) {
		this.defaultLineColour = defaultLineColour;
	}
	
	/**
	 * Returns the configured defaultLineWidth.
	 * @return The configured value.
	 */
	public @Nullable Decimal2 getDefaultLineWidth() {
		return defaultLineWidth;
	}
	
	/**
	 * Sets the configured defaultLineWidth.
	 * @param defaultLineWidth The defaultLineWidth value.
	 */
	public void setDefaultLineWidth(@Nullable Decimal2 defaultLineWidth) {
		this.defaultLineWidth = defaultLineWidth;
	}
	
	/**
	 * Returns the configured defaultElementHeight.
	 * @return The configured value.
	 */
	public @Nullable Integer getDefaultElementHeight() {
		return defaultElementHeight;
	}
	
	/**
	 * Sets the configured defaultElementHeight.
	 * @param defaultElementHeight The defaultElementHeight value.
	 */
	public void setDefaultElementHeight(@Nullable Integer defaultElementHeight) {
		this.defaultElementHeight = defaultElementHeight;
	}
	
	/**
	 * Returns the configured includePageNumbers.
	 * @return The configured value.
	 */
	public @Nullable Boolean getIncludePageNumbers() {
		return includePageNumbers;
	}
	
	/**
	 * Sets the configured includePageNumbers.
	 * @param includePageNumbers The includePageNumbers value.
	 */
	public void setIncludePageNumbers(@Nullable Boolean includePageNumbers) {
		this.includePageNumbers = includePageNumbers;
	}
	
	/**
	 * Returns the configured defaultCellTopPadding.
	 * @return The configured value.
	 */
	public @Nullable Integer getDefaultCellTopPadding() {
		return defaultCellTopPadding;
	}
	
	/**
	 * Sets the configured defaultCellTopPadding.
	 * @param defaultCellTopPadding The defaultCellTopPadding value.
	 */
	public void setDefaultCellTopPadding(@Nullable Integer defaultCellTopPadding) {
		this.defaultCellTopPadding = defaultCellTopPadding;
	}
	
	/**
	 * Returns the configured defaultCellLeftPadding.
	 * @return The configured value.
	 */
	public @Nullable Integer getDefaultCellLeftPadding() {
		return defaultCellLeftPadding;
	}
	
	/**
	 * Sets the configured defaultCellLeftPadding.
	 * @param defaultCellLeftPadding The defaultCellLeftPadding value.
	 */
	public void setDefaultCellLeftPadding(@Nullable Integer defaultCellLeftPadding) {
		this.defaultCellLeftPadding = defaultCellLeftPadding;
	}
	
	/**
	 * Returns the configured defaultCellBottomPadding.
	 * @return The configured value.
	 */
	public @Nullable Integer getDefaultCellBottomPadding() {
		return defaultCellBottomPadding;
	}
	
	/**
	 * Sets the configured defaultCellBottomPadding.
	 * @param defaultCellBottomPadding The defaultCellBottomPadding value.
	 */
	public void setDefaultCellBottomPadding(@Nullable Integer defaultCellBottomPadding) {
		this.defaultCellBottomPadding = defaultCellBottomPadding;
	}
	
	/**
	 * Returns the configured defaultCellRightPadding.
	 * @return The configured value.
	 */
	public @Nullable Integer getDefaultCellRightPadding() {
		return defaultCellRightPadding;
	}
	
	/**
	 * Sets the configured defaultCellRightPadding.
	 * @param defaultCellRightPadding The defaultCellRightPadding value.
	 */
	public void setDefaultCellRightPadding(@Nullable Integer defaultCellRightPadding) {
		this.defaultCellRightPadding = defaultCellRightPadding;
	}
	
	/**
	 * Returns the configured dynamicFlow.
	 * @return The configured value.
	 */
	public @Nullable Boolean getDynamicFlow() {
		return dynamicFlow;
	}
	
	/**
	 * Sets the configured dynamicFlow.
	 * @param dynamicFlow The dynamicFlow value.
	 */
	public void setDynamicFlow(@Nullable Boolean dynamicFlow) {
		this.dynamicFlow = dynamicFlow;
	}
	
	/**
	 * Returns the configured bandSplitType.
	 * @return The configured value.
	 */
	public @Nullable ReportBand.SplitType getBandSplitType() {
		return bandSplitType;
	}
	
	/**
	 * Sets the configured bandSplitType.
	 * @param bandSplitType The bandSplitType value.
	 */
	public void setBandSplitType(@Nullable ReportBand.SplitType bandSplitType) {
		this.bandSplitType = bandSplitType;
	}
	
	/**
	 * Returns the configured boldLabels.
	 * @return The configured value.
	 */
	public @Nullable Boolean getBoldLabels() {
		return boldLabels;
	}
	
	/**
	 * Sets the configured boldLabels.
	 * @param boldLabels The boldLabels value.
	 */
	public void setBoldLabels(@Nullable Boolean boldLabels) {
		this.boldLabels = boldLabels;
	}
	
	/**
	 * Returns the configured checkBoxFontName.
	 * @return The configured value.
	 */
	public @Nullable String getCheckBoxFontName() {
		return checkBoxFontName;
	}
	
	/**
	 * Sets the configured checkBoxFontName.
	 * @param checkBoxFontName The checkBoxFontName value.
	 */
	public void setCheckBoxFontName(@Nullable String checkBoxFontName) {
		this.checkBoxFontName = checkBoxFontName;
	}
	
	/**
	 * Returns the configured checkBoxDisplayExpression.
	 * @return The configured value.
	 */
	public @Nullable String getCheckBoxDisplayExpression() {
		return checkBoxDisplayExpression;
	}
	
	/**
	 * Sets the configured checkBoxDisplayExpression.
	 * @param checkBoxDisplayExpression The checkBoxDisplayExpression value.
	 */
	public void setCheckBoxDisplayExpression(@Nullable String checkBoxDisplayExpression) {
		this.checkBoxDisplayExpression = checkBoxDisplayExpression;
	}
	
	/**
	 * Returns the configured pixelToTwip.
	 * @return The configured value.
	 */
	public @Nullable Decimal5 getPixelToTwip() {
		return pixelToTwip;
	}
	
	/**
	 * Sets the configured pixelToTwip.
	 * @param pixelToTwip The pixelToTwip value.
	 */
	public void setPixelToTwip(@Nullable Decimal5 pixelToTwip) {
		this.pixelToTwip = pixelToTwip;
	}
	
	/**
	 * Returns the configured sectionBorderTop.
	 * @return The configured value.
	 */
	public @Nullable Boolean getSectionBorderTop() {
		return sectionBorderTop;
	}
	
	/**
	 * Sets the configured sectionBorderTop.
	 * @param sectionBorderTop The sectionBorderTop value.
	 */
	public void setSectionBorderTop(@Nullable Boolean sectionBorderTop) {
		this.sectionBorderTop = sectionBorderTop;
	}
	
	/**
	 * Returns the configured sectionBorderLeft.
	 * @return The configured value.
	 */
	public @Nullable Boolean getSectionBorderLeft() {
		return sectionBorderLeft;
	}
	
	/**
	 * Sets the configured sectionBorderLeft.
	 * @param sectionBorderLeft The sectionBorderLeft value.
	 */
	public void setSectionBorderLeft(@Nullable Boolean sectionBorderLeft) {
		this.sectionBorderLeft = sectionBorderLeft;
	}
	
	/**
	 * Returns the configured sectionBorderRight.
	 * @return The configured value.
	 */
	public @Nullable Boolean getSectionBorderRight() {
		return sectionBorderRight;
	}
	
	/**
	 * Sets the configured sectionBorderRight.
	 * @param sectionBorderRight The sectionBorderRight value.
	 */
	public void setSectionBorderRight(@Nullable Boolean sectionBorderRight) {
		this.sectionBorderRight = sectionBorderRight;
	}
	
	/**
	 * Returns the configured sectionBorderBottom.
	 * @return The configured value.
	 */
	public @Nullable Boolean getSectionBorderBottom() {
		return sectionBorderBottom;
	}
	
	/**
	 * Sets the configured sectionBorderBottom.
	 * @param sectionBorderBottom The sectionBorderBottom value.
	 */
	public void setSectionBorderBottom(@Nullable Boolean sectionBorderBottom) {
		this.sectionBorderBottom = sectionBorderBottom;
	}
	
	/**
	 * Returns the configured sectionTitleBorderTop.
	 * @return The configured value.
	 */
	public @Nullable Boolean getSectionTitleBorderTop() {
		return sectionTitleBorderTop;
	}
	
	/**
	 * Sets the configured sectionTitleBorderTop.
	 * @param sectionTitleBorderTop The sectionTitleBorderTop value.
	 */
	public void setSectionTitleBorderTop(@Nullable Boolean sectionTitleBorderTop) {
		this.sectionTitleBorderTop = sectionTitleBorderTop;
	}
	
	/**
	 * Returns the configured sectionTitleBorderLeft.
	 * @return The configured value.
	 */
	public @Nullable Boolean getSectionTitleBorderLeft() {
		return sectionTitleBorderLeft;
	}
	
	/**
	 * Sets the configured sectionTitleBorderLeft.
	 * @param sectionTitleBorderLeft The sectionTitleBorderLeft value.
	 */
	public void setSectionTitleBorderLeft(@Nullable Boolean sectionTitleBorderLeft) {
		this.sectionTitleBorderLeft = sectionTitleBorderLeft;
	}
	
	/**
	 * Returns the configured sectionTitleBorderRight.
	 * @return The configured value.
	 */
	public @Nullable Boolean getSectionTitleBorderRight() {
		return sectionTitleBorderRight;
	}
	
	/**
	 * Sets the configured sectionTitleBorderRight.
	 * @param sectionTitleBorderRight The sectionTitleBorderRight value.
	 */
	public void setSectionTitleBorderRight(@Nullable Boolean sectionTitleBorderRight) {
		this.sectionTitleBorderRight = sectionTitleBorderRight;
	}
	
	/**
	 * Returns the configured sectionTitleBorderBottom.
	 * @return The configured value.
	 */
	public @Nullable Boolean getSectionTitleBorderBottom() {
		return sectionTitleBorderBottom;
	}
	
	/**
	 * Sets the configured sectionTitleBorderBottom.
	 * @param sectionTitleBorderBottom The sectionTitleBorderBottom value.
	 */
	public void setSectionTitleBorderBottom(@Nullable Boolean sectionTitleBorderBottom) {
		this.sectionTitleBorderBottom = sectionTitleBorderBottom;
	}
	
	/**
	 * Returns the configured sectionTitleForeground.
	 * @return The configured value.
	 */
	public @Nullable String getSectionTitleForeground() {
		return sectionTitleForeground;
	}
	
	/**
	 * Sets the configured sectionTitleForeground.
	 * @param sectionTitleForeground The sectionTitleForeground value.
	 */
	public void setSectionTitleForeground(@Nullable String sectionTitleForeground) {
		this.sectionTitleForeground = sectionTitleForeground;
	}
	
	/**
	 * Returns the configured sectionTitleBackground.
	 * @return The configured value.
	 */
	public @Nullable String getSectionTitleBackground() {
		return sectionTitleBackground;
	}
	
	/**
	 * Sets the configured sectionTitleBackground.
	 * @param sectionTitleBackground The sectionTitleBackground value.
	 */
	public void setSectionTitleBackground(@Nullable String sectionTitleBackground) {
		this.sectionTitleBackground = sectionTitleBackground;
	}
	
	/**
	 * Returns the configured parameters.
	 * @return The configured value.
	 */
	public @Nonnull List<ReportParameter> getParameters() {
		return parameters;
	}

	/**
	 * Sets the configured parameters.
	 * @param parameters The parameters value.
	 */
	public void setParameters(@Nonnull List<ReportParameter> parameters) {
		this.parameters = parameters;
	}

	/**
	 * Returns the configured fields.
	 * @return The configured value.
	 */
	public @Nonnull List<ReportField> getFields() {
		return fields;
	}

	/**
	 * Sets the configured fields.
	 * @param fields The fields value.
	 */
	public void setFields(@Nonnull List<ReportField> fields) {
		this.fields = fields;
	}

	/**
	 * Returns the configured variables.
	 * @return The configured value.
	 */
	public @Nonnull List<ReportVariable> getVariables() {
		return variables;
	}

	/**
	 * Sets the configured variables.
	 * @param variables The variables value.
	 */
	public void setVariables(@Nonnull List<ReportVariable> variables) {
		this.variables = variables;
	}

	/**
	 * Returns the configured bands.
	 * @return The configured value.
	 */
	public @Nonnull List<ReportBand> getBands() {
		return bands;
	}

	/**
	 * Sets the configured bands.
	 * @param bands The bands value.
	 */
	public void setBands(@Nonnull List<ReportBand> bands) {
		this.bands = bands;
	}

	/**
	 * Returns the configured subReports.
	 * @return The configured value.
	 */
	public @Nonnull List<DesignSpecification> getSubReports() {
		return subReports;
	}

	/**
	 * Sets the configured subReports.
	 * @param subReports The subReports value.
	 */
	public void setSubReports(@Nonnull List<DesignSpecification> subReports) {
		this.subReports = subReports;
	}

	/**
	 * Returns the configured field.
	 * @return The configured value.
	 */
	public @Nullable ReportField getField() {
		return field;
	}

	/**
	 * Sets the configured field.
	 * @param field The field value.
	 */
	public void setField(@Nullable ReportField field) {
		this.field = field;
	}

	/**
	 * Returns the configured collectionType.
	 * @return The configured value.
	 */
	public @Nullable CollectionType getCollectionType() {
		return collectionType;
	}

	/**
	 * Sets the configured collectionType.
	 * @param collectionType The collectionType value.
	 */
	public void setCollectionType(@Nullable CollectionType collectionType) {
		this.collectionType = collectionType;
	}

	/**
	 * Returns the configured parentReportPersistentName.
	 * @return The configured value.
	 */
	public @Nullable String getParentReportPersistentName() {
		return parentReportPersistentName;
	}

	/**
	 * Sets the configured parentReportPersistentName.
	 * @param parentReportPersistentName The parentReportPersistentName value.
	 */
	public void setParentReportPersistentName(@Nullable String parentReportPersistentName) {
		this.parentReportPersistentName = parentReportPersistentName;
	}

	/**
	 * Returns the configured verticalise.
	 * @return The configured value.
	 */
	public @Nullable Boolean getVerticalise() {
		return verticalise;
	}

	/**
	 * Sets the configured verticalise.
	 * @param verticalise The verticalise value.
	 */
	public void setVerticalise(@Nullable Boolean verticalise) {
		this.verticalise = verticalise;
	}

	/**
	 * Returns the configured labelAlignmentOverride.
	 * @return The configured value.
	 */
	public @Nullable ReportElement.ElementAlignment getLabelAlignmentOverride() {
		return labelAlignmentOverride;
	}

	/**
	 * Sets the configured labelAlignmentOverride.
	 * @param labelAlignmentOverride The labelAlignmentOverride value.
	 */
	public void setLabelAlignmentOverride(@Nullable ReportElement.ElementAlignment labelAlignmentOverride) {
		this.labelAlignmentOverride = labelAlignmentOverride;
	}

	/**
	 * Returns the configured moduleName.
	 * @return The configured value.
	 */
	public @Nullable String getModuleName() {
		return moduleName;
	}

	/**
	 * Returns the configured documentName.
	 * @return The configured value.
	 */
	public @Nullable String getDocumentName() {
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
	public @Nullable Map<String, String> getJoinAlias() {
		return joinAlias;
	}

	/**
	 * Sets the configured joinAlias.
	 * @param joinAlias The joinAlias value.
	 */
	public void setJoinAlias(@Nullable Map<String, String> joinAlias) {
		this.joinAlias = joinAlias;
	}

	/**
	 * Returns the configured joins.
	 * @return The configured value.
	 */
	public @Nullable Map<String, String> getJoins() {
		return joins;
	}

	/**
	 * Sets the configured joins.
	 * @param joins The joins value.
	 */
	public void setJoins(@Nullable Map<String, String> joins) {
		this.joins = joins;
	}
	
	/**
	 * Registers a generated SQL join and its corresponding table alias.
	 *
	 * @param documentName The logical join key (document or join path).
	 * @param alias The table alias assigned to the join.
	 * @param join The SQL join fragment.
	 */
	public void addJoin(@SuppressWarnings("hiding") @Nonnull String documentName, @SuppressWarnings("hiding") @Nonnull String alias, @Nonnull String join){
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
	public @Nonnull Module getModule() {
		final Customer customer = CORE.getCustomer();
		return customer.getModule(getModuleName());
	}

	/**
	 * Resolves the configured document from the current customer context.
	 *
	 * @return The resolved document metadata.
	 */
	public @Nonnull Document getDocument() {
		final Customer customer = CORE.getCustomer();
		return getModule().getDocument(customer, getDocumentName());
	}

	/**
	 * Returns the configured optional Jasper expression language override.
	 *
	 * @return The configured language, or {@code null} to use Jasper defaults.
	 */
	public @Nullable String getLanguage() {
		return language;
	}

	/**
	 * Sets the configured language.
	 * @param language The language value.
	 */
	public void setLanguage(@Nullable String language) {
		this.language = language;
	}
}
