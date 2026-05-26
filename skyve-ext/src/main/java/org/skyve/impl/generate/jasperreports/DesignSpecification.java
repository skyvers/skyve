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
	
	public static enum Mode {
		sql,
		bean
	}

	public static enum DefinitionSource {
		document,
		view,
		query,
		list
	}

// TODO do I need to Capitalise the toString().
	public static enum ReportType {
		report,
		subreport;
	}

// TODO do I need to Capitalise the toString().
	public static enum Orientation {
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
	 * Returns the name.
	 */
	public String getName() {
		return name;
	}
	/**
	 * Sets the name.
	 */
	public void setName(String name) {
		this.name = name;
	}
	/**
	 * Returns the mode.
	 */
	public Mode getMode() {
		return mode;
	}
	/**
	 * Sets the mode.
	 */
	public void setMode(Mode mode) {
		this.mode = mode;
	}
	/**
	 * Returns the definitionSource.
	 */
	public DefinitionSource getDefinitionSource() {
		return definitionSource;
	}
	/**
	 * Sets the definitionSource.
	 */
	public void setDefinitionSource(DefinitionSource definitionSource) {
		this.definitionSource = definitionSource;
	}
	/**
	 * Returns the reportType.
	 */
	public ReportType getReportType() {
		return reportType;
	}
	/**
	 * Sets the reportType.
	 */
	public void setReportType(ReportType reportType) {
		this.reportType = reportType;
	}
	/**
	 * Sets the moduleName.
	 */
	public void setModuleName(String moduleName) {
		this.moduleName = moduleName;
	}
	/**
	 * Sets the documentName.
	 */
	public void setDocumentName(String documentName) {
		this.documentName = documentName;
	}
    /**
     * Returns the queryName.
     */
    public String getQueryName() {
        return queryName;
    }
    /**
     * Sets the queryName.
     */
    public void setQueryName(String queryName) {
        this.queryName = queryName;
    }
    /**
     * Returns the uxui.
     */
    public String getUxui() {
		return uxui;
	}
	/**
	 * Sets the uxui.
	 */
	public void setUxui(String uxui) {
		this.uxui = uxui;
	}
	/**
	 * Returns the repositoryPath.
	 */
	public String getRepositoryPath() {
		return repositoryPath;
	}
	/**
	 * Sets the repositoryPath.
	 */
	public void setRepositoryPath(String repositoryPath) {
		this.repositoryPath = repositoryPath;
	}
	/**
	 * Returns the saveToDocumentPackage.
	 */
	public Boolean getSaveToDocumentPackage() {
		return saveToDocumentPackage;
	}
	/**
	 * Sets the saveToDocumentPackage.
	 */
	public void setSaveToDocumentPackage(Boolean saveToDocumentPackage) {
		this.saveToDocumentPackage = saveToDocumentPackage;
	}
	/**
	 * Returns the orientation.
	 */
	public Orientation getOrientation() {
		return orientation;
	}
	/**
	 * Sets the orientation.
	 */
	public void setOrientation(Orientation orientation) {
		this.orientation = orientation;
	}
	/**
	 * Returns the width.
	 */
	public Integer getWidth() {
		return width;
	}
	/**
	 * Sets the width.
	 */
	public void setWidth(Integer width) {
		this.width = width;
	}
	/**
	 * Returns the height.
	 */
	public Integer getHeight() {
		return height;
	}
	/**
	 * Sets the height.
	 */
	public void setHeight(Integer height) {
		this.height = height;
	}
	/**
	 * Returns the leftMargin.
	 */
	public Integer getLeftMargin() {
		return leftMargin;
	}
	/**
	 * Sets the leftMargin.
	 */
	public void setLeftMargin(Integer leftMargin) {
		this.leftMargin = leftMargin;
	}
	/**
	 * Returns the rightMargin.
	 */
	public Integer getRightMargin() {
		return rightMargin;
	}
	/**
	 * Sets the rightMargin.
	 */
	public void setRightMargin(Integer rightMargin) {
		this.rightMargin = rightMargin;
	}
	/**
	 * Returns the topMargin.
	 */
	public Integer getTopMargin() {
		return topMargin;
	}
	/**
	 * Sets the topMargin.
	 */
	public void setTopMargin(Integer topMargin) {
		this.topMargin = topMargin;
	}
	/**
	 * Returns the bottomMargin.
	 */
	public Integer getBottomMargin() {
		return bottomMargin;
	}
	/**
	 * Sets the bottomMargin.
	 */
	public void setBottomMargin(Integer bottomMargin) {
		this.bottomMargin = bottomMargin;
	}
	/**
	 * Returns the columnWidth.
	 */
	public Integer getColumnWidth() {
		return columnWidth;
	}
	/**
	 * Sets the columnWidth.
	 */
	public void setColumnWidth(Integer columnWidth) {
		this.columnWidth = columnWidth;
	}
	/**
	 * Returns the defaultFontName.
	 */
	public String getDefaultFontName() {
		return defaultFontName;
	}
	/**
	 * Sets the defaultFontName.
	 */
	public void setDefaultFontName(String defaultFontName) {
		this.defaultFontName = defaultFontName;
	}
	/**
	 * Returns the titleFontSize.
	 */
	public Integer getTitleFontSize() {
		return titleFontSize;
	}
	/**
	 * Sets the titleFontSize.
	 */
	public void setTitleFontSize(Integer titleFontSize) {
		this.titleFontSize = titleFontSize;
	}
	/**
	 * Returns the defaultFontSize.
	 */
	public Integer getDefaultFontSize() {
		return defaultFontSize;
	}
	/**
	 * Sets the defaultFontSize.
	 */
	public void setDefaultFontSize(Integer defaultFontSize) {
		this.defaultFontSize = defaultFontSize;
	}
	/**
	 * Returns the renderLabelAsTextFields.
	 */
	public Boolean getRenderLabelAsTextFields() {
		return renderLabelAsTextFields;
	}
	/**
	 * Sets the renderLabelAsTextFields.
	 */
	public void setRenderLabelAsTextFields(Boolean renderLabelAsTextFields) {
		this.renderLabelAsTextFields = renderLabelAsTextFields;
	}
	/**
	 * Returns the defaultBorder.
	 */
	public Boolean getDefaultBorder() {
		return defaultBorder;
	}
	/**
	 * Sets the defaultBorder.
	 */
	public void setDefaultBorder(Boolean defaultBorder) {
		this.defaultBorder = defaultBorder;
	}
	/**
	 * Returns the defaultBorderTop.
	 */
	public Boolean getDefaultBorderTop() {
		return defaultBorderTop;
	}
	/**
	 * Sets the defaultBorderTop.
	 */
	public void setDefaultBorderTop(Boolean defaultBorderTop) {
		this.defaultBorderTop = defaultBorderTop;
	}
	/**
	 * Returns the defaultBorderLeft.
	 */
	public Boolean getDefaultBorderLeft() {
		return defaultBorderLeft;
	}
	/**
	 * Sets the defaultBorderLeft.
	 */
	public void setDefaultBorderLeft(Boolean defaultBorderLeft) {
		this.defaultBorderLeft = defaultBorderLeft;
	}
	/**
	 * Returns the defaultBorderBottom.
	 */
	public Boolean getDefaultBorderBottom() {
		return defaultBorderBottom;
	}
	/**
	 * Sets the defaultBorderBottom.
	 */
	public void setDefaultBorderBottom(Boolean defaultBorderBottom) {
		this.defaultBorderBottom = defaultBorderBottom;
	}
	/**
	 * Returns the defaultBorderRight.
	 */
	public Boolean getDefaultBorderRight() {
		return defaultBorderRight;
	}
	/**
	 * Sets the defaultBorderRight.
	 */
	public void setDefaultBorderRight(Boolean defaultBorderRight) {
		this.defaultBorderRight = defaultBorderRight;
	}
	/**
	 * Returns the defaultLineColour.
	 */
	public String getDefaultLineColour() {
		return defaultLineColour;
	}
	/**
	 * Sets the defaultLineColour.
	 */
	public void setDefaultLineColour(String defaultLineColour) {
		this.defaultLineColour = defaultLineColour;
	}
	/**
	 * Returns the defaultLineWidth.
	 */
	public Decimal2 getDefaultLineWidth() {
		return defaultLineWidth;
	}
	/**
	 * Sets the defaultLineWidth.
	 */
	public void setDefaultLineWidth(Decimal2 defaultLineWidth) {
		this.defaultLineWidth = defaultLineWidth;
	}
	/**
	 * Returns the defaultElementHeight.
	 */
	public Integer getDefaultElementHeight() {
		return defaultElementHeight;
	}
	/**
	 * Sets the defaultElementHeight.
	 */
	public void setDefaultElementHeight(Integer defaultElementHeight) {
		this.defaultElementHeight = defaultElementHeight;
	}
	/**
	 * Returns the includePageNumbers.
	 */
	public Boolean getIncludePageNumbers() {
		return includePageNumbers;
	}
	/**
	 * Sets the includePageNumbers.
	 */
	public void setIncludePageNumbers(Boolean includePageNumbers) {
		this.includePageNumbers = includePageNumbers;
	}
	/**
	 * Returns the defaultCellTopPadding.
	 */
	public Integer getDefaultCellTopPadding() {
		return defaultCellTopPadding;
	}
	/**
	 * Sets the defaultCellTopPadding.
	 */
	public void setDefaultCellTopPadding(Integer defaultCellTopPadding) {
		this.defaultCellTopPadding = defaultCellTopPadding;
	}
	/**
	 * Returns the defaultCellLeftPadding.
	 */
	public Integer getDefaultCellLeftPadding() {
		return defaultCellLeftPadding;
	}
	/**
	 * Sets the defaultCellLeftPadding.
	 */
	public void setDefaultCellLeftPadding(Integer defaultCellLeftPadding) {
		this.defaultCellLeftPadding = defaultCellLeftPadding;
	}
	/**
	 * Returns the defaultCellBottomPadding.
	 */
	public Integer getDefaultCellBottomPadding() {
		return defaultCellBottomPadding;
	}
	/**
	 * Sets the defaultCellBottomPadding.
	 */
	public void setDefaultCellBottomPadding(Integer defaultCellBottomPadding) {
		this.defaultCellBottomPadding = defaultCellBottomPadding;
	}
	/**
	 * Returns the defaultCellRightPadding.
	 */
	public Integer getDefaultCellRightPadding() {
		return defaultCellRightPadding;
	}
	/**
	 * Sets the defaultCellRightPadding.
	 */
	public void setDefaultCellRightPadding(Integer defaultCellRightPadding) {
		this.defaultCellRightPadding = defaultCellRightPadding;
	}
	/**
	 * Returns the dynamicFlow.
	 */
	public Boolean getDynamicFlow() {
		return dynamicFlow;
	}
	/**
	 * Sets the dynamicFlow.
	 */
	public void setDynamicFlow(Boolean dynamicFlow) {
		this.dynamicFlow = dynamicFlow;
	}
	/**
	 * Returns the bandSplitType.
	 */
	public ReportBand.SplitType getBandSplitType() {
		return bandSplitType;
	}
	/**
	 * Sets the bandSplitType.
	 */
	public void setBandSplitType(ReportBand.SplitType bandSplitType) {
		this.bandSplitType = bandSplitType;
	}
	/**
	 * Returns the boldLabels.
	 */
	public Boolean getBoldLabels() {
		return boldLabels;
	}
	/**
	 * Sets the boldLabels.
	 */
	public void setBoldLabels(Boolean boldLabels) {
		this.boldLabels = boldLabels;
	}
	/**
	 * Returns the checkBoxFontName.
	 */
	public String getCheckBoxFontName() {
		return checkBoxFontName;
	}
	/**
	 * Sets the checkBoxFontName.
	 */
	public void setCheckBoxFontName(String checkBoxFontName) {
		this.checkBoxFontName = checkBoxFontName;
	}
	/**
	 * Returns the checkBoxDisplayExpression.
	 */
	public String getCheckBoxDisplayExpression() {
		return checkBoxDisplayExpression;
	}
	/**
	 * Sets the checkBoxDisplayExpression.
	 */
	public void setCheckBoxDisplayExpression(String checkBoxDisplayExpression) {
		this.checkBoxDisplayExpression = checkBoxDisplayExpression;
	}
	/**
	 * Returns the pixelToTwip.
	 */
	public Decimal5 getPixelToTwip() {
		return pixelToTwip;
	}
	/**
	 * Sets the pixelToTwip.
	 */
	public void setPixelToTwip(Decimal5 pixelToTwip) {
		this.pixelToTwip = pixelToTwip;
	}
	/**
	 * Returns the sectionBorderTop.
	 */
	public Boolean getSectionBorderTop() {
		return sectionBorderTop;
	}
	/**
	 * Sets the sectionBorderTop.
	 */
	public void setSectionBorderTop(Boolean sectionBorderTop) {
		this.sectionBorderTop = sectionBorderTop;
	}
	/**
	 * Returns the sectionBorderLeft.
	 */
	public Boolean getSectionBorderLeft() {
		return sectionBorderLeft;
	}
	/**
	 * Sets the sectionBorderLeft.
	 */
	public void setSectionBorderLeft(Boolean sectionBorderLeft) {
		this.sectionBorderLeft = sectionBorderLeft;
	}
	/**
	 * Returns the sectionBorderRight.
	 */
	public Boolean getSectionBorderRight() {
		return sectionBorderRight;
	}
	/**
	 * Sets the sectionBorderRight.
	 */
	public void setSectionBorderRight(Boolean sectionBorderRight) {
		this.sectionBorderRight = sectionBorderRight;
	}
	/**
	 * Returns the sectionBorderBottom.
	 */
	public Boolean getSectionBorderBottom() {
		return sectionBorderBottom;
	}
	/**
	 * Sets the sectionBorderBottom.
	 */
	public void setSectionBorderBottom(Boolean sectionBorderBottom) {
		this.sectionBorderBottom = sectionBorderBottom;
	}
	/**
	 * Returns the sectionTitleBorderTop.
	 */
	public Boolean getSectionTitleBorderTop() {
		return sectionTitleBorderTop;
	}
	/**
	 * Sets the sectionTitleBorderTop.
	 */
	public void setSectionTitleBorderTop(Boolean sectionTitleBorderTop) {
		this.sectionTitleBorderTop = sectionTitleBorderTop;
	}
	/**
	 * Returns the sectionTitleBorderLeft.
	 */
	public Boolean getSectionTitleBorderLeft() {
		return sectionTitleBorderLeft;
	}
	/**
	 * Sets the sectionTitleBorderLeft.
	 */
	public void setSectionTitleBorderLeft(Boolean sectionTitleBorderLeft) {
		this.sectionTitleBorderLeft = sectionTitleBorderLeft;
	}
	/**
	 * Returns the sectionTitleBorderRight.
	 */
	public Boolean getSectionTitleBorderRight() {
		return sectionTitleBorderRight;
	}
	/**
	 * Sets the sectionTitleBorderRight.
	 */
	public void setSectionTitleBorderRight(Boolean sectionTitleBorderRight) {
		this.sectionTitleBorderRight = sectionTitleBorderRight;
	}
	/**
	 * Returns the sectionTitleBorderBottom.
	 */
	public Boolean getSectionTitleBorderBottom() {
		return sectionTitleBorderBottom;
	}
	/**
	 * Sets the sectionTitleBorderBottom.
	 */
	public void setSectionTitleBorderBottom(Boolean sectionTitleBorderBottom) {
		this.sectionTitleBorderBottom = sectionTitleBorderBottom;
	}
	/**
	 * Returns the sectionTitleForeground.
	 */
	public String getSectionTitleForeground() {
		return sectionTitleForeground;
	}
	/**
	 * Sets the sectionTitleForeground.
	 */
	public void setSectionTitleForeground(String sectionTitleForeground) {
		this.sectionTitleForeground = sectionTitleForeground;
	}
	/**
	 * Returns the sectionTitleBackground.
	 */
	public String getSectionTitleBackground() {
		return sectionTitleBackground;
	}
	/**
	 * Sets the sectionTitleBackground.
	 */
	public void setSectionTitleBackground(String sectionTitleBackground) {
		this.sectionTitleBackground = sectionTitleBackground;
	}
	/**
	 * Returns the parameters.
	 */
	public List<ReportParameter> getParameters() {
		return parameters;
	}
	/**
	 * Sets the parameters.
	 */
	public void setParameters(List<ReportParameter> parameters) {
		this.parameters = parameters;
	}
	/**
	 * Returns the fields.
	 */
	public List<ReportField> getFields() {
		return fields;
	}
	/**
	 * Sets the fields.
	 */
	public void setFields(List<ReportField> fields) {
		this.fields = fields;
	}
	/**
	 * Returns the variables.
	 */
	public List<ReportVariable> getVariables() {
		return variables;
	}
	/**
	 * Sets the variables.
	 */
	public void setVariables(List<ReportVariable> variables) {
		this.variables = variables;
	}
	/**
	 * Returns the bands.
	 */
	public List<ReportBand> getBands() {
		return bands;
	}
	/**
	 * Sets the bands.
	 */
	public void setBands(List<ReportBand> bands) {
		this.bands = bands;
	}
	/**
	 * Returns the subReports.
	 */
	public List<DesignSpecification> getSubReports() {
		return subReports;
	}
	/**
	 * Sets the subReports.
	 */
	public void setSubReports(List<DesignSpecification> subReports) {
		this.subReports = subReports;
	}
	/**
	 * Returns the field.
	 */
	public ReportField getField() {
		return field;
	}
	/**
	 * Sets the field.
	 */
	public void setField(ReportField field) {
		this.field = field;
	}
	/**
	 * Returns the collectionType.
	 */
	public CollectionType getCollectionType() {
		return collectionType;
	}
	/**
	 * Sets the collectionType.
	 */
	public void setCollectionType(CollectionType collectionType) {
		this.collectionType = collectionType;
	}
	/**
	 * Returns the parentReportPersistentName.
	 */
	public String getParentReportPersistentName() {
		return parentReportPersistentName;
	}
	/**
	 * Sets the parentReportPersistentName.
	 */
	public void setParentReportPersistentName(String parentReportPersistentName) {
		this.parentReportPersistentName = parentReportPersistentName;
	}
	/**
	 * Returns the verticalise.
	 */
	public Boolean getVerticalise() {
		return verticalise;
	}
	/**
	 * Sets the verticalise.
	 */
	public void setVerticalise(Boolean verticalise) {
		this.verticalise = verticalise;
	}
	/**
	 * Returns the labelAlignmentOverride.
	 */
	public ReportElement.ElementAlignment getLabelAlignmentOverride() {
		return labelAlignmentOverride;
	}
	/**
	 * Sets the labelAlignmentOverride.
	 */
	public void setLabelAlignmentOverride(ReportElement.ElementAlignment labelAlignmentOverride) {
		this.labelAlignmentOverride = labelAlignmentOverride;
	}

	/**
	 * Returns the moduleName.
	 */
	public String getModuleName() {
		return moduleName;
	}
	/**
	 * Returns the documentName.
	 */
	public String getDocumentName() {
		return documentName;
	}

	/**
	 * Returns the alias.
	 */
	public int getAlias() {
		return alias;
	}

	/**
	 * Sets the alias.
	 */
	public void setAlias(int alias) {
		this.alias = alias;
	}

	/**
	 * Returns the joinAlias.
	 */
	public Map<String, String> getJoinAlias() {
		return joinAlias;
	}

	/**
	 * Sets the joinAlias.
	 */
	public void setJoinAlias(Map<String, String> joinAlias) {
		this.joinAlias = joinAlias;
	}

	/**
	 * Returns the joins.
	 */
	public Map<String, String> getJoins() {
		return joins;
	}

	/**
	 * Sets the joins.
	 */
	public void setJoins(Map<String, String> joins) {
		this.joins = joins;
	}
	
	/**
	 * Adds a join.
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
	 * Indicates whether isIncludeCustomerLogo is satisfied.
	 */
	public boolean isIncludeCustomerLogo() {
		return includeCustomerLogo;
	}

	/**
	 * Sets the includeCustomerLogo.
	 */
	public void setIncludeCustomerLogo(boolean includeCustomerLogo) {
		this.includeCustomerLogo = includeCustomerLogo;
	}

	/**
	 * Default constructor with defaults for A4 page
	 * 
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
	 * Performs resetDesign.
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
	 * Returns the module.
	 */
	public Module getModule() {
		final Customer customer = CORE.getCustomer();
		return customer.getModule(getModuleName());
	}

	/**
	 * Returns the document.
	 */
	public Document getDocument() {
		final Customer customer = CORE.getCustomer();
		return getModule().getDocument(customer, getDocumentName());
	}

	/**
	 * Returns the language.
	 */
	public String getLanguage() {
		return language;
	}

	/**
	 * Sets the language.
	 */
	public void setLanguage(String language) {
		this.language = language;
	}
}
