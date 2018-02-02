package org.skyve.impl.generate.jasperreports;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.skyve.domain.types.Decimal2;
import org.skyve.domain.types.Decimal5;
import org.skyve.metadata.model.document.Collection.CollectionType;

public class DesignSpecification {
	private int alias = (int)('a');
	private Map<String, String> joinAlias = null;
	private Map<String, String> joins = null;
	
	public static enum Mode {
		sql,
		bean
	}

	public static enum DefinitionSource {
		document,
		view,
		query
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
	 * Label Alignment
	 **/
	private ReportElement.ElementAlignment labelAlignmentOverride;
	public String getName() {
		return name;
	}
	public void setName(String name) {
		this.name = name;
	}
	public Mode getMode() {
		return mode;
	}
	public void setMode(Mode mode) {
		this.mode = mode;
	}
	public DefinitionSource getDefinitionSource() {
		return definitionSource;
	}
	public void setDefinitionSource(DefinitionSource definitionSource) {
		this.definitionSource = definitionSource;
	}
	public ReportType getReportType() {
		return reportType;
	}
	public void setReportType(ReportType reportType) {
		this.reportType = reportType;
	}
	public void setModuleName(String moduleName) {
		this.moduleName = moduleName;
	}
	public void setDocumentName(String documentName) {
		this.documentName = documentName;
	}
	public String getUxui() {
		return uxui;
	}
	public void setUxui(String uxui) {
		this.uxui = uxui;
	}
	public String getRepositoryPath() {
		return repositoryPath;
	}
	public void setRepositoryPath(String repositoryPath) {
		this.repositoryPath = repositoryPath;
	}
	public Boolean getSaveToDocumentPackage() {
		return saveToDocumentPackage;
	}
	public void setSaveToDocumentPackage(Boolean saveToDocumentPackage) {
		this.saveToDocumentPackage = saveToDocumentPackage;
	}
	public Orientation getOrientation() {
		return orientation;
	}
	public void setOrientation(Orientation orientation) {
		this.orientation = orientation;
	}
	public Integer getWidth() {
		return width;
	}
	public void setWidth(Integer width) {
		this.width = width;
	}
	public Integer getHeight() {
		return height;
	}
	public void setHeight(Integer height) {
		this.height = height;
	}
	public Integer getLeftMargin() {
		return leftMargin;
	}
	public void setLeftMargin(Integer leftMargin) {
		this.leftMargin = leftMargin;
	}
	public Integer getRightMargin() {
		return rightMargin;
	}
	public void setRightMargin(Integer rightMargin) {
		this.rightMargin = rightMargin;
	}
	public Integer getTopMargin() {
		return topMargin;
	}
	public void setTopMargin(Integer topMargin) {
		this.topMargin = topMargin;
	}
	public Integer getBottomMargin() {
		return bottomMargin;
	}
	public void setBottomMargin(Integer bottomMargin) {
		this.bottomMargin = bottomMargin;
	}
	public Integer getColumnWidth() {
		return columnWidth;
	}
	public void setColumnWidth(Integer columnWidth) {
		this.columnWidth = columnWidth;
	}
	public String getDefaultFontName() {
		return defaultFontName;
	}
	public void setDefaultFontName(String defaultFontName) {
		this.defaultFontName = defaultFontName;
	}
	public Integer getTitleFontSize() {
		return titleFontSize;
	}
	public void setTitleFontSize(Integer titleFontSize) {
		this.titleFontSize = titleFontSize;
	}
	public Integer getDefaultFontSize() {
		return defaultFontSize;
	}
	public void setDefaultFontSize(Integer defaultFontSize) {
		this.defaultFontSize = defaultFontSize;
	}
	public Boolean getRenderLabelAsTextFields() {
		return renderLabelAsTextFields;
	}
	public void setRenderLabelAsTextFields(Boolean renderLabelAsTextFields) {
		this.renderLabelAsTextFields = renderLabelAsTextFields;
	}
	public Boolean getDefaultBorder() {
		return defaultBorder;
	}
	public void setDefaultBorder(Boolean defaultBorder) {
		this.defaultBorder = defaultBorder;
	}
	public Boolean getDefaultBorderTop() {
		return defaultBorderTop;
	}
	public void setDefaultBorderTop(Boolean defaultBorderTop) {
		this.defaultBorderTop = defaultBorderTop;
	}
	public Boolean getDefaultBorderLeft() {
		return defaultBorderLeft;
	}
	public void setDefaultBorderLeft(Boolean defaultBorderLeft) {
		this.defaultBorderLeft = defaultBorderLeft;
	}
	public Boolean getDefaultBorderBottom() {
		return defaultBorderBottom;
	}
	public void setDefaultBorderBottom(Boolean defaultBorderBottom) {
		this.defaultBorderBottom = defaultBorderBottom;
	}
	public Boolean getDefaultBorderRight() {
		return defaultBorderRight;
	}
	public void setDefaultBorderRight(Boolean defaultBorderRight) {
		this.defaultBorderRight = defaultBorderRight;
	}
	public String getDefaultLineColour() {
		return defaultLineColour;
	}
	public void setDefaultLineColour(String defaultLineColour) {
		this.defaultLineColour = defaultLineColour;
	}
	public Decimal2 getDefaultLineWidth() {
		return defaultLineWidth;
	}
	public void setDefaultLineWidth(Decimal2 defaultLineWidth) {
		this.defaultLineWidth = defaultLineWidth;
	}
	public Integer getDefaultElementHeight() {
		return defaultElementHeight;
	}
	public void setDefaultElementHeight(Integer defaultElementHeight) {
		this.defaultElementHeight = defaultElementHeight;
	}
	public Boolean getIncludePageNumbers() {
		return includePageNumbers;
	}
	public void setIncludePageNumbers(Boolean includePageNumbers) {
		this.includePageNumbers = includePageNumbers;
	}
	public Integer getDefaultCellTopPadding() {
		return defaultCellTopPadding;
	}
	public void setDefaultCellTopPadding(Integer defaultCellTopPadding) {
		this.defaultCellTopPadding = defaultCellTopPadding;
	}
	public Integer getDefaultCellLeftPadding() {
		return defaultCellLeftPadding;
	}
	public void setDefaultCellLeftPadding(Integer defaultCellLeftPadding) {
		this.defaultCellLeftPadding = defaultCellLeftPadding;
	}
	public Integer getDefaultCellBottomPadding() {
		return defaultCellBottomPadding;
	}
	public void setDefaultCellBottomPadding(Integer defaultCellBottomPadding) {
		this.defaultCellBottomPadding = defaultCellBottomPadding;
	}
	public Integer getDefaultCellRightPadding() {
		return defaultCellRightPadding;
	}
	public void setDefaultCellRightPadding(Integer defaultCellRightPadding) {
		this.defaultCellRightPadding = defaultCellRightPadding;
	}
	public Boolean getDynamicFlow() {
		return dynamicFlow;
	}
	public void setDynamicFlow(Boolean dynamicFlow) {
		this.dynamicFlow = dynamicFlow;
	}
	public ReportBand.SplitType getBandSplitType() {
		return bandSplitType;
	}
	public void setBandSplitType(ReportBand.SplitType bandSplitType) {
		this.bandSplitType = bandSplitType;
	}
	public Boolean getBoldLabels() {
		return boldLabels;
	}
	public void setBoldLabels(Boolean boldLabels) {
		this.boldLabels = boldLabels;
	}
	public String getCheckBoxFontName() {
		return checkBoxFontName;
	}
	public void setCheckBoxFontName(String checkBoxFontName) {
		this.checkBoxFontName = checkBoxFontName;
	}
	public String getCheckBoxDisplayExpression() {
		return checkBoxDisplayExpression;
	}
	public void setCheckBoxDisplayExpression(String checkBoxDisplayExpression) {
		this.checkBoxDisplayExpression = checkBoxDisplayExpression;
	}
	public Decimal5 getPixelToTwip() {
		return pixelToTwip;
	}
	public void setPixelToTwip(Decimal5 pixelToTwip) {
		this.pixelToTwip = pixelToTwip;
	}
	public Boolean getSectionBorderTop() {
		return sectionBorderTop;
	}
	public void setSectionBorderTop(Boolean sectionBorderTop) {
		this.sectionBorderTop = sectionBorderTop;
	}
	public Boolean getSectionBorderLeft() {
		return sectionBorderLeft;
	}
	public void setSectionBorderLeft(Boolean sectionBorderLeft) {
		this.sectionBorderLeft = sectionBorderLeft;
	}
	public Boolean getSectionBorderRight() {
		return sectionBorderRight;
	}
	public void setSectionBorderRight(Boolean sectionBorderRight) {
		this.sectionBorderRight = sectionBorderRight;
	}
	public Boolean getSectionBorderBottom() {
		return sectionBorderBottom;
	}
	public void setSectionBorderBottom(Boolean sectionBorderBottom) {
		this.sectionBorderBottom = sectionBorderBottom;
	}
	public Boolean getSectionTitleBorderTop() {
		return sectionTitleBorderTop;
	}
	public void setSectionTitleBorderTop(Boolean sectionTitleBorderTop) {
		this.sectionTitleBorderTop = sectionTitleBorderTop;
	}
	public Boolean getSectionTitleBorderLeft() {
		return sectionTitleBorderLeft;
	}
	public void setSectionTitleBorderLeft(Boolean sectionTitleBorderLeft) {
		this.sectionTitleBorderLeft = sectionTitleBorderLeft;
	}
	public Boolean getSectionTitleBorderRight() {
		return sectionTitleBorderRight;
	}
	public void setSectionTitleBorderRight(Boolean sectionTitleBorderRight) {
		this.sectionTitleBorderRight = sectionTitleBorderRight;
	}
	public Boolean getSectionTitleBorderBottom() {
		return sectionTitleBorderBottom;
	}
	public void setSectionTitleBorderBottom(Boolean sectionTitleBorderBottom) {
		this.sectionTitleBorderBottom = sectionTitleBorderBottom;
	}
	public String getSectionTitleForeground() {
		return sectionTitleForeground;
	}
	public void setSectionTitleForeground(String sectionTitleForeground) {
		this.sectionTitleForeground = sectionTitleForeground;
	}
	public String getSectionTitleBackground() {
		return sectionTitleBackground;
	}
	public void setSectionTitleBackground(String sectionTitleBackground) {
		this.sectionTitleBackground = sectionTitleBackground;
	}
	public List<ReportParameter> getParameters() {
		return parameters;
	}
	public void setParameters(List<ReportParameter> parameters) {
		this.parameters = parameters;
	}
	public List<ReportField> getFields() {
		return fields;
	}
	public void setFields(List<ReportField> fields) {
		this.fields = fields;
	}
	public List<ReportVariable> getVariables() {
		return variables;
	}
	public void setVariables(List<ReportVariable> variables) {
		this.variables = variables;
	}
	public List<ReportBand> getBands() {
		return bands;
	}
	public void setBands(List<ReportBand> bands) {
		this.bands = bands;
	}
	public List<DesignSpecification> getSubReports() {
		return subReports;
	}
	public void setSubReports(List<DesignSpecification> subReports) {
		this.subReports = subReports;
	}
	public ReportField getField() {
		return field;
	}
	public void setField(ReportField field) {
		this.field = field;
	}
	public CollectionType getCollectionType() {
		return collectionType;
	}
	public void setCollectionType(CollectionType collectionType) {
		this.collectionType = collectionType;
	}
	public String getParentReportPersistentName() {
		return parentReportPersistentName;
	}
	public void setParentReportPersistentName(String parentReportPersistentName) {
		this.parentReportPersistentName = parentReportPersistentName;
	}
	public Boolean getVerticalise() {
		return verticalise;
	}
	public void setVerticalise(Boolean verticalise) {
		this.verticalise = verticalise;
	}
	public ReportElement.ElementAlignment getLabelAlignmentOverride() {
		return labelAlignmentOverride;
	}
	public void setLabelAlignmentOverride(ReportElement.ElementAlignment labelAlignmentOverride) {
		this.labelAlignmentOverride = labelAlignmentOverride;
	}

	public String getModuleName() {
		return moduleName;
	}
	public String getDocumentName() {
		return documentName;
	}

	public String getJrxml() {
		try {
			return new InMemoryRenderer(this).renderDesign(this);
		} catch (Exception e) {
			e.printStackTrace();
		}

		return null;
	}

	public int getAlias() {
		return alias;
	}

	public void setAlias(int alias) {
		this.alias = alias;
	}

	public Map<String, String> getJoinAlias() {
		return joinAlias;
	}

	public void setJoinAlias(Map<String, String> joinAlias) {
		this.joinAlias = joinAlias;
	}

	public Map<String, String> getJoins() {
		return joins;
	}

	public void setJoins(Map<String, String> joins) {
		this.joins = joins;
	}
	
	public void addJoin(String documentName, String alias, String join){
		if(joins==null){
			joins = new HashMap<String, String>();
			joinAlias = new HashMap<String, String>();
		}
		joins.put(documentName, join);
		joinAlias.put(join, alias);
	}

	/**
	 * Default constructor with defaults for A4 page
	 * 
	 */
	public DesignSpecification(){
		
		fields = new ArrayList<>();
		bands = new ArrayList<>();
		
		setReportType(ReportType.report);
		
		setDefaultElementHeight(new Integer(20));
		setOrientation(Orientation.portrait);

		setLeftMargin(new Integer(30));
		setRightMargin(new Integer(30));
		setTopMargin(new Integer(42));
		setBottomMargin(new Integer(30));
		setMode(Mode.bean);
		setDefaultFontName("Arial");
		setDefaultFontSize(new Integer(12));
		setTitleFontSize(new Integer(16));
		setDefinitionSource(DefinitionSource.view);
		setDynamicFlow(Boolean.TRUE);
		setRenderLabelAsTextFields(Boolean.TRUE);
		setBandSplitType(ReportBand.SplitType.prevent);
		
		setDefaultCellTopPadding(new Integer(2));
		setDefaultCellLeftPadding(new Integer(2));
		setDefaultCellBottomPadding(new Integer(2));
		setDefaultCellRightPadding(new Integer(2));
		
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
	
	public void resetDesign() {
		if (Orientation.portrait.equals(getOrientation())) {
			setWidth(595);
			setColumnWidth(595 - getLeftMargin() - getRightMargin());
			setHeight(842);
		} else {
			setWidth(842);
			setColumnWidth(842 - getLeftMargin() - getRightMargin());
			setHeight(595);
		}
	}
}
