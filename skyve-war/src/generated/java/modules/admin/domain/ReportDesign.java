package modules.admin.domain;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlEnum;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.types.Decimal2;
import org.skyve.domain.types.Decimal5;
import org.skyve.domain.types.Enumeration;
import org.skyve.impl.domain.AbstractTransientBean;
import org.skyve.impl.domain.types.jaxb.Decimal2Mapper;
import org.skyve.impl.domain.types.jaxb.Decimal5Mapper;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.util.Util;

/**
 * Report Design
 * 
 * @depend - - - Mode
 * @depend - - - DefinitionSource
 * @depend - - - ReportType
 * @depend - - - Orientation
 * @depend - - - CollectionType
 * @stereotype "transient"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public class ReportDesign extends AbstractTransientBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "admin";

	/** @hidden */
	public static final String DOCUMENT_NAME = "ReportDesign";

	/** @hidden */
	public static final String namePropertyName = "name";

	/** @hidden */
	public static final String modePropertyName = "mode";

	/** @hidden */
	public static final String definitionSourcePropertyName = "definitionSource";

	/** @hidden */
	public static final String reportTypePropertyName = "reportType";

	/** @hidden */
	public static final String moduleNamePropertyName = "moduleName";

	/** @hidden */
	public static final String documentNamePropertyName = "documentName";

	/** @hidden */
	public static final String queryNamePropertyName = "queryName";

	/** @hidden */
	public static final String menuItemPropertyName = "menuItem";

	/** @hidden */
	public static final String repositoryPathPropertyName = "repositoryPath";

	/** @hidden */
	public static final String saveToDocumentPackagePropertyName = "saveToDocumentPackage";

	/** @hidden */
	public static final String orientationPropertyName = "orientation";

	/** @hidden */
	public static final String widthPropertyName = "width";

	/** @hidden */
	public static final String heightPropertyName = "height";

	/** @hidden */
	public static final String leftMarginPropertyName = "leftMargin";

	/** @hidden */
	public static final String rightMarginPropertyName = "rightMargin";

	/** @hidden */
	public static final String topMarginPropertyName = "topMargin";

	/** @hidden */
	public static final String bottomMarginPropertyName = "bottomMargin";

	/** @hidden */
	public static final String columnWidthPropertyName = "columnWidth";

	/** @hidden */
	public static final String defaultFontNamePropertyName = "defaultFontName";

	/** @hidden */
	public static final String titleFontSizePropertyName = "titleFontSize";

	/** @hidden */
	public static final String defaultFontSizePropertyName = "defaultFontSize";

	/** @hidden */
	public static final String defaultLineColourPropertyName = "defaultLineColour";

	/** @hidden */
	public static final String defaultLineWidthPropertyName = "defaultLineWidth";

	/** @hidden */
	public static final String renderLabelAsTextFieldsPropertyName = "renderLabelAsTextFields";

	/** @hidden */
	public static final String defaultBorderPropertyName = "defaultBorder";

	/** @hidden */
	public static final String defaultBorderTopPropertyName = "defaultBorderTop";

	/** @hidden */
	public static final String defaultBorderLeftPropertyName = "defaultBorderLeft";

	/** @hidden */
	public static final String defaultBorderBottomPropertyName = "defaultBorderBottom";

	/** @hidden */
	public static final String defaultBorderRightPropertyName = "defaultBorderRight";

	/** @hidden */
	public static final String defaultElementHeightPropertyName = "defaultElementHeight";

	/** @hidden */
	public static final String includePageNumbersPropertyName = "includePageNumbers";

	/** @hidden */
	public static final String defaultCellTopPaddingPropertyName = "defaultCellTopPadding";

	/** @hidden */
	public static final String defaultCellLeftPaddingPropertyName = "defaultCellLeftPadding";

	/** @hidden */
	public static final String defaultCellBottomPaddingPropertyName = "defaultCellBottomPadding";

	/** @hidden */
	public static final String defaultCellRightPaddingPropertyName = "defaultCellRightPadding";

	/** @hidden */
	public static final String dynamicFlowPropertyName = "dynamicFlow";

	/** @hidden */
	public static final String bandSplitTypePropertyName = "bandSplitType";

	/** @hidden */
	public static final String boldLabelsPropertyName = "boldLabels";

	/** @hidden */
	public static final String checkBoxFontNamePropertyName = "checkBoxFontName";

	/** @hidden */
	public static final String checkBoxDisplayExpressionPropertyName = "checkBoxDisplayExpression";

	/** @hidden */
	public static final String pixelToTwipPropertyName = "pixelToTwip";

	/** @hidden */
	public static final String sectionBorderTopPropertyName = "sectionBorderTop";

	/** @hidden */
	public static final String sectionBorderLeftPropertyName = "sectionBorderLeft";

	/** @hidden */
	public static final String sectionBorderRightPropertyName = "sectionBorderRight";

	/** @hidden */
	public static final String sectionBorderBottomPropertyName = "sectionBorderBottom";

	/** @hidden */
	public static final String sectionTitleBorderTopPropertyName = "sectionTitleBorderTop";

	/** @hidden */
	public static final String sectionTitleBorderLeftPropertyName = "sectionTitleBorderLeft";

	/** @hidden */
	public static final String sectionTitleBorderRightPropertyName = "sectionTitleBorderRight";

	/** @hidden */
	public static final String sectionTitleBorderBottomPropertyName = "sectionTitleBorderBottom";

	/** @hidden */
	public static final String sectionTitleForegroundPropertyName = "sectionTitleForeground";

	/** @hidden */
	public static final String sectionTitleBackgroundPropertyName = "sectionTitleBackground";

	/** @hidden */
	public static final String jrxmlPropertyName = "jrxml";

	/** @hidden */
	public static final String fieldPropertyName = "field";

	/** @hidden */
	public static final String collectionTypePropertyName = "collectionType";

	/** @hidden */
	public static final String parentReportPersistentNamePropertyName = "parentReportPersistentName";

	/** @hidden */
	public static final String verticalisePropertyName = "verticalise";

	/** @hidden */
	public static final String labelAlignmentOverridePropertyName = "labelAlignmentOverride";

	/**
	 * Mode
	 **/
	@XmlEnum
	@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
	public static enum Mode implements Enumeration {
		sql("sql", "sql"),
		bean("bean", "bean");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues = Stream.of(values()).map(Mode::toDomainValue).collect(Collectors.toUnmodifiableList());

		private Mode(String code, String description) {
			this.code = code;
			this.description = description;
			this.domainValue = new DomainValue(code, description);
		}

		@Override
		public String toCode() {
			return code;
		}

		@Override
		public String toLocalisedDescription() {
			return Util.i18n(description);
		}

		@Override
		public DomainValue toDomainValue() {
			return domainValue;
		}

		public static Mode fromCode(String code) {
			Mode result = null;

			for (Mode value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static Mode fromLocalisedDescription(String description) {
			Mode result = null;

			for (Mode value : values()) {
				if (value.toLocalisedDescription().equals(description)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static List<DomainValue> toDomainValues() {
			return domainValues;
		}
	}

	/**
	 * Definition Source
	 **/
	@XmlEnum
	@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
	public static enum DefinitionSource implements Enumeration {
		document("document", "document"),
		view("view", "view"),
		query("query", "query"),
		list("list", "list");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues = Stream.of(values()).map(DefinitionSource::toDomainValue).collect(Collectors.toUnmodifiableList());

		private DefinitionSource(String code, String description) {
			this.code = code;
			this.description = description;
			this.domainValue = new DomainValue(code, description);
		}

		@Override
		public String toCode() {
			return code;
		}

		@Override
		public String toLocalisedDescription() {
			return Util.i18n(description);
		}

		@Override
		public DomainValue toDomainValue() {
			return domainValue;
		}

		public static DefinitionSource fromCode(String code) {
			DefinitionSource result = null;

			for (DefinitionSource value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static DefinitionSource fromLocalisedDescription(String description) {
			DefinitionSource result = null;

			for (DefinitionSource value : values()) {
				if (value.toLocalisedDescription().equals(description)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static List<DomainValue> toDomainValues() {
			return domainValues;
		}
	}

	/**
	 * Type
	 **/
	@XmlEnum
	@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
	public static enum ReportType implements Enumeration {
		report("Report", "Report"),
		subreport("Subreport", "Subreport");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues = Stream.of(values()).map(ReportType::toDomainValue).collect(Collectors.toUnmodifiableList());

		private ReportType(String code, String description) {
			this.code = code;
			this.description = description;
			this.domainValue = new DomainValue(code, description);
		}

		@Override
		public String toCode() {
			return code;
		}

		@Override
		public String toLocalisedDescription() {
			return Util.i18n(description);
		}

		@Override
		public DomainValue toDomainValue() {
			return domainValue;
		}

		public static ReportType fromCode(String code) {
			ReportType result = null;

			for (ReportType value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static ReportType fromLocalisedDescription(String description) {
			ReportType result = null;

			for (ReportType value : values()) {
				if (value.toLocalisedDescription().equals(description)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static List<DomainValue> toDomainValues() {
			return domainValues;
		}
	}

	/**
	 * Orientation
	 **/
	@XmlEnum
	@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
	public static enum Orientation implements Enumeration {
		portrait("Portrait", "Portrait"),
		landscape("Landscape", "Landscape");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues = Stream.of(values()).map(Orientation::toDomainValue).collect(Collectors.toUnmodifiableList());

		private Orientation(String code, String description) {
			this.code = code;
			this.description = description;
			this.domainValue = new DomainValue(code, description);
		}

		@Override
		public String toCode() {
			return code;
		}

		@Override
		public String toLocalisedDescription() {
			return Util.i18n(description);
		}

		@Override
		public DomainValue toDomainValue() {
			return domainValue;
		}

		public static Orientation fromCode(String code) {
			Orientation result = null;

			for (Orientation value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static Orientation fromLocalisedDescription(String description) {
			Orientation result = null;

			for (Orientation value : values()) {
				if (value.toLocalisedDescription().equals(description)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static List<DomainValue> toDomainValues() {
			return domainValues;
		}
	}

	/**
	 * Collection Type
	 **/
	@XmlEnum
	@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
	public static enum CollectionType implements Enumeration {
		child("child", "child"),
		aggregation("aggregation", "aggregation"),
		composition("composition", "composition");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues = Stream.of(values()).map(CollectionType::toDomainValue).collect(Collectors.toUnmodifiableList());

		private CollectionType(String code, String description) {
			this.code = code;
			this.description = description;
			this.domainValue = new DomainValue(code, description);
		}

		@Override
		public String toCode() {
			return code;
		}

		@Override
		public String toLocalisedDescription() {
			return Util.i18n(description);
		}

		@Override
		public DomainValue toDomainValue() {
			return domainValue;
		}

		public static CollectionType fromCode(String code) {
			CollectionType result = null;

			for (CollectionType value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static CollectionType fromLocalisedDescription(String description) {
			CollectionType result = null;

			for (CollectionType value : values()) {
				if (value.toLocalisedDescription().equals(description)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static List<DomainValue> toDomainValues() {
			return domainValues;
		}
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
	 * Query
	 * <br/>
	 * Report Query.
	 **/
	private String queryName;

	/**
	 * Menu Item
	 * <br/>
	 * Menu Item
	 **/
	private String menuItem;

	/**
	 * Output Path
	 * <br/>
	 * <p>The path to the place where you want jrxml files created.</p>
<p>Normally this will be the path to the folder where you modules are defined - e.g. C:\workspace\projectName\src\skyve\</p>
	 **/
	private String repositoryPath;

	/**
	 * Save to Document Package
	 * <br/>
	 * <p>Use this if you want report files to be created in a directory structure matching the metadata repository structure.</p>
<p>This is useful if you want to put the reports into place ready to run.</p>
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
	 * Default Line Colour
	 **/
	private String defaultLineColour;

	/**
	 * Default Line Pen Stroke Thickness
	 **/
	private Decimal2 defaultLineWidth;

	/**
	 * Render Labels as TextFields
	 * <br/>
	 * This option will allow labels to flow and stretch like text fields
	 **/
	private Boolean renderLabelAsTextFields;

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
<p>Allow (as far as possible) page sections, bands and fields to flow as much as required to show all data.</p>
	 **/
	private Boolean dynamicFlow;

	/**
	 * Split Type
	 **/
	private String bandSplitType;

	/**
	 * Bold Labels
	 * <br/>
	 * Bold not working? Check that you've included a font extension jar for your font in the classpath
	 **/
	private Boolean boldLabels;

	/**
	 * Checkbox Font Name
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
	 * TWIP conversion ratio for pixel-specified item sizing in views.
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
	 * Jrxml File Destination
	 **/
	private String jrxml;

	/**
	 * Field
	 * <br/>
	 * The field on which this subreport is based (if this is a subreport).
	 **/
	private String field;

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
	private String labelAlignmentOverride;

	@Override
	@XmlTransient
	public String getBizModule() {
		return ReportDesign.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return ReportDesign.DOCUMENT_NAME;
	}

	public static ReportDesign newInstance() {
		try {
			return CORE.getUser().getCustomer().getModule(MODULE_NAME).getDocument(CORE.getUser().getCustomer(), DOCUMENT_NAME).newInstance(CORE.getUser());
		}
		catch (RuntimeException e) {
			throw e;
		}
		catch (Exception e) {
			throw new DomainException(e);
		}
	}

	@Override
	@XmlTransient
	public String getBizKey() {
		try {
			return org.skyve.util.Binder.formatMessage("{name}", this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return "Unknown";
		}
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof ReportDesign) && 
					this.getBizId().equals(((ReportDesign) o).getBizId()));
	}

	/**
	 * {@link #name} accessor.
	 * @return	The value.
	 **/
	public String getName() {
		return name;
	}

	/**
	 * {@link #name} mutator.
	 * @param name	The new value.
	 **/
	@XmlElement
	public void setName(String name) {
		preset(namePropertyName, name);
		this.name = name;
	}

	/**
	 * {@link #mode} accessor.
	 * @return	The value.
	 **/
	public Mode getMode() {
		return mode;
	}

	/**
	 * {@link #mode} mutator.
	 * @param mode	The new value.
	 **/
	@XmlElement
	public void setMode(Mode mode) {
		preset(modePropertyName, mode);
		this.mode = mode;
	}

	/**
	 * {@link #definitionSource} accessor.
	 * @return	The value.
	 **/
	public DefinitionSource getDefinitionSource() {
		return definitionSource;
	}

	/**
	 * {@link #definitionSource} mutator.
	 * @param definitionSource	The new value.
	 **/
	@XmlElement
	public void setDefinitionSource(DefinitionSource definitionSource) {
		preset(definitionSourcePropertyName, definitionSource);
		this.definitionSource = definitionSource;
	}

	/**
	 * {@link #reportType} accessor.
	 * @return	The value.
	 **/
	public ReportType getReportType() {
		return reportType;
	}

	/**
	 * {@link #reportType} mutator.
	 * @param reportType	The new value.
	 **/
	@XmlElement
	public void setReportType(ReportType reportType) {
		preset(reportTypePropertyName, reportType);
		this.reportType = reportType;
	}

	/**
	 * {@link #moduleName} accessor.
	 * @return	The value.
	 **/
	public String getModuleName() {
		return moduleName;
	}

	/**
	 * {@link #moduleName} mutator.
	 * @param moduleName	The new value.
	 **/
	@XmlElement
	public void setModuleName(String moduleName) {
		preset(moduleNamePropertyName, moduleName);
		this.moduleName = moduleName;
	}

	/**
	 * {@link #documentName} accessor.
	 * @return	The value.
	 **/
	public String getDocumentName() {
		return documentName;
	}

	/**
	 * {@link #documentName} mutator.
	 * @param documentName	The new value.
	 **/
	@XmlElement
	public void setDocumentName(String documentName) {
		preset(documentNamePropertyName, documentName);
		this.documentName = documentName;
	}

	/**
	 * {@link #queryName} accessor.
	 * @return	The value.
	 **/
	public String getQueryName() {
		return queryName;
	}

	/**
	 * {@link #queryName} mutator.
	 * @param queryName	The new value.
	 **/
	@XmlElement
	public void setQueryName(String queryName) {
		preset(queryNamePropertyName, queryName);
		this.queryName = queryName;
	}

	/**
	 * {@link #menuItem} accessor.
	 * @return	The value.
	 **/
	public String getMenuItem() {
		return menuItem;
	}

	/**
	 * {@link #menuItem} mutator.
	 * @param menuItem	The new value.
	 **/
	@XmlElement
	public void setMenuItem(String menuItem) {
		preset(menuItemPropertyName, menuItem);
		this.menuItem = menuItem;
	}

	/**
	 * {@link #repositoryPath} accessor.
	 * @return	The value.
	 **/
	public String getRepositoryPath() {
		return repositoryPath;
	}

	/**
	 * {@link #repositoryPath} mutator.
	 * @param repositoryPath	The new value.
	 **/
	@XmlElement
	public void setRepositoryPath(String repositoryPath) {
		preset(repositoryPathPropertyName, repositoryPath);
		this.repositoryPath = repositoryPath;
	}

	/**
	 * {@link #saveToDocumentPackage} accessor.
	 * @return	The value.
	 **/
	public Boolean getSaveToDocumentPackage() {
		return saveToDocumentPackage;
	}

	/**
	 * {@link #saveToDocumentPackage} mutator.
	 * @param saveToDocumentPackage	The new value.
	 **/
	@XmlElement
	public void setSaveToDocumentPackage(Boolean saveToDocumentPackage) {
		preset(saveToDocumentPackagePropertyName, saveToDocumentPackage);
		this.saveToDocumentPackage = saveToDocumentPackage;
	}

	/**
	 * {@link #orientation} accessor.
	 * @return	The value.
	 **/
	public Orientation getOrientation() {
		return orientation;
	}

	/**
	 * {@link #orientation} mutator.
	 * @param orientation	The new value.
	 **/
	@XmlElement
	public void setOrientation(Orientation orientation) {
		preset(orientationPropertyName, orientation);
		this.orientation = orientation;
	}

	/**
	 * {@link #width} accessor.
	 * @return	The value.
	 **/
	public Integer getWidth() {
		return width;
	}

	/**
	 * {@link #width} mutator.
	 * @param width	The new value.
	 **/
	@XmlElement
	public void setWidth(Integer width) {
		preset(widthPropertyName, width);
		this.width = width;
	}

	/**
	 * {@link #height} accessor.
	 * @return	The value.
	 **/
	public Integer getHeight() {
		return height;
	}

	/**
	 * {@link #height} mutator.
	 * @param height	The new value.
	 **/
	@XmlElement
	public void setHeight(Integer height) {
		preset(heightPropertyName, height);
		this.height = height;
	}

	/**
	 * {@link #leftMargin} accessor.
	 * @return	The value.
	 **/
	public Integer getLeftMargin() {
		return leftMargin;
	}

	/**
	 * {@link #leftMargin} mutator.
	 * @param leftMargin	The new value.
	 **/
	@XmlElement
	public void setLeftMargin(Integer leftMargin) {
		preset(leftMarginPropertyName, leftMargin);
		this.leftMargin = leftMargin;
	}

	/**
	 * {@link #rightMargin} accessor.
	 * @return	The value.
	 **/
	public Integer getRightMargin() {
		return rightMargin;
	}

	/**
	 * {@link #rightMargin} mutator.
	 * @param rightMargin	The new value.
	 **/
	@XmlElement
	public void setRightMargin(Integer rightMargin) {
		preset(rightMarginPropertyName, rightMargin);
		this.rightMargin = rightMargin;
	}

	/**
	 * {@link #topMargin} accessor.
	 * @return	The value.
	 **/
	public Integer getTopMargin() {
		return topMargin;
	}

	/**
	 * {@link #topMargin} mutator.
	 * @param topMargin	The new value.
	 **/
	@XmlElement
	public void setTopMargin(Integer topMargin) {
		preset(topMarginPropertyName, topMargin);
		this.topMargin = topMargin;
	}

	/**
	 * {@link #bottomMargin} accessor.
	 * @return	The value.
	 **/
	public Integer getBottomMargin() {
		return bottomMargin;
	}

	/**
	 * {@link #bottomMargin} mutator.
	 * @param bottomMargin	The new value.
	 **/
	@XmlElement
	public void setBottomMargin(Integer bottomMargin) {
		preset(bottomMarginPropertyName, bottomMargin);
		this.bottomMargin = bottomMargin;
	}

	/**
	 * {@link #columnWidth} accessor.
	 * @return	The value.
	 **/
	public Integer getColumnWidth() {
		return columnWidth;
	}

	/**
	 * {@link #columnWidth} mutator.
	 * @param columnWidth	The new value.
	 **/
	@XmlElement
	public void setColumnWidth(Integer columnWidth) {
		preset(columnWidthPropertyName, columnWidth);
		this.columnWidth = columnWidth;
	}

	/**
	 * {@link #defaultFontName} accessor.
	 * @return	The value.
	 **/
	public String getDefaultFontName() {
		return defaultFontName;
	}

	/**
	 * {@link #defaultFontName} mutator.
	 * @param defaultFontName	The new value.
	 **/
	@XmlElement
	public void setDefaultFontName(String defaultFontName) {
		preset(defaultFontNamePropertyName, defaultFontName);
		this.defaultFontName = defaultFontName;
	}

	/**
	 * {@link #titleFontSize} accessor.
	 * @return	The value.
	 **/
	public Integer getTitleFontSize() {
		return titleFontSize;
	}

	/**
	 * {@link #titleFontSize} mutator.
	 * @param titleFontSize	The new value.
	 **/
	@XmlElement
	public void setTitleFontSize(Integer titleFontSize) {
		preset(titleFontSizePropertyName, titleFontSize);
		this.titleFontSize = titleFontSize;
	}

	/**
	 * {@link #defaultFontSize} accessor.
	 * @return	The value.
	 **/
	public Integer getDefaultFontSize() {
		return defaultFontSize;
	}

	/**
	 * {@link #defaultFontSize} mutator.
	 * @param defaultFontSize	The new value.
	 **/
	@XmlElement
	public void setDefaultFontSize(Integer defaultFontSize) {
		preset(defaultFontSizePropertyName, defaultFontSize);
		this.defaultFontSize = defaultFontSize;
	}

	/**
	 * {@link #defaultLineColour} accessor.
	 * @return	The value.
	 **/
	public String getDefaultLineColour() {
		return defaultLineColour;
	}

	/**
	 * {@link #defaultLineColour} mutator.
	 * @param defaultLineColour	The new value.
	 **/
	@XmlElement
	public void setDefaultLineColour(String defaultLineColour) {
		preset(defaultLineColourPropertyName, defaultLineColour);
		this.defaultLineColour = defaultLineColour;
	}

	/**
	 * {@link #defaultLineWidth} accessor.
	 * @return	The value.
	 **/
	public Decimal2 getDefaultLineWidth() {
		return defaultLineWidth;
	}

	/**
	 * {@link #defaultLineWidth} mutator.
	 * @param defaultLineWidth	The new value.
	 **/
	@XmlElement
	@XmlJavaTypeAdapter(Decimal2Mapper.class)
	public void setDefaultLineWidth(Decimal2 defaultLineWidth) {
		preset(defaultLineWidthPropertyName, defaultLineWidth);
		this.defaultLineWidth = defaultLineWidth;
	}

	/**
	 * {@link #renderLabelAsTextFields} accessor.
	 * @return	The value.
	 **/
	public Boolean getRenderLabelAsTextFields() {
		return renderLabelAsTextFields;
	}

	/**
	 * {@link #renderLabelAsTextFields} mutator.
	 * @param renderLabelAsTextFields	The new value.
	 **/
	@XmlElement
	public void setRenderLabelAsTextFields(Boolean renderLabelAsTextFields) {
		preset(renderLabelAsTextFieldsPropertyName, renderLabelAsTextFields);
		this.renderLabelAsTextFields = renderLabelAsTextFields;
	}

	/**
	 * {@link #defaultBorder} accessor.
	 * @return	The value.
	 **/
	public Boolean getDefaultBorder() {
		return defaultBorder;
	}

	/**
	 * {@link #defaultBorder} mutator.
	 * @param defaultBorder	The new value.
	 **/
	@XmlElement
	public void setDefaultBorder(Boolean defaultBorder) {
		preset(defaultBorderPropertyName, defaultBorder);
		this.defaultBorder = defaultBorder;
	}

	/**
	 * {@link #defaultBorderTop} accessor.
	 * @return	The value.
	 **/
	public Boolean getDefaultBorderTop() {
		return defaultBorderTop;
	}

	/**
	 * {@link #defaultBorderTop} mutator.
	 * @param defaultBorderTop	The new value.
	 **/
	@XmlElement
	public void setDefaultBorderTop(Boolean defaultBorderTop) {
		preset(defaultBorderTopPropertyName, defaultBorderTop);
		this.defaultBorderTop = defaultBorderTop;
	}

	/**
	 * {@link #defaultBorderLeft} accessor.
	 * @return	The value.
	 **/
	public Boolean getDefaultBorderLeft() {
		return defaultBorderLeft;
	}

	/**
	 * {@link #defaultBorderLeft} mutator.
	 * @param defaultBorderLeft	The new value.
	 **/
	@XmlElement
	public void setDefaultBorderLeft(Boolean defaultBorderLeft) {
		preset(defaultBorderLeftPropertyName, defaultBorderLeft);
		this.defaultBorderLeft = defaultBorderLeft;
	}

	/**
	 * {@link #defaultBorderBottom} accessor.
	 * @return	The value.
	 **/
	public Boolean getDefaultBorderBottom() {
		return defaultBorderBottom;
	}

	/**
	 * {@link #defaultBorderBottom} mutator.
	 * @param defaultBorderBottom	The new value.
	 **/
	@XmlElement
	public void setDefaultBorderBottom(Boolean defaultBorderBottom) {
		preset(defaultBorderBottomPropertyName, defaultBorderBottom);
		this.defaultBorderBottom = defaultBorderBottom;
	}

	/**
	 * {@link #defaultBorderRight} accessor.
	 * @return	The value.
	 **/
	public Boolean getDefaultBorderRight() {
		return defaultBorderRight;
	}

	/**
	 * {@link #defaultBorderRight} mutator.
	 * @param defaultBorderRight	The new value.
	 **/
	@XmlElement
	public void setDefaultBorderRight(Boolean defaultBorderRight) {
		preset(defaultBorderRightPropertyName, defaultBorderRight);
		this.defaultBorderRight = defaultBorderRight;
	}

	/**
	 * {@link #defaultElementHeight} accessor.
	 * @return	The value.
	 **/
	public Integer getDefaultElementHeight() {
		return defaultElementHeight;
	}

	/**
	 * {@link #defaultElementHeight} mutator.
	 * @param defaultElementHeight	The new value.
	 **/
	@XmlElement
	public void setDefaultElementHeight(Integer defaultElementHeight) {
		preset(defaultElementHeightPropertyName, defaultElementHeight);
		this.defaultElementHeight = defaultElementHeight;
	}

	/**
	 * {@link #includePageNumbers} accessor.
	 * @return	The value.
	 **/
	public Boolean getIncludePageNumbers() {
		return includePageNumbers;
	}

	/**
	 * {@link #includePageNumbers} mutator.
	 * @param includePageNumbers	The new value.
	 **/
	@XmlElement
	public void setIncludePageNumbers(Boolean includePageNumbers) {
		preset(includePageNumbersPropertyName, includePageNumbers);
		this.includePageNumbers = includePageNumbers;
	}

	/**
	 * {@link #defaultCellTopPadding} accessor.
	 * @return	The value.
	 **/
	public Integer getDefaultCellTopPadding() {
		return defaultCellTopPadding;
	}

	/**
	 * {@link #defaultCellTopPadding} mutator.
	 * @param defaultCellTopPadding	The new value.
	 **/
	@XmlElement
	public void setDefaultCellTopPadding(Integer defaultCellTopPadding) {
		preset(defaultCellTopPaddingPropertyName, defaultCellTopPadding);
		this.defaultCellTopPadding = defaultCellTopPadding;
	}

	/**
	 * {@link #defaultCellLeftPadding} accessor.
	 * @return	The value.
	 **/
	public Integer getDefaultCellLeftPadding() {
		return defaultCellLeftPadding;
	}

	/**
	 * {@link #defaultCellLeftPadding} mutator.
	 * @param defaultCellLeftPadding	The new value.
	 **/
	@XmlElement
	public void setDefaultCellLeftPadding(Integer defaultCellLeftPadding) {
		preset(defaultCellLeftPaddingPropertyName, defaultCellLeftPadding);
		this.defaultCellLeftPadding = defaultCellLeftPadding;
	}

	/**
	 * {@link #defaultCellBottomPadding} accessor.
	 * @return	The value.
	 **/
	public Integer getDefaultCellBottomPadding() {
		return defaultCellBottomPadding;
	}

	/**
	 * {@link #defaultCellBottomPadding} mutator.
	 * @param defaultCellBottomPadding	The new value.
	 **/
	@XmlElement
	public void setDefaultCellBottomPadding(Integer defaultCellBottomPadding) {
		preset(defaultCellBottomPaddingPropertyName, defaultCellBottomPadding);
		this.defaultCellBottomPadding = defaultCellBottomPadding;
	}

	/**
	 * {@link #defaultCellRightPadding} accessor.
	 * @return	The value.
	 **/
	public Integer getDefaultCellRightPadding() {
		return defaultCellRightPadding;
	}

	/**
	 * {@link #defaultCellRightPadding} mutator.
	 * @param defaultCellRightPadding	The new value.
	 **/
	@XmlElement
	public void setDefaultCellRightPadding(Integer defaultCellRightPadding) {
		preset(defaultCellRightPaddingPropertyName, defaultCellRightPadding);
		this.defaultCellRightPadding = defaultCellRightPadding;
	}

	/**
	 * {@link #dynamicFlow} accessor.
	 * @return	The value.
	 **/
	public Boolean getDynamicFlow() {
		return dynamicFlow;
	}

	/**
	 * {@link #dynamicFlow} mutator.
	 * @param dynamicFlow	The new value.
	 **/
	@XmlElement
	public void setDynamicFlow(Boolean dynamicFlow) {
		preset(dynamicFlowPropertyName, dynamicFlow);
		this.dynamicFlow = dynamicFlow;
	}

	/**
	 * {@link #bandSplitType} accessor.
	 * @return	The value.
	 **/
	public String getBandSplitType() {
		return bandSplitType;
	}

	/**
	 * {@link #bandSplitType} mutator.
	 * @param bandSplitType	The new value.
	 **/
	@XmlElement
	public void setBandSplitType(String bandSplitType) {
		preset(bandSplitTypePropertyName, bandSplitType);
		this.bandSplitType = bandSplitType;
	}

	/**
	 * {@link #boldLabels} accessor.
	 * @return	The value.
	 **/
	public Boolean getBoldLabels() {
		return boldLabels;
	}

	/**
	 * {@link #boldLabels} mutator.
	 * @param boldLabels	The new value.
	 **/
	@XmlElement
	public void setBoldLabels(Boolean boldLabels) {
		preset(boldLabelsPropertyName, boldLabels);
		this.boldLabels = boldLabels;
	}

	/**
	 * {@link #checkBoxFontName} accessor.
	 * @return	The value.
	 **/
	public String getCheckBoxFontName() {
		return checkBoxFontName;
	}

	/**
	 * {@link #checkBoxFontName} mutator.
	 * @param checkBoxFontName	The new value.
	 **/
	@XmlElement
	public void setCheckBoxFontName(String checkBoxFontName) {
		preset(checkBoxFontNamePropertyName, checkBoxFontName);
		this.checkBoxFontName = checkBoxFontName;
	}

	/**
	 * {@link #checkBoxDisplayExpression} accessor.
	 * @return	The value.
	 **/
	public String getCheckBoxDisplayExpression() {
		return checkBoxDisplayExpression;
	}

	/**
	 * {@link #checkBoxDisplayExpression} mutator.
	 * @param checkBoxDisplayExpression	The new value.
	 **/
	@XmlElement
	public void setCheckBoxDisplayExpression(String checkBoxDisplayExpression) {
		preset(checkBoxDisplayExpressionPropertyName, checkBoxDisplayExpression);
		this.checkBoxDisplayExpression = checkBoxDisplayExpression;
	}

	/**
	 * {@link #pixelToTwip} accessor.
	 * @return	The value.
	 **/
	public Decimal5 getPixelToTwip() {
		return pixelToTwip;
	}

	/**
	 * {@link #pixelToTwip} mutator.
	 * @param pixelToTwip	The new value.
	 **/
	@XmlElement
	@XmlJavaTypeAdapter(Decimal5Mapper.class)
	public void setPixelToTwip(Decimal5 pixelToTwip) {
		preset(pixelToTwipPropertyName, pixelToTwip);
		this.pixelToTwip = pixelToTwip;
	}

	/**
	 * {@link #sectionBorderTop} accessor.
	 * @return	The value.
	 **/
	public Boolean getSectionBorderTop() {
		return sectionBorderTop;
	}

	/**
	 * {@link #sectionBorderTop} mutator.
	 * @param sectionBorderTop	The new value.
	 **/
	@XmlElement
	public void setSectionBorderTop(Boolean sectionBorderTop) {
		preset(sectionBorderTopPropertyName, sectionBorderTop);
		this.sectionBorderTop = sectionBorderTop;
	}

	/**
	 * {@link #sectionBorderLeft} accessor.
	 * @return	The value.
	 **/
	public Boolean getSectionBorderLeft() {
		return sectionBorderLeft;
	}

	/**
	 * {@link #sectionBorderLeft} mutator.
	 * @param sectionBorderLeft	The new value.
	 **/
	@XmlElement
	public void setSectionBorderLeft(Boolean sectionBorderLeft) {
		preset(sectionBorderLeftPropertyName, sectionBorderLeft);
		this.sectionBorderLeft = sectionBorderLeft;
	}

	/**
	 * {@link #sectionBorderRight} accessor.
	 * @return	The value.
	 **/
	public Boolean getSectionBorderRight() {
		return sectionBorderRight;
	}

	/**
	 * {@link #sectionBorderRight} mutator.
	 * @param sectionBorderRight	The new value.
	 **/
	@XmlElement
	public void setSectionBorderRight(Boolean sectionBorderRight) {
		preset(sectionBorderRightPropertyName, sectionBorderRight);
		this.sectionBorderRight = sectionBorderRight;
	}

	/**
	 * {@link #sectionBorderBottom} accessor.
	 * @return	The value.
	 **/
	public Boolean getSectionBorderBottom() {
		return sectionBorderBottom;
	}

	/**
	 * {@link #sectionBorderBottom} mutator.
	 * @param sectionBorderBottom	The new value.
	 **/
	@XmlElement
	public void setSectionBorderBottom(Boolean sectionBorderBottom) {
		preset(sectionBorderBottomPropertyName, sectionBorderBottom);
		this.sectionBorderBottom = sectionBorderBottom;
	}

	/**
	 * {@link #sectionTitleBorderTop} accessor.
	 * @return	The value.
	 **/
	public Boolean getSectionTitleBorderTop() {
		return sectionTitleBorderTop;
	}

	/**
	 * {@link #sectionTitleBorderTop} mutator.
	 * @param sectionTitleBorderTop	The new value.
	 **/
	@XmlElement
	public void setSectionTitleBorderTop(Boolean sectionTitleBorderTop) {
		preset(sectionTitleBorderTopPropertyName, sectionTitleBorderTop);
		this.sectionTitleBorderTop = sectionTitleBorderTop;
	}

	/**
	 * {@link #sectionTitleBorderLeft} accessor.
	 * @return	The value.
	 **/
	public Boolean getSectionTitleBorderLeft() {
		return sectionTitleBorderLeft;
	}

	/**
	 * {@link #sectionTitleBorderLeft} mutator.
	 * @param sectionTitleBorderLeft	The new value.
	 **/
	@XmlElement
	public void setSectionTitleBorderLeft(Boolean sectionTitleBorderLeft) {
		preset(sectionTitleBorderLeftPropertyName, sectionTitleBorderLeft);
		this.sectionTitleBorderLeft = sectionTitleBorderLeft;
	}

	/**
	 * {@link #sectionTitleBorderRight} accessor.
	 * @return	The value.
	 **/
	public Boolean getSectionTitleBorderRight() {
		return sectionTitleBorderRight;
	}

	/**
	 * {@link #sectionTitleBorderRight} mutator.
	 * @param sectionTitleBorderRight	The new value.
	 **/
	@XmlElement
	public void setSectionTitleBorderRight(Boolean sectionTitleBorderRight) {
		preset(sectionTitleBorderRightPropertyName, sectionTitleBorderRight);
		this.sectionTitleBorderRight = sectionTitleBorderRight;
	}

	/**
	 * {@link #sectionTitleBorderBottom} accessor.
	 * @return	The value.
	 **/
	public Boolean getSectionTitleBorderBottom() {
		return sectionTitleBorderBottom;
	}

	/**
	 * {@link #sectionTitleBorderBottom} mutator.
	 * @param sectionTitleBorderBottom	The new value.
	 **/
	@XmlElement
	public void setSectionTitleBorderBottom(Boolean sectionTitleBorderBottom) {
		preset(sectionTitleBorderBottomPropertyName, sectionTitleBorderBottom);
		this.sectionTitleBorderBottom = sectionTitleBorderBottom;
	}

	/**
	 * {@link #sectionTitleForeground} accessor.
	 * @return	The value.
	 **/
	public String getSectionTitleForeground() {
		return sectionTitleForeground;
	}

	/**
	 * {@link #sectionTitleForeground} mutator.
	 * @param sectionTitleForeground	The new value.
	 **/
	@XmlElement
	public void setSectionTitleForeground(String sectionTitleForeground) {
		preset(sectionTitleForegroundPropertyName, sectionTitleForeground);
		this.sectionTitleForeground = sectionTitleForeground;
	}

	/**
	 * {@link #sectionTitleBackground} accessor.
	 * @return	The value.
	 **/
	public String getSectionTitleBackground() {
		return sectionTitleBackground;
	}

	/**
	 * {@link #sectionTitleBackground} mutator.
	 * @param sectionTitleBackground	The new value.
	 **/
	@XmlElement
	public void setSectionTitleBackground(String sectionTitleBackground) {
		preset(sectionTitleBackgroundPropertyName, sectionTitleBackground);
		this.sectionTitleBackground = sectionTitleBackground;
	}

	/**
	 * {@link #jrxml} accessor.
	 * @return	The value.
	 **/
	public String getJrxml() {
		return jrxml;
	}

	/**
	 * {@link #jrxml} mutator.
	 * @param jrxml	The new value.
	 **/
	@XmlElement
	public void setJrxml(String jrxml) {
		this.jrxml = jrxml;
	}

	/**
	 * {@link #field} accessor.
	 * @return	The value.
	 **/
	public String getField() {
		return field;
	}

	/**
	 * {@link #field} mutator.
	 * @param field	The new value.
	 **/
	@XmlElement
	public void setField(String field) {
		preset(fieldPropertyName, field);
		this.field = field;
	}

	/**
	 * {@link #collectionType} accessor.
	 * @return	The value.
	 **/
	public CollectionType getCollectionType() {
		return collectionType;
	}

	/**
	 * {@link #collectionType} mutator.
	 * @param collectionType	The new value.
	 **/
	@XmlElement
	public void setCollectionType(CollectionType collectionType) {
		preset(collectionTypePropertyName, collectionType);
		this.collectionType = collectionType;
	}

	/**
	 * {@link #parentReportPersistentName} accessor.
	 * @return	The value.
	 **/
	public String getParentReportPersistentName() {
		return parentReportPersistentName;
	}

	/**
	 * {@link #parentReportPersistentName} mutator.
	 * @param parentReportPersistentName	The new value.
	 **/
	@XmlElement
	public void setParentReportPersistentName(String parentReportPersistentName) {
		preset(parentReportPersistentNamePropertyName, parentReportPersistentName);
		this.parentReportPersistentName = parentReportPersistentName;
	}

	/**
	 * {@link #verticalise} accessor.
	 * @return	The value.
	 **/
	public Boolean getVerticalise() {
		return verticalise;
	}

	/**
	 * {@link #verticalise} mutator.
	 * @param verticalise	The new value.
	 **/
	@XmlElement
	public void setVerticalise(Boolean verticalise) {
		preset(verticalisePropertyName, verticalise);
		this.verticalise = verticalise;
	}

	/**
	 * {@link #labelAlignmentOverride} accessor.
	 * @return	The value.
	 **/
	public String getLabelAlignmentOverride() {
		return labelAlignmentOverride;
	}

	/**
	 * {@link #labelAlignmentOverride} mutator.
	 * @param labelAlignmentOverride	The new value.
	 **/
	@XmlElement
	public void setLabelAlignmentOverride(String labelAlignmentOverride) {
		preset(labelAlignmentOverridePropertyName, labelAlignmentOverride);
		this.labelAlignmentOverride = labelAlignmentOverride;
	}
}
