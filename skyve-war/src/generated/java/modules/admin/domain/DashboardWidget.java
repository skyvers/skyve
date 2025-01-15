package modules.admin.domain;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlEnum;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import modules.admin.Dashboard.DashboardExtension;
import modules.admin.DashboardWidget.DashboardWidgetExtension;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.ChildBean;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.types.Enumeration;
import org.skyve.impl.domain.AbstractPersistentBean;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.util.Util;

/**
 * Dashboard Widget
 * 
 * @depend - - - WidgetType
 * @depend - - - ChartType
 * @depend - - - AggregateFunction
 * @depend - - - BucketType
 * @depend - - - TemporalBucketType
 * @depend - - - TopOrderBy
 * @depend - - - TopSortDirection
 * @depend - - - OrderBy
 * @depend - - - SortDirection
 * @stereotype "persistent child"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public abstract class DashboardWidget extends AbstractPersistentBean implements ChildBean<DashboardExtension> {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "admin";

	/** @hidden */
	public static final String DOCUMENT_NAME = "DashboardWidget";

	/** @hidden */
	public static final String widgetTypePropertyName = "widgetType";

	/** @hidden */
	public static final String titlePropertyName = "title";

	/** @hidden */
	public static final String chartTypePropertyName = "chartType";

	/** @hidden */
	public static final String dashboardModulePropertyName = "dashboardModule";

	/** @hidden */
	public static final String moduleEntityPropertyName = "moduleEntity";

	/** @hidden */
	public static final String categoryBindingPropertyName = "categoryBinding";

	/** @hidden */
	public static final String valueBindingPropertyName = "valueBinding";

	/** @hidden */
	public static final String dataLabelPropertyName = "dataLabel";

	/** @hidden */
	public static final String aggregateFunctionPropertyName = "aggregateFunction";

	/** @hidden */
	public static final String bucketTypePropertyName = "bucketType";

	/** @hidden */
	public static final String numericMultiplePropertyName = "numericMultiple";

	/** @hidden */
	public static final String temporalBucketTypePropertyName = "temporalBucketType";

	/** @hidden */
	public static final String colTopPropertyName = "colTop";

	/** @hidden */
	public static final String topOrderByPropertyName = "topOrderBy";

	/** @hidden */
	public static final String topSortDirectionPropertyName = "topSortDirection";

	/** @hidden */
	public static final String includeOthersPropertyName = "includeOthers";

	/** @hidden */
	public static final String orderByPropertyName = "orderBy";

	/** @hidden */
	public static final String sortDirectionPropertyName = "sortDirection";

	/**
	 * Widget
	 **/
	@XmlEnum
	@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
	public static enum WidgetType implements Enumeration {
		customChart("CustomChartModel", "Custom Chart"),
		favourites("ModuleFavourites", "Favourites"),
		mySystemUsageLineChart("MyModuleUserActivity", "My system usage (line chart)"),
		mySystemUsageBreakdownPieChart("MyModuleUserActivityContext", "My system usage breakdown (pie chart)");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues = Stream.of(values()).map(WidgetType::toDomainValue).collect(Collectors.toUnmodifiableList());

		private WidgetType(String code, String description) {
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

		public static WidgetType fromCode(String code) {
			WidgetType result = null;

			for (WidgetType value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static WidgetType fromLocalisedDescription(String description) {
			WidgetType result = null;

			for (WidgetType value : values()) {
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
	 * Chart type
	 **/
	@XmlEnum
	@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
	public static enum ChartType implements Enumeration {
		bar("bar", "bar"),
		doughnut("doughnut", "doughnut"),
		horizontalBar("horizontalBar", "horizontalBar"),
		line("line", "line"),
		lineArea("lineArea", "lineArea"),
		pie("pie", "pie"),
		polarArea("polarArea", "polarArea"),
		radar("radar", "radar");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues = Stream.of(values()).map(ChartType::toDomainValue).collect(Collectors.toUnmodifiableList());

		private ChartType(String code, String description) {
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

		public static ChartType fromCode(String code) {
			ChartType result = null;

			for (ChartType value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static ChartType fromLocalisedDescription(String description) {
			ChartType result = null;

			for (ChartType value : values()) {
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
	 * Aggregate function
	 **/
	@XmlEnum
	@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
	public static enum AggregateFunction implements Enumeration {
		avg("Avg", "Avg"),
		count("Count", "Count"),
		max("Max", "Max"),
		min("Min", "Min"),
		sum("Sum", "Sum");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues = Stream.of(values()).map(AggregateFunction::toDomainValue).collect(Collectors.toUnmodifiableList());

		private AggregateFunction(String code, String description) {
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

		public static AggregateFunction fromCode(String code) {
			AggregateFunction result = null;

			for (AggregateFunction value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static AggregateFunction fromLocalisedDescription(String description) {
			AggregateFunction result = null;

			for (AggregateFunction value : values()) {
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
	 * Bucket
	 **/
	@XmlEnum
	@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
	public static enum BucketType implements Enumeration {
		noBucket("noBucket", "noBucket"),
		numericalMultipleBucket("numericalMultipleBucket", "numericalMultipleBucket"),
		numericRangeBucket("numericRangeBucket", "numericRangeBucket"),
		temporalBucket("temporalBucket", "temporalBucket"),
		textLengthBucket("textLengthBucket", "textLengthBucket"),
		textStartsWithBucket("textStartsWithBucket", "textStartsWithBucket");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues = Stream.of(values()).map(BucketType::toDomainValue).collect(Collectors.toUnmodifiableList());

		private BucketType(String code, String description) {
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

		public static BucketType fromCode(String code) {
			BucketType result = null;

			for (BucketType value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static BucketType fromLocalisedDescription(String description) {
			BucketType result = null;

			for (BucketType value : values()) {
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
	 * Temporal bucket  type
	 **/
	@XmlEnum
	@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
	public static enum TemporalBucketType implements Enumeration {
		quarter("quarter", "quarter"),
		dayMonthYear("dayMonthYear", "dayMonthYear"),
		day("day", "day"),
		month("month", "month"),
		year("year", "year"),
		monthYear("monthYear", "monthYear"),
		hour("hour", "hour"),
		hourDay("hourDay", "hourDay"),
		hourDayMonth("hourDayMonth", "hourDayMonth"),
		minuteHour("minuteHour", "minuteHour"),
		secondMinuteHour("secondMinuteHour", "secondMinuteHour");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues = Stream.of(values()).map(TemporalBucketType::toDomainValue).collect(Collectors.toUnmodifiableList());

		private TemporalBucketType(String code, String description) {
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

		public static TemporalBucketType fromCode(String code) {
			TemporalBucketType result = null;

			for (TemporalBucketType value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static TemporalBucketType fromLocalisedDescription(String description) {
			TemporalBucketType result = null;

			for (TemporalBucketType value : values()) {
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
	 * Top order by
	 **/
	@XmlEnum
	@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
	public static enum TopOrderBy implements Enumeration {
		category("category", "category"),
		value("value", "value");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues = Stream.of(values()).map(TopOrderBy::toDomainValue).collect(Collectors.toUnmodifiableList());

		private TopOrderBy(String code, String description) {
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

		public static TopOrderBy fromCode(String code) {
			TopOrderBy result = null;

			for (TopOrderBy value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static TopOrderBy fromLocalisedDescription(String description) {
			TopOrderBy result = null;

			for (TopOrderBy value : values()) {
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
	 * Top Sort direction
	 **/
	@XmlEnum
	@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
	public static enum TopSortDirection implements Enumeration {
		ascending("ascending", "ascending"),
		descending("descending", "descending");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues = Stream.of(values()).map(TopSortDirection::toDomainValue).collect(Collectors.toUnmodifiableList());

		private TopSortDirection(String code, String description) {
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

		public static TopSortDirection fromCode(String code) {
			TopSortDirection result = null;

			for (TopSortDirection value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static TopSortDirection fromLocalisedDescription(String description) {
			TopSortDirection result = null;

			for (TopSortDirection value : values()) {
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
	 * Order by
	 **/
	@XmlEnum
	@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
	public static enum OrderBy implements Enumeration {
		category("category", "category"),
		value("value", "value");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues = Stream.of(values()).map(OrderBy::toDomainValue).collect(Collectors.toUnmodifiableList());

		private OrderBy(String code, String description) {
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

		public static OrderBy fromCode(String code) {
			OrderBy result = null;

			for (OrderBy value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static OrderBy fromLocalisedDescription(String description) {
			OrderBy result = null;

			for (OrderBy value : values()) {
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
	 * Sort direction
	 **/
	@XmlEnum
	@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
	public static enum SortDirection implements Enumeration {
		ascending("ascending", "ascending"),
		descending("descending", "descending");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues = Stream.of(values()).map(SortDirection::toDomainValue).collect(Collectors.toUnmodifiableList());

		private SortDirection(String code, String description) {
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

		public static SortDirection fromCode(String code) {
			SortDirection result = null;

			for (SortDirection value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static SortDirection fromLocalisedDescription(String description) {
			SortDirection result = null;

			for (SortDirection value : values()) {
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
	 * Widget
	 **/
	private WidgetType widgetType;

	/**
	 * Title
	 **/
	private String title;

	/**
	 * Chart type
	 **/
	private ChartType chartType = ChartType.line;

	/**
	 * Module
	 **/
	private String dashboardModule;

	/**
	 * Entity
	 **/
	private String moduleEntity;

	/**
	 * Category
	 **/
	private String categoryBinding;

	/**
	 * Value
	 **/
	private String valueBinding;

	/**
	 * Data label
	 **/
	private String dataLabel;

	/**
	 * Aggregate function
	 **/
	private AggregateFunction aggregateFunction = AggregateFunction.count;

	/**
	 * Bucket
	 **/
	private BucketType bucketType = BucketType.noBucket;

	/**
	 * Numeric multiple
	 **/
	private Integer numericMultiple = Integer.valueOf(100);

	/**
	 * Temporal bucket  type
	 **/
	private TemporalBucketType temporalBucketType = TemporalBucketType.day;

	/**
	 * Top
	 **/
	private Integer colTop = Integer.valueOf(5);

	/**
	 * Top order by
	 **/
	private TopOrderBy topOrderBy = TopOrderBy.category;

	/**
	 * Top Sort direction
	 **/
	private TopSortDirection topSortDirection = TopSortDirection.ascending;

	/**
	 * Include others
	 **/
	private Boolean includeOthers = Boolean.valueOf(false);

	/**
	 * Order by
	 **/
	private OrderBy orderBy = OrderBy.category;

	/**
	 * Sort direction
	 **/
	private SortDirection sortDirection = SortDirection.ascending;

	private DashboardExtension parent;

	private Integer bizOrdinal;

	@Override
	@XmlTransient
	public String getBizModule() {
		return DashboardWidget.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return DashboardWidget.DOCUMENT_NAME;
	}

	public static DashboardWidgetExtension newInstance() {
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
			return org.skyve.util.Binder.formatMessage("DashboardWidget", this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return "Unknown";
		}
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof DashboardWidget) && 
					this.getBizId().equals(((DashboardWidget) o).getBizId()));
	}

	/**
	 * {@link #widgetType} accessor.
	 * @return	The value.
	 **/
	public WidgetType getWidgetType() {
		return widgetType;
	}

	/**
	 * {@link #widgetType} mutator.
	 * @param widgetType	The new value.
	 **/
	@XmlElement
	public void setWidgetType(WidgetType widgetType) {
		preset(widgetTypePropertyName, widgetType);
		this.widgetType = widgetType;
	}

	/**
	 * {@link #title} accessor.
	 * @return	The value.
	 **/
	public String getTitle() {
		return title;
	}

	/**
	 * {@link #title} mutator.
	 * @param title	The new value.
	 **/
	@XmlElement
	public void setTitle(String title) {
		preset(titlePropertyName, title);
		this.title = title;
	}

	/**
	 * {@link #chartType} accessor.
	 * @return	The value.
	 **/
	public ChartType getChartType() {
		return chartType;
	}

	/**
	 * {@link #chartType} mutator.
	 * @param chartType	The new value.
	 **/
	@XmlElement
	public void setChartType(ChartType chartType) {
		preset(chartTypePropertyName, chartType);
		this.chartType = chartType;
	}

	/**
	 * {@link #dashboardModule} accessor.
	 * @return	The value.
	 **/
	public String getDashboardModule() {
		return dashboardModule;
	}

	/**
	 * {@link #dashboardModule} mutator.
	 * @param dashboardModule	The new value.
	 **/
	@XmlElement
	public void setDashboardModule(String dashboardModule) {
		preset(dashboardModulePropertyName, dashboardModule);
		this.dashboardModule = dashboardModule;
	}

	/**
	 * {@link #moduleEntity} accessor.
	 * @return	The value.
	 **/
	public String getModuleEntity() {
		return moduleEntity;
	}

	/**
	 * {@link #moduleEntity} mutator.
	 * @param moduleEntity	The new value.
	 **/
	@XmlElement
	public void setModuleEntity(String moduleEntity) {
		preset(moduleEntityPropertyName, moduleEntity);
		this.moduleEntity = moduleEntity;
	}

	/**
	 * {@link #categoryBinding} accessor.
	 * @return	The value.
	 **/
	public String getCategoryBinding() {
		return categoryBinding;
	}

	/**
	 * {@link #categoryBinding} mutator.
	 * @param categoryBinding	The new value.
	 **/
	@XmlElement
	public void setCategoryBinding(String categoryBinding) {
		preset(categoryBindingPropertyName, categoryBinding);
		this.categoryBinding = categoryBinding;
	}

	/**
	 * {@link #valueBinding} accessor.
	 * @return	The value.
	 **/
	public String getValueBinding() {
		return valueBinding;
	}

	/**
	 * {@link #valueBinding} mutator.
	 * @param valueBinding	The new value.
	 **/
	@XmlElement
	public void setValueBinding(String valueBinding) {
		preset(valueBindingPropertyName, valueBinding);
		this.valueBinding = valueBinding;
	}

	/**
	 * {@link #dataLabel} accessor.
	 * @return	The value.
	 **/
	public String getDataLabel() {
		return dataLabel;
	}

	/**
	 * {@link #dataLabel} mutator.
	 * @param dataLabel	The new value.
	 **/
	@XmlElement
	public void setDataLabel(String dataLabel) {
		preset(dataLabelPropertyName, dataLabel);
		this.dataLabel = dataLabel;
	}

	/**
	 * {@link #aggregateFunction} accessor.
	 * @return	The value.
	 **/
	public AggregateFunction getAggregateFunction() {
		return aggregateFunction;
	}

	/**
	 * {@link #aggregateFunction} mutator.
	 * @param aggregateFunction	The new value.
	 **/
	@XmlElement
	public void setAggregateFunction(AggregateFunction aggregateFunction) {
		preset(aggregateFunctionPropertyName, aggregateFunction);
		this.aggregateFunction = aggregateFunction;
	}

	/**
	 * {@link #bucketType} accessor.
	 * @return	The value.
	 **/
	public BucketType getBucketType() {
		return bucketType;
	}

	/**
	 * {@link #bucketType} mutator.
	 * @param bucketType	The new value.
	 **/
	@XmlElement
	public void setBucketType(BucketType bucketType) {
		preset(bucketTypePropertyName, bucketType);
		this.bucketType = bucketType;
	}

	/**
	 * {@link #numericMultiple} accessor.
	 * @return	The value.
	 **/
	public Integer getNumericMultiple() {
		return numericMultiple;
	}

	/**
	 * {@link #numericMultiple} mutator.
	 * @param numericMultiple	The new value.
	 **/
	@XmlElement
	public void setNumericMultiple(Integer numericMultiple) {
		preset(numericMultiplePropertyName, numericMultiple);
		this.numericMultiple = numericMultiple;
	}

	/**
	 * {@link #temporalBucketType} accessor.
	 * @return	The value.
	 **/
	public TemporalBucketType getTemporalBucketType() {
		return temporalBucketType;
	}

	/**
	 * {@link #temporalBucketType} mutator.
	 * @param temporalBucketType	The new value.
	 **/
	@XmlElement
	public void setTemporalBucketType(TemporalBucketType temporalBucketType) {
		preset(temporalBucketTypePropertyName, temporalBucketType);
		this.temporalBucketType = temporalBucketType;
	}

	/**
	 * {@link #colTop} accessor.
	 * @return	The value.
	 **/
	public Integer getColTop() {
		return colTop;
	}

	/**
	 * {@link #colTop} mutator.
	 * @param colTop	The new value.
	 **/
	@XmlElement
	public void setColTop(Integer colTop) {
		preset(colTopPropertyName, colTop);
		this.colTop = colTop;
	}

	/**
	 * {@link #topOrderBy} accessor.
	 * @return	The value.
	 **/
	public TopOrderBy getTopOrderBy() {
		return topOrderBy;
	}

	/**
	 * {@link #topOrderBy} mutator.
	 * @param topOrderBy	The new value.
	 **/
	@XmlElement
	public void setTopOrderBy(TopOrderBy topOrderBy) {
		preset(topOrderByPropertyName, topOrderBy);
		this.topOrderBy = topOrderBy;
	}

	/**
	 * {@link #topSortDirection} accessor.
	 * @return	The value.
	 **/
	public TopSortDirection getTopSortDirection() {
		return topSortDirection;
	}

	/**
	 * {@link #topSortDirection} mutator.
	 * @param topSortDirection	The new value.
	 **/
	@XmlElement
	public void setTopSortDirection(TopSortDirection topSortDirection) {
		preset(topSortDirectionPropertyName, topSortDirection);
		this.topSortDirection = topSortDirection;
	}

	/**
	 * {@link #includeOthers} accessor.
	 * @return	The value.
	 **/
	public Boolean getIncludeOthers() {
		return includeOthers;
	}

	/**
	 * {@link #includeOthers} mutator.
	 * @param includeOthers	The new value.
	 **/
	@XmlElement
	public void setIncludeOthers(Boolean includeOthers) {
		preset(includeOthersPropertyName, includeOthers);
		this.includeOthers = includeOthers;
	}

	/**
	 * {@link #orderBy} accessor.
	 * @return	The value.
	 **/
	public OrderBy getOrderBy() {
		return orderBy;
	}

	/**
	 * {@link #orderBy} mutator.
	 * @param orderBy	The new value.
	 **/
	@XmlElement
	public void setOrderBy(OrderBy orderBy) {
		preset(orderByPropertyName, orderBy);
		this.orderBy = orderBy;
	}

	/**
	 * {@link #sortDirection} accessor.
	 * @return	The value.
	 **/
	public SortDirection getSortDirection() {
		return sortDirection;
	}

	/**
	 * {@link #sortDirection} mutator.
	 * @param sortDirection	The new value.
	 **/
	@XmlElement
	public void setSortDirection(SortDirection sortDirection) {
		preset(sortDirectionPropertyName, sortDirection);
		this.sortDirection = sortDirection;
	}

	/**
	 * showCustomChartOptions
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isShowCustomChartOptions() {
		return (((DashboardWidgetExtension)this).showCustomChartOptions());
	}

	/**
	 * {@link #isShowCustomChartOptions} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotShowCustomChartOptions() {
		return (! isShowCustomChartOptions());
	}

	@Override
	public DashboardExtension getParent() {
		return parent;
	}

	@Override
	@XmlElement
	public void setParent(DashboardExtension parent) {
		if (this.parent != parent) {
			preset(ChildBean.PARENT_NAME, parent);
			this.parent = parent;
		}
	}

	@Override
	public Integer getBizOrdinal() {
		return bizOrdinal;
	}

	@Override
	@XmlElement
	public void setBizOrdinal(Integer bizOrdinal) {
		preset(Bean.ORDINAL_NAME, bizOrdinal);
		this.bizOrdinal =  bizOrdinal;
	}
}
