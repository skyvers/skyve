package modules.kitchensink.domain;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlSchemaType;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import java.util.List;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.Decimal10;
import org.skyve.domain.types.Decimal2;
import org.skyve.domain.types.Decimal5;
import org.skyve.domain.types.TimeOnly;
import org.skyve.domain.types.Timestamp;
import org.skyve.impl.domain.AbstractPersistentBean;
import org.skyve.impl.domain.ChangeTrackingArrayList;
import org.skyve.impl.domain.types.jaxb.DateOnlyMapper;
import org.skyve.impl.domain.types.jaxb.DateTimeMapper;
import org.skyve.impl.domain.types.jaxb.Decimal10Mapper;
import org.skyve.impl.domain.types.jaxb.Decimal2Mapper;
import org.skyve.impl.domain.types.jaxb.Decimal5Mapper;
import org.skyve.impl.domain.types.jaxb.TimeOnlyMapper;
import org.skyve.impl.domain.types.jaxb.TimestampMapper;

/**
 * Converters Fixture
 * <br/>
 * Kitchen Sink fixture with one attribute for each converter (ConverterName), named after it, of the
			attribute type the converter takes.

			Its edit view shows every attribute, grouped by type, and an inline data grid for each group, so that each
			converter's values can be entered in a form field and in a grid cell. Its queries list each group's columns
			for the list grids' filters: the PrimeFaces Vue list grid and SmartClient list grid of a list view, and the
			PrimeFaces data tables of ConvertersDataTables. The rows are ConvertersFixtureRow, which has the same
			attributes.
 * 
 * @navcomposed n timestampRows 0..n ConvertersFixtureRow
 * @navcomposed n dateOnlyRows 0..n ConvertersFixtureRow
 * @navcomposed n timeRows 0..n ConvertersFixtureRow
 * @navcomposed n dateRows 0..n ConvertersFixtureRow
 * @navcomposed n numberRows 0..n ConvertersFixtureRow
 * @navcomposed n dateTimeRows 0..n ConvertersFixtureRow
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public class ConvertersFixture extends AbstractPersistentBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "kitchensink";

	/** @hidden */
	public static final String DOCUMENT_NAME = "ConvertersFixture";

	/** @hidden */
	public static final String namePropertyName = "name";

	/** @hidden */
	public static final String ddMmYyyyPropertyName = "ddMmYyyy";

	/** @hidden */
	public static final String ddMmmYyyyPropertyName = "ddMmmYyyy";

	/** @hidden */
	public static final String mmDdYyyyPropertyName = "mmDdYyyy";

	/** @hidden */
	public static final String mmmDdYyyyPropertyName = "mmmDdYyyy";

	/** @hidden */
	public static final String yyyyMmDdPropertyName = "yyyyMmDd";

	/** @hidden */
	public static final String ddMmYyyyHhMiPropertyName = "ddMmYyyyHhMi";

	/** @hidden */
	public static final String ddMmYyyyHh24MiPropertyName = "ddMmYyyyHh24Mi";

	/** @hidden */
	public static final String ddMmmYyyyHhMiPropertyName = "ddMmmYyyyHhMi";

	/** @hidden */
	public static final String ddMmmYyyyHh24MiPropertyName = "ddMmmYyyyHh24Mi";

	/** @hidden */
	public static final String mmDdYyyyHhMiPropertyName = "mmDdYyyyHhMi";

	/** @hidden */
	public static final String mmDdYyyyHh24MiPropertyName = "mmDdYyyyHh24Mi";

	/** @hidden */
	public static final String mmmDdYyyyHhMiPropertyName = "mmmDdYyyyHhMi";

	/** @hidden */
	public static final String mmmDdYyyyHh24MiPropertyName = "mmmDdYyyyHh24Mi";

	/** @hidden */
	public static final String yyyyMmDdHhMiPropertyName = "yyyyMmDdHhMi";

	/** @hidden */
	public static final String yyyyMmDdHh24MiPropertyName = "yyyyMmDdHh24Mi";

	/** @hidden */
	public static final String ddMmYyyyHhMiSsPropertyName = "ddMmYyyyHhMiSs";

	/** @hidden */
	public static final String ddMmYyyyHh24MiSsPropertyName = "ddMmYyyyHh24MiSs";

	/** @hidden */
	public static final String ddMmmYyyyHhMiSsPropertyName = "ddMmmYyyyHhMiSs";

	/** @hidden */
	public static final String ddMmmYyyyHh24MiSsPropertyName = "ddMmmYyyyHh24MiSs";

	/** @hidden */
	public static final String mmDdYyyyHhMiSsPropertyName = "mmDdYyyyHhMiSs";

	/** @hidden */
	public static final String mmDdYyyyHh24MiSsPropertyName = "mmDdYyyyHh24MiSs";

	/** @hidden */
	public static final String mmmDdYyyyHhMiSsPropertyName = "mmmDdYyyyHhMiSs";

	/** @hidden */
	public static final String mmmDdYyyyHh24MiSsPropertyName = "mmmDdYyyyHh24MiSs";

	/** @hidden */
	public static final String yyyyMmDdHhMiSsPropertyName = "yyyyMmDdHhMiSs";

	/** @hidden */
	public static final String yyyyMmDdHh24MiSsPropertyName = "yyyyMmDdHh24MiSs";

	/** @hidden */
	public static final String ddMmYyyyDateTimePropertyName = "ddMmYyyyDateTime";

	/** @hidden */
	public static final String ddMmmYyyyDateTimePropertyName = "ddMmmYyyyDateTime";

	/** @hidden */
	public static final String mmDdYyyyDateTimePropertyName = "mmDdYyyyDateTime";

	/** @hidden */
	public static final String mmmDdYyyyDateTimePropertyName = "mmmDdYyyyDateTime";

	/** @hidden */
	public static final String yyyyMmDdDateTimePropertyName = "yyyyMmDdDateTime";

	/** @hidden */
	public static final String ddMmYyyyTimestampPropertyName = "ddMmYyyyTimestamp";

	/** @hidden */
	public static final String ddMmmYyyyTimestampPropertyName = "ddMmmYyyyTimestamp";

	/** @hidden */
	public static final String mmDdYyyyTimestampPropertyName = "mmDdYyyyTimestamp";

	/** @hidden */
	public static final String mmmDdYyyyTimestampPropertyName = "mmmDdYyyyTimestamp";

	/** @hidden */
	public static final String yyyyMmDdTimestampPropertyName = "yyyyMmDdTimestamp";

	/** @hidden */
	public static final String hhMiPropertyName = "hhMi";

	/** @hidden */
	public static final String hh24MiPropertyName = "hh24Mi";

	/** @hidden */
	public static final String hhMiSsPropertyName = "hhMiSs";

	/** @hidden */
	public static final String hh24MiSsPropertyName = "hh24MiSs";

	/** @hidden */
	public static final String decimal2DollarsAndCentsPropertyName = "decimal2DollarsAndCents";

	/** @hidden */
	public static final String decimal2DollarsAndCentsAbsolutePropertyName = "decimal2DollarsAndCentsAbsolute";

	/** @hidden */
	public static final String decimal2IntegerPropertyName = "decimal2Integer";

	/** @hidden */
	public static final String decimal2IntegerPercentagePropertyName = "decimal2IntegerPercentage";

	/** @hidden */
	public static final String decimal2OneDecimalPlacePropertyName = "decimal2OneDecimalPlace";

	/** @hidden */
	public static final String decimal5IntegerPropertyName = "decimal5Integer";

	/** @hidden */
	public static final String decimal5IntegerPercentagePropertyName = "decimal5IntegerPercentage";

	/** @hidden */
	public static final String decimal5DollarsAndCentsPropertyName = "decimal5DollarsAndCents";

	/** @hidden */
	public static final String decimal5TimeDurationPropertyName = "decimal5TimeDuration";

	/** @hidden */
	public static final String decimal5OneDecimalPlacePropertyName = "decimal5OneDecimalPlace";

	/** @hidden */
	public static final String decimal5TwoDecimalPlacesPropertyName = "decimal5TwoDecimalPlaces";

	/** @hidden */
	public static final String decimal5TwoDecimalPlacesPercentagePropertyName = "decimal5TwoDecimalPlacesPercentage";

	/** @hidden */
	public static final String decimal10IntegerPropertyName = "decimal10Integer";

	/** @hidden */
	public static final String decimal10IntegerPercentagePropertyName = "decimal10IntegerPercentage";

	/** @hidden */
	public static final String decimal10DollarsAndCentsPropertyName = "decimal10DollarsAndCents";

	/** @hidden */
	public static final String decimal10OneDecimalPlacePropertyName = "decimal10OneDecimalPlace";

	/** @hidden */
	public static final String decimal10TwoDecimalPlacesPropertyName = "decimal10TwoDecimalPlaces";

	/** @hidden */
	public static final String decimal10TwoDecimalPlacesPercentagePropertyName = "decimal10TwoDecimalPlacesPercentage";

	/** @hidden */
	public static final String simplePercentagePropertyName = "simplePercentage";

	/** @hidden */
	public static final String integerSeparatorPropertyName = "integerSeparator";

	/** @hidden */
	public static final String longIntegerSeparatorPropertyName = "longIntegerSeparator";

	/** @hidden */
	public static final String plainDecimal2PropertyName = "plainDecimal2";

	/** @hidden */
	public static final String dateRowsPropertyName = "dateRows";

	/** @hidden */
	public static final String dateTimeRowsPropertyName = "dateTimeRows";

	/** @hidden */
	public static final String timestampRowsPropertyName = "timestampRows";

	/** @hidden */
	public static final String dateOnlyRowsPropertyName = "dateOnlyRows";

	/** @hidden */
	public static final String timeRowsPropertyName = "timeRows";

	/** @hidden */
	public static final String numberRowsPropertyName = "numberRows";

	/**
	 * Name
	 **/
	private String name;

	/**
	 * DD_MM_YYYY
	 **/
	private DateOnly ddMmYyyy;

	/**
	 * DD_MMM_YYYY
	 **/
	private DateOnly ddMmmYyyy;

	/**
	 * MM_DD_YYYY
	 **/
	private DateOnly mmDdYyyy;

	/**
	 * MMM_DD_YYYY
	 **/
	private DateOnly mmmDdYyyy;

	/**
	 * YYYY_MM_DD
	 **/
	private DateOnly yyyyMmDd;

	/**
	 * DD_MM_YYYY_HH_MI
	 **/
	private DateTime ddMmYyyyHhMi;

	/**
	 * DD_MM_YYYY_HH24_MI
	 **/
	private DateTime ddMmYyyyHh24Mi;

	/**
	 * DD_MMM_YYYY_HH_MI
	 **/
	private DateTime ddMmmYyyyHhMi;

	/**
	 * DD_MMM_YYYY_HH24_MI
	 **/
	private DateTime ddMmmYyyyHh24Mi;

	/**
	 * MM_DD_YYYY_HH_MI
	 **/
	private DateTime mmDdYyyyHhMi;

	/**
	 * MM_DD_YYYY_HH24_MI
	 **/
	private DateTime mmDdYyyyHh24Mi;

	/**
	 * MMM_DD_YYYY_HH_MI
	 **/
	private DateTime mmmDdYyyyHhMi;

	/**
	 * MMM_DD_YYYY_HH24_MI
	 **/
	private DateTime mmmDdYyyyHh24Mi;

	/**
	 * YYYY_MM_DD_HH_MI
	 **/
	private DateTime yyyyMmDdHhMi;

	/**
	 * YYYY_MM_DD_HH24_MI
	 **/
	private DateTime yyyyMmDdHh24Mi;

	/**
	 * DD_MM_YYYY_HH_MI_SS
	 **/
	private Timestamp ddMmYyyyHhMiSs;

	/**
	 * DD_MM_YYYY_HH24_MI_SS
	 **/
	private Timestamp ddMmYyyyHh24MiSs;

	/**
	 * DD_MMM_YYYY_HH_MI_SS
	 **/
	private Timestamp ddMmmYyyyHhMiSs;

	/**
	 * DD_MMM_YYYY_HH24_MI_SS
	 **/
	private Timestamp ddMmmYyyyHh24MiSs;

	/**
	 * MM_DD_YYYY_HH_MI_SS
	 **/
	private Timestamp mmDdYyyyHhMiSs;

	/**
	 * MM_DD_YYYY_HH24_MI_SS
	 **/
	private Timestamp mmDdYyyyHh24MiSs;

	/**
	 * MMM_DD_YYYY_HH_MI_SS
	 **/
	private Timestamp mmmDdYyyyHhMiSs;

	/**
	 * MMM_DD_YYYY_HH24_MI_SS
	 **/
	private Timestamp mmmDdYyyyHh24MiSs;

	/**
	 * YYYY_MM_DD_HH_MI_SS
	 **/
	private Timestamp yyyyMmDdHhMiSs;

	/**
	 * YYYY_MM_DD_HH24_MI_SS
	 **/
	private Timestamp yyyyMmDdHh24MiSs;

	/**
	 * DD_MM_YYYY_DateTime
	 **/
	private DateTime ddMmYyyyDateTime;

	/**
	 * DD_MMM_YYYY_DateTime
	 **/
	private DateTime ddMmmYyyyDateTime;

	/**
	 * MM_DD_YYYY_DateTime
	 **/
	private DateTime mmDdYyyyDateTime;

	/**
	 * MMM_DD_YYYY_DateTime
	 **/
	private DateTime mmmDdYyyyDateTime;

	/**
	 * YYYY_MM_DD_DateTime
	 **/
	private DateTime yyyyMmDdDateTime;

	/**
	 * DD_MM_YYYY_Timestamp
	 **/
	private Timestamp ddMmYyyyTimestamp;

	/**
	 * DD_MMM_YYYY_Timestamp
	 **/
	private Timestamp ddMmmYyyyTimestamp;

	/**
	 * MM_DD_YYYY_Timestamp
	 **/
	private Timestamp mmDdYyyyTimestamp;

	/**
	 * MMM_DD_YYYY_Timestamp
	 **/
	private Timestamp mmmDdYyyyTimestamp;

	/**
	 * YYYY_MM_DD_Timestamp
	 **/
	private Timestamp yyyyMmDdTimestamp;

	/**
	 * HH_MI
	 **/
	private TimeOnly hhMi;

	/**
	 * HH24_MI
	 **/
	private TimeOnly hh24Mi;

	/**
	 * HH_MI_SS
	 **/
	private TimeOnly hhMiSs;

	/**
	 * HH24_MI_SS
	 **/
	private TimeOnly hh24MiSs;

	/**
	 * Decimal2DollarsAndCents
	 **/
	private Decimal2 decimal2DollarsAndCents;

	/**
	 * Decimal2DollarsAndCentsAbsolute
	 **/
	private Decimal2 decimal2DollarsAndCentsAbsolute;

	/**
	 * Decimal2Integer
	 **/
	private Decimal2 decimal2Integer;

	/**
	 * Decimal2IntegerPercentage
	 **/
	private Decimal2 decimal2IntegerPercentage;

	/**
	 * Decimal2OneDecimalPlace
	 **/
	private Decimal2 decimal2OneDecimalPlace;

	/**
	 * Decimal5Integer
	 **/
	private Decimal5 decimal5Integer;

	/**
	 * Decimal5IntegerPercentage
	 **/
	private Decimal5 decimal5IntegerPercentage;

	/**
	 * Decimal5DollarsAndCents
	 **/
	private Decimal5 decimal5DollarsAndCents;

	/**
	 * Decimal5TimeDuration
	 **/
	private Decimal5 decimal5TimeDuration;

	/**
	 * Decimal5OneDecimalPlace
	 **/
	private Decimal5 decimal5OneDecimalPlace;

	/**
	 * Decimal5TwoDecimalPlaces
	 **/
	private Decimal5 decimal5TwoDecimalPlaces;

	/**
	 * Decimal5TwoDecimalPlacesPercentage
	 **/
	private Decimal5 decimal5TwoDecimalPlacesPercentage;

	/**
	 * Decimal10Integer
	 **/
	private Decimal10 decimal10Integer;

	/**
	 * Decimal10IntegerPercentage
	 **/
	private Decimal10 decimal10IntegerPercentage;

	/**
	 * Decimal10DollarsAndCents
	 **/
	private Decimal10 decimal10DollarsAndCents;

	/**
	 * Decimal10OneDecimalPlace
	 **/
	private Decimal10 decimal10OneDecimalPlace;

	/**
	 * Decimal10TwoDecimalPlaces
	 **/
	private Decimal10 decimal10TwoDecimalPlaces;

	/**
	 * Decimal10TwoDecimalPlacesPercentage
	 **/
	private Decimal10 decimal10TwoDecimalPlacesPercentage;

	/**
	 * SimplePercentage
	 **/
	private Integer simplePercentage;

	/**
	 * IntegerSeparator
	 **/
	private Integer integerSeparator;

	/**
	 * LongIntegerSeparator
	 **/
	private Long longIntegerSeparator;

	/**
	 * Decimal2 (no converter)
	 **/
	private Decimal2 plainDecimal2;

	/**
	 * Dates rows
	 * <br/>
	 * Rows of the inline data grid of dates, for entering each converter's values in a grid cell.
	 **/
	private List<ConvertersFixtureRow> dateRows = new ChangeTrackingArrayList<>("dateRows", this);

	/**
	 * Date times rows
	 * <br/>
	 * Rows of the inline data grid of date times, for entering each converter's values in a grid cell.
	 **/
	private List<ConvertersFixtureRow> dateTimeRows = new ChangeTrackingArrayList<>("dateTimeRows", this);

	/**
	 * Timestamps rows
	 * <br/>
	 * Rows of the inline data grid of timestamps, for entering each converter's values in a grid cell.
	 **/
	private List<ConvertersFixtureRow> timestampRows = new ChangeTrackingArrayList<>("timestampRows", this);

	/**
	 * Date times and timestamps shown as dates rows
	 * <br/>
	 * Rows of the inline data grid of date times and timestamps shown as dates, for entering each converter's values in a grid cell.
	 **/
	private List<ConvertersFixtureRow> dateOnlyRows = new ChangeTrackingArrayList<>("dateOnlyRows", this);

	/**
	 * Times rows
	 * <br/>
	 * Rows of the inline data grid of times, for entering each converter's values in a grid cell.
	 **/
	private List<ConvertersFixtureRow> timeRows = new ChangeTrackingArrayList<>("timeRows", this);

	/**
	 * Numbers rows
	 * <br/>
	 * Rows of the inline data grid of numbers, for entering each converter's values in a grid cell.
	 **/
	private List<ConvertersFixtureRow> numberRows = new ChangeTrackingArrayList<>("numberRows", this);

	@Override
	@XmlTransient
	public String getBizModule() {
		return ConvertersFixture.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return ConvertersFixture.DOCUMENT_NAME;
	}

	public static ConvertersFixture newInstance() {
		try {
			return KitchensinkDocument.CONVERTERS_FIXTURE.toDocument(CORE.getUser().getCustomer()).newInstance(CORE.getUser());
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
	 * {@link #ddMmYyyy} accessor.
	 * @return	The value.
	 **/
	public DateOnly getDdMmYyyy() {
		return ddMmYyyy;
	}

	/**
	 * {@link #ddMmYyyy} mutator.
	 * @param ddMmYyyy	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "date")
	@XmlJavaTypeAdapter(DateOnlyMapper.class)
	public void setDdMmYyyy(DateOnly ddMmYyyy) {
		preset(ddMmYyyyPropertyName, ddMmYyyy);
		this.ddMmYyyy = ddMmYyyy;
	}

	/**
	 * {@link #ddMmmYyyy} accessor.
	 * @return	The value.
	 **/
	public DateOnly getDdMmmYyyy() {
		return ddMmmYyyy;
	}

	/**
	 * {@link #ddMmmYyyy} mutator.
	 * @param ddMmmYyyy	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "date")
	@XmlJavaTypeAdapter(DateOnlyMapper.class)
	public void setDdMmmYyyy(DateOnly ddMmmYyyy) {
		preset(ddMmmYyyyPropertyName, ddMmmYyyy);
		this.ddMmmYyyy = ddMmmYyyy;
	}

	/**
	 * {@link #mmDdYyyy} accessor.
	 * @return	The value.
	 **/
	public DateOnly getMmDdYyyy() {
		return mmDdYyyy;
	}

	/**
	 * {@link #mmDdYyyy} mutator.
	 * @param mmDdYyyy	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "date")
	@XmlJavaTypeAdapter(DateOnlyMapper.class)
	public void setMmDdYyyy(DateOnly mmDdYyyy) {
		preset(mmDdYyyyPropertyName, mmDdYyyy);
		this.mmDdYyyy = mmDdYyyy;
	}

	/**
	 * {@link #mmmDdYyyy} accessor.
	 * @return	The value.
	 **/
	public DateOnly getMmmDdYyyy() {
		return mmmDdYyyy;
	}

	/**
	 * {@link #mmmDdYyyy} mutator.
	 * @param mmmDdYyyy	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "date")
	@XmlJavaTypeAdapter(DateOnlyMapper.class)
	public void setMmmDdYyyy(DateOnly mmmDdYyyy) {
		preset(mmmDdYyyyPropertyName, mmmDdYyyy);
		this.mmmDdYyyy = mmmDdYyyy;
	}

	/**
	 * {@link #yyyyMmDd} accessor.
	 * @return	The value.
	 **/
	public DateOnly getYyyyMmDd() {
		return yyyyMmDd;
	}

	/**
	 * {@link #yyyyMmDd} mutator.
	 * @param yyyyMmDd	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "date")
	@XmlJavaTypeAdapter(DateOnlyMapper.class)
	public void setYyyyMmDd(DateOnly yyyyMmDd) {
		preset(yyyyMmDdPropertyName, yyyyMmDd);
		this.yyyyMmDd = yyyyMmDd;
	}

	/**
	 * {@link #ddMmYyyyHhMi} accessor.
	 * @return	The value.
	 **/
	public DateTime getDdMmYyyyHhMi() {
		return ddMmYyyyHhMi;
	}

	/**
	 * {@link #ddMmYyyyHhMi} mutator.
	 * @param ddMmYyyyHhMi	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(DateTimeMapper.class)
	public void setDdMmYyyyHhMi(DateTime ddMmYyyyHhMi) {
		preset(ddMmYyyyHhMiPropertyName, ddMmYyyyHhMi);
		this.ddMmYyyyHhMi = ddMmYyyyHhMi;
	}

	/**
	 * {@link #ddMmYyyyHh24Mi} accessor.
	 * @return	The value.
	 **/
	public DateTime getDdMmYyyyHh24Mi() {
		return ddMmYyyyHh24Mi;
	}

	/**
	 * {@link #ddMmYyyyHh24Mi} mutator.
	 * @param ddMmYyyyHh24Mi	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(DateTimeMapper.class)
	public void setDdMmYyyyHh24Mi(DateTime ddMmYyyyHh24Mi) {
		preset(ddMmYyyyHh24MiPropertyName, ddMmYyyyHh24Mi);
		this.ddMmYyyyHh24Mi = ddMmYyyyHh24Mi;
	}

	/**
	 * {@link #ddMmmYyyyHhMi} accessor.
	 * @return	The value.
	 **/
	public DateTime getDdMmmYyyyHhMi() {
		return ddMmmYyyyHhMi;
	}

	/**
	 * {@link #ddMmmYyyyHhMi} mutator.
	 * @param ddMmmYyyyHhMi	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(DateTimeMapper.class)
	public void setDdMmmYyyyHhMi(DateTime ddMmmYyyyHhMi) {
		preset(ddMmmYyyyHhMiPropertyName, ddMmmYyyyHhMi);
		this.ddMmmYyyyHhMi = ddMmmYyyyHhMi;
	}

	/**
	 * {@link #ddMmmYyyyHh24Mi} accessor.
	 * @return	The value.
	 **/
	public DateTime getDdMmmYyyyHh24Mi() {
		return ddMmmYyyyHh24Mi;
	}

	/**
	 * {@link #ddMmmYyyyHh24Mi} mutator.
	 * @param ddMmmYyyyHh24Mi	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(DateTimeMapper.class)
	public void setDdMmmYyyyHh24Mi(DateTime ddMmmYyyyHh24Mi) {
		preset(ddMmmYyyyHh24MiPropertyName, ddMmmYyyyHh24Mi);
		this.ddMmmYyyyHh24Mi = ddMmmYyyyHh24Mi;
	}

	/**
	 * {@link #mmDdYyyyHhMi} accessor.
	 * @return	The value.
	 **/
	public DateTime getMmDdYyyyHhMi() {
		return mmDdYyyyHhMi;
	}

	/**
	 * {@link #mmDdYyyyHhMi} mutator.
	 * @param mmDdYyyyHhMi	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(DateTimeMapper.class)
	public void setMmDdYyyyHhMi(DateTime mmDdYyyyHhMi) {
		preset(mmDdYyyyHhMiPropertyName, mmDdYyyyHhMi);
		this.mmDdYyyyHhMi = mmDdYyyyHhMi;
	}

	/**
	 * {@link #mmDdYyyyHh24Mi} accessor.
	 * @return	The value.
	 **/
	public DateTime getMmDdYyyyHh24Mi() {
		return mmDdYyyyHh24Mi;
	}

	/**
	 * {@link #mmDdYyyyHh24Mi} mutator.
	 * @param mmDdYyyyHh24Mi	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(DateTimeMapper.class)
	public void setMmDdYyyyHh24Mi(DateTime mmDdYyyyHh24Mi) {
		preset(mmDdYyyyHh24MiPropertyName, mmDdYyyyHh24Mi);
		this.mmDdYyyyHh24Mi = mmDdYyyyHh24Mi;
	}

	/**
	 * {@link #mmmDdYyyyHhMi} accessor.
	 * @return	The value.
	 **/
	public DateTime getMmmDdYyyyHhMi() {
		return mmmDdYyyyHhMi;
	}

	/**
	 * {@link #mmmDdYyyyHhMi} mutator.
	 * @param mmmDdYyyyHhMi	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(DateTimeMapper.class)
	public void setMmmDdYyyyHhMi(DateTime mmmDdYyyyHhMi) {
		preset(mmmDdYyyyHhMiPropertyName, mmmDdYyyyHhMi);
		this.mmmDdYyyyHhMi = mmmDdYyyyHhMi;
	}

	/**
	 * {@link #mmmDdYyyyHh24Mi} accessor.
	 * @return	The value.
	 **/
	public DateTime getMmmDdYyyyHh24Mi() {
		return mmmDdYyyyHh24Mi;
	}

	/**
	 * {@link #mmmDdYyyyHh24Mi} mutator.
	 * @param mmmDdYyyyHh24Mi	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(DateTimeMapper.class)
	public void setMmmDdYyyyHh24Mi(DateTime mmmDdYyyyHh24Mi) {
		preset(mmmDdYyyyHh24MiPropertyName, mmmDdYyyyHh24Mi);
		this.mmmDdYyyyHh24Mi = mmmDdYyyyHh24Mi;
	}

	/**
	 * {@link #yyyyMmDdHhMi} accessor.
	 * @return	The value.
	 **/
	public DateTime getYyyyMmDdHhMi() {
		return yyyyMmDdHhMi;
	}

	/**
	 * {@link #yyyyMmDdHhMi} mutator.
	 * @param yyyyMmDdHhMi	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(DateTimeMapper.class)
	public void setYyyyMmDdHhMi(DateTime yyyyMmDdHhMi) {
		preset(yyyyMmDdHhMiPropertyName, yyyyMmDdHhMi);
		this.yyyyMmDdHhMi = yyyyMmDdHhMi;
	}

	/**
	 * {@link #yyyyMmDdHh24Mi} accessor.
	 * @return	The value.
	 **/
	public DateTime getYyyyMmDdHh24Mi() {
		return yyyyMmDdHh24Mi;
	}

	/**
	 * {@link #yyyyMmDdHh24Mi} mutator.
	 * @param yyyyMmDdHh24Mi	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(DateTimeMapper.class)
	public void setYyyyMmDdHh24Mi(DateTime yyyyMmDdHh24Mi) {
		preset(yyyyMmDdHh24MiPropertyName, yyyyMmDdHh24Mi);
		this.yyyyMmDdHh24Mi = yyyyMmDdHh24Mi;
	}

	/**
	 * {@link #ddMmYyyyHhMiSs} accessor.
	 * @return	The value.
	 **/
	public Timestamp getDdMmYyyyHhMiSs() {
		return ddMmYyyyHhMiSs;
	}

	/**
	 * {@link #ddMmYyyyHhMiSs} mutator.
	 * @param ddMmYyyyHhMiSs	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(TimestampMapper.class)
	public void setDdMmYyyyHhMiSs(Timestamp ddMmYyyyHhMiSs) {
		preset(ddMmYyyyHhMiSsPropertyName, ddMmYyyyHhMiSs);
		this.ddMmYyyyHhMiSs = ddMmYyyyHhMiSs;
	}

	/**
	 * {@link #ddMmYyyyHh24MiSs} accessor.
	 * @return	The value.
	 **/
	public Timestamp getDdMmYyyyHh24MiSs() {
		return ddMmYyyyHh24MiSs;
	}

	/**
	 * {@link #ddMmYyyyHh24MiSs} mutator.
	 * @param ddMmYyyyHh24MiSs	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(TimestampMapper.class)
	public void setDdMmYyyyHh24MiSs(Timestamp ddMmYyyyHh24MiSs) {
		preset(ddMmYyyyHh24MiSsPropertyName, ddMmYyyyHh24MiSs);
		this.ddMmYyyyHh24MiSs = ddMmYyyyHh24MiSs;
	}

	/**
	 * {@link #ddMmmYyyyHhMiSs} accessor.
	 * @return	The value.
	 **/
	public Timestamp getDdMmmYyyyHhMiSs() {
		return ddMmmYyyyHhMiSs;
	}

	/**
	 * {@link #ddMmmYyyyHhMiSs} mutator.
	 * @param ddMmmYyyyHhMiSs	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(TimestampMapper.class)
	public void setDdMmmYyyyHhMiSs(Timestamp ddMmmYyyyHhMiSs) {
		preset(ddMmmYyyyHhMiSsPropertyName, ddMmmYyyyHhMiSs);
		this.ddMmmYyyyHhMiSs = ddMmmYyyyHhMiSs;
	}

	/**
	 * {@link #ddMmmYyyyHh24MiSs} accessor.
	 * @return	The value.
	 **/
	public Timestamp getDdMmmYyyyHh24MiSs() {
		return ddMmmYyyyHh24MiSs;
	}

	/**
	 * {@link #ddMmmYyyyHh24MiSs} mutator.
	 * @param ddMmmYyyyHh24MiSs	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(TimestampMapper.class)
	public void setDdMmmYyyyHh24MiSs(Timestamp ddMmmYyyyHh24MiSs) {
		preset(ddMmmYyyyHh24MiSsPropertyName, ddMmmYyyyHh24MiSs);
		this.ddMmmYyyyHh24MiSs = ddMmmYyyyHh24MiSs;
	}

	/**
	 * {@link #mmDdYyyyHhMiSs} accessor.
	 * @return	The value.
	 **/
	public Timestamp getMmDdYyyyHhMiSs() {
		return mmDdYyyyHhMiSs;
	}

	/**
	 * {@link #mmDdYyyyHhMiSs} mutator.
	 * @param mmDdYyyyHhMiSs	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(TimestampMapper.class)
	public void setMmDdYyyyHhMiSs(Timestamp mmDdYyyyHhMiSs) {
		preset(mmDdYyyyHhMiSsPropertyName, mmDdYyyyHhMiSs);
		this.mmDdYyyyHhMiSs = mmDdYyyyHhMiSs;
	}

	/**
	 * {@link #mmDdYyyyHh24MiSs} accessor.
	 * @return	The value.
	 **/
	public Timestamp getMmDdYyyyHh24MiSs() {
		return mmDdYyyyHh24MiSs;
	}

	/**
	 * {@link #mmDdYyyyHh24MiSs} mutator.
	 * @param mmDdYyyyHh24MiSs	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(TimestampMapper.class)
	public void setMmDdYyyyHh24MiSs(Timestamp mmDdYyyyHh24MiSs) {
		preset(mmDdYyyyHh24MiSsPropertyName, mmDdYyyyHh24MiSs);
		this.mmDdYyyyHh24MiSs = mmDdYyyyHh24MiSs;
	}

	/**
	 * {@link #mmmDdYyyyHhMiSs} accessor.
	 * @return	The value.
	 **/
	public Timestamp getMmmDdYyyyHhMiSs() {
		return mmmDdYyyyHhMiSs;
	}

	/**
	 * {@link #mmmDdYyyyHhMiSs} mutator.
	 * @param mmmDdYyyyHhMiSs	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(TimestampMapper.class)
	public void setMmmDdYyyyHhMiSs(Timestamp mmmDdYyyyHhMiSs) {
		preset(mmmDdYyyyHhMiSsPropertyName, mmmDdYyyyHhMiSs);
		this.mmmDdYyyyHhMiSs = mmmDdYyyyHhMiSs;
	}

	/**
	 * {@link #mmmDdYyyyHh24MiSs} accessor.
	 * @return	The value.
	 **/
	public Timestamp getMmmDdYyyyHh24MiSs() {
		return mmmDdYyyyHh24MiSs;
	}

	/**
	 * {@link #mmmDdYyyyHh24MiSs} mutator.
	 * @param mmmDdYyyyHh24MiSs	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(TimestampMapper.class)
	public void setMmmDdYyyyHh24MiSs(Timestamp mmmDdYyyyHh24MiSs) {
		preset(mmmDdYyyyHh24MiSsPropertyName, mmmDdYyyyHh24MiSs);
		this.mmmDdYyyyHh24MiSs = mmmDdYyyyHh24MiSs;
	}

	/**
	 * {@link #yyyyMmDdHhMiSs} accessor.
	 * @return	The value.
	 **/
	public Timestamp getYyyyMmDdHhMiSs() {
		return yyyyMmDdHhMiSs;
	}

	/**
	 * {@link #yyyyMmDdHhMiSs} mutator.
	 * @param yyyyMmDdHhMiSs	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(TimestampMapper.class)
	public void setYyyyMmDdHhMiSs(Timestamp yyyyMmDdHhMiSs) {
		preset(yyyyMmDdHhMiSsPropertyName, yyyyMmDdHhMiSs);
		this.yyyyMmDdHhMiSs = yyyyMmDdHhMiSs;
	}

	/**
	 * {@link #yyyyMmDdHh24MiSs} accessor.
	 * @return	The value.
	 **/
	public Timestamp getYyyyMmDdHh24MiSs() {
		return yyyyMmDdHh24MiSs;
	}

	/**
	 * {@link #yyyyMmDdHh24MiSs} mutator.
	 * @param yyyyMmDdHh24MiSs	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(TimestampMapper.class)
	public void setYyyyMmDdHh24MiSs(Timestamp yyyyMmDdHh24MiSs) {
		preset(yyyyMmDdHh24MiSsPropertyName, yyyyMmDdHh24MiSs);
		this.yyyyMmDdHh24MiSs = yyyyMmDdHh24MiSs;
	}

	/**
	 * {@link #ddMmYyyyDateTime} accessor.
	 * @return	The value.
	 **/
	public DateTime getDdMmYyyyDateTime() {
		return ddMmYyyyDateTime;
	}

	/**
	 * {@link #ddMmYyyyDateTime} mutator.
	 * @param ddMmYyyyDateTime	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(DateTimeMapper.class)
	public void setDdMmYyyyDateTime(DateTime ddMmYyyyDateTime) {
		preset(ddMmYyyyDateTimePropertyName, ddMmYyyyDateTime);
		this.ddMmYyyyDateTime = ddMmYyyyDateTime;
	}

	/**
	 * {@link #ddMmmYyyyDateTime} accessor.
	 * @return	The value.
	 **/
	public DateTime getDdMmmYyyyDateTime() {
		return ddMmmYyyyDateTime;
	}

	/**
	 * {@link #ddMmmYyyyDateTime} mutator.
	 * @param ddMmmYyyyDateTime	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(DateTimeMapper.class)
	public void setDdMmmYyyyDateTime(DateTime ddMmmYyyyDateTime) {
		preset(ddMmmYyyyDateTimePropertyName, ddMmmYyyyDateTime);
		this.ddMmmYyyyDateTime = ddMmmYyyyDateTime;
	}

	/**
	 * {@link #mmDdYyyyDateTime} accessor.
	 * @return	The value.
	 **/
	public DateTime getMmDdYyyyDateTime() {
		return mmDdYyyyDateTime;
	}

	/**
	 * {@link #mmDdYyyyDateTime} mutator.
	 * @param mmDdYyyyDateTime	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(DateTimeMapper.class)
	public void setMmDdYyyyDateTime(DateTime mmDdYyyyDateTime) {
		preset(mmDdYyyyDateTimePropertyName, mmDdYyyyDateTime);
		this.mmDdYyyyDateTime = mmDdYyyyDateTime;
	}

	/**
	 * {@link #mmmDdYyyyDateTime} accessor.
	 * @return	The value.
	 **/
	public DateTime getMmmDdYyyyDateTime() {
		return mmmDdYyyyDateTime;
	}

	/**
	 * {@link #mmmDdYyyyDateTime} mutator.
	 * @param mmmDdYyyyDateTime	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(DateTimeMapper.class)
	public void setMmmDdYyyyDateTime(DateTime mmmDdYyyyDateTime) {
		preset(mmmDdYyyyDateTimePropertyName, mmmDdYyyyDateTime);
		this.mmmDdYyyyDateTime = mmmDdYyyyDateTime;
	}

	/**
	 * {@link #yyyyMmDdDateTime} accessor.
	 * @return	The value.
	 **/
	public DateTime getYyyyMmDdDateTime() {
		return yyyyMmDdDateTime;
	}

	/**
	 * {@link #yyyyMmDdDateTime} mutator.
	 * @param yyyyMmDdDateTime	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(DateTimeMapper.class)
	public void setYyyyMmDdDateTime(DateTime yyyyMmDdDateTime) {
		preset(yyyyMmDdDateTimePropertyName, yyyyMmDdDateTime);
		this.yyyyMmDdDateTime = yyyyMmDdDateTime;
	}

	/**
	 * {@link #ddMmYyyyTimestamp} accessor.
	 * @return	The value.
	 **/
	public Timestamp getDdMmYyyyTimestamp() {
		return ddMmYyyyTimestamp;
	}

	/**
	 * {@link #ddMmYyyyTimestamp} mutator.
	 * @param ddMmYyyyTimestamp	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(TimestampMapper.class)
	public void setDdMmYyyyTimestamp(Timestamp ddMmYyyyTimestamp) {
		preset(ddMmYyyyTimestampPropertyName, ddMmYyyyTimestamp);
		this.ddMmYyyyTimestamp = ddMmYyyyTimestamp;
	}

	/**
	 * {@link #ddMmmYyyyTimestamp} accessor.
	 * @return	The value.
	 **/
	public Timestamp getDdMmmYyyyTimestamp() {
		return ddMmmYyyyTimestamp;
	}

	/**
	 * {@link #ddMmmYyyyTimestamp} mutator.
	 * @param ddMmmYyyyTimestamp	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(TimestampMapper.class)
	public void setDdMmmYyyyTimestamp(Timestamp ddMmmYyyyTimestamp) {
		preset(ddMmmYyyyTimestampPropertyName, ddMmmYyyyTimestamp);
		this.ddMmmYyyyTimestamp = ddMmmYyyyTimestamp;
	}

	/**
	 * {@link #mmDdYyyyTimestamp} accessor.
	 * @return	The value.
	 **/
	public Timestamp getMmDdYyyyTimestamp() {
		return mmDdYyyyTimestamp;
	}

	/**
	 * {@link #mmDdYyyyTimestamp} mutator.
	 * @param mmDdYyyyTimestamp	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(TimestampMapper.class)
	public void setMmDdYyyyTimestamp(Timestamp mmDdYyyyTimestamp) {
		preset(mmDdYyyyTimestampPropertyName, mmDdYyyyTimestamp);
		this.mmDdYyyyTimestamp = mmDdYyyyTimestamp;
	}

	/**
	 * {@link #mmmDdYyyyTimestamp} accessor.
	 * @return	The value.
	 **/
	public Timestamp getMmmDdYyyyTimestamp() {
		return mmmDdYyyyTimestamp;
	}

	/**
	 * {@link #mmmDdYyyyTimestamp} mutator.
	 * @param mmmDdYyyyTimestamp	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(TimestampMapper.class)
	public void setMmmDdYyyyTimestamp(Timestamp mmmDdYyyyTimestamp) {
		preset(mmmDdYyyyTimestampPropertyName, mmmDdYyyyTimestamp);
		this.mmmDdYyyyTimestamp = mmmDdYyyyTimestamp;
	}

	/**
	 * {@link #yyyyMmDdTimestamp} accessor.
	 * @return	The value.
	 **/
	public Timestamp getYyyyMmDdTimestamp() {
		return yyyyMmDdTimestamp;
	}

	/**
	 * {@link #yyyyMmDdTimestamp} mutator.
	 * @param yyyyMmDdTimestamp	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(TimestampMapper.class)
	public void setYyyyMmDdTimestamp(Timestamp yyyyMmDdTimestamp) {
		preset(yyyyMmDdTimestampPropertyName, yyyyMmDdTimestamp);
		this.yyyyMmDdTimestamp = yyyyMmDdTimestamp;
	}

	/**
	 * {@link #hhMi} accessor.
	 * @return	The value.
	 **/
	public TimeOnly getHhMi() {
		return hhMi;
	}

	/**
	 * {@link #hhMi} mutator.
	 * @param hhMi	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "time")
	@XmlJavaTypeAdapter(TimeOnlyMapper.class)
	public void setHhMi(TimeOnly hhMi) {
		preset(hhMiPropertyName, hhMi);
		this.hhMi = hhMi;
	}

	/**
	 * {@link #hh24Mi} accessor.
	 * @return	The value.
	 **/
	public TimeOnly getHh24Mi() {
		return hh24Mi;
	}

	/**
	 * {@link #hh24Mi} mutator.
	 * @param hh24Mi	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "time")
	@XmlJavaTypeAdapter(TimeOnlyMapper.class)
	public void setHh24Mi(TimeOnly hh24Mi) {
		preset(hh24MiPropertyName, hh24Mi);
		this.hh24Mi = hh24Mi;
	}

	/**
	 * {@link #hhMiSs} accessor.
	 * @return	The value.
	 **/
	public TimeOnly getHhMiSs() {
		return hhMiSs;
	}

	/**
	 * {@link #hhMiSs} mutator.
	 * @param hhMiSs	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "time")
	@XmlJavaTypeAdapter(TimeOnlyMapper.class)
	public void setHhMiSs(TimeOnly hhMiSs) {
		preset(hhMiSsPropertyName, hhMiSs);
		this.hhMiSs = hhMiSs;
	}

	/**
	 * {@link #hh24MiSs} accessor.
	 * @return	The value.
	 **/
	public TimeOnly getHh24MiSs() {
		return hh24MiSs;
	}

	/**
	 * {@link #hh24MiSs} mutator.
	 * @param hh24MiSs	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "time")
	@XmlJavaTypeAdapter(TimeOnlyMapper.class)
	public void setHh24MiSs(TimeOnly hh24MiSs) {
		preset(hh24MiSsPropertyName, hh24MiSs);
		this.hh24MiSs = hh24MiSs;
	}

	/**
	 * {@link #decimal2DollarsAndCents} accessor.
	 * @return	The value.
	 **/
	public Decimal2 getDecimal2DollarsAndCents() {
		return decimal2DollarsAndCents;
	}

	/**
	 * {@link #decimal2DollarsAndCents} mutator.
	 * @param decimal2DollarsAndCents	The new value.
	 **/
	@XmlElement
	@XmlJavaTypeAdapter(Decimal2Mapper.class)
	public void setDecimal2DollarsAndCents(Decimal2 decimal2DollarsAndCents) {
		preset(decimal2DollarsAndCentsPropertyName, decimal2DollarsAndCents);
		this.decimal2DollarsAndCents = decimal2DollarsAndCents;
	}

	/**
	 * {@link #decimal2DollarsAndCentsAbsolute} accessor.
	 * @return	The value.
	 **/
	public Decimal2 getDecimal2DollarsAndCentsAbsolute() {
		return decimal2DollarsAndCentsAbsolute;
	}

	/**
	 * {@link #decimal2DollarsAndCentsAbsolute} mutator.
	 * @param decimal2DollarsAndCentsAbsolute	The new value.
	 **/
	@XmlElement
	@XmlJavaTypeAdapter(Decimal2Mapper.class)
	public void setDecimal2DollarsAndCentsAbsolute(Decimal2 decimal2DollarsAndCentsAbsolute) {
		preset(decimal2DollarsAndCentsAbsolutePropertyName, decimal2DollarsAndCentsAbsolute);
		this.decimal2DollarsAndCentsAbsolute = decimal2DollarsAndCentsAbsolute;
	}

	/**
	 * {@link #decimal2Integer} accessor.
	 * @return	The value.
	 **/
	public Decimal2 getDecimal2Integer() {
		return decimal2Integer;
	}

	/**
	 * {@link #decimal2Integer} mutator.
	 * @param decimal2Integer	The new value.
	 **/
	@XmlElement
	@XmlJavaTypeAdapter(Decimal2Mapper.class)
	public void setDecimal2Integer(Decimal2 decimal2Integer) {
		preset(decimal2IntegerPropertyName, decimal2Integer);
		this.decimal2Integer = decimal2Integer;
	}

	/**
	 * {@link #decimal2IntegerPercentage} accessor.
	 * @return	The value.
	 **/
	public Decimal2 getDecimal2IntegerPercentage() {
		return decimal2IntegerPercentage;
	}

	/**
	 * {@link #decimal2IntegerPercentage} mutator.
	 * @param decimal2IntegerPercentage	The new value.
	 **/
	@XmlElement
	@XmlJavaTypeAdapter(Decimal2Mapper.class)
	public void setDecimal2IntegerPercentage(Decimal2 decimal2IntegerPercentage) {
		preset(decimal2IntegerPercentagePropertyName, decimal2IntegerPercentage);
		this.decimal2IntegerPercentage = decimal2IntegerPercentage;
	}

	/**
	 * {@link #decimal2OneDecimalPlace} accessor.
	 * @return	The value.
	 **/
	public Decimal2 getDecimal2OneDecimalPlace() {
		return decimal2OneDecimalPlace;
	}

	/**
	 * {@link #decimal2OneDecimalPlace} mutator.
	 * @param decimal2OneDecimalPlace	The new value.
	 **/
	@XmlElement
	@XmlJavaTypeAdapter(Decimal2Mapper.class)
	public void setDecimal2OneDecimalPlace(Decimal2 decimal2OneDecimalPlace) {
		preset(decimal2OneDecimalPlacePropertyName, decimal2OneDecimalPlace);
		this.decimal2OneDecimalPlace = decimal2OneDecimalPlace;
	}

	/**
	 * {@link #decimal5Integer} accessor.
	 * @return	The value.
	 **/
	public Decimal5 getDecimal5Integer() {
		return decimal5Integer;
	}

	/**
	 * {@link #decimal5Integer} mutator.
	 * @param decimal5Integer	The new value.
	 **/
	@XmlElement
	@XmlJavaTypeAdapter(Decimal5Mapper.class)
	public void setDecimal5Integer(Decimal5 decimal5Integer) {
		preset(decimal5IntegerPropertyName, decimal5Integer);
		this.decimal5Integer = decimal5Integer;
	}

	/**
	 * {@link #decimal5IntegerPercentage} accessor.
	 * @return	The value.
	 **/
	public Decimal5 getDecimal5IntegerPercentage() {
		return decimal5IntegerPercentage;
	}

	/**
	 * {@link #decimal5IntegerPercentage} mutator.
	 * @param decimal5IntegerPercentage	The new value.
	 **/
	@XmlElement
	@XmlJavaTypeAdapter(Decimal5Mapper.class)
	public void setDecimal5IntegerPercentage(Decimal5 decimal5IntegerPercentage) {
		preset(decimal5IntegerPercentagePropertyName, decimal5IntegerPercentage);
		this.decimal5IntegerPercentage = decimal5IntegerPercentage;
	}

	/**
	 * {@link #decimal5DollarsAndCents} accessor.
	 * @return	The value.
	 **/
	public Decimal5 getDecimal5DollarsAndCents() {
		return decimal5DollarsAndCents;
	}

	/**
	 * {@link #decimal5DollarsAndCents} mutator.
	 * @param decimal5DollarsAndCents	The new value.
	 **/
	@XmlElement
	@XmlJavaTypeAdapter(Decimal5Mapper.class)
	public void setDecimal5DollarsAndCents(Decimal5 decimal5DollarsAndCents) {
		preset(decimal5DollarsAndCentsPropertyName, decimal5DollarsAndCents);
		this.decimal5DollarsAndCents = decimal5DollarsAndCents;
	}

	/**
	 * {@link #decimal5TimeDuration} accessor.
	 * @return	The value.
	 **/
	public Decimal5 getDecimal5TimeDuration() {
		return decimal5TimeDuration;
	}

	/**
	 * {@link #decimal5TimeDuration} mutator.
	 * @param decimal5TimeDuration	The new value.
	 **/
	@XmlElement
	@XmlJavaTypeAdapter(Decimal5Mapper.class)
	public void setDecimal5TimeDuration(Decimal5 decimal5TimeDuration) {
		preset(decimal5TimeDurationPropertyName, decimal5TimeDuration);
		this.decimal5TimeDuration = decimal5TimeDuration;
	}

	/**
	 * {@link #decimal5OneDecimalPlace} accessor.
	 * @return	The value.
	 **/
	public Decimal5 getDecimal5OneDecimalPlace() {
		return decimal5OneDecimalPlace;
	}

	/**
	 * {@link #decimal5OneDecimalPlace} mutator.
	 * @param decimal5OneDecimalPlace	The new value.
	 **/
	@XmlElement
	@XmlJavaTypeAdapter(Decimal5Mapper.class)
	public void setDecimal5OneDecimalPlace(Decimal5 decimal5OneDecimalPlace) {
		preset(decimal5OneDecimalPlacePropertyName, decimal5OneDecimalPlace);
		this.decimal5OneDecimalPlace = decimal5OneDecimalPlace;
	}

	/**
	 * {@link #decimal5TwoDecimalPlaces} accessor.
	 * @return	The value.
	 **/
	public Decimal5 getDecimal5TwoDecimalPlaces() {
		return decimal5TwoDecimalPlaces;
	}

	/**
	 * {@link #decimal5TwoDecimalPlaces} mutator.
	 * @param decimal5TwoDecimalPlaces	The new value.
	 **/
	@XmlElement
	@XmlJavaTypeAdapter(Decimal5Mapper.class)
	public void setDecimal5TwoDecimalPlaces(Decimal5 decimal5TwoDecimalPlaces) {
		preset(decimal5TwoDecimalPlacesPropertyName, decimal5TwoDecimalPlaces);
		this.decimal5TwoDecimalPlaces = decimal5TwoDecimalPlaces;
	}

	/**
	 * {@link #decimal5TwoDecimalPlacesPercentage} accessor.
	 * @return	The value.
	 **/
	public Decimal5 getDecimal5TwoDecimalPlacesPercentage() {
		return decimal5TwoDecimalPlacesPercentage;
	}

	/**
	 * {@link #decimal5TwoDecimalPlacesPercentage} mutator.
	 * @param decimal5TwoDecimalPlacesPercentage	The new value.
	 **/
	@XmlElement
	@XmlJavaTypeAdapter(Decimal5Mapper.class)
	public void setDecimal5TwoDecimalPlacesPercentage(Decimal5 decimal5TwoDecimalPlacesPercentage) {
		preset(decimal5TwoDecimalPlacesPercentagePropertyName, decimal5TwoDecimalPlacesPercentage);
		this.decimal5TwoDecimalPlacesPercentage = decimal5TwoDecimalPlacesPercentage;
	}

	/**
	 * {@link #decimal10Integer} accessor.
	 * @return	The value.
	 **/
	public Decimal10 getDecimal10Integer() {
		return decimal10Integer;
	}

	/**
	 * {@link #decimal10Integer} mutator.
	 * @param decimal10Integer	The new value.
	 **/
	@XmlElement
	@XmlJavaTypeAdapter(Decimal10Mapper.class)
	public void setDecimal10Integer(Decimal10 decimal10Integer) {
		preset(decimal10IntegerPropertyName, decimal10Integer);
		this.decimal10Integer = decimal10Integer;
	}

	/**
	 * {@link #decimal10IntegerPercentage} accessor.
	 * @return	The value.
	 **/
	public Decimal10 getDecimal10IntegerPercentage() {
		return decimal10IntegerPercentage;
	}

	/**
	 * {@link #decimal10IntegerPercentage} mutator.
	 * @param decimal10IntegerPercentage	The new value.
	 **/
	@XmlElement
	@XmlJavaTypeAdapter(Decimal10Mapper.class)
	public void setDecimal10IntegerPercentage(Decimal10 decimal10IntegerPercentage) {
		preset(decimal10IntegerPercentagePropertyName, decimal10IntegerPercentage);
		this.decimal10IntegerPercentage = decimal10IntegerPercentage;
	}

	/**
	 * {@link #decimal10DollarsAndCents} accessor.
	 * @return	The value.
	 **/
	public Decimal10 getDecimal10DollarsAndCents() {
		return decimal10DollarsAndCents;
	}

	/**
	 * {@link #decimal10DollarsAndCents} mutator.
	 * @param decimal10DollarsAndCents	The new value.
	 **/
	@XmlElement
	@XmlJavaTypeAdapter(Decimal10Mapper.class)
	public void setDecimal10DollarsAndCents(Decimal10 decimal10DollarsAndCents) {
		preset(decimal10DollarsAndCentsPropertyName, decimal10DollarsAndCents);
		this.decimal10DollarsAndCents = decimal10DollarsAndCents;
	}

	/**
	 * {@link #decimal10OneDecimalPlace} accessor.
	 * @return	The value.
	 **/
	public Decimal10 getDecimal10OneDecimalPlace() {
		return decimal10OneDecimalPlace;
	}

	/**
	 * {@link #decimal10OneDecimalPlace} mutator.
	 * @param decimal10OneDecimalPlace	The new value.
	 **/
	@XmlElement
	@XmlJavaTypeAdapter(Decimal10Mapper.class)
	public void setDecimal10OneDecimalPlace(Decimal10 decimal10OneDecimalPlace) {
		preset(decimal10OneDecimalPlacePropertyName, decimal10OneDecimalPlace);
		this.decimal10OneDecimalPlace = decimal10OneDecimalPlace;
	}

	/**
	 * {@link #decimal10TwoDecimalPlaces} accessor.
	 * @return	The value.
	 **/
	public Decimal10 getDecimal10TwoDecimalPlaces() {
		return decimal10TwoDecimalPlaces;
	}

	/**
	 * {@link #decimal10TwoDecimalPlaces} mutator.
	 * @param decimal10TwoDecimalPlaces	The new value.
	 **/
	@XmlElement
	@XmlJavaTypeAdapter(Decimal10Mapper.class)
	public void setDecimal10TwoDecimalPlaces(Decimal10 decimal10TwoDecimalPlaces) {
		preset(decimal10TwoDecimalPlacesPropertyName, decimal10TwoDecimalPlaces);
		this.decimal10TwoDecimalPlaces = decimal10TwoDecimalPlaces;
	}

	/**
	 * {@link #decimal10TwoDecimalPlacesPercentage} accessor.
	 * @return	The value.
	 **/
	public Decimal10 getDecimal10TwoDecimalPlacesPercentage() {
		return decimal10TwoDecimalPlacesPercentage;
	}

	/**
	 * {@link #decimal10TwoDecimalPlacesPercentage} mutator.
	 * @param decimal10TwoDecimalPlacesPercentage	The new value.
	 **/
	@XmlElement
	@XmlJavaTypeAdapter(Decimal10Mapper.class)
	public void setDecimal10TwoDecimalPlacesPercentage(Decimal10 decimal10TwoDecimalPlacesPercentage) {
		preset(decimal10TwoDecimalPlacesPercentagePropertyName, decimal10TwoDecimalPlacesPercentage);
		this.decimal10TwoDecimalPlacesPercentage = decimal10TwoDecimalPlacesPercentage;
	}

	/**
	 * {@link #simplePercentage} accessor.
	 * @return	The value.
	 **/
	public Integer getSimplePercentage() {
		return simplePercentage;
	}

	/**
	 * {@link #simplePercentage} mutator.
	 * @param simplePercentage	The new value.
	 **/
	@XmlElement
	public void setSimplePercentage(Integer simplePercentage) {
		preset(simplePercentagePropertyName, simplePercentage);
		this.simplePercentage = simplePercentage;
	}

	/**
	 * {@link #integerSeparator} accessor.
	 * @return	The value.
	 **/
	public Integer getIntegerSeparator() {
		return integerSeparator;
	}

	/**
	 * {@link #integerSeparator} mutator.
	 * @param integerSeparator	The new value.
	 **/
	@XmlElement
	public void setIntegerSeparator(Integer integerSeparator) {
		preset(integerSeparatorPropertyName, integerSeparator);
		this.integerSeparator = integerSeparator;
	}

	/**
	 * {@link #longIntegerSeparator} accessor.
	 * @return	The value.
	 **/
	public Long getLongIntegerSeparator() {
		return longIntegerSeparator;
	}

	/**
	 * {@link #longIntegerSeparator} mutator.
	 * @param longIntegerSeparator	The new value.
	 **/
	@XmlElement
	public void setLongIntegerSeparator(Long longIntegerSeparator) {
		preset(longIntegerSeparatorPropertyName, longIntegerSeparator);
		this.longIntegerSeparator = longIntegerSeparator;
	}

	/**
	 * {@link #plainDecimal2} accessor.
	 * @return	The value.
	 **/
	public Decimal2 getPlainDecimal2() {
		return plainDecimal2;
	}

	/**
	 * {@link #plainDecimal2} mutator.
	 * @param plainDecimal2	The new value.
	 **/
	@XmlElement
	@XmlJavaTypeAdapter(Decimal2Mapper.class)
	public void setPlainDecimal2(Decimal2 plainDecimal2) {
		preset(plainDecimal2PropertyName, plainDecimal2);
		this.plainDecimal2 = plainDecimal2;
	}

	/**
	 * {@link #dateRows} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<ConvertersFixtureRow> getDateRows() {
		return dateRows;
	}

	/**
	 * {@link #dateRows} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public ConvertersFixtureRow getDateRowsElementById(String bizId) {
		return getElementById(dateRows, bizId);
	}

	/**
	 * {@link #dateRows} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setDateRowsElementById(String bizId, ConvertersFixtureRow element) {
		setElementById(dateRows, element);
	}

	/**
	 * {@link #dateRows} add.
	 * @param element	The element to add.
	 **/
	public boolean addDateRowsElement(ConvertersFixtureRow element) {
		return dateRows.add(element);
	}

	/**
	 * {@link #dateRows} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addDateRowsElement(int index, ConvertersFixtureRow element) {
		dateRows.add(index, element);
	}

	/**
	 * {@link #dateRows} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeDateRowsElement(ConvertersFixtureRow element) {
		return dateRows.remove(element);
	}

	/**
	 * {@link #dateRows} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public ConvertersFixtureRow removeDateRowsElement(int index) {
		return dateRows.remove(index);
	}

	/**
	 * {@link #dateTimeRows} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<ConvertersFixtureRow> getDateTimeRows() {
		return dateTimeRows;
	}

	/**
	 * {@link #dateTimeRows} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public ConvertersFixtureRow getDateTimeRowsElementById(String bizId) {
		return getElementById(dateTimeRows, bizId);
	}

	/**
	 * {@link #dateTimeRows} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setDateTimeRowsElementById(String bizId, ConvertersFixtureRow element) {
		setElementById(dateTimeRows, element);
	}

	/**
	 * {@link #dateTimeRows} add.
	 * @param element	The element to add.
	 **/
	public boolean addDateTimeRowsElement(ConvertersFixtureRow element) {
		return dateTimeRows.add(element);
	}

	/**
	 * {@link #dateTimeRows} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addDateTimeRowsElement(int index, ConvertersFixtureRow element) {
		dateTimeRows.add(index, element);
	}

	/**
	 * {@link #dateTimeRows} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeDateTimeRowsElement(ConvertersFixtureRow element) {
		return dateTimeRows.remove(element);
	}

	/**
	 * {@link #dateTimeRows} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public ConvertersFixtureRow removeDateTimeRowsElement(int index) {
		return dateTimeRows.remove(index);
	}

	/**
	 * {@link #timestampRows} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<ConvertersFixtureRow> getTimestampRows() {
		return timestampRows;
	}

	/**
	 * {@link #timestampRows} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public ConvertersFixtureRow getTimestampRowsElementById(String bizId) {
		return getElementById(timestampRows, bizId);
	}

	/**
	 * {@link #timestampRows} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setTimestampRowsElementById(String bizId, ConvertersFixtureRow element) {
		setElementById(timestampRows, element);
	}

	/**
	 * {@link #timestampRows} add.
	 * @param element	The element to add.
	 **/
	public boolean addTimestampRowsElement(ConvertersFixtureRow element) {
		return timestampRows.add(element);
	}

	/**
	 * {@link #timestampRows} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addTimestampRowsElement(int index, ConvertersFixtureRow element) {
		timestampRows.add(index, element);
	}

	/**
	 * {@link #timestampRows} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeTimestampRowsElement(ConvertersFixtureRow element) {
		return timestampRows.remove(element);
	}

	/**
	 * {@link #timestampRows} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public ConvertersFixtureRow removeTimestampRowsElement(int index) {
		return timestampRows.remove(index);
	}

	/**
	 * {@link #dateOnlyRows} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<ConvertersFixtureRow> getDateOnlyRows() {
		return dateOnlyRows;
	}

	/**
	 * {@link #dateOnlyRows} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public ConvertersFixtureRow getDateOnlyRowsElementById(String bizId) {
		return getElementById(dateOnlyRows, bizId);
	}

	/**
	 * {@link #dateOnlyRows} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setDateOnlyRowsElementById(String bizId, ConvertersFixtureRow element) {
		setElementById(dateOnlyRows, element);
	}

	/**
	 * {@link #dateOnlyRows} add.
	 * @param element	The element to add.
	 **/
	public boolean addDateOnlyRowsElement(ConvertersFixtureRow element) {
		return dateOnlyRows.add(element);
	}

	/**
	 * {@link #dateOnlyRows} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addDateOnlyRowsElement(int index, ConvertersFixtureRow element) {
		dateOnlyRows.add(index, element);
	}

	/**
	 * {@link #dateOnlyRows} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeDateOnlyRowsElement(ConvertersFixtureRow element) {
		return dateOnlyRows.remove(element);
	}

	/**
	 * {@link #dateOnlyRows} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public ConvertersFixtureRow removeDateOnlyRowsElement(int index) {
		return dateOnlyRows.remove(index);
	}

	/**
	 * {@link #timeRows} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<ConvertersFixtureRow> getTimeRows() {
		return timeRows;
	}

	/**
	 * {@link #timeRows} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public ConvertersFixtureRow getTimeRowsElementById(String bizId) {
		return getElementById(timeRows, bizId);
	}

	/**
	 * {@link #timeRows} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setTimeRowsElementById(String bizId, ConvertersFixtureRow element) {
		setElementById(timeRows, element);
	}

	/**
	 * {@link #timeRows} add.
	 * @param element	The element to add.
	 **/
	public boolean addTimeRowsElement(ConvertersFixtureRow element) {
		return timeRows.add(element);
	}

	/**
	 * {@link #timeRows} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addTimeRowsElement(int index, ConvertersFixtureRow element) {
		timeRows.add(index, element);
	}

	/**
	 * {@link #timeRows} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeTimeRowsElement(ConvertersFixtureRow element) {
		return timeRows.remove(element);
	}

	/**
	 * {@link #timeRows} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public ConvertersFixtureRow removeTimeRowsElement(int index) {
		return timeRows.remove(index);
	}

	/**
	 * {@link #numberRows} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<ConvertersFixtureRow> getNumberRows() {
		return numberRows;
	}

	/**
	 * {@link #numberRows} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public ConvertersFixtureRow getNumberRowsElementById(String bizId) {
		return getElementById(numberRows, bizId);
	}

	/**
	 * {@link #numberRows} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setNumberRowsElementById(String bizId, ConvertersFixtureRow element) {
		setElementById(numberRows, element);
	}

	/**
	 * {@link #numberRows} add.
	 * @param element	The element to add.
	 **/
	public boolean addNumberRowsElement(ConvertersFixtureRow element) {
		return numberRows.add(element);
	}

	/**
	 * {@link #numberRows} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addNumberRowsElement(int index, ConvertersFixtureRow element) {
		numberRows.add(index, element);
	}

	/**
	 * {@link #numberRows} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeNumberRowsElement(ConvertersFixtureRow element) {
		return numberRows.remove(element);
	}

	/**
	 * {@link #numberRows} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public ConvertersFixtureRow removeNumberRowsElement(int index) {
		return numberRows.remove(index);
	}
}
