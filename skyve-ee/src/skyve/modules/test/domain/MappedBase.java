package modules.test.domain;

import com.vividsolutions.jts.geom.Geometry;
import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlSchemaType;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import org.skyve.CORE;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.Decimal10;
import org.skyve.domain.types.Decimal2;
import org.skyve.domain.types.Decimal5;
import org.skyve.domain.types.Enumeration;
import org.skyve.domain.types.TimeOnly;
import org.skyve.domain.types.Timestamp;
import org.skyve.impl.domain.AbstractPersistentBean;
import org.skyve.impl.domain.types.jaxb.DateOnlyMapper;
import org.skyve.impl.domain.types.jaxb.DateTimeMapper;
import org.skyve.impl.domain.types.jaxb.Decimal10Mapper;
import org.skyve.impl.domain.types.jaxb.Decimal2Mapper;
import org.skyve.impl.domain.types.jaxb.Decimal5Mapper;
import org.skyve.impl.domain.types.jaxb.GeometryMapper;
import org.skyve.impl.domain.types.jaxb.TimeOnlyMapper;
import org.skyve.impl.domain.types.jaxb.TimestampMapper;
import org.skyve.metadata.model.document.Bizlet.DomainValue;

/**
 * A base document using the mapped strategy.
 * 
 * @depend - - - Enum3
 * @navcomposed n composedAssociation 0..1 MappedExtensionSingleStrategy
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
public class MappedBase extends AbstractPersistentBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "test";
	/** @hidden */
	public static final String DOCUMENT_NAME = "MappedBase";

	/** @hidden */
	public static final String booleanFlagPropertyName = "booleanFlag";
	/** @hidden */
	public static final String colourPropertyName = "colour";
	/** @hidden */
	public static final String datePropertyName = "date";
	/** @hidden */
	public static final String dateTimePropertyName = "dateTime";
	/** @hidden */
	public static final String decimal10PropertyName = "decimal10";
	/** @hidden */
	public static final String decimal2PropertyName = "decimal2";
	/** @hidden */
	public static final String decimal5PropertyName = "decimal5";
	/** @hidden */
	public static final String enum3PropertyName = "enum3";
	/** @hidden */
	public static final String geometryPropertyName = "geometry";
	/** @hidden */
	public static final String idPropertyName = "id";
	/** @hidden */
	public static final String integerPropertyName = "integer";
	/** @hidden */
	public static final String longIntegerPropertyName = "longInteger";
	/** @hidden */
	public static final String markupPropertyName = "markup";
	/** @hidden */
	public static final String memoPropertyName = "memo";
	/** @hidden */
	public static final String textPropertyName = "text";
	/** @hidden */
	public static final String timePropertyName = "time";
	/** @hidden */
	public static final String timestampPropertyName = "timestamp";
	/** @hidden */
	public static final String baseDerivedIntegerPropertyName = "baseDerivedInteger";
	/** @hidden */
	public static final String composedAssociationPropertyName = "composedAssociation";

	/**
	 * Enum 3
	 **/
	@XmlEnum
	public static enum Enum3 implements Enumeration {
		one("one", "one"),
		two("two", "two"),
		three("three", "three");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues;

		private Enum3(String code, String description) {
			this.code = code;
			this.description = description;
			this.domainValue = new DomainValue(code, description);
		}

		@Override
		public String toCode() {
			return code;
		}

		@Override
		public String toDescription() {
			return description;
		}

		@Override
		public DomainValue toDomainValue() {
			return domainValue;
		}

		public static Enum3 fromCode(String code) {
			Enum3 result = null;

			for (Enum3 value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static Enum3 fromDescription(String description) {
			Enum3 result = null;

			for (Enum3 value : values()) {
				if (value.description.equals(description)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static List<DomainValue> toDomainValues() {
			if (domainValues == null) {
				Enum3[] values = values();
				domainValues = new ArrayList<>(values.length);
				for (Enum3 value : values) {
					domainValues.add(value.domainValue);
				}
			}

			return domainValues;
		}
	}

	private Boolean booleanFlag;
	private String colour;
	private DateOnly date;
	private DateTime dateTime;
	private Decimal10 decimal10;
	private Decimal2 decimal2;
	private Decimal5 decimal5;
	private Enum3 enum3;
	private Geometry geometry;
	private String id;
	private Integer integer;
	private Long longInteger;
	private String markup;
	private String memo;
	private String text;
	private TimeOnly time;
	private Timestamp timestamp;
	private Integer baseDerivedInteger;
	private MappedExtensionSingleStrategy composedAssociation = null;

	@Override
	@XmlTransient
	public String getBizModule() {
		return MappedBase.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return MappedBase.DOCUMENT_NAME;
	}

	public static MappedBase newInstance() throws Exception {
		return CORE.getUser().getCustomer().getModule(MODULE_NAME).getDocument(CORE.getUser().getCustomer(), DOCUMENT_NAME).newInstance(CORE.getUser());
	}

	@Override
	@XmlTransient
	public String getBizKey() {
		try {
			return org.skyve.util.Binder.formatMessage(org.skyve.CORE.getUser().getCustomer(),
														"{text}",
														this);
		}
		catch (Exception e) {
			return "Unknown";
		}
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof MappedBase) && 
					this.getBizId().equals(((MappedBase) o).getBizId()));
	}

	/**
	 * {@link #booleanFlag} accessor.
	 **/
	public Boolean getBooleanFlag() {
		return booleanFlag;
	}

	/**
	 * {@link #booleanFlag} mutator.
	 * 
	 * @param booleanFlag	The new value to set.
	 **/
	@XmlElement
	public void setBooleanFlag(Boolean booleanFlag) {
		preset(booleanFlagPropertyName, booleanFlag);
		this.booleanFlag = booleanFlag;
	}

	/**
	 * {@link #colour} accessor.
	 **/
	public String getColour() {
		return colour;
	}

	/**
	 * {@link #colour} mutator.
	 * 
	 * @param colour	The new value to set.
	 **/
	@XmlElement
	public void setColour(String colour) {
		preset(colourPropertyName, colour);
		this.colour = colour;
	}

	/**
	 * {@link #date} accessor.
	 **/
	public DateOnly getDate() {
		return date;
	}

	/**
	 * {@link #date} mutator.
	 * 
	 * @param date	The new value to set.
	 **/
	@XmlSchemaType(name = "date")
	@XmlJavaTypeAdapter(DateOnlyMapper.class)
	@XmlElement
	public void setDate(DateOnly date) {
		preset(datePropertyName, date);
		this.date = date;
	}

	/**
	 * {@link #dateTime} accessor.
	 **/
	public DateTime getDateTime() {
		return dateTime;
	}

	/**
	 * {@link #dateTime} mutator.
	 * 
	 * @param dateTime	The new value to set.
	 **/
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(DateTimeMapper.class)
	@XmlElement
	public void setDateTime(DateTime dateTime) {
		preset(dateTimePropertyName, dateTime);
		this.dateTime = dateTime;
	}

	/**
	 * {@link #decimal10} accessor.
	 **/
	public Decimal10 getDecimal10() {
		return decimal10;
	}

	/**
	 * {@link #decimal10} mutator.
	 * 
	 * @param decimal10	The new value to set.
	 **/
	@XmlJavaTypeAdapter(Decimal10Mapper.class)
	@XmlElement
	public void setDecimal10(Decimal10 decimal10) {
		preset(decimal10PropertyName, decimal10);
		this.decimal10 = decimal10;
	}

	/**
	 * {@link #decimal2} accessor.
	 **/
	public Decimal2 getDecimal2() {
		return decimal2;
	}

	/**
	 * {@link #decimal2} mutator.
	 * 
	 * @param decimal2	The new value to set.
	 **/
	@XmlJavaTypeAdapter(Decimal2Mapper.class)
	@XmlElement
	public void setDecimal2(Decimal2 decimal2) {
		preset(decimal2PropertyName, decimal2);
		this.decimal2 = decimal2;
	}

	/**
	 * {@link #decimal5} accessor.
	 **/
	public Decimal5 getDecimal5() {
		return decimal5;
	}

	/**
	 * {@link #decimal5} mutator.
	 * 
	 * @param decimal5	The new value to set.
	 **/
	@XmlJavaTypeAdapter(Decimal5Mapper.class)
	@XmlElement
	public void setDecimal5(Decimal5 decimal5) {
		preset(decimal5PropertyName, decimal5);
		this.decimal5 = decimal5;
	}

	/**
	 * {@link #enum3} accessor.
	 **/
	public Enum3 getEnum3() {
		return enum3;
	}

	/**
	 * {@link #enum3} mutator.
	 * 
	 * @param enum3	The new value to set.
	 **/
	@XmlElement
	public void setEnum3(Enum3 enum3) {
		preset(enum3PropertyName, enum3);
		this.enum3 = enum3;
	}

	/**
	 * {@link #geometry} accessor.
	 **/
	public Geometry getGeometry() {
		return geometry;
	}

	/**
	 * {@link #geometry} mutator.
	 * 
	 * @param geometry	The new value to set.
	 **/
	@XmlJavaTypeAdapter(GeometryMapper.class)
	@XmlElement
	public void setGeometry(Geometry geometry) {
		preset(geometryPropertyName, geometry);
		this.geometry = geometry;
	}

	/**
	 * {@link #id} accessor.
	 **/
	public String getId() {
		return id;
	}

	/**
	 * {@link #id} mutator.
	 * 
	 * @param id	The new value to set.
	 **/
	@XmlElement
	public void setId(String id) {
		preset(idPropertyName, id);
		this.id = id;
	}

	/**
	 * {@link #integer} accessor.
	 **/
	public Integer getInteger() {
		return integer;
	}

	/**
	 * {@link #integer} mutator.
	 * 
	 * @param integer	The new value to set.
	 **/
	@XmlElement
	public void setInteger(Integer integer) {
		preset(integerPropertyName, integer);
		this.integer = integer;
	}

	/**
	 * {@link #longInteger} accessor.
	 **/
	public Long getLongInteger() {
		return longInteger;
	}

	/**
	 * {@link #longInteger} mutator.
	 * 
	 * @param longInteger	The new value to set.
	 **/
	@XmlElement
	public void setLongInteger(Long longInteger) {
		preset(longIntegerPropertyName, longInteger);
		this.longInteger = longInteger;
	}

	/**
	 * {@link #markup} accessor.
	 **/
	public String getMarkup() {
		return markup;
	}

	/**
	 * {@link #markup} mutator.
	 * 
	 * @param markup	The new value to set.
	 **/
	@XmlElement
	public void setMarkup(String markup) {
		preset(markupPropertyName, markup);
		this.markup = markup;
	}

	/**
	 * {@link #memo} accessor.
	 **/
	public String getMemo() {
		return memo;
	}

	/**
	 * {@link #memo} mutator.
	 * 
	 * @param memo	The new value to set.
	 **/
	@XmlElement
	public void setMemo(String memo) {
		preset(memoPropertyName, memo);
		this.memo = memo;
	}

	/**
	 * {@link #text} accessor.
	 **/
	public String getText() {
		return text;
	}

	/**
	 * {@link #text} mutator.
	 * 
	 * @param text	The new value to set.
	 **/
	@XmlElement
	public void setText(String text) {
		preset(textPropertyName, text);
		this.text = text;
	}

	/**
	 * {@link #time} accessor.
	 **/
	public TimeOnly getTime() {
		return time;
	}

	/**
	 * {@link #time} mutator.
	 * 
	 * @param time	The new value to set.
	 **/
	@XmlSchemaType(name = "time")
	@XmlJavaTypeAdapter(TimeOnlyMapper.class)
	@XmlElement
	public void setTime(TimeOnly time) {
		preset(timePropertyName, time);
		this.time = time;
	}

	/**
	 * {@link #timestamp} accessor.
	 **/
	public Timestamp getTimestamp() {
		return timestamp;
	}

	/**
	 * {@link #timestamp} mutator.
	 * 
	 * @param timestamp	The new value to set.
	 **/
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(TimestampMapper.class)
	@XmlElement
	public void setTimestamp(Timestamp timestamp) {
		preset(timestampPropertyName, timestamp);
		this.timestamp = timestamp;
	}

	/**
	 * {@link #baseDerivedInteger} accessor.
	 **/
	public Integer getBaseDerivedInteger() {
		return baseDerivedInteger;
	}

	/**
	 * {@link #baseDerivedInteger} mutator.
	 * 
	 * @param baseDerivedInteger	The new value to set.
	 **/
	@XmlElement
	public void setBaseDerivedInteger(Integer baseDerivedInteger) {
		this.baseDerivedInteger = baseDerivedInteger;
	}

	/**
	 * {@link #composedAssociation} accessor.
	 **/
	public MappedExtensionSingleStrategy getComposedAssociation() {
		return composedAssociation;
	}

	/**
	 * {@link #composedAssociation} mutator.
	 * 
	 * @param composedAssociation	The new value to set.
	 **/
	@XmlElement
	public void setComposedAssociation(MappedExtensionSingleStrategy composedAssociation) {
		preset(composedAssociationPropertyName, composedAssociation);
		this.composedAssociation = composedAssociation;
	}
}
