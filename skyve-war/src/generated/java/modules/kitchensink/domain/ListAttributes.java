package modules.kitchensink.domain;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlEnum;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlSchemaType;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
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
import org.skyve.impl.domain.types.jaxb.TimeOnlyMapper;
import org.skyve.impl.domain.types.jaxb.TimestampMapper;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.util.Binder;
import org.skyve.util.ExpressionEvaluator;
import org.skyve.util.Util;

/**
 * List Attributes
 * 
 * @depend - - - ConstantEnum
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator", date = "2024-03-25T03:13:15.000Z")
public class ListAttributes extends AbstractPersistentBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "kitchensink";

	/** @hidden */
	public static final String DOCUMENT_NAME = "ListAttributes";

	/** @hidden */
	public static final String constantEnumPropertyName = "constantEnum";

	/** @hidden */
	public static final String constantDomainPropertyName = "constantDomain";

	/** @hidden */
	public static final String variantDomainPropertyName = "variantDomain";

	/** @hidden */
	public static final String dynamicDomainPropertyName = "dynamicDomain";

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
	public static final String idPropertyName = "id";

	/** @hidden */
	public static final String normalIntegerPropertyName = "normalInteger";

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

	/**
	 * ConstantEnum
	 **/
	@XmlEnum
	@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator", date = "2024-03-25T03:13:15.000Z")
	public static enum ConstantEnum implements Enumeration {
		one1("one", "One (1)!"),
		two2("two", "Two (2)!"),
		three3("three", "Three (3)!");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues = Stream.of(values()).map(ConstantEnum::toDomainValue).collect(Collectors.toUnmodifiableList());

		private ConstantEnum(String code, String description) {
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

		public static ConstantEnum fromCode(String code) {
			ConstantEnum result = null;

			for (ConstantEnum value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static ConstantEnum fromLocalisedDescription(String description) {
			ConstantEnum result = null;

			for (ConstantEnum value : values()) {
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
	 * ConstantEnum
	 **/
	private ConstantEnum constantEnum;

	/**
	 * ConstantDomain
	 **/
	private String constantDomain;

	/**
	 * VariantDomain
	 **/
	private String variantDomain;

	/**
	 * DynamicDomain
	 **/
	private String dynamicDomain;

	/**
	 * Boolean Flag
	 **/
	private Boolean booleanFlag = (Boolean) ExpressionEvaluator.evaluate("{el:false}", this);

	/**
	 * Colour
	 **/
	private String colour = Binder.formatMessage("{el:'#000000'}", this);

	/**
	 * Date
	 **/
	private DateOnly date = (DateOnly) ExpressionEvaluator.evaluate("{el:newDateOnly()}", this);

	/**
	 * Date Time
	 **/
	private DateTime dateTime = (DateTime) ExpressionEvaluator.evaluate("{el:newDateTime()}", this);

	/**
	 * Decimal 10
	 **/
	private Decimal10 decimal10 = (Decimal10) ExpressionEvaluator.evaluate("{el:Decimal10.ZERO}", this);

	/**
	 * Decimal 2
	 **/
	private Decimal2 decimal2 = (Decimal2) ExpressionEvaluator.evaluate("{el:Decimal2.ZERO}", this);

	/**
	 * Decimal 5
	 **/
	private Decimal5 decimal5 = (Decimal5) ExpressionEvaluator.evaluate("{el:Decimal5.ZERO}", this);

	/**
	 * Id
	 **/
	private String id;

	/**
	 * Integer
	 **/
	private Integer normalInteger;

	/**
	 * Long Integer
	 **/
	private Long longInteger;

	/**
	 * Markup
	 **/
	private String markup;

	/**
	 * Memo
	 **/
	private String memo = Binder.formatMessage("Test {el:newGeometry('POINT(0 0)')}", this);

	/**
	 * Text
	 **/
	private String text;

	/**
	 * Time
	 **/
	private TimeOnly time;

	/**
	 * Timestamp
	 **/
	private Timestamp timestamp;

	@Override
	@XmlTransient
	public String getBizModule() {
		return ListAttributes.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return ListAttributes.DOCUMENT_NAME;
	}

	public static ListAttributes newInstance() {
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
			return org.skyve.util.Binder.formatMessage("{text}", this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return "Unknown";
		}
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof ListAttributes) && 
					this.getBizId().equals(((ListAttributes) o).getBizId()));
	}

	/**
	 * {@link #constantEnum} accessor.
	 * @return	The value.
	 **/
	public ConstantEnum getConstantEnum() {
		return constantEnum;
	}

	/**
	 * {@link #constantEnum} mutator.
	 * @param constantEnum	The new value.
	 **/
	@XmlElement
	public void setConstantEnum(ConstantEnum constantEnum) {
		preset(constantEnumPropertyName, constantEnum);
		this.constantEnum = constantEnum;
	}

	/**
	 * {@link #constantDomain} accessor.
	 * @return	The value.
	 **/
	public String getConstantDomain() {
		return constantDomain;
	}

	/**
	 * {@link #constantDomain} mutator.
	 * @param constantDomain	The new value.
	 **/
	@XmlElement
	public void setConstantDomain(String constantDomain) {
		preset(constantDomainPropertyName, constantDomain);
		this.constantDomain = constantDomain;
	}

	/**
	 * {@link #variantDomain} accessor.
	 * @return	The value.
	 **/
	public String getVariantDomain() {
		return variantDomain;
	}

	/**
	 * {@link #variantDomain} mutator.
	 * @param variantDomain	The new value.
	 **/
	@XmlElement
	public void setVariantDomain(String variantDomain) {
		preset(variantDomainPropertyName, variantDomain);
		this.variantDomain = variantDomain;
	}

	/**
	 * {@link #dynamicDomain} accessor.
	 * @return	The value.
	 **/
	public String getDynamicDomain() {
		return dynamicDomain;
	}

	/**
	 * {@link #dynamicDomain} mutator.
	 * @param dynamicDomain	The new value.
	 **/
	@XmlElement
	public void setDynamicDomain(String dynamicDomain) {
		preset(dynamicDomainPropertyName, dynamicDomain);
		this.dynamicDomain = dynamicDomain;
	}

	/**
	 * {@link #booleanFlag} accessor.
	 * @return	The value.
	 **/
	public Boolean getBooleanFlag() {
		return booleanFlag;
	}

	/**
	 * {@link #booleanFlag} mutator.
	 * @param booleanFlag	The new value.
	 **/
	@XmlElement
	public void setBooleanFlag(Boolean booleanFlag) {
		preset(booleanFlagPropertyName, booleanFlag);
		this.booleanFlag = booleanFlag;
	}

	/**
	 * {@link #colour} accessor.
	 * @return	The value.
	 **/
	public String getColour() {
		return colour;
	}

	/**
	 * {@link #colour} mutator.
	 * @param colour	The new value.
	 **/
	@XmlElement
	public void setColour(String colour) {
		preset(colourPropertyName, colour);
		this.colour = colour;
	}

	/**
	 * {@link #date} accessor.
	 * @return	The value.
	 **/
	public DateOnly getDate() {
		return date;
	}

	/**
	 * {@link #date} mutator.
	 * @param date	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "date")
	@XmlJavaTypeAdapter(DateOnlyMapper.class)
	public void setDate(DateOnly date) {
		preset(datePropertyName, date);
		this.date = date;
	}

	/**
	 * {@link #dateTime} accessor.
	 * @return	The value.
	 **/
	public DateTime getDateTime() {
		return dateTime;
	}

	/**
	 * {@link #dateTime} mutator.
	 * @param dateTime	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(DateTimeMapper.class)
	public void setDateTime(DateTime dateTime) {
		preset(dateTimePropertyName, dateTime);
		this.dateTime = dateTime;
	}

	/**
	 * {@link #decimal10} accessor.
	 * @return	The value.
	 **/
	public Decimal10 getDecimal10() {
		return decimal10;
	}

	/**
	 * {@link #decimal10} mutator.
	 * @param decimal10	The new value.
	 **/
	@XmlElement
	@XmlJavaTypeAdapter(Decimal10Mapper.class)
	public void setDecimal10(Decimal10 decimal10) {
		preset(decimal10PropertyName, decimal10);
		this.decimal10 = decimal10;
	}

	/**
	 * {@link #decimal2} accessor.
	 * @return	The value.
	 **/
	public Decimal2 getDecimal2() {
		return decimal2;
	}

	/**
	 * {@link #decimal2} mutator.
	 * @param decimal2	The new value.
	 **/
	@XmlElement
	@XmlJavaTypeAdapter(Decimal2Mapper.class)
	public void setDecimal2(Decimal2 decimal2) {
		preset(decimal2PropertyName, decimal2);
		this.decimal2 = decimal2;
	}

	/**
	 * {@link #decimal5} accessor.
	 * @return	The value.
	 **/
	public Decimal5 getDecimal5() {
		return decimal5;
	}

	/**
	 * {@link #decimal5} mutator.
	 * @param decimal5	The new value.
	 **/
	@XmlElement
	@XmlJavaTypeAdapter(Decimal5Mapper.class)
	public void setDecimal5(Decimal5 decimal5) {
		preset(decimal5PropertyName, decimal5);
		this.decimal5 = decimal5;
	}

	/**
	 * {@link #id} accessor.
	 * @return	The value.
	 **/
	public String getId() {
		return id;
	}

	/**
	 * {@link #id} mutator.
	 * @param id	The new value.
	 **/
	@XmlElement
	public void setId(String id) {
		preset(idPropertyName, id);
		this.id = id;
	}

	/**
	 * {@link #normalInteger} accessor.
	 * @return	The value.
	 **/
	public Integer getNormalInteger() {
		return normalInteger;
	}

	/**
	 * {@link #normalInteger} mutator.
	 * @param normalInteger	The new value.
	 **/
	@XmlElement
	public void setNormalInteger(Integer normalInteger) {
		preset(normalIntegerPropertyName, normalInteger);
		this.normalInteger = normalInteger;
	}

	/**
	 * {@link #longInteger} accessor.
	 * @return	The value.
	 **/
	public Long getLongInteger() {
		return longInteger;
	}

	/**
	 * {@link #longInteger} mutator.
	 * @param longInteger	The new value.
	 **/
	@XmlElement
	public void setLongInteger(Long longInteger) {
		preset(longIntegerPropertyName, longInteger);
		this.longInteger = longInteger;
	}

	/**
	 * {@link #markup} accessor.
	 * @return	The value.
	 **/
	public String getMarkup() {
		return markup;
	}

	/**
	 * {@link #markup} mutator.
	 * @param markup	The new value.
	 **/
	@XmlElement
	public void setMarkup(String markup) {
		preset(markupPropertyName, markup);
		this.markup = markup;
	}

	/**
	 * {@link #memo} accessor.
	 * @return	The value.
	 **/
	public String getMemo() {
		return memo;
	}

	/**
	 * {@link #memo} mutator.
	 * @param memo	The new value.
	 **/
	@XmlElement
	public void setMemo(String memo) {
		preset(memoPropertyName, memo);
		this.memo = memo;
	}

	/**
	 * {@link #text} accessor.
	 * @return	The value.
	 **/
	public String getText() {
		return text;
	}

	/**
	 * {@link #text} mutator.
	 * @param text	The new value.
	 **/
	@XmlElement
	public void setText(String text) {
		preset(textPropertyName, text);
		this.text = text;
	}

	/**
	 * {@link #time} accessor.
	 * @return	The value.
	 **/
	public TimeOnly getTime() {
		return time;
	}

	/**
	 * {@link #time} mutator.
	 * @param time	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "time")
	@XmlJavaTypeAdapter(TimeOnlyMapper.class)
	public void setTime(TimeOnly time) {
		preset(timePropertyName, time);
		this.time = time;
	}

	/**
	 * {@link #timestamp} accessor.
	 * @return	The value.
	 **/
	public Timestamp getTimestamp() {
		return timestamp;
	}

	/**
	 * {@link #timestamp} mutator.
	 * @param timestamp	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(TimestampMapper.class)
	public void setTimestamp(Timestamp timestamp) {
		preset(timestampPropertyName, timestamp);
		this.timestamp = timestamp;
	}

	/**
	 * condition
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isCondition() {
		return (true);
	}

	/**
	 * {@link #isCondition} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotCondition() {
		return (! isCondition());
	}
}
