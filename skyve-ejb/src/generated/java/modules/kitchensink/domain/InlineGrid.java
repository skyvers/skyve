package modules.kitchensink.domain;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlSchemaType;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.ChildBean;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.Decimal5;
import org.skyve.domain.types.Enumeration;
import org.skyve.impl.domain.AbstractTransientBean;
import org.skyve.impl.domain.types.jaxb.DateOnlyMapper;
import org.skyve.impl.domain.types.jaxb.Decimal5Mapper;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.util.Binder;
import org.skyve.util.ExpressionEvaluator;
import org.skyve.util.Util;

/**
 * Inline Grid
 * 
 * @depend - - - ConstantEnum
 * @stereotype "transient child"
 */
@XmlType
@XmlRootElement
public class InlineGrid extends AbstractTransientBean implements ChildBean<KitchenSink> {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "kitchensink";

	/** @hidden */
	public static final String DOCUMENT_NAME = "InlineGrid";

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
	public static final String decimal5PropertyName = "decimal5";

	/**
	 * ConstantEnum
	 **/
	@XmlEnum
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
	 * Decimal 5
	 **/
	private Decimal5 decimal5 = (Decimal5) ExpressionEvaluator.evaluate("{el:Decimal5.ZERO}", this);

	private KitchenSink parent;

	private Integer bizOrdinal;

	@Override
	@XmlTransient
	public String getBizModule() {
		return InlineGrid.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return InlineGrid.DOCUMENT_NAME;
	}

	public static InlineGrid newInstance() {
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
			return org.skyve.util.Binder.formatMessage("Inline Grid", this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return "Unknown";
		}
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof InlineGrid) && 
					this.getBizId().equals(((InlineGrid) o).getBizId()));
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

	@Override
	public KitchenSink getParent() {
		return parent;
	}

	@Override
	@XmlElement
	public void setParent(KitchenSink parent) {
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
