package modules.kitchensink.domain;

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
import org.locationtech.jts.geom.Geometry;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.types.Enumeration;
import org.skyve.impl.domain.AbstractTransientBean;
import org.skyve.impl.domain.types.jaxb.GeometryMapper;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.util.Util;

/**
 * Conditions Fixture Row
 * <br/>
 * Non-persistent row of the Kitchen Sink conditions fixture's data grids and data repeaters,
			with an attribute for each widget a grid column can hold.
 * 
 * @depend - - - Choice
 * @depend - - - Radio
 * @navhas n lookup 0..1 LookupDescription
 * @stereotype "transient"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public class ConditionsFixtureRow extends AbstractTransientBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "kitchensink";

	/** @hidden */
	public static final String DOCUMENT_NAME = "ConditionsFixtureRow";

	/** @hidden */
	public static final String textPropertyName = "text";

	/** @hidden */
	public static final String passwordPropertyName = "password";

	/** @hidden */
	public static final String memoPropertyName = "memo";

	/** @hidden */
	public static final String markupPropertyName = "markup";

	/** @hidden */
	public static final String htmlPropertyName = "html";

	/** @hidden */
	public static final String flagPropertyName = "flag";

	/** @hidden */
	public static final String colourPropertyName = "colour";

	/** @hidden */
	public static final String choicePropertyName = "choice";

	/** @hidden */
	public static final String radioPropertyName = "radio";

	/** @hidden */
	public static final String numberPropertyName = "number";

	/** @hidden */
	public static final String sliderPropertyName = "slider";

	/** @hidden */
	public static final String geometryPropertyName = "geometry";

	/** @hidden */
	public static final String contentPropertyName = "content";

	/** @hidden */
	public static final String lookupPropertyName = "lookup";

	/**
	 * Choice
	 **/
	@XmlEnum
	@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
	public enum Choice implements Enumeration {
		one("one", "one"),
		two("two", "two");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues = Stream.of(values()).map(Choice::toDomainValue).collect(Collectors.toUnmodifiableList());

		private Choice(String code, String description) {
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

		public static Choice fromCode(String code) {
			Choice result = null;

			for (Choice value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static Choice fromLocalisedDescription(String description) {
			Choice result = null;

			for (Choice value : values()) {
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
	 * Radio
	 **/
	@XmlEnum
	@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
	public enum Radio implements Enumeration {
		one("one", "one"),
		two("two", "two");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues = Stream.of(values()).map(Radio::toDomainValue).collect(Collectors.toUnmodifiableList());

		private Radio(String code, String description) {
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

		public static Radio fromCode(String code) {
			Radio result = null;

			for (Radio value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static Radio fromLocalisedDescription(String description) {
			Radio result = null;

			for (Radio value : values()) {
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
	 * Text
	 **/
	private String text;

	/**
	 * Password
	 **/
	private String password;

	/**
	 * Memo
	 **/
	private String memo;

	/**
	 * Markup
	 **/
	private String markup;

	/**
	 * Html
	 **/
	private String html;

	/**
	 * Flag
	 **/
	private Boolean flag;

	/**
	 * Colour
	 **/
	private String colour;

	/**
	 * Choice
	 **/
	private Choice choice;

	/**
	 * Radio
	 **/
	private Radio radio;

	/**
	 * Number
	 **/
	private Integer number;

	/**
	 * Slider
	 **/
	private Integer slider;

	/**
	 * Geometry
	 **/
	private Geometry geometry;

	/**
	 * Content
	 **/
	private String content;

	/**
	 * Lookup
	 **/
	private LookupDescription lookup = null;

	@Override
	@XmlTransient
	public String getBizModule() {
		return ConditionsFixtureRow.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return ConditionsFixtureRow.DOCUMENT_NAME;
	}

	public static ConditionsFixtureRow newInstance() {
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
			return org.skyve.util.Binder.formatMessage("Conditions Fixture Row", this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return "Unknown";
		}
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
	 * {@link #password} accessor.
	 * @return	The value.
	 **/
	public String getPassword() {
		return password;
	}

	/**
	 * {@link #password} mutator.
	 * @param password	The new value.
	 **/
	@XmlElement
	public void setPassword(String password) {
		preset(passwordPropertyName, password);
		this.password = password;
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
	 * {@link #html} accessor.
	 * @return	The value.
	 **/
	public String getHtml() {
		return html;
	}

	/**
	 * {@link #html} mutator.
	 * @param html	The new value.
	 **/
	@XmlElement
	public void setHtml(String html) {
		preset(htmlPropertyName, html);
		this.html = html;
	}

	/**
	 * {@link #flag} accessor.
	 * @return	The value.
	 **/
	public Boolean getFlag() {
		return flag;
	}

	/**
	 * {@link #flag} mutator.
	 * @param flag	The new value.
	 **/
	@XmlElement
	public void setFlag(Boolean flag) {
		preset(flagPropertyName, flag);
		this.flag = flag;
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
	 * {@link #choice} accessor.
	 * @return	The value.
	 **/
	public Choice getChoice() {
		return choice;
	}

	/**
	 * {@link #choice} mutator.
	 * @param choice	The new value.
	 **/
	@XmlElement
	public void setChoice(Choice choice) {
		preset(choicePropertyName, choice);
		this.choice = choice;
	}

	/**
	 * {@link #radio} accessor.
	 * @return	The value.
	 **/
	public Radio getRadio() {
		return radio;
	}

	/**
	 * {@link #radio} mutator.
	 * @param radio	The new value.
	 **/
	@XmlElement
	public void setRadio(Radio radio) {
		preset(radioPropertyName, radio);
		this.radio = radio;
	}

	/**
	 * {@link #number} accessor.
	 * @return	The value.
	 **/
	public Integer getNumber() {
		return number;
	}

	/**
	 * {@link #number} mutator.
	 * @param number	The new value.
	 **/
	@XmlElement
	public void setNumber(Integer number) {
		preset(numberPropertyName, number);
		this.number = number;
	}

	/**
	 * {@link #slider} accessor.
	 * @return	The value.
	 **/
	public Integer getSlider() {
		return slider;
	}

	/**
	 * {@link #slider} mutator.
	 * @param slider	The new value.
	 **/
	@XmlElement
	public void setSlider(Integer slider) {
		preset(sliderPropertyName, slider);
		this.slider = slider;
	}

	/**
	 * {@link #geometry} accessor.
	 * @return	The value.
	 **/
	public Geometry getGeometry() {
		return geometry;
	}

	/**
	 * {@link #geometry} mutator.
	 * @param geometry	The new value.
	 **/
	@XmlElement
	@XmlJavaTypeAdapter(GeometryMapper.class)
	public void setGeometry(Geometry geometry) {
		preset(geometryPropertyName, geometry);
		this.geometry = geometry;
	}

	/**
	 * {@link #content} accessor.
	 * @return	The value.
	 **/
	public String getContent() {
		return content;
	}

	/**
	 * {@link #content} mutator.
	 * @param content	The new value.
	 **/
	@XmlElement
	public void setContent(String content) {
		preset(contentPropertyName, content);
		this.content = content;
	}

	/**
	 * {@link #lookup} accessor.
	 * @return	The value.
	 **/
	public LookupDescription getLookup() {
		return lookup;
	}

	/**
	 * {@link #lookup} mutator.
	 * @param lookup	The new value.
	 **/
	@XmlElement
	public void setLookup(LookupDescription lookup) {
		if (this.lookup != lookup) {
			preset(lookupPropertyName, lookup);
			this.lookup = lookup;
		}
	}
}
