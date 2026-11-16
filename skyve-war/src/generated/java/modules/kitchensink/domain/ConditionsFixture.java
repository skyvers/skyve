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
import org.skyve.impl.domain.ChangeTrackingArrayList;
import org.skyve.impl.domain.types.jaxb.GeometryMapper;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.util.ExpressionEvaluator;
import org.skyve.util.Util;

/**
 * Conditions Fixture
 * <br/>
 * Non-persistent Kitchen Sink fixture whose edit view puts a condition on every view attribute that takes one.

			Show drives every visible/invisible condition and Allow drives every other condition, so that each widget
			on the first four tabs of the edit view is shown when Show is set and usable when Allow is set.
			Those tabs author the same widgets four ways: positive attributes (visible, enabled, enableAdd...) and
			negative attributes (invisible, disabled, disableAdd...), each holding condition names and then inline expressions.
			An attribute ending in 1, 2, 3 or 4 is bound on the tab of that number.
 * 
 * @depend - - - Choice
 * @depend - - - Combo1
 * @depend - - - Radio1
 * @depend - - - Combo2
 * @depend - - - Radio2
 * @depend - - - Combo3
 * @depend - - - Radio3
 * @depend - - - Combo4
 * @depend - - - Radio4
 * @navhas n checkMembership3 0..n LookupDescription
 * @navhas n listMembership4 0..n LookupDescription
 * @navhas n checkMembership2 0..n LookupDescription
 * @navhas n checkMembership4 0..n LookupDescription
 * @navhas n checkMembership1 0..n LookupDescription
 * @navhas n zoomInContainer3 0..1 LookupDescription
 * @navhas n zoomInContainer4 0..1 LookupDescription
 * @navhas n zoomInContainer1 0..1 LookupDescription
 * @navhas n dataRepeater1 0..n ConditionsFixtureRow
 * @navhas n zoomInContainer2 0..1 LookupDescription
 * @navhas n lookupDescription3 0..1 LookupDescription
 * @navhas n nested 0..1 ConditionsFixture
 * @navhas n lookupDescription2 0..1 LookupDescription
 * @navhas n lookupDescription4 0..1 LookupDescription
 * @navhas n lookupDescription1 0..1 LookupDescription
 * @navhas n dataGrid4 0..n ConditionsFixtureRow
 * @navhas n dataGrid1 0..n ConditionsFixtureRow
 * @navhas n dataGrid2 0..n ConditionsFixtureRow
 * @navhas n dataGrid3 0..n ConditionsFixtureRow
 * @navhas n zoomIn1 0..1 LookupDescription
 * @navhas n inlineGrid2 0..n ConditionsFixtureRow
 * @navhas n inlineGrid1 0..n ConditionsFixtureRow
 * @navhas n zoomIn3 0..1 LookupDescription
 * @navhas n zoomIn2 0..1 LookupDescription
 * @navhas n zoomIn4 0..1 LookupDescription
 * @navhas n inlineGrid4 0..n ConditionsFixtureRow
 * @navhas n inlineGrid3 0..n ConditionsFixtureRow
 * @navhas n dataRepeater3 0..n ConditionsFixtureRow
 * @navhas n dataRepeater2 0..n ConditionsFixtureRow
 * @navhas n dataRepeater4 0..n ConditionsFixtureRow
 * @navhas n comparison3 0..1 LookupDescription
 * @navhas n comparison4 0..1 LookupDescription
 * @navhas n comparison1 0..1 LookupDescription
 * @navhas n comparison2 0..1 LookupDescription
 * @navhas n listMembership1 0..n LookupDescription
 * @navhas n listMembership2 0..n LookupDescription
 * @navhas n listMembership3 0..n LookupDescription
 * @stereotype "transient"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public class ConditionsFixture extends AbstractTransientBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "kitchensink";

	/** @hidden */
	public static final String DOCUMENT_NAME = "ConditionsFixture";

	/** @hidden */
	public static final String showPropertyName = "show";

	/** @hidden */
	public static final String allowPropertyName = "allow";

	/** @hidden */
	public static final String textPropertyName = "text";

	/** @hidden */
	public static final String numberPropertyName = "number";

	/** @hidden */
	public static final String choicePropertyName = "choice";

	/** @hidden */
	public static final String nestedPropertyName = "nested";

	/** @hidden */
	public static final String eventTargetPropertyName = "eventTarget";

	/** @hidden */
	public static final String checkBox1PropertyName = "checkBox1";

	/** @hidden */
	public static final String colourPicker1PropertyName = "colourPicker1";

	/** @hidden */
	public static final String combo1PropertyName = "combo1";

	/** @hidden */
	public static final String content1PropertyName = "content1";

	/** @hidden */
	public static final String contentSignature1PropertyName = "contentSignature1";

	/** @hidden */
	public static final String geometry1PropertyName = "geometry1";

	/** @hidden */
	public static final String geometryMap1PropertyName = "geometryMap1";

	/** @hidden */
	public static final String html1PropertyName = "html1";

	/** @hidden */
	public static final String password1PropertyName = "password1";

	/** @hidden */
	public static final String progressBar1PropertyName = "progressBar1";

	/** @hidden */
	public static final String radio1PropertyName = "radio1";

	/** @hidden */
	public static final String richText1PropertyName = "richText1";

	/** @hidden */
	public static final String slider1PropertyName = "slider1";

	/** @hidden */
	public static final String spinner1PropertyName = "spinner1";

	/** @hidden */
	public static final String textArea1PropertyName = "textArea1";

	/** @hidden */
	public static final String textField1PropertyName = "textField1";

	/** @hidden */
	public static final String lookupDescription1PropertyName = "lookupDescription1";

	/** @hidden */
	public static final String zoomIn1PropertyName = "zoomIn1";

	/** @hidden */
	public static final String zoomInContainer1PropertyName = "zoomInContainer1";

	/** @hidden */
	public static final String comparison1PropertyName = "comparison1";

	/** @hidden */
	public static final String checkMembership1PropertyName = "checkMembership1";

	/** @hidden */
	public static final String listMembership1PropertyName = "listMembership1";

	/** @hidden */
	public static final String dataGrid1PropertyName = "dataGrid1";

	/** @hidden */
	public static final String inlineGrid1PropertyName = "inlineGrid1";

	/** @hidden */
	public static final String dataRepeater1PropertyName = "dataRepeater1";

	/** @hidden */
	public static final String checkBox2PropertyName = "checkBox2";

	/** @hidden */
	public static final String colourPicker2PropertyName = "colourPicker2";

	/** @hidden */
	public static final String combo2PropertyName = "combo2";

	/** @hidden */
	public static final String content2PropertyName = "content2";

	/** @hidden */
	public static final String contentSignature2PropertyName = "contentSignature2";

	/** @hidden */
	public static final String geometry2PropertyName = "geometry2";

	/** @hidden */
	public static final String geometryMap2PropertyName = "geometryMap2";

	/** @hidden */
	public static final String html2PropertyName = "html2";

	/** @hidden */
	public static final String password2PropertyName = "password2";

	/** @hidden */
	public static final String progressBar2PropertyName = "progressBar2";

	/** @hidden */
	public static final String radio2PropertyName = "radio2";

	/** @hidden */
	public static final String richText2PropertyName = "richText2";

	/** @hidden */
	public static final String slider2PropertyName = "slider2";

	/** @hidden */
	public static final String spinner2PropertyName = "spinner2";

	/** @hidden */
	public static final String textArea2PropertyName = "textArea2";

	/** @hidden */
	public static final String textField2PropertyName = "textField2";

	/** @hidden */
	public static final String lookupDescription2PropertyName = "lookupDescription2";

	/** @hidden */
	public static final String zoomIn2PropertyName = "zoomIn2";

	/** @hidden */
	public static final String zoomInContainer2PropertyName = "zoomInContainer2";

	/** @hidden */
	public static final String comparison2PropertyName = "comparison2";

	/** @hidden */
	public static final String checkMembership2PropertyName = "checkMembership2";

	/** @hidden */
	public static final String listMembership2PropertyName = "listMembership2";

	/** @hidden */
	public static final String dataGrid2PropertyName = "dataGrid2";

	/** @hidden */
	public static final String inlineGrid2PropertyName = "inlineGrid2";

	/** @hidden */
	public static final String dataRepeater2PropertyName = "dataRepeater2";

	/** @hidden */
	public static final String checkBox3PropertyName = "checkBox3";

	/** @hidden */
	public static final String colourPicker3PropertyName = "colourPicker3";

	/** @hidden */
	public static final String combo3PropertyName = "combo3";

	/** @hidden */
	public static final String content3PropertyName = "content3";

	/** @hidden */
	public static final String contentSignature3PropertyName = "contentSignature3";

	/** @hidden */
	public static final String geometry3PropertyName = "geometry3";

	/** @hidden */
	public static final String geometryMap3PropertyName = "geometryMap3";

	/** @hidden */
	public static final String html3PropertyName = "html3";

	/** @hidden */
	public static final String password3PropertyName = "password3";

	/** @hidden */
	public static final String progressBar3PropertyName = "progressBar3";

	/** @hidden */
	public static final String radio3PropertyName = "radio3";

	/** @hidden */
	public static final String richText3PropertyName = "richText3";

	/** @hidden */
	public static final String slider3PropertyName = "slider3";

	/** @hidden */
	public static final String spinner3PropertyName = "spinner3";

	/** @hidden */
	public static final String textArea3PropertyName = "textArea3";

	/** @hidden */
	public static final String textField3PropertyName = "textField3";

	/** @hidden */
	public static final String lookupDescription3PropertyName = "lookupDescription3";

	/** @hidden */
	public static final String zoomIn3PropertyName = "zoomIn3";

	/** @hidden */
	public static final String zoomInContainer3PropertyName = "zoomInContainer3";

	/** @hidden */
	public static final String comparison3PropertyName = "comparison3";

	/** @hidden */
	public static final String checkMembership3PropertyName = "checkMembership3";

	/** @hidden */
	public static final String listMembership3PropertyName = "listMembership3";

	/** @hidden */
	public static final String dataGrid3PropertyName = "dataGrid3";

	/** @hidden */
	public static final String inlineGrid3PropertyName = "inlineGrid3";

	/** @hidden */
	public static final String dataRepeater3PropertyName = "dataRepeater3";

	/** @hidden */
	public static final String checkBox4PropertyName = "checkBox4";

	/** @hidden */
	public static final String colourPicker4PropertyName = "colourPicker4";

	/** @hidden */
	public static final String combo4PropertyName = "combo4";

	/** @hidden */
	public static final String content4PropertyName = "content4";

	/** @hidden */
	public static final String contentSignature4PropertyName = "contentSignature4";

	/** @hidden */
	public static final String geometry4PropertyName = "geometry4";

	/** @hidden */
	public static final String geometryMap4PropertyName = "geometryMap4";

	/** @hidden */
	public static final String html4PropertyName = "html4";

	/** @hidden */
	public static final String password4PropertyName = "password4";

	/** @hidden */
	public static final String progressBar4PropertyName = "progressBar4";

	/** @hidden */
	public static final String radio4PropertyName = "radio4";

	/** @hidden */
	public static final String richText4PropertyName = "richText4";

	/** @hidden */
	public static final String slider4PropertyName = "slider4";

	/** @hidden */
	public static final String spinner4PropertyName = "spinner4";

	/** @hidden */
	public static final String textArea4PropertyName = "textArea4";

	/** @hidden */
	public static final String textField4PropertyName = "textField4";

	/** @hidden */
	public static final String lookupDescription4PropertyName = "lookupDescription4";

	/** @hidden */
	public static final String zoomIn4PropertyName = "zoomIn4";

	/** @hidden */
	public static final String zoomInContainer4PropertyName = "zoomInContainer4";

	/** @hidden */
	public static final String comparison4PropertyName = "comparison4";

	/** @hidden */
	public static final String checkMembership4PropertyName = "checkMembership4";

	/** @hidden */
	public static final String listMembership4PropertyName = "listMembership4";

	/** @hidden */
	public static final String dataGrid4PropertyName = "dataGrid4";

	/** @hidden */
	public static final String inlineGrid4PropertyName = "inlineGrid4";

	/** @hidden */
	public static final String dataRepeater4PropertyName = "dataRepeater4";

	/**
	 * Choice
	 * <br/>
	 * Choose one to flip the two el:bean.choice eq Choice.one labels on the Condition forms tab. Changing it does not refresh the view: tick Show or Allow, or save, to see the effect.
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
	 * combo 1
	 **/
	@XmlEnum
	@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
	public enum Combo1 implements Enumeration {
		one("one", "one"),
		two("two", "two");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues = Stream.of(values()).map(Combo1::toDomainValue).collect(Collectors.toUnmodifiableList());

		private Combo1(String code, String description) {
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

		public static Combo1 fromCode(String code) {
			Combo1 result = null;

			for (Combo1 value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static Combo1 fromLocalisedDescription(String description) {
			Combo1 result = null;

			for (Combo1 value : values()) {
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
	 * radio 1
	 **/
	@XmlEnum
	@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
	public enum Radio1 implements Enumeration {
		one("one", "one"),
		two("two", "two");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues = Stream.of(values()).map(Radio1::toDomainValue).collect(Collectors.toUnmodifiableList());

		private Radio1(String code, String description) {
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

		public static Radio1 fromCode(String code) {
			Radio1 result = null;

			for (Radio1 value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static Radio1 fromLocalisedDescription(String description) {
			Radio1 result = null;

			for (Radio1 value : values()) {
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
	 * combo 2
	 **/
	@XmlEnum
	@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
	public enum Combo2 implements Enumeration {
		one("one", "one"),
		two("two", "two");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues = Stream.of(values()).map(Combo2::toDomainValue).collect(Collectors.toUnmodifiableList());

		private Combo2(String code, String description) {
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

		public static Combo2 fromCode(String code) {
			Combo2 result = null;

			for (Combo2 value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static Combo2 fromLocalisedDescription(String description) {
			Combo2 result = null;

			for (Combo2 value : values()) {
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
	 * radio 2
	 **/
	@XmlEnum
	@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
	public enum Radio2 implements Enumeration {
		one("one", "one"),
		two("two", "two");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues = Stream.of(values()).map(Radio2::toDomainValue).collect(Collectors.toUnmodifiableList());

		private Radio2(String code, String description) {
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

		public static Radio2 fromCode(String code) {
			Radio2 result = null;

			for (Radio2 value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static Radio2 fromLocalisedDescription(String description) {
			Radio2 result = null;

			for (Radio2 value : values()) {
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
	 * combo 3
	 **/
	@XmlEnum
	@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
	public enum Combo3 implements Enumeration {
		one("one", "one"),
		two("two", "two");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues = Stream.of(values()).map(Combo3::toDomainValue).collect(Collectors.toUnmodifiableList());

		private Combo3(String code, String description) {
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

		public static Combo3 fromCode(String code) {
			Combo3 result = null;

			for (Combo3 value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static Combo3 fromLocalisedDescription(String description) {
			Combo3 result = null;

			for (Combo3 value : values()) {
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
	 * radio 3
	 **/
	@XmlEnum
	@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
	public enum Radio3 implements Enumeration {
		one("one", "one"),
		two("two", "two");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues = Stream.of(values()).map(Radio3::toDomainValue).collect(Collectors.toUnmodifiableList());

		private Radio3(String code, String description) {
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

		public static Radio3 fromCode(String code) {
			Radio3 result = null;

			for (Radio3 value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static Radio3 fromLocalisedDescription(String description) {
			Radio3 result = null;

			for (Radio3 value : values()) {
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
	 * combo 4
	 **/
	@XmlEnum
	@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
	public enum Combo4 implements Enumeration {
		one("one", "one"),
		two("two", "two");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues = Stream.of(values()).map(Combo4::toDomainValue).collect(Collectors.toUnmodifiableList());

		private Combo4(String code, String description) {
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

		public static Combo4 fromCode(String code) {
			Combo4 result = null;

			for (Combo4 value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static Combo4 fromLocalisedDescription(String description) {
			Combo4 result = null;

			for (Combo4 value : values()) {
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
	 * radio 4
	 **/
	@XmlEnum
	@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
	public enum Radio4 implements Enumeration {
		one("one", "one"),
		two("two", "two");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues = Stream.of(values()).map(Radio4::toDomainValue).collect(Collectors.toUnmodifiableList());

		private Radio4(String code, String description) {
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

		public static Radio4 fromCode(String code) {
			Radio4 result = null;

			for (Radio4 value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static Radio4 fromLocalisedDescription(String description) {
			Radio4 result = null;

			for (Radio4 value : values()) {
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
	 * Show
	 * <br/>
	 * Drives every visible and invisible condition on the four attribute tabs: when it is ticked, every widget there is shown. Changing it refreshes the view.
	 **/
	private Boolean show = Boolean.valueOf(true);

	/**
	 * Allow
	 * <br/>
	 * Drives every other condition on the four attribute tabs (enabled and disabled, the add, edit, zoom, remove, pick and clear flags, and postRefresh): when it is ticked, every widget there can be used. Changing it refreshes the view.
	 **/
	private Boolean allow = Boolean.valueOf(true);

	/**
	 * Text
	 * <br/>
	 * Type x to flip the two el:bean.text eq 'x' labels on the Condition forms tab: the visible one appears and the invisible one disappears. Type mapped to show the component's el:bean.text eq 'mapped' label on the first three attribute tabs; on the fourth that condition is mapped to shownByExpression, so there it follows Show. Changing it does not refresh the view: tick Show or Allow, or save, to see the effect.
	 **/
	private String text;

	/**
	 * Number
	 * <br/>
	 * Enter 6 or more to flip the two el:bean.number gt 5 labels on the Condition forms tab. Changing it does not refresh the view: tick Show or Allow, or save, to see the effect.
	 **/
	private Integer number;

	/**
	 * Choice
	 * <br/>
	 * Choose one to flip the two el:bean.choice eq Choice.one labels on the Condition forms tab. Changing it does not refresh the view: tick Show or Allow, or save, to see the effect.
	 **/
	private Choice choice;

	/**
	 * Nested
	 **/
	private ConditionsFixture nested = null;

	/**
	 * Event Target
	 * <br/>
	 * Nothing reads this. It is what every setDisabled and setInvisible event action disables and hides, according to its condition, in the handlers on the four attribute tabs: focus, blur and change of the text field; picked, cleared, edited and added of the lookup; added, edited, removed, selected and deleted of the grids; and change of the geometry map and list membership. It is a checkbox because only a boolean attribute can be their target. PrimeFaces does not apply these conditions, so only SmartClient changes it.
	 **/
	private Boolean eventTarget;

	/**
	 * checkBox 1
	 **/
	private Boolean checkBox1;

	/**
	 * colourPicker 1
	 **/
	private String colourPicker1;

	/**
	 * combo 1
	 **/
	private Combo1 combo1;

	/**
	 * content 1
	 **/
	private String content1;

	/**
	 * contentSignature 1
	 **/
	private String contentSignature1;

	/**
	 * geometry 1
	 **/
	private Geometry geometry1;

	/**
	 * geometryMap 1
	 **/
	private Geometry geometryMap1;

	/**
	 * html 1
	 **/
	private String html1;

	/**
	 * password 1
	 **/
	private String password1;

	/**
	 * progressBar 1
	 **/
	private Integer progressBar1;

	/**
	 * radio 1
	 **/
	private Radio1 radio1;

	/**
	 * richText 1
	 **/
	private String richText1;

	/**
	 * slider 1
	 **/
	private Integer slider1;

	/**
	 * spinner 1
	 **/
	private Integer spinner1;

	/**
	 * textArea 1
	 **/
	private String textArea1;

	/**
	 * textField 1
	 **/
	private String textField1;

	/**
	 * lookupDescription 1
	 **/
	private LookupDescription lookupDescription1 = null;

	/**
	 * zoomIn 1
	 **/
	private LookupDescription zoomIn1 = null;

	/**
	 * zoomInContainer 1
	 **/
	private LookupDescription zoomInContainer1 = null;

	/**
	 * comparison 1
	 **/
	private LookupDescription comparison1 = null;

	/**
	 * checkMembership 1
	 **/
	private List<LookupDescription> checkMembership1 = new ChangeTrackingArrayList<>("checkMembership1", this);

	/**
	 * listMembership 1
	 **/
	private List<LookupDescription> listMembership1 = new ChangeTrackingArrayList<>("listMembership1", this);

	/**
	 * dataGrid 1
	 **/
	private List<ConditionsFixtureRow> dataGrid1 = new ChangeTrackingArrayList<>("dataGrid1", this);

	/**
	 * inlineGrid 1
	 **/
	private List<ConditionsFixtureRow> inlineGrid1 = new ChangeTrackingArrayList<>("inlineGrid1", this);

	/**
	 * dataRepeater 1
	 **/
	private List<ConditionsFixtureRow> dataRepeater1 = new ChangeTrackingArrayList<>("dataRepeater1", this);

	/**
	 * checkBox 2
	 **/
	private Boolean checkBox2;

	/**
	 * colourPicker 2
	 **/
	private String colourPicker2;

	/**
	 * combo 2
	 **/
	private Combo2 combo2;

	/**
	 * content 2
	 **/
	private String content2;

	/**
	 * contentSignature 2
	 **/
	private String contentSignature2;

	/**
	 * geometry 2
	 **/
	private Geometry geometry2;

	/**
	 * geometryMap 2
	 **/
	private Geometry geometryMap2;

	/**
	 * html 2
	 **/
	private String html2;

	/**
	 * password 2
	 **/
	private String password2;

	/**
	 * progressBar 2
	 **/
	private Integer progressBar2;

	/**
	 * radio 2
	 **/
	private Radio2 radio2;

	/**
	 * richText 2
	 **/
	private String richText2;

	/**
	 * slider 2
	 **/
	private Integer slider2;

	/**
	 * spinner 2
	 **/
	private Integer spinner2;

	/**
	 * textArea 2
	 **/
	private String textArea2;

	/**
	 * textField 2
	 **/
	private String textField2;

	/**
	 * lookupDescription 2
	 **/
	private LookupDescription lookupDescription2 = null;

	/**
	 * zoomIn 2
	 **/
	private LookupDescription zoomIn2 = null;

	/**
	 * zoomInContainer 2
	 **/
	private LookupDescription zoomInContainer2 = null;

	/**
	 * comparison 2
	 **/
	private LookupDescription comparison2 = null;

	/**
	 * checkMembership 2
	 **/
	private List<LookupDescription> checkMembership2 = new ChangeTrackingArrayList<>("checkMembership2", this);

	/**
	 * listMembership 2
	 **/
	private List<LookupDescription> listMembership2 = new ChangeTrackingArrayList<>("listMembership2", this);

	/**
	 * dataGrid 2
	 **/
	private List<ConditionsFixtureRow> dataGrid2 = new ChangeTrackingArrayList<>("dataGrid2", this);

	/**
	 * inlineGrid 2
	 **/
	private List<ConditionsFixtureRow> inlineGrid2 = new ChangeTrackingArrayList<>("inlineGrid2", this);

	/**
	 * dataRepeater 2
	 **/
	private List<ConditionsFixtureRow> dataRepeater2 = new ChangeTrackingArrayList<>("dataRepeater2", this);

	/**
	 * checkBox 3
	 **/
	private Boolean checkBox3;

	/**
	 * colourPicker 3
	 **/
	private String colourPicker3;

	/**
	 * combo 3
	 **/
	private Combo3 combo3;

	/**
	 * content 3
	 **/
	private String content3;

	/**
	 * contentSignature 3
	 **/
	private String contentSignature3;

	/**
	 * geometry 3
	 **/
	private Geometry geometry3;

	/**
	 * geometryMap 3
	 **/
	private Geometry geometryMap3;

	/**
	 * html 3
	 **/
	private String html3;

	/**
	 * password 3
	 **/
	private String password3;

	/**
	 * progressBar 3
	 **/
	private Integer progressBar3;

	/**
	 * radio 3
	 **/
	private Radio3 radio3;

	/**
	 * richText 3
	 **/
	private String richText3;

	/**
	 * slider 3
	 **/
	private Integer slider3;

	/**
	 * spinner 3
	 **/
	private Integer spinner3;

	/**
	 * textArea 3
	 **/
	private String textArea3;

	/**
	 * textField 3
	 **/
	private String textField3;

	/**
	 * lookupDescription 3
	 **/
	private LookupDescription lookupDescription3 = null;

	/**
	 * zoomIn 3
	 **/
	private LookupDescription zoomIn3 = null;

	/**
	 * zoomInContainer 3
	 **/
	private LookupDescription zoomInContainer3 = null;

	/**
	 * comparison 3
	 **/
	private LookupDescription comparison3 = null;

	/**
	 * checkMembership 3
	 **/
	private List<LookupDescription> checkMembership3 = new ChangeTrackingArrayList<>("checkMembership3", this);

	/**
	 * listMembership 3
	 **/
	private List<LookupDescription> listMembership3 = new ChangeTrackingArrayList<>("listMembership3", this);

	/**
	 * dataGrid 3
	 **/
	private List<ConditionsFixtureRow> dataGrid3 = new ChangeTrackingArrayList<>("dataGrid3", this);

	/**
	 * inlineGrid 3
	 **/
	private List<ConditionsFixtureRow> inlineGrid3 = new ChangeTrackingArrayList<>("inlineGrid3", this);

	/**
	 * dataRepeater 3
	 **/
	private List<ConditionsFixtureRow> dataRepeater3 = new ChangeTrackingArrayList<>("dataRepeater3", this);

	/**
	 * checkBox 4
	 **/
	private Boolean checkBox4;

	/**
	 * colourPicker 4
	 **/
	private String colourPicker4;

	/**
	 * combo 4
	 **/
	private Combo4 combo4;

	/**
	 * content 4
	 **/
	private String content4;

	/**
	 * contentSignature 4
	 **/
	private String contentSignature4;

	/**
	 * geometry 4
	 **/
	private Geometry geometry4;

	/**
	 * geometryMap 4
	 **/
	private Geometry geometryMap4;

	/**
	 * html 4
	 **/
	private String html4;

	/**
	 * password 4
	 **/
	private String password4;

	/**
	 * progressBar 4
	 **/
	private Integer progressBar4;

	/**
	 * radio 4
	 **/
	private Radio4 radio4;

	/**
	 * richText 4
	 **/
	private String richText4;

	/**
	 * slider 4
	 **/
	private Integer slider4;

	/**
	 * spinner 4
	 **/
	private Integer spinner4;

	/**
	 * textArea 4
	 **/
	private String textArea4;

	/**
	 * textField 4
	 **/
	private String textField4;

	/**
	 * lookupDescription 4
	 **/
	private LookupDescription lookupDescription4 = null;

	/**
	 * zoomIn 4
	 **/
	private LookupDescription zoomIn4 = null;

	/**
	 * zoomInContainer 4
	 **/
	private LookupDescription zoomInContainer4 = null;

	/**
	 * comparison 4
	 **/
	private LookupDescription comparison4 = null;

	/**
	 * checkMembership 4
	 **/
	private List<LookupDescription> checkMembership4 = new ChangeTrackingArrayList<>("checkMembership4", this);

	/**
	 * listMembership 4
	 **/
	private List<LookupDescription> listMembership4 = new ChangeTrackingArrayList<>("listMembership4", this);

	/**
	 * dataGrid 4
	 **/
	private List<ConditionsFixtureRow> dataGrid4 = new ChangeTrackingArrayList<>("dataGrid4", this);

	/**
	 * inlineGrid 4
	 **/
	private List<ConditionsFixtureRow> inlineGrid4 = new ChangeTrackingArrayList<>("inlineGrid4", this);

	/**
	 * dataRepeater 4
	 **/
	private List<ConditionsFixtureRow> dataRepeater4 = new ChangeTrackingArrayList<>("dataRepeater4", this);

	@Override
	@XmlTransient
	public String getBizModule() {
		return ConditionsFixture.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return ConditionsFixture.DOCUMENT_NAME;
	}

	public static ConditionsFixture newInstance() {
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
			return org.skyve.util.Binder.formatMessage("Conditions Fixture", this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return "Unknown";
		}
	}

	/**
	 * {@link #show} accessor.
	 * @return	The value.
	 **/
	public Boolean getShow() {
		return show;
	}

	/**
	 * {@link #show} mutator.
	 * @param show	The new value.
	 **/
	@XmlElement
	public void setShow(Boolean show) {
		preset(showPropertyName, show);
		this.show = show;
	}

	/**
	 * {@link #allow} accessor.
	 * @return	The value.
	 **/
	public Boolean getAllow() {
		return allow;
	}

	/**
	 * {@link #allow} mutator.
	 * @param allow	The new value.
	 **/
	@XmlElement
	public void setAllow(Boolean allow) {
		preset(allowPropertyName, allow);
		this.allow = allow;
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
	 * {@link #nested} accessor.
	 * @return	The value.
	 **/
	public ConditionsFixture getNested() {
		return nested;
	}

	/**
	 * {@link #nested} mutator.
	 * @param nested	The new value.
	 **/
	@XmlElement
	public void setNested(ConditionsFixture nested) {
		if (this.nested != nested) {
			preset(nestedPropertyName, nested);
			this.nested = nested;
		}
	}

	/**
	 * {@link #eventTarget} accessor.
	 * @return	The value.
	 **/
	public Boolean getEventTarget() {
		return eventTarget;
	}

	/**
	 * {@link #eventTarget} mutator.
	 * @param eventTarget	The new value.
	 **/
	@XmlElement
	public void setEventTarget(Boolean eventTarget) {
		preset(eventTargetPropertyName, eventTarget);
		this.eventTarget = eventTarget;
	}

	/**
	 * {@link #checkBox1} accessor.
	 * @return	The value.
	 **/
	public Boolean getCheckBox1() {
		return checkBox1;
	}

	/**
	 * {@link #checkBox1} mutator.
	 * @param checkBox1	The new value.
	 **/
	@XmlElement
	public void setCheckBox1(Boolean checkBox1) {
		preset(checkBox1PropertyName, checkBox1);
		this.checkBox1 = checkBox1;
	}

	/**
	 * {@link #colourPicker1} accessor.
	 * @return	The value.
	 **/
	public String getColourPicker1() {
		return colourPicker1;
	}

	/**
	 * {@link #colourPicker1} mutator.
	 * @param colourPicker1	The new value.
	 **/
	@XmlElement
	public void setColourPicker1(String colourPicker1) {
		preset(colourPicker1PropertyName, colourPicker1);
		this.colourPicker1 = colourPicker1;
	}

	/**
	 * {@link #combo1} accessor.
	 * @return	The value.
	 **/
	public Combo1 getCombo1() {
		return combo1;
	}

	/**
	 * {@link #combo1} mutator.
	 * @param combo1	The new value.
	 **/
	@XmlElement
	public void setCombo1(Combo1 combo1) {
		preset(combo1PropertyName, combo1);
		this.combo1 = combo1;
	}

	/**
	 * {@link #content1} accessor.
	 * @return	The value.
	 **/
	public String getContent1() {
		return content1;
	}

	/**
	 * {@link #content1} mutator.
	 * @param content1	The new value.
	 **/
	@XmlElement
	public void setContent1(String content1) {
		preset(content1PropertyName, content1);
		this.content1 = content1;
	}

	/**
	 * {@link #contentSignature1} accessor.
	 * @return	The value.
	 **/
	public String getContentSignature1() {
		return contentSignature1;
	}

	/**
	 * {@link #contentSignature1} mutator.
	 * @param contentSignature1	The new value.
	 **/
	@XmlElement
	public void setContentSignature1(String contentSignature1) {
		preset(contentSignature1PropertyName, contentSignature1);
		this.contentSignature1 = contentSignature1;
	}

	/**
	 * {@link #geometry1} accessor.
	 * @return	The value.
	 **/
	public Geometry getGeometry1() {
		return geometry1;
	}

	/**
	 * {@link #geometry1} mutator.
	 * @param geometry1	The new value.
	 **/
	@XmlElement
	@XmlJavaTypeAdapter(GeometryMapper.class)
	public void setGeometry1(Geometry geometry1) {
		preset(geometry1PropertyName, geometry1);
		this.geometry1 = geometry1;
	}

	/**
	 * {@link #geometryMap1} accessor.
	 * @return	The value.
	 **/
	public Geometry getGeometryMap1() {
		return geometryMap1;
	}

	/**
	 * {@link #geometryMap1} mutator.
	 * @param geometryMap1	The new value.
	 **/
	@XmlElement
	@XmlJavaTypeAdapter(GeometryMapper.class)
	public void setGeometryMap1(Geometry geometryMap1) {
		preset(geometryMap1PropertyName, geometryMap1);
		this.geometryMap1 = geometryMap1;
	}

	/**
	 * {@link #html1} accessor.
	 * @return	The value.
	 **/
	public String getHtml1() {
		return html1;
	}

	/**
	 * {@link #html1} mutator.
	 * @param html1	The new value.
	 **/
	@XmlElement
	public void setHtml1(String html1) {
		preset(html1PropertyName, html1);
		this.html1 = html1;
	}

	/**
	 * {@link #password1} accessor.
	 * @return	The value.
	 **/
	public String getPassword1() {
		return password1;
	}

	/**
	 * {@link #password1} mutator.
	 * @param password1	The new value.
	 **/
	@XmlElement
	public void setPassword1(String password1) {
		preset(password1PropertyName, password1);
		this.password1 = password1;
	}

	/**
	 * {@link #progressBar1} accessor.
	 * @return	The value.
	 **/
	public Integer getProgressBar1() {
		return progressBar1;
	}

	/**
	 * {@link #progressBar1} mutator.
	 * @param progressBar1	The new value.
	 **/
	@XmlElement
	public void setProgressBar1(Integer progressBar1) {
		preset(progressBar1PropertyName, progressBar1);
		this.progressBar1 = progressBar1;
	}

	/**
	 * {@link #radio1} accessor.
	 * @return	The value.
	 **/
	public Radio1 getRadio1() {
		return radio1;
	}

	/**
	 * {@link #radio1} mutator.
	 * @param radio1	The new value.
	 **/
	@XmlElement
	public void setRadio1(Radio1 radio1) {
		preset(radio1PropertyName, radio1);
		this.radio1 = radio1;
	}

	/**
	 * {@link #richText1} accessor.
	 * @return	The value.
	 **/
	public String getRichText1() {
		return richText1;
	}

	/**
	 * {@link #richText1} mutator.
	 * @param richText1	The new value.
	 **/
	@XmlElement
	public void setRichText1(String richText1) {
		preset(richText1PropertyName, richText1);
		this.richText1 = richText1;
	}

	/**
	 * {@link #slider1} accessor.
	 * @return	The value.
	 **/
	public Integer getSlider1() {
		return slider1;
	}

	/**
	 * {@link #slider1} mutator.
	 * @param slider1	The new value.
	 **/
	@XmlElement
	public void setSlider1(Integer slider1) {
		preset(slider1PropertyName, slider1);
		this.slider1 = slider1;
	}

	/**
	 * {@link #spinner1} accessor.
	 * @return	The value.
	 **/
	public Integer getSpinner1() {
		return spinner1;
	}

	/**
	 * {@link #spinner1} mutator.
	 * @param spinner1	The new value.
	 **/
	@XmlElement
	public void setSpinner1(Integer spinner1) {
		preset(spinner1PropertyName, spinner1);
		this.spinner1 = spinner1;
	}

	/**
	 * {@link #textArea1} accessor.
	 * @return	The value.
	 **/
	public String getTextArea1() {
		return textArea1;
	}

	/**
	 * {@link #textArea1} mutator.
	 * @param textArea1	The new value.
	 **/
	@XmlElement
	public void setTextArea1(String textArea1) {
		preset(textArea1PropertyName, textArea1);
		this.textArea1 = textArea1;
	}

	/**
	 * {@link #textField1} accessor.
	 * @return	The value.
	 **/
	public String getTextField1() {
		return textField1;
	}

	/**
	 * {@link #textField1} mutator.
	 * @param textField1	The new value.
	 **/
	@XmlElement
	public void setTextField1(String textField1) {
		preset(textField1PropertyName, textField1);
		this.textField1 = textField1;
	}

	/**
	 * {@link #lookupDescription1} accessor.
	 * @return	The value.
	 **/
	public LookupDescription getLookupDescription1() {
		return lookupDescription1;
	}

	/**
	 * {@link #lookupDescription1} mutator.
	 * @param lookupDescription1	The new value.
	 **/
	@XmlElement
	public void setLookupDescription1(LookupDescription lookupDescription1) {
		if (this.lookupDescription1 != lookupDescription1) {
			preset(lookupDescription1PropertyName, lookupDescription1);
			this.lookupDescription1 = lookupDescription1;
		}
	}

	/**
	 * {@link #zoomIn1} accessor.
	 * @return	The value.
	 **/
	public LookupDescription getZoomIn1() {
		return zoomIn1;
	}

	/**
	 * {@link #zoomIn1} mutator.
	 * @param zoomIn1	The new value.
	 **/
	@XmlElement
	public void setZoomIn1(LookupDescription zoomIn1) {
		if (this.zoomIn1 != zoomIn1) {
			preset(zoomIn1PropertyName, zoomIn1);
			this.zoomIn1 = zoomIn1;
		}
	}

	/**
	 * {@link #zoomInContainer1} accessor.
	 * @return	The value.
	 **/
	public LookupDescription getZoomInContainer1() {
		return zoomInContainer1;
	}

	/**
	 * {@link #zoomInContainer1} mutator.
	 * @param zoomInContainer1	The new value.
	 **/
	@XmlElement
	public void setZoomInContainer1(LookupDescription zoomInContainer1) {
		if (this.zoomInContainer1 != zoomInContainer1) {
			preset(zoomInContainer1PropertyName, zoomInContainer1);
			this.zoomInContainer1 = zoomInContainer1;
		}
	}

	/**
	 * {@link #comparison1} accessor.
	 * @return	The value.
	 **/
	public LookupDescription getComparison1() {
		return comparison1;
	}

	/**
	 * {@link #comparison1} mutator.
	 * @param comparison1	The new value.
	 **/
	@XmlElement
	public void setComparison1(LookupDescription comparison1) {
		if (this.comparison1 != comparison1) {
			preset(comparison1PropertyName, comparison1);
			this.comparison1 = comparison1;
		}
	}

	/**
	 * {@link #checkMembership1} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<LookupDescription> getCheckMembership1() {
		return checkMembership1;
	}

	/**
	 * {@link #checkMembership1} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public LookupDescription getCheckMembership1ElementById(String bizId) {
		return getElementById(checkMembership1, bizId);
	}

	/**
	 * {@link #checkMembership1} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setCheckMembership1ElementById(String bizId, LookupDescription element) {
		setElementById(checkMembership1, element);
	}

	/**
	 * {@link #checkMembership1} add.
	 * @param element	The element to add.
	 **/
	public boolean addCheckMembership1Element(LookupDescription element) {
		return checkMembership1.add(element);
	}

	/**
	 * {@link #checkMembership1} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addCheckMembership1Element(int index, LookupDescription element) {
		checkMembership1.add(index, element);
	}

	/**
	 * {@link #checkMembership1} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeCheckMembership1Element(LookupDescription element) {
		return checkMembership1.remove(element);
	}

	/**
	 * {@link #checkMembership1} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public LookupDescription removeCheckMembership1Element(int index) {
		return checkMembership1.remove(index);
	}

	/**
	 * {@link #listMembership1} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<LookupDescription> getListMembership1() {
		return listMembership1;
	}

	/**
	 * {@link #listMembership1} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public LookupDescription getListMembership1ElementById(String bizId) {
		return getElementById(listMembership1, bizId);
	}

	/**
	 * {@link #listMembership1} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setListMembership1ElementById(String bizId, LookupDescription element) {
		setElementById(listMembership1, element);
	}

	/**
	 * {@link #listMembership1} add.
	 * @param element	The element to add.
	 **/
	public boolean addListMembership1Element(LookupDescription element) {
		return listMembership1.add(element);
	}

	/**
	 * {@link #listMembership1} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addListMembership1Element(int index, LookupDescription element) {
		listMembership1.add(index, element);
	}

	/**
	 * {@link #listMembership1} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeListMembership1Element(LookupDescription element) {
		return listMembership1.remove(element);
	}

	/**
	 * {@link #listMembership1} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public LookupDescription removeListMembership1Element(int index) {
		return listMembership1.remove(index);
	}

	/**
	 * {@link #dataGrid1} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<ConditionsFixtureRow> getDataGrid1() {
		return dataGrid1;
	}

	/**
	 * {@link #dataGrid1} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public ConditionsFixtureRow getDataGrid1ElementById(String bizId) {
		return getElementById(dataGrid1, bizId);
	}

	/**
	 * {@link #dataGrid1} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setDataGrid1ElementById(String bizId, ConditionsFixtureRow element) {
		setElementById(dataGrid1, element);
	}

	/**
	 * {@link #dataGrid1} add.
	 * @param element	The element to add.
	 **/
	public boolean addDataGrid1Element(ConditionsFixtureRow element) {
		return dataGrid1.add(element);
	}

	/**
	 * {@link #dataGrid1} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addDataGrid1Element(int index, ConditionsFixtureRow element) {
		dataGrid1.add(index, element);
	}

	/**
	 * {@link #dataGrid1} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeDataGrid1Element(ConditionsFixtureRow element) {
		return dataGrid1.remove(element);
	}

	/**
	 * {@link #dataGrid1} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public ConditionsFixtureRow removeDataGrid1Element(int index) {
		return dataGrid1.remove(index);
	}

	/**
	 * {@link #inlineGrid1} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<ConditionsFixtureRow> getInlineGrid1() {
		return inlineGrid1;
	}

	/**
	 * {@link #inlineGrid1} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public ConditionsFixtureRow getInlineGrid1ElementById(String bizId) {
		return getElementById(inlineGrid1, bizId);
	}

	/**
	 * {@link #inlineGrid1} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setInlineGrid1ElementById(String bizId, ConditionsFixtureRow element) {
		setElementById(inlineGrid1, element);
	}

	/**
	 * {@link #inlineGrid1} add.
	 * @param element	The element to add.
	 **/
	public boolean addInlineGrid1Element(ConditionsFixtureRow element) {
		return inlineGrid1.add(element);
	}

	/**
	 * {@link #inlineGrid1} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addInlineGrid1Element(int index, ConditionsFixtureRow element) {
		inlineGrid1.add(index, element);
	}

	/**
	 * {@link #inlineGrid1} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeInlineGrid1Element(ConditionsFixtureRow element) {
		return inlineGrid1.remove(element);
	}

	/**
	 * {@link #inlineGrid1} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public ConditionsFixtureRow removeInlineGrid1Element(int index) {
		return inlineGrid1.remove(index);
	}

	/**
	 * {@link #dataRepeater1} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<ConditionsFixtureRow> getDataRepeater1() {
		return dataRepeater1;
	}

	/**
	 * {@link #dataRepeater1} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public ConditionsFixtureRow getDataRepeater1ElementById(String bizId) {
		return getElementById(dataRepeater1, bizId);
	}

	/**
	 * {@link #dataRepeater1} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setDataRepeater1ElementById(String bizId, ConditionsFixtureRow element) {
		setElementById(dataRepeater1, element);
	}

	/**
	 * {@link #dataRepeater1} add.
	 * @param element	The element to add.
	 **/
	public boolean addDataRepeater1Element(ConditionsFixtureRow element) {
		return dataRepeater1.add(element);
	}

	/**
	 * {@link #dataRepeater1} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addDataRepeater1Element(int index, ConditionsFixtureRow element) {
		dataRepeater1.add(index, element);
	}

	/**
	 * {@link #dataRepeater1} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeDataRepeater1Element(ConditionsFixtureRow element) {
		return dataRepeater1.remove(element);
	}

	/**
	 * {@link #dataRepeater1} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public ConditionsFixtureRow removeDataRepeater1Element(int index) {
		return dataRepeater1.remove(index);
	}

	/**
	 * {@link #checkBox2} accessor.
	 * @return	The value.
	 **/
	public Boolean getCheckBox2() {
		return checkBox2;
	}

	/**
	 * {@link #checkBox2} mutator.
	 * @param checkBox2	The new value.
	 **/
	@XmlElement
	public void setCheckBox2(Boolean checkBox2) {
		preset(checkBox2PropertyName, checkBox2);
		this.checkBox2 = checkBox2;
	}

	/**
	 * {@link #colourPicker2} accessor.
	 * @return	The value.
	 **/
	public String getColourPicker2() {
		return colourPicker2;
	}

	/**
	 * {@link #colourPicker2} mutator.
	 * @param colourPicker2	The new value.
	 **/
	@XmlElement
	public void setColourPicker2(String colourPicker2) {
		preset(colourPicker2PropertyName, colourPicker2);
		this.colourPicker2 = colourPicker2;
	}

	/**
	 * {@link #combo2} accessor.
	 * @return	The value.
	 **/
	public Combo2 getCombo2() {
		return combo2;
	}

	/**
	 * {@link #combo2} mutator.
	 * @param combo2	The new value.
	 **/
	@XmlElement
	public void setCombo2(Combo2 combo2) {
		preset(combo2PropertyName, combo2);
		this.combo2 = combo2;
	}

	/**
	 * {@link #content2} accessor.
	 * @return	The value.
	 **/
	public String getContent2() {
		return content2;
	}

	/**
	 * {@link #content2} mutator.
	 * @param content2	The new value.
	 **/
	@XmlElement
	public void setContent2(String content2) {
		preset(content2PropertyName, content2);
		this.content2 = content2;
	}

	/**
	 * {@link #contentSignature2} accessor.
	 * @return	The value.
	 **/
	public String getContentSignature2() {
		return contentSignature2;
	}

	/**
	 * {@link #contentSignature2} mutator.
	 * @param contentSignature2	The new value.
	 **/
	@XmlElement
	public void setContentSignature2(String contentSignature2) {
		preset(contentSignature2PropertyName, contentSignature2);
		this.contentSignature2 = contentSignature2;
	}

	/**
	 * {@link #geometry2} accessor.
	 * @return	The value.
	 **/
	public Geometry getGeometry2() {
		return geometry2;
	}

	/**
	 * {@link #geometry2} mutator.
	 * @param geometry2	The new value.
	 **/
	@XmlElement
	@XmlJavaTypeAdapter(GeometryMapper.class)
	public void setGeometry2(Geometry geometry2) {
		preset(geometry2PropertyName, geometry2);
		this.geometry2 = geometry2;
	}

	/**
	 * {@link #geometryMap2} accessor.
	 * @return	The value.
	 **/
	public Geometry getGeometryMap2() {
		return geometryMap2;
	}

	/**
	 * {@link #geometryMap2} mutator.
	 * @param geometryMap2	The new value.
	 **/
	@XmlElement
	@XmlJavaTypeAdapter(GeometryMapper.class)
	public void setGeometryMap2(Geometry geometryMap2) {
		preset(geometryMap2PropertyName, geometryMap2);
		this.geometryMap2 = geometryMap2;
	}

	/**
	 * {@link #html2} accessor.
	 * @return	The value.
	 **/
	public String getHtml2() {
		return html2;
	}

	/**
	 * {@link #html2} mutator.
	 * @param html2	The new value.
	 **/
	@XmlElement
	public void setHtml2(String html2) {
		preset(html2PropertyName, html2);
		this.html2 = html2;
	}

	/**
	 * {@link #password2} accessor.
	 * @return	The value.
	 **/
	public String getPassword2() {
		return password2;
	}

	/**
	 * {@link #password2} mutator.
	 * @param password2	The new value.
	 **/
	@XmlElement
	public void setPassword2(String password2) {
		preset(password2PropertyName, password2);
		this.password2 = password2;
	}

	/**
	 * {@link #progressBar2} accessor.
	 * @return	The value.
	 **/
	public Integer getProgressBar2() {
		return progressBar2;
	}

	/**
	 * {@link #progressBar2} mutator.
	 * @param progressBar2	The new value.
	 **/
	@XmlElement
	public void setProgressBar2(Integer progressBar2) {
		preset(progressBar2PropertyName, progressBar2);
		this.progressBar2 = progressBar2;
	}

	/**
	 * {@link #radio2} accessor.
	 * @return	The value.
	 **/
	public Radio2 getRadio2() {
		return radio2;
	}

	/**
	 * {@link #radio2} mutator.
	 * @param radio2	The new value.
	 **/
	@XmlElement
	public void setRadio2(Radio2 radio2) {
		preset(radio2PropertyName, radio2);
		this.radio2 = radio2;
	}

	/**
	 * {@link #richText2} accessor.
	 * @return	The value.
	 **/
	public String getRichText2() {
		return richText2;
	}

	/**
	 * {@link #richText2} mutator.
	 * @param richText2	The new value.
	 **/
	@XmlElement
	public void setRichText2(String richText2) {
		preset(richText2PropertyName, richText2);
		this.richText2 = richText2;
	}

	/**
	 * {@link #slider2} accessor.
	 * @return	The value.
	 **/
	public Integer getSlider2() {
		return slider2;
	}

	/**
	 * {@link #slider2} mutator.
	 * @param slider2	The new value.
	 **/
	@XmlElement
	public void setSlider2(Integer slider2) {
		preset(slider2PropertyName, slider2);
		this.slider2 = slider2;
	}

	/**
	 * {@link #spinner2} accessor.
	 * @return	The value.
	 **/
	public Integer getSpinner2() {
		return spinner2;
	}

	/**
	 * {@link #spinner2} mutator.
	 * @param spinner2	The new value.
	 **/
	@XmlElement
	public void setSpinner2(Integer spinner2) {
		preset(spinner2PropertyName, spinner2);
		this.spinner2 = spinner2;
	}

	/**
	 * {@link #textArea2} accessor.
	 * @return	The value.
	 **/
	public String getTextArea2() {
		return textArea2;
	}

	/**
	 * {@link #textArea2} mutator.
	 * @param textArea2	The new value.
	 **/
	@XmlElement
	public void setTextArea2(String textArea2) {
		preset(textArea2PropertyName, textArea2);
		this.textArea2 = textArea2;
	}

	/**
	 * {@link #textField2} accessor.
	 * @return	The value.
	 **/
	public String getTextField2() {
		return textField2;
	}

	/**
	 * {@link #textField2} mutator.
	 * @param textField2	The new value.
	 **/
	@XmlElement
	public void setTextField2(String textField2) {
		preset(textField2PropertyName, textField2);
		this.textField2 = textField2;
	}

	/**
	 * {@link #lookupDescription2} accessor.
	 * @return	The value.
	 **/
	public LookupDescription getLookupDescription2() {
		return lookupDescription2;
	}

	/**
	 * {@link #lookupDescription2} mutator.
	 * @param lookupDescription2	The new value.
	 **/
	@XmlElement
	public void setLookupDescription2(LookupDescription lookupDescription2) {
		if (this.lookupDescription2 != lookupDescription2) {
			preset(lookupDescription2PropertyName, lookupDescription2);
			this.lookupDescription2 = lookupDescription2;
		}
	}

	/**
	 * {@link #zoomIn2} accessor.
	 * @return	The value.
	 **/
	public LookupDescription getZoomIn2() {
		return zoomIn2;
	}

	/**
	 * {@link #zoomIn2} mutator.
	 * @param zoomIn2	The new value.
	 **/
	@XmlElement
	public void setZoomIn2(LookupDescription zoomIn2) {
		if (this.zoomIn2 != zoomIn2) {
			preset(zoomIn2PropertyName, zoomIn2);
			this.zoomIn2 = zoomIn2;
		}
	}

	/**
	 * {@link #zoomInContainer2} accessor.
	 * @return	The value.
	 **/
	public LookupDescription getZoomInContainer2() {
		return zoomInContainer2;
	}

	/**
	 * {@link #zoomInContainer2} mutator.
	 * @param zoomInContainer2	The new value.
	 **/
	@XmlElement
	public void setZoomInContainer2(LookupDescription zoomInContainer2) {
		if (this.zoomInContainer2 != zoomInContainer2) {
			preset(zoomInContainer2PropertyName, zoomInContainer2);
			this.zoomInContainer2 = zoomInContainer2;
		}
	}

	/**
	 * {@link #comparison2} accessor.
	 * @return	The value.
	 **/
	public LookupDescription getComparison2() {
		return comparison2;
	}

	/**
	 * {@link #comparison2} mutator.
	 * @param comparison2	The new value.
	 **/
	@XmlElement
	public void setComparison2(LookupDescription comparison2) {
		if (this.comparison2 != comparison2) {
			preset(comparison2PropertyName, comparison2);
			this.comparison2 = comparison2;
		}
	}

	/**
	 * {@link #checkMembership2} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<LookupDescription> getCheckMembership2() {
		return checkMembership2;
	}

	/**
	 * {@link #checkMembership2} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public LookupDescription getCheckMembership2ElementById(String bizId) {
		return getElementById(checkMembership2, bizId);
	}

	/**
	 * {@link #checkMembership2} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setCheckMembership2ElementById(String bizId, LookupDescription element) {
		setElementById(checkMembership2, element);
	}

	/**
	 * {@link #checkMembership2} add.
	 * @param element	The element to add.
	 **/
	public boolean addCheckMembership2Element(LookupDescription element) {
		return checkMembership2.add(element);
	}

	/**
	 * {@link #checkMembership2} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addCheckMembership2Element(int index, LookupDescription element) {
		checkMembership2.add(index, element);
	}

	/**
	 * {@link #checkMembership2} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeCheckMembership2Element(LookupDescription element) {
		return checkMembership2.remove(element);
	}

	/**
	 * {@link #checkMembership2} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public LookupDescription removeCheckMembership2Element(int index) {
		return checkMembership2.remove(index);
	}

	/**
	 * {@link #listMembership2} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<LookupDescription> getListMembership2() {
		return listMembership2;
	}

	/**
	 * {@link #listMembership2} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public LookupDescription getListMembership2ElementById(String bizId) {
		return getElementById(listMembership2, bizId);
	}

	/**
	 * {@link #listMembership2} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setListMembership2ElementById(String bizId, LookupDescription element) {
		setElementById(listMembership2, element);
	}

	/**
	 * {@link #listMembership2} add.
	 * @param element	The element to add.
	 **/
	public boolean addListMembership2Element(LookupDescription element) {
		return listMembership2.add(element);
	}

	/**
	 * {@link #listMembership2} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addListMembership2Element(int index, LookupDescription element) {
		listMembership2.add(index, element);
	}

	/**
	 * {@link #listMembership2} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeListMembership2Element(LookupDescription element) {
		return listMembership2.remove(element);
	}

	/**
	 * {@link #listMembership2} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public LookupDescription removeListMembership2Element(int index) {
		return listMembership2.remove(index);
	}

	/**
	 * {@link #dataGrid2} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<ConditionsFixtureRow> getDataGrid2() {
		return dataGrid2;
	}

	/**
	 * {@link #dataGrid2} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public ConditionsFixtureRow getDataGrid2ElementById(String bizId) {
		return getElementById(dataGrid2, bizId);
	}

	/**
	 * {@link #dataGrid2} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setDataGrid2ElementById(String bizId, ConditionsFixtureRow element) {
		setElementById(dataGrid2, element);
	}

	/**
	 * {@link #dataGrid2} add.
	 * @param element	The element to add.
	 **/
	public boolean addDataGrid2Element(ConditionsFixtureRow element) {
		return dataGrid2.add(element);
	}

	/**
	 * {@link #dataGrid2} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addDataGrid2Element(int index, ConditionsFixtureRow element) {
		dataGrid2.add(index, element);
	}

	/**
	 * {@link #dataGrid2} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeDataGrid2Element(ConditionsFixtureRow element) {
		return dataGrid2.remove(element);
	}

	/**
	 * {@link #dataGrid2} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public ConditionsFixtureRow removeDataGrid2Element(int index) {
		return dataGrid2.remove(index);
	}

	/**
	 * {@link #inlineGrid2} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<ConditionsFixtureRow> getInlineGrid2() {
		return inlineGrid2;
	}

	/**
	 * {@link #inlineGrid2} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public ConditionsFixtureRow getInlineGrid2ElementById(String bizId) {
		return getElementById(inlineGrid2, bizId);
	}

	/**
	 * {@link #inlineGrid2} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setInlineGrid2ElementById(String bizId, ConditionsFixtureRow element) {
		setElementById(inlineGrid2, element);
	}

	/**
	 * {@link #inlineGrid2} add.
	 * @param element	The element to add.
	 **/
	public boolean addInlineGrid2Element(ConditionsFixtureRow element) {
		return inlineGrid2.add(element);
	}

	/**
	 * {@link #inlineGrid2} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addInlineGrid2Element(int index, ConditionsFixtureRow element) {
		inlineGrid2.add(index, element);
	}

	/**
	 * {@link #inlineGrid2} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeInlineGrid2Element(ConditionsFixtureRow element) {
		return inlineGrid2.remove(element);
	}

	/**
	 * {@link #inlineGrid2} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public ConditionsFixtureRow removeInlineGrid2Element(int index) {
		return inlineGrid2.remove(index);
	}

	/**
	 * {@link #dataRepeater2} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<ConditionsFixtureRow> getDataRepeater2() {
		return dataRepeater2;
	}

	/**
	 * {@link #dataRepeater2} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public ConditionsFixtureRow getDataRepeater2ElementById(String bizId) {
		return getElementById(dataRepeater2, bizId);
	}

	/**
	 * {@link #dataRepeater2} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setDataRepeater2ElementById(String bizId, ConditionsFixtureRow element) {
		setElementById(dataRepeater2, element);
	}

	/**
	 * {@link #dataRepeater2} add.
	 * @param element	The element to add.
	 **/
	public boolean addDataRepeater2Element(ConditionsFixtureRow element) {
		return dataRepeater2.add(element);
	}

	/**
	 * {@link #dataRepeater2} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addDataRepeater2Element(int index, ConditionsFixtureRow element) {
		dataRepeater2.add(index, element);
	}

	/**
	 * {@link #dataRepeater2} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeDataRepeater2Element(ConditionsFixtureRow element) {
		return dataRepeater2.remove(element);
	}

	/**
	 * {@link #dataRepeater2} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public ConditionsFixtureRow removeDataRepeater2Element(int index) {
		return dataRepeater2.remove(index);
	}

	/**
	 * {@link #checkBox3} accessor.
	 * @return	The value.
	 **/
	public Boolean getCheckBox3() {
		return checkBox3;
	}

	/**
	 * {@link #checkBox3} mutator.
	 * @param checkBox3	The new value.
	 **/
	@XmlElement
	public void setCheckBox3(Boolean checkBox3) {
		preset(checkBox3PropertyName, checkBox3);
		this.checkBox3 = checkBox3;
	}

	/**
	 * {@link #colourPicker3} accessor.
	 * @return	The value.
	 **/
	public String getColourPicker3() {
		return colourPicker3;
	}

	/**
	 * {@link #colourPicker3} mutator.
	 * @param colourPicker3	The new value.
	 **/
	@XmlElement
	public void setColourPicker3(String colourPicker3) {
		preset(colourPicker3PropertyName, colourPicker3);
		this.colourPicker3 = colourPicker3;
	}

	/**
	 * {@link #combo3} accessor.
	 * @return	The value.
	 **/
	public Combo3 getCombo3() {
		return combo3;
	}

	/**
	 * {@link #combo3} mutator.
	 * @param combo3	The new value.
	 **/
	@XmlElement
	public void setCombo3(Combo3 combo3) {
		preset(combo3PropertyName, combo3);
		this.combo3 = combo3;
	}

	/**
	 * {@link #content3} accessor.
	 * @return	The value.
	 **/
	public String getContent3() {
		return content3;
	}

	/**
	 * {@link #content3} mutator.
	 * @param content3	The new value.
	 **/
	@XmlElement
	public void setContent3(String content3) {
		preset(content3PropertyName, content3);
		this.content3 = content3;
	}

	/**
	 * {@link #contentSignature3} accessor.
	 * @return	The value.
	 **/
	public String getContentSignature3() {
		return contentSignature3;
	}

	/**
	 * {@link #contentSignature3} mutator.
	 * @param contentSignature3	The new value.
	 **/
	@XmlElement
	public void setContentSignature3(String contentSignature3) {
		preset(contentSignature3PropertyName, contentSignature3);
		this.contentSignature3 = contentSignature3;
	}

	/**
	 * {@link #geometry3} accessor.
	 * @return	The value.
	 **/
	public Geometry getGeometry3() {
		return geometry3;
	}

	/**
	 * {@link #geometry3} mutator.
	 * @param geometry3	The new value.
	 **/
	@XmlElement
	@XmlJavaTypeAdapter(GeometryMapper.class)
	public void setGeometry3(Geometry geometry3) {
		preset(geometry3PropertyName, geometry3);
		this.geometry3 = geometry3;
	}

	/**
	 * {@link #geometryMap3} accessor.
	 * @return	The value.
	 **/
	public Geometry getGeometryMap3() {
		return geometryMap3;
	}

	/**
	 * {@link #geometryMap3} mutator.
	 * @param geometryMap3	The new value.
	 **/
	@XmlElement
	@XmlJavaTypeAdapter(GeometryMapper.class)
	public void setGeometryMap3(Geometry geometryMap3) {
		preset(geometryMap3PropertyName, geometryMap3);
		this.geometryMap3 = geometryMap3;
	}

	/**
	 * {@link #html3} accessor.
	 * @return	The value.
	 **/
	public String getHtml3() {
		return html3;
	}

	/**
	 * {@link #html3} mutator.
	 * @param html3	The new value.
	 **/
	@XmlElement
	public void setHtml3(String html3) {
		preset(html3PropertyName, html3);
		this.html3 = html3;
	}

	/**
	 * {@link #password3} accessor.
	 * @return	The value.
	 **/
	public String getPassword3() {
		return password3;
	}

	/**
	 * {@link #password3} mutator.
	 * @param password3	The new value.
	 **/
	@XmlElement
	public void setPassword3(String password3) {
		preset(password3PropertyName, password3);
		this.password3 = password3;
	}

	/**
	 * {@link #progressBar3} accessor.
	 * @return	The value.
	 **/
	public Integer getProgressBar3() {
		return progressBar3;
	}

	/**
	 * {@link #progressBar3} mutator.
	 * @param progressBar3	The new value.
	 **/
	@XmlElement
	public void setProgressBar3(Integer progressBar3) {
		preset(progressBar3PropertyName, progressBar3);
		this.progressBar3 = progressBar3;
	}

	/**
	 * {@link #radio3} accessor.
	 * @return	The value.
	 **/
	public Radio3 getRadio3() {
		return radio3;
	}

	/**
	 * {@link #radio3} mutator.
	 * @param radio3	The new value.
	 **/
	@XmlElement
	public void setRadio3(Radio3 radio3) {
		preset(radio3PropertyName, radio3);
		this.radio3 = radio3;
	}

	/**
	 * {@link #richText3} accessor.
	 * @return	The value.
	 **/
	public String getRichText3() {
		return richText3;
	}

	/**
	 * {@link #richText3} mutator.
	 * @param richText3	The new value.
	 **/
	@XmlElement
	public void setRichText3(String richText3) {
		preset(richText3PropertyName, richText3);
		this.richText3 = richText3;
	}

	/**
	 * {@link #slider3} accessor.
	 * @return	The value.
	 **/
	public Integer getSlider3() {
		return slider3;
	}

	/**
	 * {@link #slider3} mutator.
	 * @param slider3	The new value.
	 **/
	@XmlElement
	public void setSlider3(Integer slider3) {
		preset(slider3PropertyName, slider3);
		this.slider3 = slider3;
	}

	/**
	 * {@link #spinner3} accessor.
	 * @return	The value.
	 **/
	public Integer getSpinner3() {
		return spinner3;
	}

	/**
	 * {@link #spinner3} mutator.
	 * @param spinner3	The new value.
	 **/
	@XmlElement
	public void setSpinner3(Integer spinner3) {
		preset(spinner3PropertyName, spinner3);
		this.spinner3 = spinner3;
	}

	/**
	 * {@link #textArea3} accessor.
	 * @return	The value.
	 **/
	public String getTextArea3() {
		return textArea3;
	}

	/**
	 * {@link #textArea3} mutator.
	 * @param textArea3	The new value.
	 **/
	@XmlElement
	public void setTextArea3(String textArea3) {
		preset(textArea3PropertyName, textArea3);
		this.textArea3 = textArea3;
	}

	/**
	 * {@link #textField3} accessor.
	 * @return	The value.
	 **/
	public String getTextField3() {
		return textField3;
	}

	/**
	 * {@link #textField3} mutator.
	 * @param textField3	The new value.
	 **/
	@XmlElement
	public void setTextField3(String textField3) {
		preset(textField3PropertyName, textField3);
		this.textField3 = textField3;
	}

	/**
	 * {@link #lookupDescription3} accessor.
	 * @return	The value.
	 **/
	public LookupDescription getLookupDescription3() {
		return lookupDescription3;
	}

	/**
	 * {@link #lookupDescription3} mutator.
	 * @param lookupDescription3	The new value.
	 **/
	@XmlElement
	public void setLookupDescription3(LookupDescription lookupDescription3) {
		if (this.lookupDescription3 != lookupDescription3) {
			preset(lookupDescription3PropertyName, lookupDescription3);
			this.lookupDescription3 = lookupDescription3;
		}
	}

	/**
	 * {@link #zoomIn3} accessor.
	 * @return	The value.
	 **/
	public LookupDescription getZoomIn3() {
		return zoomIn3;
	}

	/**
	 * {@link #zoomIn3} mutator.
	 * @param zoomIn3	The new value.
	 **/
	@XmlElement
	public void setZoomIn3(LookupDescription zoomIn3) {
		if (this.zoomIn3 != zoomIn3) {
			preset(zoomIn3PropertyName, zoomIn3);
			this.zoomIn3 = zoomIn3;
		}
	}

	/**
	 * {@link #zoomInContainer3} accessor.
	 * @return	The value.
	 **/
	public LookupDescription getZoomInContainer3() {
		return zoomInContainer3;
	}

	/**
	 * {@link #zoomInContainer3} mutator.
	 * @param zoomInContainer3	The new value.
	 **/
	@XmlElement
	public void setZoomInContainer3(LookupDescription zoomInContainer3) {
		if (this.zoomInContainer3 != zoomInContainer3) {
			preset(zoomInContainer3PropertyName, zoomInContainer3);
			this.zoomInContainer3 = zoomInContainer3;
		}
	}

	/**
	 * {@link #comparison3} accessor.
	 * @return	The value.
	 **/
	public LookupDescription getComparison3() {
		return comparison3;
	}

	/**
	 * {@link #comparison3} mutator.
	 * @param comparison3	The new value.
	 **/
	@XmlElement
	public void setComparison3(LookupDescription comparison3) {
		if (this.comparison3 != comparison3) {
			preset(comparison3PropertyName, comparison3);
			this.comparison3 = comparison3;
		}
	}

	/**
	 * {@link #checkMembership3} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<LookupDescription> getCheckMembership3() {
		return checkMembership3;
	}

	/**
	 * {@link #checkMembership3} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public LookupDescription getCheckMembership3ElementById(String bizId) {
		return getElementById(checkMembership3, bizId);
	}

	/**
	 * {@link #checkMembership3} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setCheckMembership3ElementById(String bizId, LookupDescription element) {
		setElementById(checkMembership3, element);
	}

	/**
	 * {@link #checkMembership3} add.
	 * @param element	The element to add.
	 **/
	public boolean addCheckMembership3Element(LookupDescription element) {
		return checkMembership3.add(element);
	}

	/**
	 * {@link #checkMembership3} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addCheckMembership3Element(int index, LookupDescription element) {
		checkMembership3.add(index, element);
	}

	/**
	 * {@link #checkMembership3} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeCheckMembership3Element(LookupDescription element) {
		return checkMembership3.remove(element);
	}

	/**
	 * {@link #checkMembership3} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public LookupDescription removeCheckMembership3Element(int index) {
		return checkMembership3.remove(index);
	}

	/**
	 * {@link #listMembership3} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<LookupDescription> getListMembership3() {
		return listMembership3;
	}

	/**
	 * {@link #listMembership3} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public LookupDescription getListMembership3ElementById(String bizId) {
		return getElementById(listMembership3, bizId);
	}

	/**
	 * {@link #listMembership3} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setListMembership3ElementById(String bizId, LookupDescription element) {
		setElementById(listMembership3, element);
	}

	/**
	 * {@link #listMembership3} add.
	 * @param element	The element to add.
	 **/
	public boolean addListMembership3Element(LookupDescription element) {
		return listMembership3.add(element);
	}

	/**
	 * {@link #listMembership3} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addListMembership3Element(int index, LookupDescription element) {
		listMembership3.add(index, element);
	}

	/**
	 * {@link #listMembership3} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeListMembership3Element(LookupDescription element) {
		return listMembership3.remove(element);
	}

	/**
	 * {@link #listMembership3} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public LookupDescription removeListMembership3Element(int index) {
		return listMembership3.remove(index);
	}

	/**
	 * {@link #dataGrid3} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<ConditionsFixtureRow> getDataGrid3() {
		return dataGrid3;
	}

	/**
	 * {@link #dataGrid3} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public ConditionsFixtureRow getDataGrid3ElementById(String bizId) {
		return getElementById(dataGrid3, bizId);
	}

	/**
	 * {@link #dataGrid3} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setDataGrid3ElementById(String bizId, ConditionsFixtureRow element) {
		setElementById(dataGrid3, element);
	}

	/**
	 * {@link #dataGrid3} add.
	 * @param element	The element to add.
	 **/
	public boolean addDataGrid3Element(ConditionsFixtureRow element) {
		return dataGrid3.add(element);
	}

	/**
	 * {@link #dataGrid3} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addDataGrid3Element(int index, ConditionsFixtureRow element) {
		dataGrid3.add(index, element);
	}

	/**
	 * {@link #dataGrid3} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeDataGrid3Element(ConditionsFixtureRow element) {
		return dataGrid3.remove(element);
	}

	/**
	 * {@link #dataGrid3} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public ConditionsFixtureRow removeDataGrid3Element(int index) {
		return dataGrid3.remove(index);
	}

	/**
	 * {@link #inlineGrid3} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<ConditionsFixtureRow> getInlineGrid3() {
		return inlineGrid3;
	}

	/**
	 * {@link #inlineGrid3} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public ConditionsFixtureRow getInlineGrid3ElementById(String bizId) {
		return getElementById(inlineGrid3, bizId);
	}

	/**
	 * {@link #inlineGrid3} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setInlineGrid3ElementById(String bizId, ConditionsFixtureRow element) {
		setElementById(inlineGrid3, element);
	}

	/**
	 * {@link #inlineGrid3} add.
	 * @param element	The element to add.
	 **/
	public boolean addInlineGrid3Element(ConditionsFixtureRow element) {
		return inlineGrid3.add(element);
	}

	/**
	 * {@link #inlineGrid3} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addInlineGrid3Element(int index, ConditionsFixtureRow element) {
		inlineGrid3.add(index, element);
	}

	/**
	 * {@link #inlineGrid3} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeInlineGrid3Element(ConditionsFixtureRow element) {
		return inlineGrid3.remove(element);
	}

	/**
	 * {@link #inlineGrid3} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public ConditionsFixtureRow removeInlineGrid3Element(int index) {
		return inlineGrid3.remove(index);
	}

	/**
	 * {@link #dataRepeater3} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<ConditionsFixtureRow> getDataRepeater3() {
		return dataRepeater3;
	}

	/**
	 * {@link #dataRepeater3} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public ConditionsFixtureRow getDataRepeater3ElementById(String bizId) {
		return getElementById(dataRepeater3, bizId);
	}

	/**
	 * {@link #dataRepeater3} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setDataRepeater3ElementById(String bizId, ConditionsFixtureRow element) {
		setElementById(dataRepeater3, element);
	}

	/**
	 * {@link #dataRepeater3} add.
	 * @param element	The element to add.
	 **/
	public boolean addDataRepeater3Element(ConditionsFixtureRow element) {
		return dataRepeater3.add(element);
	}

	/**
	 * {@link #dataRepeater3} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addDataRepeater3Element(int index, ConditionsFixtureRow element) {
		dataRepeater3.add(index, element);
	}

	/**
	 * {@link #dataRepeater3} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeDataRepeater3Element(ConditionsFixtureRow element) {
		return dataRepeater3.remove(element);
	}

	/**
	 * {@link #dataRepeater3} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public ConditionsFixtureRow removeDataRepeater3Element(int index) {
		return dataRepeater3.remove(index);
	}

	/**
	 * {@link #checkBox4} accessor.
	 * @return	The value.
	 **/
	public Boolean getCheckBox4() {
		return checkBox4;
	}

	/**
	 * {@link #checkBox4} mutator.
	 * @param checkBox4	The new value.
	 **/
	@XmlElement
	public void setCheckBox4(Boolean checkBox4) {
		preset(checkBox4PropertyName, checkBox4);
		this.checkBox4 = checkBox4;
	}

	/**
	 * {@link #colourPicker4} accessor.
	 * @return	The value.
	 **/
	public String getColourPicker4() {
		return colourPicker4;
	}

	/**
	 * {@link #colourPicker4} mutator.
	 * @param colourPicker4	The new value.
	 **/
	@XmlElement
	public void setColourPicker4(String colourPicker4) {
		preset(colourPicker4PropertyName, colourPicker4);
		this.colourPicker4 = colourPicker4;
	}

	/**
	 * {@link #combo4} accessor.
	 * @return	The value.
	 **/
	public Combo4 getCombo4() {
		return combo4;
	}

	/**
	 * {@link #combo4} mutator.
	 * @param combo4	The new value.
	 **/
	@XmlElement
	public void setCombo4(Combo4 combo4) {
		preset(combo4PropertyName, combo4);
		this.combo4 = combo4;
	}

	/**
	 * {@link #content4} accessor.
	 * @return	The value.
	 **/
	public String getContent4() {
		return content4;
	}

	/**
	 * {@link #content4} mutator.
	 * @param content4	The new value.
	 **/
	@XmlElement
	public void setContent4(String content4) {
		preset(content4PropertyName, content4);
		this.content4 = content4;
	}

	/**
	 * {@link #contentSignature4} accessor.
	 * @return	The value.
	 **/
	public String getContentSignature4() {
		return contentSignature4;
	}

	/**
	 * {@link #contentSignature4} mutator.
	 * @param contentSignature4	The new value.
	 **/
	@XmlElement
	public void setContentSignature4(String contentSignature4) {
		preset(contentSignature4PropertyName, contentSignature4);
		this.contentSignature4 = contentSignature4;
	}

	/**
	 * {@link #geometry4} accessor.
	 * @return	The value.
	 **/
	public Geometry getGeometry4() {
		return geometry4;
	}

	/**
	 * {@link #geometry4} mutator.
	 * @param geometry4	The new value.
	 **/
	@XmlElement
	@XmlJavaTypeAdapter(GeometryMapper.class)
	public void setGeometry4(Geometry geometry4) {
		preset(geometry4PropertyName, geometry4);
		this.geometry4 = geometry4;
	}

	/**
	 * {@link #geometryMap4} accessor.
	 * @return	The value.
	 **/
	public Geometry getGeometryMap4() {
		return geometryMap4;
	}

	/**
	 * {@link #geometryMap4} mutator.
	 * @param geometryMap4	The new value.
	 **/
	@XmlElement
	@XmlJavaTypeAdapter(GeometryMapper.class)
	public void setGeometryMap4(Geometry geometryMap4) {
		preset(geometryMap4PropertyName, geometryMap4);
		this.geometryMap4 = geometryMap4;
	}

	/**
	 * {@link #html4} accessor.
	 * @return	The value.
	 **/
	public String getHtml4() {
		return html4;
	}

	/**
	 * {@link #html4} mutator.
	 * @param html4	The new value.
	 **/
	@XmlElement
	public void setHtml4(String html4) {
		preset(html4PropertyName, html4);
		this.html4 = html4;
	}

	/**
	 * {@link #password4} accessor.
	 * @return	The value.
	 **/
	public String getPassword4() {
		return password4;
	}

	/**
	 * {@link #password4} mutator.
	 * @param password4	The new value.
	 **/
	@XmlElement
	public void setPassword4(String password4) {
		preset(password4PropertyName, password4);
		this.password4 = password4;
	}

	/**
	 * {@link #progressBar4} accessor.
	 * @return	The value.
	 **/
	public Integer getProgressBar4() {
		return progressBar4;
	}

	/**
	 * {@link #progressBar4} mutator.
	 * @param progressBar4	The new value.
	 **/
	@XmlElement
	public void setProgressBar4(Integer progressBar4) {
		preset(progressBar4PropertyName, progressBar4);
		this.progressBar4 = progressBar4;
	}

	/**
	 * {@link #radio4} accessor.
	 * @return	The value.
	 **/
	public Radio4 getRadio4() {
		return radio4;
	}

	/**
	 * {@link #radio4} mutator.
	 * @param radio4	The new value.
	 **/
	@XmlElement
	public void setRadio4(Radio4 radio4) {
		preset(radio4PropertyName, radio4);
		this.radio4 = radio4;
	}

	/**
	 * {@link #richText4} accessor.
	 * @return	The value.
	 **/
	public String getRichText4() {
		return richText4;
	}

	/**
	 * {@link #richText4} mutator.
	 * @param richText4	The new value.
	 **/
	@XmlElement
	public void setRichText4(String richText4) {
		preset(richText4PropertyName, richText4);
		this.richText4 = richText4;
	}

	/**
	 * {@link #slider4} accessor.
	 * @return	The value.
	 **/
	public Integer getSlider4() {
		return slider4;
	}

	/**
	 * {@link #slider4} mutator.
	 * @param slider4	The new value.
	 **/
	@XmlElement
	public void setSlider4(Integer slider4) {
		preset(slider4PropertyName, slider4);
		this.slider4 = slider4;
	}

	/**
	 * {@link #spinner4} accessor.
	 * @return	The value.
	 **/
	public Integer getSpinner4() {
		return spinner4;
	}

	/**
	 * {@link #spinner4} mutator.
	 * @param spinner4	The new value.
	 **/
	@XmlElement
	public void setSpinner4(Integer spinner4) {
		preset(spinner4PropertyName, spinner4);
		this.spinner4 = spinner4;
	}

	/**
	 * {@link #textArea4} accessor.
	 * @return	The value.
	 **/
	public String getTextArea4() {
		return textArea4;
	}

	/**
	 * {@link #textArea4} mutator.
	 * @param textArea4	The new value.
	 **/
	@XmlElement
	public void setTextArea4(String textArea4) {
		preset(textArea4PropertyName, textArea4);
		this.textArea4 = textArea4;
	}

	/**
	 * {@link #textField4} accessor.
	 * @return	The value.
	 **/
	public String getTextField4() {
		return textField4;
	}

	/**
	 * {@link #textField4} mutator.
	 * @param textField4	The new value.
	 **/
	@XmlElement
	public void setTextField4(String textField4) {
		preset(textField4PropertyName, textField4);
		this.textField4 = textField4;
	}

	/**
	 * {@link #lookupDescription4} accessor.
	 * @return	The value.
	 **/
	public LookupDescription getLookupDescription4() {
		return lookupDescription4;
	}

	/**
	 * {@link #lookupDescription4} mutator.
	 * @param lookupDescription4	The new value.
	 **/
	@XmlElement
	public void setLookupDescription4(LookupDescription lookupDescription4) {
		if (this.lookupDescription4 != lookupDescription4) {
			preset(lookupDescription4PropertyName, lookupDescription4);
			this.lookupDescription4 = lookupDescription4;
		}
	}

	/**
	 * {@link #zoomIn4} accessor.
	 * @return	The value.
	 **/
	public LookupDescription getZoomIn4() {
		return zoomIn4;
	}

	/**
	 * {@link #zoomIn4} mutator.
	 * @param zoomIn4	The new value.
	 **/
	@XmlElement
	public void setZoomIn4(LookupDescription zoomIn4) {
		if (this.zoomIn4 != zoomIn4) {
			preset(zoomIn4PropertyName, zoomIn4);
			this.zoomIn4 = zoomIn4;
		}
	}

	/**
	 * {@link #zoomInContainer4} accessor.
	 * @return	The value.
	 **/
	public LookupDescription getZoomInContainer4() {
		return zoomInContainer4;
	}

	/**
	 * {@link #zoomInContainer4} mutator.
	 * @param zoomInContainer4	The new value.
	 **/
	@XmlElement
	public void setZoomInContainer4(LookupDescription zoomInContainer4) {
		if (this.zoomInContainer4 != zoomInContainer4) {
			preset(zoomInContainer4PropertyName, zoomInContainer4);
			this.zoomInContainer4 = zoomInContainer4;
		}
	}

	/**
	 * {@link #comparison4} accessor.
	 * @return	The value.
	 **/
	public LookupDescription getComparison4() {
		return comparison4;
	}

	/**
	 * {@link #comparison4} mutator.
	 * @param comparison4	The new value.
	 **/
	@XmlElement
	public void setComparison4(LookupDescription comparison4) {
		if (this.comparison4 != comparison4) {
			preset(comparison4PropertyName, comparison4);
			this.comparison4 = comparison4;
		}
	}

	/**
	 * {@link #checkMembership4} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<LookupDescription> getCheckMembership4() {
		return checkMembership4;
	}

	/**
	 * {@link #checkMembership4} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public LookupDescription getCheckMembership4ElementById(String bizId) {
		return getElementById(checkMembership4, bizId);
	}

	/**
	 * {@link #checkMembership4} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setCheckMembership4ElementById(String bizId, LookupDescription element) {
		setElementById(checkMembership4, element);
	}

	/**
	 * {@link #checkMembership4} add.
	 * @param element	The element to add.
	 **/
	public boolean addCheckMembership4Element(LookupDescription element) {
		return checkMembership4.add(element);
	}

	/**
	 * {@link #checkMembership4} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addCheckMembership4Element(int index, LookupDescription element) {
		checkMembership4.add(index, element);
	}

	/**
	 * {@link #checkMembership4} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeCheckMembership4Element(LookupDescription element) {
		return checkMembership4.remove(element);
	}

	/**
	 * {@link #checkMembership4} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public LookupDescription removeCheckMembership4Element(int index) {
		return checkMembership4.remove(index);
	}

	/**
	 * {@link #listMembership4} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<LookupDescription> getListMembership4() {
		return listMembership4;
	}

	/**
	 * {@link #listMembership4} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public LookupDescription getListMembership4ElementById(String bizId) {
		return getElementById(listMembership4, bizId);
	}

	/**
	 * {@link #listMembership4} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setListMembership4ElementById(String bizId, LookupDescription element) {
		setElementById(listMembership4, element);
	}

	/**
	 * {@link #listMembership4} add.
	 * @param element	The element to add.
	 **/
	public boolean addListMembership4Element(LookupDescription element) {
		return listMembership4.add(element);
	}

	/**
	 * {@link #listMembership4} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addListMembership4Element(int index, LookupDescription element) {
		listMembership4.add(index, element);
	}

	/**
	 * {@link #listMembership4} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeListMembership4Element(LookupDescription element) {
		return listMembership4.remove(element);
	}

	/**
	 * {@link #listMembership4} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public LookupDescription removeListMembership4Element(int index) {
		return listMembership4.remove(index);
	}

	/**
	 * {@link #dataGrid4} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<ConditionsFixtureRow> getDataGrid4() {
		return dataGrid4;
	}

	/**
	 * {@link #dataGrid4} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public ConditionsFixtureRow getDataGrid4ElementById(String bizId) {
		return getElementById(dataGrid4, bizId);
	}

	/**
	 * {@link #dataGrid4} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setDataGrid4ElementById(String bizId, ConditionsFixtureRow element) {
		setElementById(dataGrid4, element);
	}

	/**
	 * {@link #dataGrid4} add.
	 * @param element	The element to add.
	 **/
	public boolean addDataGrid4Element(ConditionsFixtureRow element) {
		return dataGrid4.add(element);
	}

	/**
	 * {@link #dataGrid4} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addDataGrid4Element(int index, ConditionsFixtureRow element) {
		dataGrid4.add(index, element);
	}

	/**
	 * {@link #dataGrid4} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeDataGrid4Element(ConditionsFixtureRow element) {
		return dataGrid4.remove(element);
	}

	/**
	 * {@link #dataGrid4} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public ConditionsFixtureRow removeDataGrid4Element(int index) {
		return dataGrid4.remove(index);
	}

	/**
	 * {@link #inlineGrid4} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<ConditionsFixtureRow> getInlineGrid4() {
		return inlineGrid4;
	}

	/**
	 * {@link #inlineGrid4} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public ConditionsFixtureRow getInlineGrid4ElementById(String bizId) {
		return getElementById(inlineGrid4, bizId);
	}

	/**
	 * {@link #inlineGrid4} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setInlineGrid4ElementById(String bizId, ConditionsFixtureRow element) {
		setElementById(inlineGrid4, element);
	}

	/**
	 * {@link #inlineGrid4} add.
	 * @param element	The element to add.
	 **/
	public boolean addInlineGrid4Element(ConditionsFixtureRow element) {
		return inlineGrid4.add(element);
	}

	/**
	 * {@link #inlineGrid4} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addInlineGrid4Element(int index, ConditionsFixtureRow element) {
		inlineGrid4.add(index, element);
	}

	/**
	 * {@link #inlineGrid4} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeInlineGrid4Element(ConditionsFixtureRow element) {
		return inlineGrid4.remove(element);
	}

	/**
	 * {@link #inlineGrid4} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public ConditionsFixtureRow removeInlineGrid4Element(int index) {
		return inlineGrid4.remove(index);
	}

	/**
	 * {@link #dataRepeater4} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<ConditionsFixtureRow> getDataRepeater4() {
		return dataRepeater4;
	}

	/**
	 * {@link #dataRepeater4} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public ConditionsFixtureRow getDataRepeater4ElementById(String bizId) {
		return getElementById(dataRepeater4, bizId);
	}

	/**
	 * {@link #dataRepeater4} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setDataRepeater4ElementById(String bizId, ConditionsFixtureRow element) {
		setElementById(dataRepeater4, element);
	}

	/**
	 * {@link #dataRepeater4} add.
	 * @param element	The element to add.
	 **/
	public boolean addDataRepeater4Element(ConditionsFixtureRow element) {
		return dataRepeater4.add(element);
	}

	/**
	 * {@link #dataRepeater4} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addDataRepeater4Element(int index, ConditionsFixtureRow element) {
		dataRepeater4.add(index, element);
	}

	/**
	 * {@link #dataRepeater4} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeDataRepeater4Element(ConditionsFixtureRow element) {
		return dataRepeater4.remove(element);
	}

	/**
	 * {@link #dataRepeater4} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public ConditionsFixtureRow removeDataRepeater4Element(int index) {
		return dataRepeater4.remove(index);
	}

	/**
	 * Allow is set; a Java expression.
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isAllowed() {
		return (Boolean.TRUE.equals(getAllow()));
	}

	/**
	 * {@link #isAllowed} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotAllowed() {
		return (! isAllowed());
	}

	/**
	 * Allow is set; a Skyve expression.
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isAllowedByExpression() {
		return Boolean.TRUE.equals(ExpressionEvaluator.evaluate("{el:bean.allow}", this));
	}

	/**
	 * {@link #isAllowedByExpression} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotAllowedByExpression() {
		return (! isAllowedByExpression());
	}

	/**
	 * Show is set; a Java expression.
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isShown() {
		return (Boolean.TRUE.equals(getShow()));
	}

	/**
	 * {@link #isShown} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotShown() {
		return (! isShown());
	}

	/**
	 * Show is set; a Skyve expression.
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isShownByExpression() {
		return Boolean.TRUE.equals(ExpressionEvaluator.evaluate("{el:bean.show}", this));
	}

	/**
	 * {@link #isShownByExpression} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotShownByExpression() {
		return (! isShownByExpression());
	}
}
