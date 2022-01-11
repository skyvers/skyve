package modules.kitchensink.domain;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlSchemaType;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import org.locationtech.jts.geom.Geometry;
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
import org.skyve.impl.domain.AbstractTransientBean;
import org.skyve.impl.domain.ChangeTrackingArrayList;
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
 * Kitchen Sink
 * 
 * @depend - - - Combo
 * @depend - - - Radio
 * @navcomposed 1 containerGrid 0..n ContainerGrid
 * @navcomposed 1 orderedGrid 0..n OrderedGrid
 * @navcomposed 1 dataRepeater 0..n DataRepeater
 * @navcomposed 1 inlineGrid 0..n InlineGrid
 * @navhas n lookupDescription 0..1 LookupDescription
 * @stereotype "transient"
 */
@XmlType
@XmlRootElement
public class KitchenSink extends AbstractTransientBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "kitchensink";

	/** @hidden */
	public static final String DOCUMENT_NAME = "KitchenSink";

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
	public static final String comboPropertyName = "combo";

	/** @hidden */
	public static final String radioPropertyName = "radio";

	/** @hidden */
	public static final String geometryPropertyName = "geometry";

	/** @hidden */
	public static final String idPropertyName = "id";

	/** @hidden */
	public static final String normalIntegerPropertyName = "normalInteger";

	/** @hidden */
	public static final String spinnerPropertyName = "spinner";

	/** @hidden */
	public static final String sliderPropertyName = "slider";

	/** @hidden */
	public static final String longIntegerPropertyName = "longInteger";

	/** @hidden */
	public static final String htmlPropertyName = "html";

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
	public static final String contentLinkPropertyName = "contentLink";

	/** @hidden */
	public static final String contentImagePropertyName = "contentImage";

	/** @hidden */
	public static final String contentSignaturePropertyName = "contentSignature";

	/** @hidden */
	public static final String lookupDescriptionPropertyName = "lookupDescription";

	/** @hidden */
	public static final String containerGridPropertyName = "containerGrid";

	/** @hidden */
	public static final String inlineGridPropertyName = "inlineGrid";

	/** @hidden */
	public static final String orderedGridPropertyName = "orderedGrid";

	/** @hidden */
	public static final String dataRepeaterPropertyName = "dataRepeater";

	/**
	 * Combo
	 **/
	@XmlEnum
	public static enum Combo implements Enumeration {
		one("one", "one"),
		two("two", "two"),
		three("three", "three");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues;

		private Combo(String code, String description) {
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

		public static Combo fromCode(String code) {
			Combo result = null;

			for (Combo value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static Combo fromDescription(String description) {
			Combo result = null;

			for (Combo value : values()) {
				if (value.description.equals(description)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static List<DomainValue> toDomainValues() {
			if (domainValues == null) {
				Combo[] values = values();
				domainValues = new ArrayList<>(values.length);
				for (Combo value : values) {
					domainValues.add(value.domainValue);
				}
			}

			return domainValues;
		}
	}

	/**
	 * Radio
	 **/
	@XmlEnum
	public static enum Radio implements Enumeration {
		one("one", "one"),
		two("two", "two"),
		three("three", "three");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues;

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
		public String toDescription() {
			return description;
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

		public static Radio fromDescription(String description) {
			Radio result = null;

			for (Radio value : values()) {
				if (value.description.equals(description)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static List<DomainValue> toDomainValues() {
			if (domainValues == null) {
				Radio[] values = values();
				domainValues = new ArrayList<>(values.length);
				for (Radio value : values) {
					domainValues.add(value.domainValue);
				}
			}

			return domainValues;
		}
	}

	/**
	 * Boolean Flag
	 **/
	private Boolean booleanFlag;

	/**
	 * Colour
	 **/
	private String colour;

	/**
	 * Date
	 **/
	private DateOnly date;

	/**
	 * Date Time
	 **/
	private DateTime dateTime;

	/**
	 * Decimal 10
	 **/
	private Decimal10 decimal10;

	/**
	 * Decimal 2
	 **/
	private Decimal2 decimal2;

	/**
	 * Decimal 5
	 **/
	private Decimal5 decimal5;

	/**
	 * Combo
	 **/
	private Combo combo;

	/**
	 * Radio
	 **/
	private Radio radio;

	/**
	 * Geometry
	 **/
	private Geometry geometry;

	/**
	 * Id
	 **/
	private String id;

	/**
	 * Integer
	 **/
	private Integer normalInteger;

	/**
	 * Spinner
	 **/
	private Integer spinner;

	/**
	 * Slider
	 **/
	private Integer slider;

	/**
	 * Long Integer
	 **/
	private Long longInteger;

	/**
	 * HTML
	 **/
	private String html;

	/**
	 * Markup
	 **/
	private String markup;

	/**
	 * Memo
	 **/
	private String memo;

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

	/**
	 * Content Link
	 **/
	private String contentLink;

	/**
	 * Content Image
	 **/
	private String contentImage;

	/**
	 * Content Signature
	 **/
	private String contentSignature;

	/**
	 * Lookup Description
	 **/
	private LookupDescription lookupDescription = null;

	/**
	 * Container
	 **/
	private List<ContainerGrid> containerGrid = new ChangeTrackingArrayList<>("containerGrid", this);

	/**
	 * Inline
	 **/
	private List<InlineGrid> inlineGrid = new ChangeTrackingArrayList<>("inlineGrid", this);

	/**
	 * Ordered
	 **/
	private List<OrderedGrid> orderedGrid = new ChangeTrackingArrayList<>("orderedGrid", this);

	/**
	 * Data Repeater
	 **/
	private List<DataRepeater> dataRepeater = new ChangeTrackingArrayList<>("dataRepeater", this);

	@Override
	@XmlTransient
	public String getBizModule() {
		return KitchenSink.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return KitchenSink.DOCUMENT_NAME;
	}

	public static KitchenSink newInstance() {
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
		return ((o instanceof KitchenSink) && 
					this.getBizId().equals(((KitchenSink) o).getBizId()));
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
	 * {@link #combo} accessor.
	 * @return	The value.
	 **/
	public Combo getCombo() {
		return combo;
	}

	/**
	 * {@link #combo} mutator.
	 * @param combo	The new value.
	 **/
	@XmlElement
	public void setCombo(Combo combo) {
		preset(comboPropertyName, combo);
		this.combo = combo;
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
	 * {@link #spinner} accessor.
	 * @return	The value.
	 **/
	public Integer getSpinner() {
		return spinner;
	}

	/**
	 * {@link #spinner} mutator.
	 * @param spinner	The new value.
	 **/
	@XmlElement
	public void setSpinner(Integer spinner) {
		preset(spinnerPropertyName, spinner);
		this.spinner = spinner;
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
	 * {@link #contentLink} accessor.
	 * @return	The value.
	 **/
	public String getContentLink() {
		return contentLink;
	}

	/**
	 * {@link #contentLink} mutator.
	 * @param contentLink	The new value.
	 **/
	@XmlElement
	public void setContentLink(String contentLink) {
		preset(contentLinkPropertyName, contentLink);
		this.contentLink = contentLink;
	}

	/**
	 * {@link #contentImage} accessor.
	 * @return	The value.
	 **/
	public String getContentImage() {
		return contentImage;
	}

	/**
	 * {@link #contentImage} mutator.
	 * @param contentImage	The new value.
	 **/
	@XmlElement
	public void setContentImage(String contentImage) {
		preset(contentImagePropertyName, contentImage);
		this.contentImage = contentImage;
	}

	/**
	 * {@link #contentSignature} accessor.
	 * @return	The value.
	 **/
	public String getContentSignature() {
		return contentSignature;
	}

	/**
	 * {@link #contentSignature} mutator.
	 * @param contentSignature	The new value.
	 **/
	@XmlElement
	public void setContentSignature(String contentSignature) {
		preset(contentSignaturePropertyName, contentSignature);
		this.contentSignature = contentSignature;
	}

	/**
	 * {@link #lookupDescription} accessor.
	 * @return	The value.
	 **/
	public LookupDescription getLookupDescription() {
		return lookupDescription;
	}

	/**
	 * {@link #lookupDescription} mutator.
	 * @param lookupDescription	The new value.
	 **/
	@XmlElement
	public void setLookupDescription(LookupDescription lookupDescription) {
		if (this.lookupDescription != lookupDescription) {
			preset(lookupDescriptionPropertyName, lookupDescription);
			this.lookupDescription = lookupDescription;
		}
	}

	/**
	 * {@link #containerGrid} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<ContainerGrid> getContainerGrid() {
		return containerGrid;
	}

	/**
	 * {@link #containerGrid} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public ContainerGrid getContainerGridElementById(String bizId) {
		return getElementById(containerGrid, bizId);
	}

	/**
	 * {@link #containerGrid} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setContainerGridElementById(String bizId, ContainerGrid element) {
		setElementById(containerGrid, element);
	}

	/**
	 * {@link #containerGrid} add.
	 * @param element	The element to add.
	 **/
	public boolean addContainerGridElement(ContainerGrid element) {
		boolean result = containerGrid.add(element);
		element.setParent(this);
		return result;
	}

	/**
	 * {@link #containerGrid} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addContainerGridElement(int index, ContainerGrid element) {
		containerGrid.add(index, element);
		element.setParent(this);
	}

	/**
	 * {@link #containerGrid} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeContainerGridElement(ContainerGrid element) {
		boolean result = containerGrid.remove(element);
		element.setParent(null);
		return result;
	}

	/**
	 * {@link #containerGrid} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public ContainerGrid removeContainerGridElement(int index) {
		ContainerGrid result = containerGrid.remove(index);
		result.setParent(null);
		return result;
	}

	/**
	 * {@link #inlineGrid} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<InlineGrid> getInlineGrid() {
		return inlineGrid;
	}

	/**
	 * {@link #inlineGrid} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public InlineGrid getInlineGridElementById(String bizId) {
		return getElementById(inlineGrid, bizId);
	}

	/**
	 * {@link #inlineGrid} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setInlineGridElementById(String bizId, InlineGrid element) {
		setElementById(inlineGrid, element);
	}

	/**
	 * {@link #inlineGrid} add.
	 * @param element	The element to add.
	 **/
	public boolean addInlineGridElement(InlineGrid element) {
		boolean result = inlineGrid.add(element);
		element.setParent(this);
		return result;
	}

	/**
	 * {@link #inlineGrid} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addInlineGridElement(int index, InlineGrid element) {
		inlineGrid.add(index, element);
		element.setParent(this);
	}

	/**
	 * {@link #inlineGrid} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeInlineGridElement(InlineGrid element) {
		boolean result = inlineGrid.remove(element);
		element.setParent(null);
		return result;
	}

	/**
	 * {@link #inlineGrid} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public InlineGrid removeInlineGridElement(int index) {
		InlineGrid result = inlineGrid.remove(index);
		result.setParent(null);
		return result;
	}

	/**
	 * {@link #orderedGrid} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<OrderedGrid> getOrderedGrid() {
		return orderedGrid;
	}

	/**
	 * {@link #orderedGrid} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public OrderedGrid getOrderedGridElementById(String bizId) {
		return getElementById(orderedGrid, bizId);
	}

	/**
	 * {@link #orderedGrid} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setOrderedGridElementById(String bizId, OrderedGrid element) {
		setElementById(orderedGrid, element);
	}

	/**
	 * {@link #orderedGrid} add.
	 * @param element	The element to add.
	 **/
	public boolean addOrderedGridElement(OrderedGrid element) {
		boolean result = orderedGrid.add(element);
		element.setParent(this);
		return result;
	}

	/**
	 * {@link #orderedGrid} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addOrderedGridElement(int index, OrderedGrid element) {
		orderedGrid.add(index, element);
		element.setParent(this);
	}

	/**
	 * {@link #orderedGrid} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeOrderedGridElement(OrderedGrid element) {
		boolean result = orderedGrid.remove(element);
		element.setParent(null);
		return result;
	}

	/**
	 * {@link #orderedGrid} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public OrderedGrid removeOrderedGridElement(int index) {
		OrderedGrid result = orderedGrid.remove(index);
		result.setParent(null);
		return result;
	}

	/**
	 * {@link #dataRepeater} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<DataRepeater> getDataRepeater() {
		return dataRepeater;
	}

	/**
	 * {@link #dataRepeater} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public DataRepeater getDataRepeaterElementById(String bizId) {
		return getElementById(dataRepeater, bizId);
	}

	/**
	 * {@link #dataRepeater} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setDataRepeaterElementById(String bizId, DataRepeater element) {
		setElementById(dataRepeater, element);
	}

	/**
	 * {@link #dataRepeater} add.
	 * @param element	The element to add.
	 **/
	public boolean addDataRepeaterElement(DataRepeater element) {
		boolean result = dataRepeater.add(element);
		element.setParent(this);
		return result;
	}

	/**
	 * {@link #dataRepeater} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addDataRepeaterElement(int index, DataRepeater element) {
		dataRepeater.add(index, element);
		element.setParent(this);
	}

	/**
	 * {@link #dataRepeater} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeDataRepeaterElement(DataRepeater element) {
		boolean result = dataRepeater.remove(element);
		element.setParent(null);
		return result;
	}

	/**
	 * {@link #dataRepeater} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public DataRepeater removeDataRepeaterElement(int index) {
		DataRepeater result = dataRepeater.remove(index);
		result.setParent(null);
		return result;
	}
}
