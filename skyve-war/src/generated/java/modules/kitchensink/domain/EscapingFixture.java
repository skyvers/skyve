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
import org.skyve.domain.types.Enumeration;
import org.skyve.impl.domain.AbstractTransientBean;
import org.skyve.impl.domain.ChangeTrackingArrayList;
import org.skyve.impl.domain.types.jaxb.DateOnlyMapper;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.util.Util;

/**
 * Escaping Fixture
 * <br/>
 * Escaping fixture <i>document description</i>
 * <br/>
 * Non-persistent Kitchen Sink fixture for manually inspecting view boilerplate escaping.
 * 
 * @depend - - - MetadataEnum
 * @navhas n members 0..n LookupDescription
 * @navhas n gridRows 0..n ListAttributes
 * @navhas n zoomTarget 0..1 LookupDescription
 * @stereotype "transient"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public class EscapingFixture extends AbstractTransientBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "kitchensink";

	/** @hidden */
	public static final String DOCUMENT_NAME = "EscapingFixture";

	/** @hidden */
	public static final String text1PropertyName = "text1";

	/** @hidden */
	public static final String text2PropertyName = "text2";

	/** @hidden */
	public static final String metadataLabelTextPropertyName = "metadataLabelText";

	/** @hidden */
	public static final String metadataWidgetTextPropertyName = "metadataWidgetText";

	/** @hidden */
	public static final String metadataMemoPropertyName = "metadataMemo";

	/** @hidden */
	public static final String metadataMarkupPropertyName = "metadataMarkup";

	/** @hidden */
	public static final String metadataBooleanPropertyName = "metadataBoolean";

	/** @hidden */
	public static final String metadataIntegerPropertyName = "metadataInteger";

	/** @hidden */
	public static final String metadataDatePropertyName = "metadataDate";

	/** @hidden */
	public static final String metadataEnumPropertyName = "metadataEnum";

	/** @hidden */
	public static final String zoomTargetPropertyName = "zoomTarget";

	/** @hidden */
	public static final String gridRowsPropertyName = "gridRows";

	/** @hidden */
	public static final String membersPropertyName = "members";

	/**
	 * Trusted <i>enum display</i>
	 * <br/>
	 * Trusted <i>enum description</i>
	 **/
	@XmlEnum
	@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
	public enum MetadataEnum implements Enumeration {
		escaped("escaped", "Escaped <i>enum value</i>"),
		trusted("trusted", "Trusted <i>enum value</i>");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues = Stream.of(values()).map(MetadataEnum::toDomainValue).collect(Collectors.toUnmodifiableList());

		private MetadataEnum(String code, String description) {
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

		public static MetadataEnum fromCode(String code) {
			MetadataEnum result = null;

			for (MetadataEnum value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static MetadataEnum fromLocalisedDescription(String description) {
			MetadataEnum result = null;

			for (MetadataEnum value : values()) {
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
	 * Text 1
	 **/
	private String text1;

	/**
	 * Text 2
	 **/
	private String text2;

	/**
	 * Escaped <i>attribute display</i>
	 * <br/>
	 * Escaped <i>attribute description</i>
	 **/
	private String metadataLabelText = "Escaped <i>default text</i>";

	/**
	 * Trusted <i>default widget display</i>
	 * <br/>
	 * Trusted <i>default widget description</i>
	 **/
	private String metadataWidgetText = "Trusted <i>widget default</i>";

	/**
	 * Escaped <i>memo display</i>
	 * <br/>
	 * Escaped <i>memo description</i>
	 **/
	private String metadataMemo = "Escaped <i>memo default</i>";

	/**
	 * Trusted <i>markup display</i>
	 * <br/>
	 * Trusted <i>markup description</i>
	 **/
	private String metadataMarkup = "<p>Trusted markup default</p>";

	/**
	 * Escaped <i>boolean display</i>
	 * <br/>
	 * Escaped <i>boolean description</i>
	 **/
	private Boolean metadataBoolean = Boolean.valueOf(false);

	/**
	 * Escaped <i>integer display</i>
	 * <br/>
	 * Escaped <i>integer description</i>
	 **/
	private Integer metadataInteger = Integer.valueOf(3);

	/**
	 * Escaped <i>date display</i>
	 * <br/>
	 * Escaped <i>date description</i>
	 **/
	private DateOnly metadataDate;

	/**
	 * Trusted <i>enum display</i>
	 * <br/>
	 * Trusted <i>enum description</i>
	 **/
	private MetadataEnum metadataEnum;

	/**
	 * Zoom Target
	 * <br/>
	 * Escaped <i>association description</i>
	 **/
	private LookupDescription zoomTarget = null;

	/**
	 * Grid Rows
	 * <br/>
	 * Escaped <i>grid collection description</i>
	 **/
	private List<ListAttributes> gridRows = new ChangeTrackingArrayList<>("gridRows", this);

	/**
	 * Members
	 * <br/>
	 * Escaped <i>members collection description</i>
	 **/
	private List<LookupDescription> members = new ChangeTrackingArrayList<>("members", this);

	@Override
	@XmlTransient
	public String getBizModule() {
		return EscapingFixture.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return EscapingFixture.DOCUMENT_NAME;
	}

	public static EscapingFixture newInstance() {
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
			return org.skyve.util.Binder.formatMessage("{text1}", this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return "Unknown";
		}
	}

	/**
	 * {@link #text1} accessor.
	 * @return	The value.
	 **/
	public String getText1() {
		return text1;
	}

	/**
	 * {@link #text1} mutator.
	 * @param text1	The new value.
	 **/
	@XmlElement
	public void setText1(String text1) {
		preset(text1PropertyName, text1);
		this.text1 = text1;
	}

	/**
	 * {@link #text2} accessor.
	 * @return	The value.
	 **/
	public String getText2() {
		return text2;
	}

	/**
	 * {@link #text2} mutator.
	 * @param text2	The new value.
	 **/
	@XmlElement
	public void setText2(String text2) {
		preset(text2PropertyName, text2);
		this.text2 = text2;
	}

	/**
	 * {@link #metadataLabelText} accessor.
	 * @return	The value.
	 **/
	public String getMetadataLabelText() {
		return metadataLabelText;
	}

	/**
	 * {@link #metadataLabelText} mutator.
	 * @param metadataLabelText	The new value.
	 **/
	@XmlElement
	public void setMetadataLabelText(String metadataLabelText) {
		preset(metadataLabelTextPropertyName, metadataLabelText);
		this.metadataLabelText = metadataLabelText;
	}

	/**
	 * {@link #metadataWidgetText} accessor.
	 * @return	The value.
	 **/
	public String getMetadataWidgetText() {
		return metadataWidgetText;
	}

	/**
	 * {@link #metadataWidgetText} mutator.
	 * @param metadataWidgetText	The new value.
	 **/
	@XmlElement
	public void setMetadataWidgetText(String metadataWidgetText) {
		preset(metadataWidgetTextPropertyName, metadataWidgetText);
		this.metadataWidgetText = metadataWidgetText;
	}

	/**
	 * {@link #metadataMemo} accessor.
	 * @return	The value.
	 **/
	public String getMetadataMemo() {
		return metadataMemo;
	}

	/**
	 * {@link #metadataMemo} mutator.
	 * @param metadataMemo	The new value.
	 **/
	@XmlElement
	public void setMetadataMemo(String metadataMemo) {
		preset(metadataMemoPropertyName, metadataMemo);
		this.metadataMemo = metadataMemo;
	}

	/**
	 * {@link #metadataMarkup} accessor.
	 * @return	The value.
	 **/
	public String getMetadataMarkup() {
		return metadataMarkup;
	}

	/**
	 * {@link #metadataMarkup} mutator.
	 * @param metadataMarkup	The new value.
	 **/
	@XmlElement
	public void setMetadataMarkup(String metadataMarkup) {
		preset(metadataMarkupPropertyName, metadataMarkup);
		this.metadataMarkup = metadataMarkup;
	}

	/**
	 * {@link #metadataBoolean} accessor.
	 * @return	The value.
	 **/
	public Boolean getMetadataBoolean() {
		return metadataBoolean;
	}

	/**
	 * {@link #metadataBoolean} mutator.
	 * @param metadataBoolean	The new value.
	 **/
	@XmlElement
	public void setMetadataBoolean(Boolean metadataBoolean) {
		preset(metadataBooleanPropertyName, metadataBoolean);
		this.metadataBoolean = metadataBoolean;
	}

	/**
	 * {@link #metadataInteger} accessor.
	 * @return	The value.
	 **/
	public Integer getMetadataInteger() {
		return metadataInteger;
	}

	/**
	 * {@link #metadataInteger} mutator.
	 * @param metadataInteger	The new value.
	 **/
	@XmlElement
	public void setMetadataInteger(Integer metadataInteger) {
		preset(metadataIntegerPropertyName, metadataInteger);
		this.metadataInteger = metadataInteger;
	}

	/**
	 * {@link #metadataDate} accessor.
	 * @return	The value.
	 **/
	public DateOnly getMetadataDate() {
		return metadataDate;
	}

	/**
	 * {@link #metadataDate} mutator.
	 * @param metadataDate	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "date")
	@XmlJavaTypeAdapter(DateOnlyMapper.class)
	public void setMetadataDate(DateOnly metadataDate) {
		preset(metadataDatePropertyName, metadataDate);
		this.metadataDate = metadataDate;
	}

	/**
	 * {@link #metadataEnum} accessor.
	 * @return	The value.
	 **/
	public MetadataEnum getMetadataEnum() {
		return metadataEnum;
	}

	/**
	 * {@link #metadataEnum} mutator.
	 * @param metadataEnum	The new value.
	 **/
	@XmlElement
	public void setMetadataEnum(MetadataEnum metadataEnum) {
		preset(metadataEnumPropertyName, metadataEnum);
		this.metadataEnum = metadataEnum;
	}

	/**
	 * {@link #zoomTarget} accessor.
	 * @return	The value.
	 **/
	public LookupDescription getZoomTarget() {
		return zoomTarget;
	}

	/**
	 * {@link #zoomTarget} mutator.
	 * @param zoomTarget	The new value.
	 **/
	@XmlElement
	public void setZoomTarget(LookupDescription zoomTarget) {
		if (this.zoomTarget != zoomTarget) {
			preset(zoomTargetPropertyName, zoomTarget);
			this.zoomTarget = zoomTarget;
		}
	}

	/**
	 * {@link #gridRows} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<ListAttributes> getGridRows() {
		return gridRows;
	}

	/**
	 * {@link #gridRows} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public ListAttributes getGridRowsElementById(String bizId) {
		return getElementById(gridRows, bizId);
	}

	/**
	 * {@link #gridRows} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setGridRowsElementById(String bizId, ListAttributes element) {
		setElementById(gridRows, element);
	}

	/**
	 * {@link #gridRows} add.
	 * @param element	The element to add.
	 **/
	public boolean addGridRowsElement(ListAttributes element) {
		return gridRows.add(element);
	}

	/**
	 * {@link #gridRows} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addGridRowsElement(int index, ListAttributes element) {
		gridRows.add(index, element);
	}

	/**
	 * {@link #gridRows} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeGridRowsElement(ListAttributes element) {
		return gridRows.remove(element);
	}

	/**
	 * {@link #gridRows} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public ListAttributes removeGridRowsElement(int index) {
		return gridRows.remove(index);
	}

	/**
	 * {@link #members} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<LookupDescription> getMembers() {
		return members;
	}

	/**
	 * {@link #members} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public LookupDescription getMembersElementById(String bizId) {
		return getElementById(members, bizId);
	}

	/**
	 * {@link #members} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setMembersElementById(String bizId, LookupDescription element) {
		setElementById(members, element);
	}

	/**
	 * {@link #members} add.
	 * @param element	The element to add.
	 **/
	public boolean addMembersElement(LookupDescription element) {
		return members.add(element);
	}

	/**
	 * {@link #members} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addMembersElement(int index, LookupDescription element) {
		members.add(index, element);
	}

	/**
	 * {@link #members} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeMembersElement(LookupDescription element) {
		return members.remove(element);
	}

	/**
	 * {@link #members} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public LookupDescription removeMembersElement(int index) {
		return members.remove(index);
	}
}
