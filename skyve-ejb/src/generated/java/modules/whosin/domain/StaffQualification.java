package modules.whosin.domain;

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
import org.skyve.domain.Bean;
import org.skyve.domain.ChildBean;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.Enumeration;
import org.skyve.impl.domain.AbstractPersistentBean;
import org.skyve.impl.domain.types.jaxb.DateOnlyMapper;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.util.Util;

/**
 * Qualification
 * <br/>
 * A skill, experience or formal qualification attained by this Staff person
 * 
 * @depend - - - Type
 * @stereotype "persistent child"
 */
@XmlType
@XmlRootElement
public class StaffQualification extends AbstractPersistentBean implements ChildBean<Staff> {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "whosin";

	/** @hidden */
	public static final String DOCUMENT_NAME = "StaffQualification";

	/** @hidden */
	public static final String typePropertyName = "type";

	/** @hidden */
	public static final String namePropertyName = "name";

	/** @hidden */
	public static final String issuingOrganisationPropertyName = "issuingOrganisation";

	/** @hidden */
	public static final String descriptionPropertyName = "description";

	/** @hidden */
	public static final String dateAttainedPropertyName = "dateAttained";

	/** @hidden */
	public static final String dateExpiryPropertyName = "dateExpiry";

	/**
	 * Type
	 **/
	@XmlEnum
	public static enum Type implements Enumeration {
		skill("Skill", "Skill"),
		experience("Experience", "Experience"),
		diploma("Diploma", "Diploma"),
		bachelor("Bachelor", "Bachelor"),
		postGraduate("PostGraduate", "Post Graduate"),
		master("Master", "Master"),
		phD("PhD", "PhD"),
		other("Other", "Other");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues;

		private Type(String code, String description) {
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

		public static Type fromCode(String code) {
			Type result = null;

			for (Type value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static Type fromLocalisedDescription(String description) {
			Type result = null;

			for (Type value : values()) {
				if (value.toLocalisedDescription().equals(description)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static List<DomainValue> toDomainValues() {
			if (domainValues == null) {
				Type[] values = values();
				domainValues = new ArrayList<>(values.length);
				for (Type value : values) {
					domainValues.add(value.domainValue);
				}
			}

			return domainValues;
		}
	}

	/**
	 * Type
	 **/
	private Type type;

	/**
	 * Name
	 **/
	private String name;

	/**
	 * Issuing Organisation
	 **/
	private String issuingOrganisation;

	/**
	 * Description
	 **/
	private String description;

	/**
	 * Date Attained
	 **/
	private DateOnly dateAttained;

	/**
	 * Date of Expiry (if applicable)
	 **/
	private DateOnly dateExpiry;

	private Staff parent;

	private Integer bizOrdinal;

	@Override
	@XmlTransient
	public String getBizModule() {
		return StaffQualification.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return StaffQualification.DOCUMENT_NAME;
	}

	public static StaffQualification newInstance() {
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
			return org.skyve.util.Binder.formatMessage("{name} ({type})", this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return "Unknown";
		}
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof StaffQualification) && 
					this.getBizId().equals(((StaffQualification) o).getBizId()));
	}

	/**
	 * {@link #type} accessor.
	 * @return	The value.
	 **/
	public Type getType() {
		return type;
	}

	/**
	 * {@link #type} mutator.
	 * @param type	The new value.
	 **/
	@XmlElement
	public void setType(Type type) {
		preset(typePropertyName, type);
		this.type = type;
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
	 * {@link #issuingOrganisation} accessor.
	 * @return	The value.
	 **/
	public String getIssuingOrganisation() {
		return issuingOrganisation;
	}

	/**
	 * {@link #issuingOrganisation} mutator.
	 * @param issuingOrganisation	The new value.
	 **/
	@XmlElement
	public void setIssuingOrganisation(String issuingOrganisation) {
		preset(issuingOrganisationPropertyName, issuingOrganisation);
		this.issuingOrganisation = issuingOrganisation;
	}

	/**
	 * {@link #description} accessor.
	 * @return	The value.
	 **/
	public String getDescription() {
		return description;
	}

	/**
	 * {@link #description} mutator.
	 * @param description	The new value.
	 **/
	@XmlElement
	public void setDescription(String description) {
		preset(descriptionPropertyName, description);
		this.description = description;
	}

	/**
	 * {@link #dateAttained} accessor.
	 * @return	The value.
	 **/
	public DateOnly getDateAttained() {
		return dateAttained;
	}

	/**
	 * {@link #dateAttained} mutator.
	 * @param dateAttained	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "date")
	@XmlJavaTypeAdapter(DateOnlyMapper.class)
	public void setDateAttained(DateOnly dateAttained) {
		preset(dateAttainedPropertyName, dateAttained);
		this.dateAttained = dateAttained;
	}

	/**
	 * {@link #dateExpiry} accessor.
	 * @return	The value.
	 **/
	public DateOnly getDateExpiry() {
		return dateExpiry;
	}

	/**
	 * {@link #dateExpiry} mutator.
	 * @param dateExpiry	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "date")
	@XmlJavaTypeAdapter(DateOnlyMapper.class)
	public void setDateExpiry(DateOnly dateExpiry) {
		preset(dateExpiryPropertyName, dateExpiry);
		this.dateExpiry = dateExpiry;
	}

	@Override
	public Staff getParent() {
		return parent;
	}

	@Override
	@XmlElement
	public void setParent(Staff parent) {
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
