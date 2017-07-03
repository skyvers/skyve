package modules.whosin.domain;

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
import modules.admin.domain.Contact;
import org.skyve.CORE;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.Enumeration;
import org.skyve.impl.domain.AbstractPersistentBean;
import org.skyve.impl.domain.types.jaxb.DateOnlyMapper;
import org.skyve.impl.domain.types.jaxb.DateTimeMapper;
import org.skyve.impl.domain.types.jaxb.GeometryMapper;
import org.skyve.metadata.model.document.Bizlet.DomainValue;

/**
 * Someone who works for this organisation
 * 
 * @depend - - - Status
 * @navhas n contact 0..1 Contact
 * @navhas n baseOffice 0..1 Office
 * @navhas n reportsTo 0..1 Position
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
public class Staff extends AbstractPersistentBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "whosin";
	/** @hidden */
	public static final String DOCUMENT_NAME = "Staff";

	/** @hidden */
	public static final String contactPropertyName = "contact";
	/** @hidden */
	public static final String staffCodePropertyName = "staffCode";
	/** @hidden */
	public static final String dateOfBirthPropertyName = "dateOfBirth";
	/** @hidden */
	public static final String roleTitlePropertyName = "roleTitle";
	/** @hidden */
	public static final String baseOfficePropertyName = "baseOffice";
	/** @hidden */
	public static final String locationPropertyName = "location";
	/** @hidden */
	public static final String statusPropertyName = "status";
	/** @hidden */
	public static final String dueBackPropertyName = "dueBack";
	/** @hidden */
	public static final String demoDataPropertyName = "demoData";
	/** @hidden */
	public static final String reportsToPropertyName = "reportsTo";
	/** @hidden */
	public static final String positionPropertyName = "position";

	/**
	 * Status
	 **/
	@XmlEnum
	public static enum Status implements Enumeration {
		inTheOffice("inOffice", "In the Office"),
		outOfTheOffice("outOffice", "Out of the Office"),
		onLeave("onLeave", "On Leave"),
		atLunch("atLunch", "At Lunch");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues;

		private Status(String code, String description) {
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

		public static Status fromCode(String code) {
			Status result = null;

			for (Status value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static Status fromDescription(String description) {
			Status result = null;

			for (Status value : values()) {
				if (value.description.equals(description)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static List<DomainValue> toDomainValues() {
			if (domainValues == null) {
				Status[] values = values();
				domainValues = new ArrayList<>(values.length);
				for (Status value : values) {
					domainValues.add(value.domainValue);
				}
			}

			return domainValues;
		}
	}

	/**
	 * Contact
	 **/
	private Contact contact = null;
	/**
	 * Code
	 **/
	private String staffCode;
	/**
	 * Date of Birth
	 **/
	private DateOnly dateOfBirth;
	/**
	 * Role Title
	 * <br/>
	 * The person's organisational title or role
	 **/
	private String roleTitle;
	/**
	 * Base Office
	 * <br/>
	 * The office this person usually operates from.
	 **/
	private Office baseOffice = null;
	/**
	 * Location
	 **/
	private Geometry location;
	/**
	 * Status
	 **/
	private Status status;
	/**
	 * Due Back
	 * <br/>
	 * If not in the office, when the staff member is due back.
	 **/
	private DateTime dueBack;
	/**
	 * Demonstration Data
	 * <br/>
	 * If this is set, the data was created by the demo data job and can safely be deleted.
	 **/
	private Boolean demoData;
	/**
	 * Reports To
	 **/
	private Position reportsTo = null;
	/**
	 * Position
	 **/
	private Position position;

	@Override
	@XmlTransient
	public String getBizModule() {
		return Staff.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return Staff.DOCUMENT_NAME;
	}

	public static Staff newInstance() throws Exception {
		return CORE.getUser().getCustomer().getModule(MODULE_NAME).getDocument(CORE.getUser().getCustomer(), DOCUMENT_NAME).newInstance(CORE.getUser());
	}

	@Override
	@XmlTransient
	public String getBizKey() {
		try {
			return org.skyve.util.Binder.formatMessage(org.skyve.CORE.getUser().getCustomer(),
														"{contact.name} ({contact.mobile})",
														this);
		}
		catch (Exception e) {
			return "Unknown";
		}
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof Staff) && 
					this.getBizId().equals(((Staff) o).getBizId()));
	}

	/**
	 * {@link #contact} accessor.
	 * @return	The value.
	 **/
	public Contact getContact() {
		return contact;
	}

	/**
	 * {@link #contact} mutator.
	 * @param contact	The new value.
	 **/
	@XmlElement
	public void setContact(Contact contact) {
		preset(contactPropertyName, contact);
		this.contact = contact;
	}

	/**
	 * {@link #staffCode} accessor.
	 * @return	The value.
	 **/
	public String getStaffCode() {
		return staffCode;
	}

	/**
	 * {@link #staffCode} mutator.
	 * @param staffCode	The new value.
	 **/
	@XmlElement
	public void setStaffCode(String staffCode) {
		preset(staffCodePropertyName, staffCode);
		this.staffCode = staffCode;
	}

	/**
	 * {@link #dateOfBirth} accessor.
	 * @return	The value.
	 **/
	public DateOnly getDateOfBirth() {
		return dateOfBirth;
	}

	/**
	 * {@link #dateOfBirth} mutator.
	 * @param dateOfBirth	The new value.
	 **/
	@XmlSchemaType(name = "date")
	@XmlJavaTypeAdapter(DateOnlyMapper.class)
	@XmlElement
	public void setDateOfBirth(DateOnly dateOfBirth) {
		preset(dateOfBirthPropertyName, dateOfBirth);
		this.dateOfBirth = dateOfBirth;
	}

	/**
	 * {@link #roleTitle} accessor.
	 * @return	The value.
	 **/
	public String getRoleTitle() {
		return roleTitle;
	}

	/**
	 * {@link #roleTitle} mutator.
	 * @param roleTitle	The new value.
	 **/
	@XmlElement
	public void setRoleTitle(String roleTitle) {
		preset(roleTitlePropertyName, roleTitle);
		this.roleTitle = roleTitle;
	}

	/**
	 * {@link #baseOffice} accessor.
	 * @return	The value.
	 **/
	public Office getBaseOffice() {
		return baseOffice;
	}

	/**
	 * {@link #baseOffice} mutator.
	 * @param baseOffice	The new value.
	 **/
	@XmlElement
	public void setBaseOffice(Office baseOffice) {
		preset(baseOfficePropertyName, baseOffice);
		this.baseOffice = baseOffice;
	}

	/**
	 * {@link #location} accessor.
	 * @return	The value.
	 **/
	public Geometry getLocation() {
		return location;
	}

	/**
	 * {@link #location} mutator.
	 * @param location	The new value.
	 **/
	@XmlJavaTypeAdapter(GeometryMapper.class)
	@XmlElement
	public void setLocation(Geometry location) {
		preset(locationPropertyName, location);
		this.location = location;
	}

	/**
	 * {@link #status} accessor.
	 * @return	The value.
	 **/
	public Status getStatus() {
		return status;
	}

	/**
	 * {@link #status} mutator.
	 * @param status	The new value.
	 **/
	@XmlElement
	public void setStatus(Status status) {
		preset(statusPropertyName, status);
		this.status = status;
	}

	/**
	 * {@link #dueBack} accessor.
	 * @return	The value.
	 **/
	public DateTime getDueBack() {
		return dueBack;
	}

	/**
	 * {@link #dueBack} mutator.
	 * @param dueBack	The new value.
	 **/
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(DateTimeMapper.class)
	@XmlElement
	public void setDueBack(DateTime dueBack) {
		preset(dueBackPropertyName, dueBack);
		this.dueBack = dueBack;
	}

	/**
	 * {@link #demoData} accessor.
	 * @return	The value.
	 **/
	public Boolean getDemoData() {
		return demoData;
	}

	/**
	 * {@link #demoData} mutator.
	 * @param demoData	The new value.
	 **/
	@XmlElement
	public void setDemoData(Boolean demoData) {
		preset(demoDataPropertyName, demoData);
		this.demoData = demoData;
	}

	/**
	 * {@link #reportsTo} accessor.
	 * @return	The value.
	 **/
	public Position getReportsTo() {
		return reportsTo;
	}

	/**
	 * {@link #reportsTo} mutator.
	 * @param reportsTo	The new value.
	 **/
	@XmlElement
	public void setReportsTo(Position reportsTo) {
		preset(reportsToPropertyName, reportsTo);
		this.reportsTo = reportsTo;
	}

	/**
	 * {@link #position} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public Position getPosition() {
		return position;
	}

	/**
	 * {@link #position} mutator.
	 * @param position	The new value.
	 **/
	public void setPosition(Position position) {
		this.position = position;
	}
}
