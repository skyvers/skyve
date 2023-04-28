package modules.whosin.domain;

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
import modules.admin.domain.Contact;
import org.locationtech.jts.geom.Geometry;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.HierarchicalBean;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.Enumeration;
import org.skyve.impl.domain.AbstractPersistentBean;
import org.skyve.impl.domain.ChangeTrackingArrayList;
import org.skyve.impl.domain.types.jaxb.DateOnlyMapper;
import org.skyve.impl.domain.types.jaxb.DateTimeMapper;
import org.skyve.impl.domain.types.jaxb.GeometryMapper;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.util.Util;

/**
 * Staff
 * <br/>
 * Someone who works for this organisation.
			Note that this document is hierarchical - as Staff report to other Staff.
			The hierarchy relationship is defined by Staff having a parent relationship to other Staff.
			As the parent relationship can't be set directly through the UI, the Staff document contains a transient reportsTo attribute - 
			an association to another staff. In the preSave method in the StaffBizlet, this association is used to update the bean's
			bizParentId to persist the parent relationship.
			A filter parameter is used in the view to ensure a Staff member can't be assigned as reporting to themselves.
 * 
 * @depend - - - Status
 * @navcomposed 1 qualifications 0..n StaffQualification
 * @navhas n contact 0..1 Contact
 * @navhas n baseOffice 0..1 Office
 * @navhas n reportsTo 0..1 Staff
 * @stereotype "persistent child"
 */
@XmlType
@XmlRootElement
public class Staff extends AbstractPersistentBean implements HierarchicalBean<Staff> {
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
	public static final String qualificationsPropertyName = "qualifications";

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
		private static List<DomainValue> domainValues = Stream.of(values()).map(Status::toDomainValue).collect(Collectors.toUnmodifiableList());

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
		public String toLocalisedDescription() {
			return Util.i18n(description);
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

		public static Status fromLocalisedDescription(String description) {
			Status result = null;

			for (Status value : values()) {
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
	 * The person's organisational title or role.
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
	private Staff reportsTo = null;

	/**
	 * Qualifications
	 **/
	private List<StaffQualification> qualifications = new ChangeTrackingArrayList<>("qualifications", this);

	private String bizParentId;

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

	public static Staff newInstance() {
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
			return org.skyve.util.Binder.formatMessage("{contact.name} ({contact.mobile})", this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
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
		if (this.contact != contact) {
			preset(contactPropertyName, contact);
			this.contact = contact;
		}
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
	@XmlElement
	@XmlSchemaType(name = "date")
	@XmlJavaTypeAdapter(DateOnlyMapper.class)
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
		if (this.baseOffice != baseOffice) {
			preset(baseOfficePropertyName, baseOffice);
			this.baseOffice = baseOffice;
		}
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
	@XmlElement
	@XmlJavaTypeAdapter(GeometryMapper.class)
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
	@XmlElement
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(DateTimeMapper.class)
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
	public Staff getReportsTo() {
		return reportsTo;
	}

	/**
	 * {@link #reportsTo} mutator.
	 * @param reportsTo	The new value.
	 **/
	@XmlElement
	public void setReportsTo(Staff reportsTo) {
		if (this.reportsTo != reportsTo) {
			preset(reportsToPropertyName, reportsTo);
			this.reportsTo = reportsTo;
		}
	}

	/**
	 * {@link #qualifications} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<StaffQualification> getQualifications() {
		return qualifications;
	}

	/**
	 * {@link #qualifications} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public StaffQualification getQualificationsElementById(String bizId) {
		return getElementById(qualifications, bizId);
	}

	/**
	 * {@link #qualifications} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setQualificationsElementById(String bizId, StaffQualification element) {
		setElementById(qualifications, element);
	}

	/**
	 * {@link #qualifications} add.
	 * @param element	The element to add.
	 **/
	public boolean addQualificationsElement(StaffQualification element) {
		boolean result = qualifications.add(element);
		if (result) {
			element.setParent(this);
		}
		return result;
	}

	/**
	 * {@link #qualifications} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addQualificationsElement(int index, StaffQualification element) {
		qualifications.add(index, element);
		element.setParent(this);
	}

	/**
	 * {@link #qualifications} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeQualificationsElement(StaffQualification element) {
		boolean result = qualifications.remove(element);
		if (result) {
			element.setParent(null);
		}
		return result;
	}

	/**
	 * {@link #qualifications} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public StaffQualification removeQualificationsElement(int index) {
		StaffQualification result = qualifications.remove(index);
		result.setParent(null);
		return result;
	}

	@Override
	public String getBizParentId() {
		return bizParentId;
	}

	@Override
	@XmlElement
	public void setBizParentId(String bizParentId) {
		preset(HierarchicalBean.PARENT_ID, bizParentId);
		this.bizParentId = bizParentId;
	}

	@Override
	public Staff getParent() {
		Staff result = null;

		if (bizParentId != null) {
			Persistence p = CORE.getPersistence();
			DocumentQuery q = p.newDocumentQuery(Staff.MODULE_NAME, Staff.DOCUMENT_NAME);
			q.getFilter().addEquals(Bean.DOCUMENT_ID, bizParentId);
			result = q.retrieveBean();
		}

		return result;
	}

	@Override
	@XmlTransient
	public List<Staff> getChildren() {
		Persistence p = CORE.getPersistence();
		DocumentQuery q = p.newDocumentQuery(Staff.MODULE_NAME, Staff.DOCUMENT_NAME);
		q.getFilter().addEquals(HierarchicalBean.PARENT_ID, getBizId());
		return q.beanResults();
	}
}
