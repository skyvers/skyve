package modules.whosinIntegrate.domain;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;
import org.skyve.CORE;
import org.skyve.wildcat.domain.AbstractTransientBean;

/**
 * MyStatus
 * 
 * @navhas n myStaff 0..1 Staff
 * @stereotype "transient"
 */
@XmlType
@XmlRootElement
public class MyStatus extends AbstractTransientBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "whosinIntegrate";
	/** @hidden */
	public static final String DOCUMENT_NAME = "MyStatus";

	/** @hidden */
	public static final String myStaffPropertyName = "myStaff";

	private Staff myStaff = null;

	@Override
	@XmlTransient
	public String getBizModule() {
		return MyStatus.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return MyStatus.DOCUMENT_NAME;
	}

	public static MyStatus newInstance() throws Exception {
		return CORE.getUser().getCustomer().getModule(MODULE_NAME).getDocument(CORE.getUser().getCustomer(), DOCUMENT_NAME).newInstance(CORE.getUser());
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof MyStatus) && 
					this.getBizId().equals(((MyStatus) o).getBizId()));
	}

	/**
	 * {@link #myStaff} accessor.
	 **/
	public Staff getMyStaff() {
		return myStaff;
	}

	/**
	 * {@link #myStaff} mutator.
	 * 
	 * @param myStaff	The new value to set.
	 **/
	@XmlElement
	public void setMyStaff(Staff myStaff) {
		preset(myStaffPropertyName, myStaff);
		this.myStaff = myStaff;
	}

	@XmlTransient
	public boolean isExists() {
		return (getMyStaff()!=null);
	}

	public boolean isNotExists() {
		return (! isExists());
	}

	@XmlTransient
	public boolean isStaffMember() {
		return (isUserInRole("whosinIntegrate","StaffMember"));
	}

	public boolean isNotStaffMember() {
		return (! isStaffMember());
	}
}
