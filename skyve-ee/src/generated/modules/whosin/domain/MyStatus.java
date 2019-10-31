package modules.whosin.domain;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.domain.AbstractTransientBean;

/**
 * My Status
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
	public static final String MODULE_NAME = "whosin";
	/** @hidden */
	public static final String DOCUMENT_NAME = "MyStatus";

	/** @hidden */
	public static final String myStaffPropertyName = "myStaff";

	/**
	 * My Staff
	 **/
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

	public static MyStatus newInstance() {
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
return "My Status";
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof MyStatus) && 
					this.getBizId().equals(((MyStatus) o).getBizId()));
	}

	/**
	 * {@link #myStaff} accessor.
	 * @return	The value.
	 **/
	public Staff getMyStaff() {
		return myStaff;
	}

	/**
	 * {@link #myStaff} mutator.
	 * @param myStaff	The new value.
	 **/
	@XmlElement
	public void setMyStaff(Staff myStaff) {
		preset(myStaffPropertyName, myStaff);
		this.myStaff = myStaff;
	}

	/**
	 * My Staff exists
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isExists() {
		return (getMyStaff()!=null);
	}

	/**
	 * {@link #isExists} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotExists() {
		return (! isExists());
	}
}
