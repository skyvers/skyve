package modules.admin.domain;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import java.util.List;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.domain.AbstractTransientBean;
import org.skyve.impl.domain.ChangeTrackingArrayList;

/**
 * System Dashboard
 * 
 * @navhas n status 0..n Generic
 * @stereotype "transient"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public class SystemDashboard extends AbstractTransientBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "admin";

	/** @hidden */
	public static final String DOCUMENT_NAME = "SystemDashboard";

	/** @hidden */
	public static final String statusPropertyName = "status";

	/**
	 * Status
	 **/
	private List<Generic> status = new ChangeTrackingArrayList<>("status", this);

	@Override
	@XmlTransient
	public String getBizModule() {
		return SystemDashboard.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return SystemDashboard.DOCUMENT_NAME;
	}

	public static SystemDashboard newInstance() {
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
		return toString();

	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof SystemDashboard) && 
					this.getBizId().equals(((SystemDashboard) o).getBizId()));
	}

	/**
	 * {@link #status} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<Generic> getStatus() {
		return status;
	}

	/**
	 * {@link #status} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public Generic getStatusElementById(String bizId) {
		return getElementById(status, bizId);
	}

	/**
	 * {@link #status} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setStatusElementById(String bizId, Generic element) {
		setElementById(status, element);
	}

	/**
	 * {@link #status} add.
	 * @param element	The element to add.
	 **/
	public boolean addStatusElement(Generic element) {
		return status.add(element);
	}

	/**
	 * {@link #status} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addStatusElement(int index, Generic element) {
		status.add(index, element);
	}

	/**
	 * {@link #status} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeStatusElement(Generic element) {
		return status.remove(element);
	}

	/**
	 * {@link #status} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public Generic removeStatusElement(int index) {
		return status.remove(index);
	}
}
