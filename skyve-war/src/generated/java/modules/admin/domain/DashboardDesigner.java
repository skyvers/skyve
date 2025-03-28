package modules.admin.domain;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import java.util.List;
import modules.admin.Dashboard.DashboardExtension;
import modules.admin.DashboardDesigner.DashboardDesignerExtension;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.domain.AbstractPersistentBean;
import org.skyve.impl.domain.ChangeTrackingArrayList;

/**
 * DashboardDesigner
 * 
 * @navhas n dashboards 0..n Dashboard
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public abstract class DashboardDesigner extends AbstractPersistentBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "admin";

	/** @hidden */
	public static final String DOCUMENT_NAME = "DashboardDesigner";

	/** @hidden */
	public static final String dashboardsPropertyName = "dashboards";

	/**
	 * Dashboards
	 **/
	private List<DashboardExtension> dashboards = new ChangeTrackingArrayList<>("dashboards", this);

	@Override
	@XmlTransient
	public String getBizModule() {
		return DashboardDesigner.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return DashboardDesigner.DOCUMENT_NAME;
	}

	public static DashboardDesignerExtension newInstance() {
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
			return org.skyve.util.Binder.formatMessage("DashboardDesigner", this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return "Unknown";
		}
	}

	/**
	 * {@link #dashboards} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<DashboardExtension> getDashboards() {
		return dashboards;
	}

	/**
	 * {@link #dashboards} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public DashboardExtension getDashboardsElementById(String bizId) {
		return getElementById(dashboards, bizId);
	}

	/**
	 * {@link #dashboards} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setDashboardsElementById(String bizId, DashboardExtension element) {
		setElementById(dashboards, element);
	}

	/**
	 * {@link #dashboards} add.
	 * @param element	The element to add.
	 **/
	public boolean addDashboardsElement(DashboardExtension element) {
		return dashboards.add(element);
	}

	/**
	 * {@link #dashboards} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addDashboardsElement(int index, DashboardExtension element) {
		dashboards.add(index, element);
	}

	/**
	 * {@link #dashboards} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeDashboardsElement(DashboardExtension element) {
		return dashboards.remove(element);
	}

	/**
	 * {@link #dashboards} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public DashboardExtension removeDashboardsElement(int index) {
		return dashboards.remove(index);
	}
}
