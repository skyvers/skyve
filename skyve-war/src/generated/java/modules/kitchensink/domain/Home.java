package modules.kitchensink.domain;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import modules.admin.Dashboard.DashboardExtension;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.domain.AbstractPersistentBean;

/**
 * Home
 * 
 * @navhas n dashboard 0..1 Dashboard
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public class Home extends AbstractPersistentBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "kitchensink";

	/** @hidden */
	public static final String DOCUMENT_NAME = "Home";

	/** @hidden */
	public static final String dashboardPropertyName = "dashboard";

	/**
	 * Dashboard
	 **/
	private DashboardExtension dashboard = null;

	@Override
	@XmlTransient
	public String getBizModule() {
		return Home.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return Home.DOCUMENT_NAME;
	}

	public static Home newInstance() {
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
			return org.skyve.util.Binder.formatMessage("Home", this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return "Unknown";
		}
	}

	/**
	 * {@link #dashboard} accessor.
	 * @return	The value.
	 **/
	public DashboardExtension getDashboard() {
		return dashboard;
	}

	/**
	 * {@link #dashboard} mutator.
	 * @param dashboard	The new value.
	 **/
	@XmlElement
	public void setDashboard(DashboardExtension dashboard) {
		if (this.dashboard != dashboard) {
			preset(dashboardPropertyName, dashboard);
			this.dashboard = dashboard;
		}
	}
}
