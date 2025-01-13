package modules.admin.domain;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.domain.AbstractPersistentBean;

/**
 * Dashboard Tile
 * 
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public class DashboardTile extends AbstractPersistentBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "admin";

	/** @hidden */
	public static final String DOCUMENT_NAME = "DashboardTile";

	/** @hidden */
	public static final String tileMarkupPropertyName = "tileMarkup";

	/**
	 * Display tile markup
	 **/
	private String tileMarkup;

	@Override
	@XmlTransient
	public String getBizModule() {
		return DashboardTile.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return DashboardTile.DOCUMENT_NAME;
	}

	public static DashboardTile newInstance() {
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
			return org.skyve.util.Binder.formatMessage("DashboardTile", this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return "Unknown";
		}
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof DashboardTile) && 
					this.getBizId().equals(((DashboardTile) o).getBizId()));
	}

	/**
	 * {@link #tileMarkup} accessor.
	 * @return	The value.
	 **/
	public String getTileMarkup() {
		return tileMarkup;
	}

	/**
	 * {@link #tileMarkup} mutator.
	 * @param tileMarkup	The new value.
	 **/
	@XmlElement
	public void setTileMarkup(String tileMarkup) {
		preset(tileMarkupPropertyName, tileMarkup);
		this.tileMarkup = tileMarkup;
	}
}
