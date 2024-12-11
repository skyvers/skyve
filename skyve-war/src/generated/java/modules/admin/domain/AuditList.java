package modules.admin.domain;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import modules.admin.AuditList.AuditListExtension;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.domain.AbstractTransientBean;

/**
 * Audit List
 * <br/>
 * This transient document is used to display Audit list grids. Either from the DB
        only; or (if enabled) archived and DB Audits.
 * 
 * @stereotype "transient"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public abstract class AuditList extends AbstractTransientBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "admin";

	/** @hidden */
	public static final String DOCUMENT_NAME = "AuditList";

	@Override
	@XmlTransient
	public String getBizModule() {
		return AuditList.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return AuditList.DOCUMENT_NAME;
	}

	public static AuditListExtension newInstance() {
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
			return org.skyve.util.Binder.formatMessage("AuditList", this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return "Unknown";
		}
	}

	/**
	 * Show archived Audits, overriden in extension
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isShowArchived() {
		return (true);
	}

	/**
	 * {@link #isShowArchived} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotShowArchived() {
		return (! isShowArchived());
	}

	/**
	 * Show non-archived Audits (ie, in the database)
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isShowNonArchived() {
		return (true);
	}

	/**
	 * {@link #isShowNonArchived} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotShowNonArchived() {
		return (! isShowNonArchived());
	}
}
