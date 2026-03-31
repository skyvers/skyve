package modules.admin.domain;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import modules.admin.MailLogList.MailLogListExtension;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.domain.AbstractTransientBean;

/**
 * Mail Log List
 * <br/>
 * This transient document is used to display Mail Log list grids from the database,
		or (if enabled) from both archived and database Mail Logs.
 * 
 * @stereotype "transient"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public abstract class MailLogList extends AbstractTransientBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "admin";

	/** @hidden */
	public static final String DOCUMENT_NAME = "MailLogList";

	@Override
	@XmlTransient
	public String getBizModule() {
		return MailLogList.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return MailLogList.DOCUMENT_NAME;
	}

	public static MailLogListExtension newInstance() {
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
			return org.skyve.util.Binder.formatMessage("MailLogList", this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return "Unknown";
		}
	}

	/**
	 * Show archived Mail Logs, overridden in extension
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
	 * Show non-archived Mail Logs (in the database)
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
