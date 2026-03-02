package modules.admin.domain;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.domain.AbstractTransientBean;

/**
 * Heap Dump List
 * <br/>
 * Represents a view of available JVM heap dump files. 
		This document is used to list and select heap dumps for inspection, analysis, or diagnostic purposes.
 * 
 * @stereotype "transient"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public class HeapDumpList extends AbstractTransientBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "admin";

	/** @hidden */
	public static final String DOCUMENT_NAME = "HeapDumpList";

	/** @hidden */
	public static final String refreshPropertyName = "refresh";

	/** @hidden */
	public static final String selectedNamePropertyName = "selectedName";

	/**
	 * Refresh
	 **/
	private Boolean refresh = Boolean.valueOf(true);

	/**
	 * Selected Name
	 **/
	private String selectedName;

	@Override
	@XmlTransient
	public String getBizModule() {
		return HeapDumpList.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return HeapDumpList.DOCUMENT_NAME;
	}

	public static HeapDumpList newInstance() {
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
			return org.skyve.util.Binder.formatMessage("Heap Dump List", this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return "Unknown";
		}
	}

	/**
	 * {@link #refresh} accessor.
	 * @return	The value.
	 **/
	public Boolean getRefresh() {
		return refresh;
	}

	/**
	 * {@link #refresh} mutator.
	 * @param refresh	The new value.
	 **/
	@XmlElement
	public void setRefresh(Boolean refresh) {
		this.refresh = refresh;
	}

	/**
	 * {@link #selectedName} accessor.
	 * @return	The value.
	 **/
	public String getSelectedName() {
		return selectedName;
	}

	/**
	 * {@link #selectedName} mutator.
	 * @param selectedName	The new value.
	 **/
	@XmlElement
	public void setSelectedName(String selectedName) {
		this.selectedName = selectedName;
	}

	/**
	 * refreshRequired
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isRefreshRequired() {
		return (Boolean.TRUE.equals(refresh));
	}

	/**
	 * {@link #isRefreshRequired} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotRefreshRequired() {
		return (! isRefreshRequired());
	}

	/**
	 * selected
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isSelected() {
		return (selectedName != null);
	}

	/**
	 * {@link #isSelected} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotSelected() {
		return (! isSelected());
	}
}
