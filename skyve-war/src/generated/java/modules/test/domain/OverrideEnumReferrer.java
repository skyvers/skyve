package modules.test.domain;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import modules.test.domain.OverrideNarrowedVanilla.Status;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.domain.AbstractPersistentBean;

/**
 * Override Enum Referrer
 * <br/>
 * Customer override fixture: references OverrideNarrowed and its status enum, which the test customer's override leaves out and the test2 customer's keeps.
 * 
 * @depend - - - Status
 * @navhas n narrowed 0..1 OverrideNarrowed
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public class OverrideEnumReferrer extends AbstractPersistentBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "test";

	/** @hidden */
	public static final String DOCUMENT_NAME = "OverrideEnumReferrer";

	/** @hidden */
	public static final String textPropertyName = "text";

	/** @hidden */
	public static final String narrowedPropertyName = "narrowed";

	/** @hidden */
	public static final String narrowedStatusPropertyName = "narrowedStatus";

	/**
	 * Text
	 **/
	private String text;

	/**
	 * Narrowed
	 **/
	private OverrideNarrowed narrowed = null;

	/**
	 * Narrowed Status
	 **/
	private Status narrowedStatus;

	@Override
	@XmlTransient
	public String getBizModule() {
		return OverrideEnumReferrer.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return OverrideEnumReferrer.DOCUMENT_NAME;
	}

	public static OverrideEnumReferrer newInstance() {
		try {
			return TestDocument.OVERRIDE_ENUM_REFERRER.toDocument(CORE.getUser().getCustomer()).newInstance(CORE.getUser());
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
			return org.skyve.util.Binder.formatMessage("{text}", this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return "Unknown";
		}
	}

	/**
	 * {@link #text} accessor.
	 * @return	The value.
	 **/
	public String getText() {
		return text;
	}

	/**
	 * {@link #text} mutator.
	 * @param text	The new value.
	 **/
	@XmlElement
	public void setText(String text) {
		preset(textPropertyName, text);
		this.text = text;
	}

	/**
	 * {@link #narrowed} accessor.
	 * @return	The value.
	 **/
	public OverrideNarrowed getNarrowed() {
		return narrowed;
	}

	/**
	 * {@link #narrowed} mutator.
	 * @param narrowed	The new value.
	 **/
	@XmlElement
	public void setNarrowed(OverrideNarrowed narrowed) {
		if (this.narrowed != narrowed) {
			preset(narrowedPropertyName, narrowed);
			this.narrowed = narrowed;
		}
	}

	/**
	 * {@link #narrowedStatus} accessor.
	 * @return	The value.
	 **/
	public Status getNarrowedStatus() {
		return narrowedStatus;
	}

	/**
	 * {@link #narrowedStatus} mutator.
	 * @param narrowedStatus	The new value.
	 **/
	@XmlElement
	public void setNarrowedStatus(Status narrowedStatus) {
		preset(narrowedStatusPropertyName, narrowedStatus);
		this.narrowedStatus = narrowedStatus;
	}
}
