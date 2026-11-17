package modules.test.domain;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;

/**
 * Override Mapped Sub
 * <br/>
 * Customer override fixture: a subclass of a mapped base the test customer's override adds an attribute to.
 * 
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public class OverrideMappedSub extends OverrideMappedBase {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	@SuppressWarnings("hiding")
	public static final String MODULE_NAME = "test";

	/** @hidden */
	@SuppressWarnings("hiding")
	public static final String DOCUMENT_NAME = "OverrideMappedSub";

	/** @hidden */
	public static final String subTextPropertyName = "subText";

	/**
	 * Sub Text
	 **/
	private String subText;

	@Override
	@XmlTransient
	public String getBizModule() {
		return OverrideMappedSub.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return OverrideMappedSub.DOCUMENT_NAME;
	}

	public static OverrideMappedSub newInstance() {
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
			return org.skyve.util.Binder.formatMessage("{text}", this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return "Unknown";
		}
	}

	/**
	 * {@link #subText} accessor.
	 * @return	The value.
	 **/
	public String getSubText() {
		return subText;
	}

	/**
	 * {@link #subText} mutator.
	 * @param subText	The new value.
	 **/
	@XmlElement
	public void setSubText(String subText) {
		preset(subTextPropertyName, subText);
		this.subText = subText;
	}
}
