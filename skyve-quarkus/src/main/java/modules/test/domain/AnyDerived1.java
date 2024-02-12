package modules.test.domain;

import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;

/**
 * AnyDerived1
 * <br/>
 * A document that extends AnyBase.
 * 
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
public class AnyDerived1 extends AnyBase {
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
	public static final String DOCUMENT_NAME = "AnyDerived1";

	/** @hidden */
	public static final String text1PropertyName = "text1";

	/**
	 * Text 1
	 **/
	private String text1;

	@Override
	@XmlTransient
	public String getBizModule() {
		return AnyDerived1.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return AnyDerived1.DOCUMENT_NAME;
	}

	public static AnyDerived1 newInstance() {
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

	@Override
	public boolean equals(Object o) {
		return ((o instanceof AnyDerived1) && 
					this.getBizId().equals(((AnyDerived1) o).getBizId()));
	}

	/**
	 * {@link #text1} accessor.
	 * @return	The value.
	 **/
	public String getText1() {
		return text1;
	}

	/**
	 * {@link #text1} mutator.
	 * @param text1	The new value.
	 **/
	@XmlElement
	public void setText1(String text1) {
		preset(text1PropertyName, text1);
		this.text1 = text1;
	}
}
