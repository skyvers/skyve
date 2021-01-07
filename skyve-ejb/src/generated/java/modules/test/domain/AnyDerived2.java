package modules.test.domain;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;
import modules.test.domain.AnyBase;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;

/**
 * AnyDerived2
 * <br/>
 * A document that extends AnyBase.
 * 
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
public class AnyDerived2 extends AnyBase {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "test";
	/** @hidden */
	public static final String DOCUMENT_NAME = "AnyDerived2";

	/** @hidden */
	public static final String text2PropertyName = "text2";

	/**
	 * Text 2
	 **/
	private String text2;

	@Override
	@XmlTransient
	public String getBizModule() {
		return AnyDerived2.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return AnyDerived2.DOCUMENT_NAME;
	}

	public static AnyDerived2 newInstance() {
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
		return ((o instanceof AnyDerived2) && 
					this.getBizId().equals(((AnyDerived2) o).getBizId()));
	}

	/**
	 * {@link #text2} accessor.
	 * @return	The value.
	 **/
	public String getText2() {
		return text2;
	}

	/**
	 * {@link #text2} mutator.
	 * @param text2	The new value.
	 **/
	@XmlElement
	public void setText2(String text2) {
		preset(text2PropertyName, text2);
		this.text2 = text2;
	}
}
