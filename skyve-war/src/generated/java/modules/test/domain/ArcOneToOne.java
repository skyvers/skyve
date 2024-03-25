package modules.test.domain;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.domain.AbstractPersistentBean;

/**
 * ArcOneToOne
 * <br/>
 * A single Arc.
 * 
 * @navhas n arc 0..1 AnyBase
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator", date = "2024-03-25T03:04:55.000Z")
public class ArcOneToOne extends AbstractPersistentBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "test";

	/** @hidden */
	public static final String DOCUMENT_NAME = "ArcOneToOne";

	/** @hidden */
	public static final String arcPropertyName = "arc";

	/**
	 * Arc
	 **/
	private AnyBase arc = null;

	@Override
	@XmlTransient
	public String getBizModule() {
		return ArcOneToOne.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return ArcOneToOne.DOCUMENT_NAME;
	}

	public static ArcOneToOne newInstance() {
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
			return org.skyve.util.Binder.formatMessage("ArcOneToOne", this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return "Unknown";
		}
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof ArcOneToOne) && 
					this.getBizId().equals(((ArcOneToOne) o).getBizId()));
	}

	/**
	 * {@link #arc} accessor.
	 * @return	The value.
	 **/
	public AnyBase getArc() {
		return arc;
	}

	/**
	 * {@link #arc} mutator.
	 * @param arc	The new value.
	 **/
	@XmlElement
	public void setArc(AnyBase arc) {
		if (this.arc != arc) {
			preset(arcPropertyName, arc);
			this.arc = arc;
		}
	}
}
