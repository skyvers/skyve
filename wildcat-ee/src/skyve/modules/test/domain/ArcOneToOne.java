package modules.test.domain;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;
import org.skyve.CORE;
import org.skyve.wildcat.domain.AbstractPersistentBean;

/**
 * A single Arc.
 * 
 * @navhas n arc 0..1 AnyBase
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
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

	public static ArcOneToOne newInstance() throws Exception {
		return CORE.getUser().getCustomer().getModule(MODULE_NAME).getDocument(CORE.getUser().getCustomer(), DOCUMENT_NAME).newInstance(CORE.getUser());
	}

	@Override
	@XmlTransient
	public String getBizKey() {
		try {
			return org.skyve.util.Binder.formatMessage(org.skyve.CORE.getUser().getCustomer(),
														"ArcOneToOne",
														this);
		}
		catch (Exception e) {
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
	 **/
	public AnyBase getArc() {
		return arc;
	}

	/**
	 * {@link #arc} mutator.
	 * 
	 * @param arc	The new value to set.
	 **/
	@XmlElement
	public void setArc(AnyBase arc) {
		preset(arcPropertyName, arc);
		this.arc = arc;
	}
}
