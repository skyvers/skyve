package modules.kitchensink.domain;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.ChildBean;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.domain.AbstractTransientBean;

/**
 * Data Repeater
 * 
 * @stereotype "transient child"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator", date = "2024-03-15T01:02:36.000Z")
public class DataRepeater extends AbstractTransientBean implements ChildBean<KitchenSink> {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "kitchensink";

	/** @hidden */
	public static final String DOCUMENT_NAME = "DataRepeater";

	/** @hidden */
	public static final String blurbPropertyName = "blurb";

	/**
	 * Blurb
	 **/
	private String blurb;

	private KitchenSink parent;

	private Integer bizOrdinal;

	@Override
	@XmlTransient
	public String getBizModule() {
		return DataRepeater.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return DataRepeater.DOCUMENT_NAME;
	}

	public static DataRepeater newInstance() {
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
			return org.skyve.util.Binder.formatMessage("Data Repeater", this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return "Unknown";
		}
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof DataRepeater) && 
					this.getBizId().equals(((DataRepeater) o).getBizId()));
	}

	/**
	 * {@link #blurb} accessor.
	 * @return	The value.
	 **/
	public String getBlurb() {
		return blurb;
	}

	/**
	 * {@link #blurb} mutator.
	 * @param blurb	The new value.
	 **/
	@XmlElement
	public void setBlurb(String blurb) {
		preset(blurbPropertyName, blurb);
		this.blurb = blurb;
	}

	@Override
	public KitchenSink getParent() {
		return parent;
	}

	@Override
	@XmlElement
	public void setParent(KitchenSink parent) {
		if (this.parent != parent) {
			preset(ChildBean.PARENT_NAME, parent);
			this.parent = parent;
		}
	}

	@Override
	public Integer getBizOrdinal() {
		return bizOrdinal;
	}

	@Override
	@XmlElement
	public void setBizOrdinal(Integer bizOrdinal) {
		preset(Bean.ORDINAL_NAME, bizOrdinal);
		this.bizOrdinal =  bizOrdinal;
	}
}
