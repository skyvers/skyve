package modules.kitchensink.domain;

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
 * Container Grid
 * 
 * @stereotype "transient child"
 */
@XmlType
@XmlRootElement
public class ContainerGrid extends AbstractTransientBean implements ChildBean<KitchenSink> {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "kitchensink";

	/** @hidden */
	public static final String DOCUMENT_NAME = "ContainerGrid";

	/** @hidden */
	public static final String urlPropertyName = "url";

	/**
	 * URL
	 **/
	private String url;

	private KitchenSink parent;

	private Integer bizOrdinal;

	@Override
	@XmlTransient
	public String getBizModule() {
		return ContainerGrid.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return ContainerGrid.DOCUMENT_NAME;
	}

	public static ContainerGrid newInstance() {
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
			return org.skyve.util.Binder.formatMessage("Container Grid", this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return "Unknown";
		}
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof ContainerGrid) && 
					this.getBizId().equals(((ContainerGrid) o).getBizId()));
	}

	/**
	 * {@link #url} accessor.
	 * @return	The value.
	 **/
	public String getUrl() {
		return url;
	}

	/**
	 * {@link #url} mutator.
	 * @param url	The new value.
	 **/
	@XmlElement
	public void setUrl(String url) {
		preset(urlPropertyName, url);
		this.url = url;
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
