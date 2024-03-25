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
 * DownloadFolder
 * 
 * @stereotype "transient"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public class DownloadFolder extends AbstractTransientBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "admin";

	/** @hidden */
	public static final String DOCUMENT_NAME = "DownloadFolder";

	/** @hidden */
	public static final String namePropertyName = "name";

	/** @hidden */
	public static final String sizePropertyName = "size";

	/**
	 * Name
	 **/
	private String name;

	/**
	 * Size (MB)
	 **/
	private Long size;

	@Override
	@XmlTransient
	public String getBizModule() {
		return DownloadFolder.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return DownloadFolder.DOCUMENT_NAME;
	}

	public static DownloadFolder newInstance() {
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
		return toString();

	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof DownloadFolder) && 
					this.getBizId().equals(((DownloadFolder) o).getBizId()));
	}

	/**
	 * {@link #name} accessor.
	 * @return	The value.
	 **/
	public String getName() {
		return name;
	}

	/**
	 * {@link #name} mutator.
	 * @param name	The new value.
	 **/
	@XmlElement
	public void setName(String name) {
		preset(namePropertyName, name);
		this.name = name;
	}

	/**
	 * {@link #size} accessor.
	 * @return	The value.
	 **/
	public Long getSize() {
		return size;
	}

	/**
	 * {@link #size} mutator.
	 * @param size	The new value.
	 **/
	@XmlElement
	public void setSize(Long size) {
		preset(sizePropertyName, size);
		this.size = size;
	}
}
