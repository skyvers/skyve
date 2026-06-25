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
 * Upload Fixture Grid Row
 * 
 * @stereotype "transient child"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public class UploadFixtureGridRow extends AbstractTransientBean implements ChildBean<UploadFixture> {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "kitchensink";

	/** @hidden */
	public static final String DOCUMENT_NAME = "UploadFixtureGridRow";

	/** @hidden */
	public static final String namePropertyName = "name";

	/** @hidden */
	public static final String boundColumnContentPropertyName = "boundColumnContent";

	/** @hidden */
	public static final String containerColumnContentPropertyName = "containerColumnContent";

	/**
	 * Name
	 **/
	private String name = "Grid upload row";

	/**
	 * Bound-column Content
	 **/
	private String boundColumnContent;

	/**
	 * Container-column Content
	 **/
	private String containerColumnContent;

	private UploadFixture parent;

	private Integer bizOrdinal;

	@Override
	@XmlTransient
	public String getBizModule() {
		return UploadFixtureGridRow.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return UploadFixtureGridRow.DOCUMENT_NAME;
	}

	public static UploadFixtureGridRow newInstance() {
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
			return org.skyve.util.Binder.formatMessage("{name}", this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return "Unknown";
		}
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
	 * {@link #boundColumnContent} accessor.
	 * @return	The value.
	 **/
	public String getBoundColumnContent() {
		return boundColumnContent;
	}

	/**
	 * {@link #boundColumnContent} mutator.
	 * @param boundColumnContent	The new value.
	 **/
	@XmlElement
	public void setBoundColumnContent(String boundColumnContent) {
		preset(boundColumnContentPropertyName, boundColumnContent);
		this.boundColumnContent = boundColumnContent;
	}

	/**
	 * {@link #containerColumnContent} accessor.
	 * @return	The value.
	 **/
	public String getContainerColumnContent() {
		return containerColumnContent;
	}

	/**
	 * {@link #containerColumnContent} mutator.
	 * @param containerColumnContent	The new value.
	 **/
	@XmlElement
	public void setContainerColumnContent(String containerColumnContent) {
		preset(containerColumnContentPropertyName, containerColumnContent);
		this.containerColumnContent = containerColumnContent;
	}

	@Override
	public UploadFixture getParent() {
		return parent;
	}

	@Override
	@XmlElement
	public void setParent(UploadFixture parent) {
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
