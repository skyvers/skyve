package modules.admin.domain;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.domain.AbstractPersistentBean;

/**
 * Communication Template
 * 
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public class CommunicationTemplate extends AbstractPersistentBean implements org.skyve.domain.app.admin.CommunicationTemplate {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "admin";

	/** @hidden */
	public static final String DOCUMENT_NAME = "CommunicationTemplate";

	/** @hidden */
	public static final String namePropertyName = "name";

	/** @hidden */
	public static final String descriptionPropertyName = "description";

	/** @hidden */
	public static final String templatePropertyName = "template";

	/**
	 * Name
	 **/
	private String name;

	/**
	 * Description
	 **/
	private String description;

	/**
	 * Template
	 * <br/>
	 * Include "{body}" to designate where the body of the communication will be placed.
	 **/
	private String template = "<p>{body}</p>";

	@Override
	@XmlTransient
	public String getBizModule() {
		return CommunicationTemplate.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return CommunicationTemplate.DOCUMENT_NAME;
	}

	public static CommunicationTemplate newInstance() {
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

	@Override
	public boolean equals(Object o) {
		return ((o instanceof CommunicationTemplate) && 
					this.getBizId().equals(((CommunicationTemplate) o).getBizId()));
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
	 * {@link #description} accessor.
	 * @return	The value.
	 **/
	public String getDescription() {
		return description;
	}

	/**
	 * {@link #description} mutator.
	 * @param description	The new value.
	 **/
	@XmlElement
	public void setDescription(String description) {
		preset(descriptionPropertyName, description);
		this.description = description;
	}

	/**
	 * {@link #template} accessor.
	 * @return	The value.
	 **/
	public String getTemplate() {
		return template;
	}

	/**
	 * {@link #template} mutator.
	 * @param template	The new value.
	 **/
	@XmlElement
	public void setTemplate(String template) {
		preset(templatePropertyName, template);
		this.template = template;
	}
}
