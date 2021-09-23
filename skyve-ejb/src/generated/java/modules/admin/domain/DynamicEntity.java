package modules.admin.domain;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.domain.AbstractPersistentBean;

/**
 * Dynamic Entity
 * 
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
public class DynamicEntity extends AbstractPersistentBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "admin";

	/** @hidden */
	public static final String DOCUMENT_NAME = "DynamicEntity";

	/** @hidden */
	public static final String jsonPropertyName = "json";

	/**
	 * Name
	 **/
	private String json;

	@Override
	@XmlTransient
	public String getBizModule() {
		return DynamicEntity.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return DynamicEntity.DOCUMENT_NAME;
	}

	public static DynamicEntity newInstance() {
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
			return org.skyve.util.Binder.formatMessage("{bizModule}.{bizDocument}#{bizId}", this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return "Unknown";
		}
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof DynamicEntity) && 
					this.getBizId().equals(((DynamicEntity) o).getBizId()));
	}

	/**
	 * {@link #json} accessor.
	 * @return	The value.
	 **/
	public String getJson() {
		return json;
	}

	/**
	 * {@link #json} mutator.
	 * @param json	The new value.
	 **/
	@XmlElement
	public void setJson(String json) {
		preset(jsonPropertyName, json);
		this.json = json;
	}
}
