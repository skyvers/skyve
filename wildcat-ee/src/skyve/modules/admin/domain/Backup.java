package modules.admin.domain;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;
import org.skyve.CORE;
import org.skyve.wildcat.domain.AbstractTransientBean;

/**
 * Backup
 * 
 * @stereotype "transient"
 */
@XmlType
@XmlRootElement
public class Backup extends AbstractTransientBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "admin";
	/** @hidden */
	public static final String DOCUMENT_NAME = "Backup";

	/** @hidden */
	public static final String namePropertyName = "name";

	private String name;

	@Override
	@XmlTransient
	public String getBizModule() {
		return Backup.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return Backup.DOCUMENT_NAME;
	}

	public static Backup newInstance() throws Exception {
		return CORE.getUser().getCustomer().getModule(MODULE_NAME).getDocument(CORE.getUser().getCustomer(), DOCUMENT_NAME).newInstance(CORE.getUser());
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof Backup) && 
					this.getBizId().equals(((Backup) o).getBizId()));
	}

	/**
	 * {@link #name} accessor.
	 **/
	public String getName() {
		return name;
	}

	/**
	 * {@link #name} mutator.
	 * 
	 * @param name	The new value to set.
	 **/
	@XmlElement
	public void setName(String name) {
		preset(namePropertyName, name);
		this.name = name;
	}
}
