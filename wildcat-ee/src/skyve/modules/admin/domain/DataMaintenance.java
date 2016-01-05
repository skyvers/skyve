package modules.admin.domain;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;
import org.skyve.CORE;
import org.skyve.wildcat.domain.AbstractPersistentBean;

/**
 * DataMaintenance
 * 
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
public class DataMaintenance extends AbstractPersistentBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "admin";
	/** @hidden */
	public static final String DOCUMENT_NAME = "DataMaintenance";

	/** @hidden */
	public static final String modDocNamePropertyName = "modDocName";
	/** @hidden */
	public static final String schemaNamePropertyName = "schemaName";

	private String modDocName;
	private String schemaName;

	@Override
	@XmlTransient
	public String getBizModule() {
		return DataMaintenance.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return DataMaintenance.DOCUMENT_NAME;
	}

	public static DataMaintenance newInstance() throws Exception {
		return CORE.getUser().getCustomer().getModule(MODULE_NAME).getDocument(CORE.getUser().getCustomer(), DOCUMENT_NAME).newInstance(CORE.getUser());
	}

	@Override
	@XmlTransient
	public String getBizKey() {
		try {
			return org.skyve.util.Binder.formatMessage(org.skyve.CORE.getUser().getCustomer(),
														"Data Maintenance",
														this);
		}
		catch (Exception e) {
			return "Unknown";
		}
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof DataMaintenance) && 
					this.getBizId().equals(((DataMaintenance) o).getBizId()));
	}

	/**
	 * {@link #modDocName} accessor.
	 **/
	public String getModDocName() {
		return modDocName;
	}

	/**
	 * {@link #modDocName} mutator.
	 * 
	 * @param modDocName	The new value to set.
	 **/
	@XmlElement
	public void setModDocName(String modDocName) {
		preset(modDocNamePropertyName, modDocName);
		this.modDocName = modDocName;
	}

	/**
	 * {@link #schemaName} accessor.
	 **/
	public String getSchemaName() {
		return schemaName;
	}

	/**
	 * {@link #schemaName} mutator.
	 * 
	 * @param schemaName	The new value to set.
	 **/
	@XmlElement
	public void setSchemaName(String schemaName) {
		preset(schemaNamePropertyName, schemaName);
		this.schemaName = schemaName;
	}
}
