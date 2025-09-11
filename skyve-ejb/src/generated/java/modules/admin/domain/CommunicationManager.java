package modules.admin.domain;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;
import modules.admin.Communication.CommunicationExtension;
import modules.admin.CommunicationManager.CommunicationManagerExtension;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.types.Enumeration;
import org.skyve.impl.domain.AbstractTransientBean;
import org.skyve.impl.domain.ChangeTrackingArrayList;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.util.Util;

/**
 * Communication Manager
 * <br/>
 * <p>The Communication Manager document provides functions to manage system communications and communication Configurations.</p>
 * 
 * @depend - - - ImportActionType
 * @navhas n currentCommunications 0..n Communication
 * @stereotype "transient"
 */
@XmlType
@XmlRootElement
public abstract class CommunicationManager extends AbstractTransientBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "admin";

	/** @hidden */
	public static final String DOCUMENT_NAME = "CommunicationManager";

	/** @hidden */
	public static final String pathToZipPropertyName = "pathToZip";

	/** @hidden */
	public static final String importActionTypePropertyName = "importActionType";

	/** @hidden */
	public static final String currentCommunicationsPropertyName = "currentCommunications";

	/**
	 * Import action
	 **/
	@XmlEnum
	public static enum ImportActionType implements Enumeration {
		validateOnlyCommunicationConfigurationsAndTemplates("validate", "Validate only communication configurations and templates"),
		validateThenImportCommunicationConfigurationsAndTemplates("import", "Validate then import communication configurations and templates");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues;

		private ImportActionType(String code, String description) {
			this.code = code;
			this.description = description;
			this.domainValue = new DomainValue(code, description);
		}

		@Override
		public String toCode() {
			return code;
		}

		@Override
		public String toLocalisedDescription() {
			return Util.i18n(description);
		}

		@Override
		public DomainValue toDomainValue() {
			return domainValue;
		}

		public static ImportActionType fromCode(String code) {
			ImportActionType result = null;

			for (ImportActionType value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static ImportActionType fromLocalisedDescription(String description) {
			ImportActionType result = null;

			for (ImportActionType value : values()) {
				if (value.toLocalisedDescription().equals(description)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static List<DomainValue> toDomainValues() {
			if (domainValues == null) {
				ImportActionType[] values = values();
				domainValues = new ArrayList<>(values.length);
				for (ImportActionType value : values) {
					domainValues.add(value.domainValue);
				}
			}

			return domainValues;
		}
	}

	/**
	 * Path to Zip
	 * <br/>
	 * The path of the zip of communication configurations for download
	 **/
	private String pathToZip;

	/**
	 * Import action
	 **/
	private ImportActionType importActionType = ImportActionType.validateOnlyCommunicationConfigurationsAndTemplates;

	/**
	 * Current communications
	 **/
	private List<CommunicationExtension> currentCommunications = new ChangeTrackingArrayList<>("currentCommunications", this);

	@Override
	@XmlTransient
	public String getBizModule() {
		return CommunicationManager.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return CommunicationManager.DOCUMENT_NAME;
	}

	public static CommunicationManagerExtension newInstance() {
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
		return ((o instanceof CommunicationManager) && 
					this.getBizId().equals(((CommunicationManager) o).getBizId()));
	}

	/**
	 * {@link #pathToZip} accessor.
	 * @return	The value.
	 **/
	public String getPathToZip() {
		return pathToZip;
	}

	/**
	 * {@link #pathToZip} mutator.
	 * @param pathToZip	The new value.
	 **/
	@XmlElement
	public void setPathToZip(String pathToZip) {
		preset(pathToZipPropertyName, pathToZip);
		this.pathToZip = pathToZip;
	}

	/**
	 * {@link #importActionType} accessor.
	 * @return	The value.
	 **/
	public ImportActionType getImportActionType() {
		return importActionType;
	}

	/**
	 * {@link #importActionType} mutator.
	 * @param importActionType	The new value.
	 **/
	@XmlElement
	public void setImportActionType(ImportActionType importActionType) {
		preset(importActionTypePropertyName, importActionType);
		this.importActionType = importActionType;
	}

	/**
	 * {@link #currentCommunications} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<CommunicationExtension> getCurrentCommunications() {
		return currentCommunications;
	}

	/**
	 * {@link #currentCommunications} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public CommunicationExtension getCurrentCommunicationsElementById(String bizId) {
		return getElementById(currentCommunications, bizId);
	}

	/**
	 * {@link #currentCommunications} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setCurrentCommunicationsElementById(String bizId, CommunicationExtension element) {
		setElementById(currentCommunications, element);
	}

	/**
	 * {@link #currentCommunications} add.
	 * @param element	The element to add.
	 **/
	public boolean addCurrentCommunicationsElement(CommunicationExtension element) {
		return currentCommunications.add(element);
	}

	/**
	 * {@link #currentCommunications} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addCurrentCommunicationsElement(int index, CommunicationExtension element) {
		currentCommunications.add(index, element);
	}

	/**
	 * {@link #currentCommunications} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeCurrentCommunicationsElement(CommunicationExtension element) {
		return currentCommunications.remove(element);
	}

	/**
	 * {@link #currentCommunications} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public CommunicationExtension removeCurrentCommunicationsElement(int index) {
		return currentCommunications.remove(index);
	}
}
