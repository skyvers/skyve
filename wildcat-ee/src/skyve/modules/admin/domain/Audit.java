package modules.admin.domain;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlSchemaType;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import org.skyve.domain.types.Enumeration;
import org.skyve.domain.types.Timestamp;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.wildcat.domain.AbstractPersistentBean;
import org.skyve.wildcat.domain.types.jaxb.TimestampMapper;

/**
 * Audit
 * 
 * @depend - - - Operation
 * @navhas n comparisonVersion 0..1 Audit
 * @navhas n sourceVersion 1 Audit
 * @navhas n me 0..1 Audit
 * @stereotype "persistent"
 */
@XmlType
public class Audit extends AbstractPersistentBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "admin";
	/** @hidden */
	public static final String DOCUMENT_NAME = "Audit";

	/** @hidden */
	public static final String auditModuleNamePropertyName = "auditModuleName";
	/** @hidden */
	public static final String auditDocumentNamePropertyName = "auditDocumentName";
	/** @hidden */
	public static final String auditBizIdPropertyName = "auditBizId";
	/** @hidden */
	public static final String auditBizKeyPropertyName = "auditBizKey";
	/** @hidden */
	public static final String auditBizVersionPropertyName = "auditBizVersion";
	/** @hidden */
	public static final String operationPropertyName = "operation";
	/** @hidden */
	public static final String timestampPropertyName = "timestamp";
	/** @hidden */
	public static final String userNamePropertyName = "userName";
	/** @hidden */
	public static final String auditPropertyName = "audit";
	/** @hidden */
	public static final String sourceVersionPropertyName = "sourceVersion";
	/** @hidden */
	public static final String comparisonVersionPropertyName = "comparisonVersion";
	/** @hidden */
	public static final String mePropertyName = "me";

	/**
	 * Operation
	 **/
	@XmlEnum
	public static enum Operation implements Enumeration {
		insert("I", "Insert"),
		update("U", "Update"),
		delete("D", "Delete");

		private String code;
		private String description;

		/** @hidden */
		private static List<DomainValue> domainValues;

		private Operation(String code, String description) {
			this.code = code;
			this.description = description;
		}

		@Override
		public String toCode() {
			return code;
		}

		@Override
		public String toDescription() {
			return description;
		}

		public static Operation fromCode(String code) {
			Operation result = null;

			for (Operation value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static Operation fromDescription(String description) {
			Operation result = null;

			for (Operation value : values()) {
				if (value.description.equals(description)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static List<DomainValue> toDomainValues() {
			if (domainValues == null) {
				Operation[] values = values();
				domainValues = new ArrayList<>(values.length);
				for (Operation value : values) {
					domainValues.add(new DomainValue(value.code, value.description));
				}
			}

			return domainValues;
		}
	}

	private String auditModuleName;
	private String auditDocumentName;
	private String auditBizId;
	private String auditBizKey;
	private Integer auditBizVersion;
	private Operation operation;
	private Timestamp timestamp;
	private String userName;
	private String audit;
	private Audit sourceVersion = null;
	private Audit comparisonVersion = null;
	private Audit me = null;

	@Override
	@XmlTransient
	public String getBizModule() {
		return Audit.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return Audit.DOCUMENT_NAME;
	}

	@Override
	@XmlTransient
	public String getBizKey() {
		try {
			return org.skyve.util.Binder.formatMessage(org.skyve.CORE.getUser().getCustomer(),
														"{auditBizVersion} - {operation} by {userName} at {timestamp}",
														this);
		}
		catch (Exception e) {
			return "Unknown";
		}
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof Audit) && 
					this.getBizId().equals(((Audit) o).getBizId()));
	}

	/**
	 * {@link #auditModuleName} accessor.
	 **/
	public String getAuditModuleName() {
		return auditModuleName;
	}

	/**
	 * {@link #auditModuleName} mutator.
	 * 
	 * @param auditModuleName	The new value to set.
	 **/
	@XmlElement
	public void setAuditModuleName(String auditModuleName) {
		preset(auditModuleNamePropertyName, auditModuleName);
		this.auditModuleName = auditModuleName;
	}

	/**
	 * {@link #auditDocumentName} accessor.
	 **/
	public String getAuditDocumentName() {
		return auditDocumentName;
	}

	/**
	 * {@link #auditDocumentName} mutator.
	 * 
	 * @param auditDocumentName	The new value to set.
	 **/
	@XmlElement
	public void setAuditDocumentName(String auditDocumentName) {
		preset(auditDocumentNamePropertyName, auditDocumentName);
		this.auditDocumentName = auditDocumentName;
	}

	/**
	 * {@link #auditBizId} accessor.
	 **/
	public String getAuditBizId() {
		return auditBizId;
	}

	/**
	 * {@link #auditBizId} mutator.
	 * 
	 * @param auditBizId	The new value to set.
	 **/
	@XmlElement
	public void setAuditBizId(String auditBizId) {
		preset(auditBizIdPropertyName, auditBizId);
		this.auditBizId = auditBizId;
	}

	/**
	 * {@link #auditBizKey} accessor.
	 **/
	public String getAuditBizKey() {
		return auditBizKey;
	}

	/**
	 * {@link #auditBizKey} mutator.
	 * 
	 * @param auditBizKey	The new value to set.
	 **/
	@XmlElement
	public void setAuditBizKey(String auditBizKey) {
		preset(auditBizKeyPropertyName, auditBizKey);
		this.auditBizKey = auditBizKey;
	}

	/**
	 * {@link #auditBizVersion} accessor.
	 **/
	public Integer getAuditBizVersion() {
		return auditBizVersion;
	}

	/**
	 * {@link #auditBizVersion} mutator.
	 * 
	 * @param auditBizVersion	The new value to set.
	 **/
	@XmlElement
	public void setAuditBizVersion(Integer auditBizVersion) {
		preset(auditBizVersionPropertyName, auditBizVersion);
		this.auditBizVersion = auditBizVersion;
	}

	/**
	 * {@link #operation} accessor.
	 **/
	public Operation getOperation() {
		return operation;
	}

	/**
	 * {@link #operation} mutator.
	 * 
	 * @param operation	The new value to set.
	 **/
	@XmlElement
	public void setOperation(Operation operation) {
		preset(operationPropertyName, operation);
		this.operation = operation;
	}

	/**
	 * {@link #timestamp} accessor.
	 **/
	public Timestamp getTimestamp() {
		return timestamp;
	}

	/**
	 * {@link #timestamp} mutator.
	 * 
	 * @param timestamp	The new value to set.
	 **/
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(TimestampMapper.class)
	@XmlElement
	public void setTimestamp(Timestamp timestamp) {
		preset(timestampPropertyName, timestamp);
		this.timestamp = timestamp;
	}

	/**
	 * {@link #userName} accessor.
	 **/
	public String getUserName() {
		return userName;
	}

	/**
	 * {@link #userName} mutator.
	 * 
	 * @param userName	The new value to set.
	 **/
	@XmlElement
	public void setUserName(String userName) {
		preset(userNamePropertyName, userName);
		this.userName = userName;
	}

	/**
	 * {@link #audit} accessor.
	 **/
	public String getAudit() {
		return audit;
	}

	/**
	 * {@link #audit} mutator.
	 * 
	 * @param audit	The new value to set.
	 **/
	@XmlElement
	public void setAudit(String audit) {
		preset(auditPropertyName, audit);
		this.audit = audit;
	}

	/**
	 * {@link #sourceVersion} accessor.
	 **/
	public Audit getSourceVersion() {
		return sourceVersion;
	}

	/**
	 * {@link #sourceVersion} mutator.
	 * 
	 * @param sourceVersion	The new value to set.
	 **/
	@XmlElement
	public void setSourceVersion(Audit sourceVersion) {
		preset(sourceVersionPropertyName, sourceVersion);
		this.sourceVersion = sourceVersion;
	}

	/**
	 * {@link #comparisonVersion} accessor.
	 **/
	public Audit getComparisonVersion() {
		return comparisonVersion;
	}

	/**
	 * {@link #comparisonVersion} mutator.
	 * 
	 * @param comparisonVersion	The new value to set.
	 **/
	@XmlElement
	public void setComparisonVersion(Audit comparisonVersion) {
		preset(comparisonVersionPropertyName, comparisonVersion);
		this.comparisonVersion = comparisonVersion;
	}

	/**
	 * {@link #me} accessor.
	 **/
	public Audit getMe() {
		return me;
	}

	/**
	 * {@link #me} mutator.
	 * 
	 * @param me	The new value to set.
	 **/
	@XmlElement
	public void setMe(Audit me) {
		preset(mePropertyName, me);
		this.me = me;
	}
}
