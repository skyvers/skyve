package modules.admin.domain;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlEnum;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlSchemaType;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.types.Enumeration;
import org.skyve.domain.types.Timestamp;
import org.skyve.impl.domain.AbstractPersistentBean;
import org.skyve.impl.domain.types.jaxb.TimestampMapper;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.util.Util;

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
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
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
	public static final String operationPropertyName = "operation";

	/** @hidden */
	public static final String timestampPropertyName = "timestamp";

	/** @hidden */
	public static final String millisPropertyName = "millis";

	/** @hidden */
	public static final String userNamePropertyName = "userName";

	/** @hidden */
	public static final String auditDetailPropertyName = "auditDetail";

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
	@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
	public static enum Operation implements Enumeration {
		insert("I", "Insert"),
		update("U", "Update"),
		delete("D", "Delete");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues = Stream.of(values()).map(Operation::toDomainValue).collect(Collectors.toUnmodifiableList());

		private Operation(String code, String description) {
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

		public static Operation fromLocalisedDescription(String description) {
			Operation result = null;

			for (Operation value : values()) {
				if (value.toLocalisedDescription().equals(description)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static List<DomainValue> toDomainValues() {
			return domainValues;
		}
	}

	/**
	 * Module
	 **/
	private String auditModuleName;

	/**
	 * Document
	 **/
	private String auditDocumentName;

	/**
	 * BizId
	 **/
	private String auditBizId;

	/**
	 * Description
	 **/
	private String auditBizKey;

	/**
	 * Operation
	 **/
	private Operation operation;

	/**
	 * Timestamp
	 **/
	private Timestamp timestamp;

	/**
	 * Millis
	 **/
	private Long millis;

	/**
	 * User
	 **/
	private String userName;

	/**
	 * Audit
	 **/
	private String auditDetail;

	/**
	 * Source Version To Compare
	 **/
	private Audit sourceVersion = null;

	/**
	 * Other Version To Compare
	 **/
	private Audit comparisonVersion = null;

	/**
	 * Me
	 **/
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

	public static Audit newInstance() {
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
			return org.skyve.util.Binder.formatMessage("{operation} by {userName} at {timestamp}", this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
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
	 * @return	The value.
	 **/
	public String getAuditModuleName() {
		return auditModuleName;
	}

	/**
	 * {@link #auditModuleName} mutator.
	 * @param auditModuleName	The new value.
	 **/
	@XmlElement
	public void setAuditModuleName(String auditModuleName) {
		preset(auditModuleNamePropertyName, auditModuleName);
		this.auditModuleName = auditModuleName;
	}

	/**
	 * {@link #auditDocumentName} accessor.
	 * @return	The value.
	 **/
	public String getAuditDocumentName() {
		return auditDocumentName;
	}

	/**
	 * {@link #auditDocumentName} mutator.
	 * @param auditDocumentName	The new value.
	 **/
	@XmlElement
	public void setAuditDocumentName(String auditDocumentName) {
		preset(auditDocumentNamePropertyName, auditDocumentName);
		this.auditDocumentName = auditDocumentName;
	}

	/**
	 * {@link #auditBizId} accessor.
	 * @return	The value.
	 **/
	public String getAuditBizId() {
		return auditBizId;
	}

	/**
	 * {@link #auditBizId} mutator.
	 * @param auditBizId	The new value.
	 **/
	@XmlElement
	public void setAuditBizId(String auditBizId) {
		preset(auditBizIdPropertyName, auditBizId);
		this.auditBizId = auditBizId;
	}

	/**
	 * {@link #auditBizKey} accessor.
	 * @return	The value.
	 **/
	public String getAuditBizKey() {
		return auditBizKey;
	}

	/**
	 * {@link #auditBizKey} mutator.
	 * @param auditBizKey	The new value.
	 **/
	@XmlElement
	public void setAuditBizKey(String auditBizKey) {
		preset(auditBizKeyPropertyName, auditBizKey);
		this.auditBizKey = auditBizKey;
	}

	/**
	 * {@link #operation} accessor.
	 * @return	The value.
	 **/
	public Operation getOperation() {
		return operation;
	}

	/**
	 * {@link #operation} mutator.
	 * @param operation	The new value.
	 **/
	@XmlElement
	public void setOperation(Operation operation) {
		preset(operationPropertyName, operation);
		this.operation = operation;
	}

	/**
	 * {@link #timestamp} accessor.
	 * @return	The value.
	 **/
	public Timestamp getTimestamp() {
		return timestamp;
	}

	/**
	 * {@link #timestamp} mutator.
	 * @param timestamp	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(TimestampMapper.class)
	public void setTimestamp(Timestamp timestamp) {
		preset(timestampPropertyName, timestamp);
		this.timestamp = timestamp;
	}

	/**
	 * {@link #millis} accessor.
	 * @return	The value.
	 **/
	public Long getMillis() {
		return millis;
	}

	/**
	 * {@link #millis} mutator.
	 * @param millis	The new value.
	 **/
	@XmlElement
	public void setMillis(Long millis) {
		preset(millisPropertyName, millis);
		this.millis = millis;
	}

	/**
	 * {@link #userName} accessor.
	 * @return	The value.
	 **/
	public String getUserName() {
		return userName;
	}

	/**
	 * {@link #userName} mutator.
	 * @param userName	The new value.
	 **/
	@XmlElement
	public void setUserName(String userName) {
		preset(userNamePropertyName, userName);
		this.userName = userName;
	}

	/**
	 * {@link #auditDetail} accessor.
	 * @return	The value.
	 **/
	public String getAuditDetail() {
		return auditDetail;
	}

	/**
	 * {@link #auditDetail} mutator.
	 * @param auditDetail	The new value.
	 **/
	@XmlElement
	public void setAuditDetail(String auditDetail) {
		preset(auditDetailPropertyName, auditDetail);
		this.auditDetail = auditDetail;
	}

	/**
	 * {@link #sourceVersion} accessor.
	 * @return	The value.
	 **/
	public Audit getSourceVersion() {
		return sourceVersion;
	}

	/**
	 * {@link #sourceVersion} mutator.
	 * @param sourceVersion	The new value.
	 **/
	@XmlElement
	public void setSourceVersion(Audit sourceVersion) {
		if (this.sourceVersion != sourceVersion) {
			preset(sourceVersionPropertyName, sourceVersion);
			this.sourceVersion = sourceVersion;
		}
	}

	/**
	 * {@link #comparisonVersion} accessor.
	 * @return	The value.
	 **/
	public Audit getComparisonVersion() {
		return comparisonVersion;
	}

	/**
	 * {@link #comparisonVersion} mutator.
	 * @param comparisonVersion	The new value.
	 **/
	@XmlElement
	public void setComparisonVersion(Audit comparisonVersion) {
		if (this.comparisonVersion != comparisonVersion) {
			preset(comparisonVersionPropertyName, comparisonVersion);
			this.comparisonVersion = comparisonVersion;
		}
	}

	/**
	 * {@link #me} accessor.
	 * @return	The value.
	 **/
	public Audit getMe() {
		return me;
	}

	/**
	 * {@link #me} mutator.
	 * @param me	The new value.
	 **/
	@XmlElement
	public void setMe(Audit me) {
		if (this.me != me) {
			preset(mePropertyName, me);
			this.me = me;
		}
	}
}
