package org.skyve.domain.app.admin;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.skyve.CORE;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.app.AppConstants;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.types.Enumeration;
import org.skyve.domain.types.Timestamp;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.util.Util;

import jakarta.xml.bind.annotation.XmlEnum;

public interface SecurityLog extends PersistentBean {

	@XmlEnum
	public static enum ExceptionType implements Enumeration {
		securityException("securityException", "Security Exception"), 
		accessException("accessException", "Access Exception");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues = Stream.of(values())
				.map(ExceptionType::toDomainValue)
				.collect(Collectors.toUnmodifiableList());

		private ExceptionType(String code, String description) {
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

		public static ExceptionType fromCode(String code) {
			ExceptionType result = null;

			for (ExceptionType value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static ExceptionType fromLocalisedDescription(String description) {
			ExceptionType result = null;

			for (ExceptionType value : values()) {
				if (value.toLocalisedDescription()
						.equals(description)) {
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

	public static SecurityLog newInstance() {
		try {
			return CORE.getUser()
					.getCustomer()
					.getModule(AppConstants.ADMIN_MODULE_NAME)
					.getDocument(CORE.getUser()
							.getCustomer(), AppConstants.SECURITY_LOG_DOCUMENT_NAME)
					.newInstance(CORE.getUser());
		} catch (RuntimeException e) {
			throw e;
		} catch (Exception e) {
			throw new DomainException(e);
		}
	}

	Timestamp getTimestamp();
	Long getThreadID();
	String getSourceIP();
	String getUsername();
	String getLoggedInUserId();
	String getResource();
	String getProvenance();
	ExceptionType getExceptionType();
	String getExceptionMessage();

	void setTimestamp(Timestamp timestamp);
	void setThreadID(Long threadID);
	void setSourceIP(String sourceIP);
	void setUsername(String username);
	void setLoggedInUserId(String bizId);
	void setResource(String resource);
	void setProvenance(String provenance);
	void setExceptionType(ExceptionType exceptionType);
	void setExceptionMessage(String exceptionMessage);
}
