package modules.test.domain;

import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import modules.test.MappedExtensionJoinedStrategy.MappedExtensionJoinedStrategyExtension;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;

/**
 * Mapped Subclassed Joined Strategy
 * <br/>
 * Another Extension document to test that the generated class extends 
			MappedExtensionExtension.java
 * 
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
public class MappedSubclassedJoinedStrategy extends MappedExtensionJoinedStrategyExtension {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	@SuppressWarnings("hiding")
	public static final String MODULE_NAME = "test";

	/** @hidden */
	@SuppressWarnings("hiding")
	public static final String DOCUMENT_NAME = "MappedSubclassedJoinedStrategy";

	/** @hidden */
	public static final String subclassIntegerPropertyName = "subclassInteger";

	/** @hidden */
	public static final String dynamicSubclassIntegerPropertyName = "dynamicSubclassInteger";

	/**
	 * Subclass Integer
	 **/
	private Integer subclassInteger;

	@Override
	@XmlTransient
	public String getBizModule() {
		return MappedSubclassedJoinedStrategy.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return MappedSubclassedJoinedStrategy.DOCUMENT_NAME;
	}

	public static MappedSubclassedJoinedStrategy newInstance() {
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
			return org.skyve.util.Binder.formatMessage("{text}", this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return "Unknown";
		}
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof MappedSubclassedJoinedStrategy) && 
					this.getBizId().equals(((MappedSubclassedJoinedStrategy) o).getBizId()));
	}

	/**
	 * {@link #subclassInteger} accessor.
	 * @return	The value.
	 **/
	public Integer getSubclassInteger() {
		return subclassInteger;
	}

	/**
	 * {@link #subclassInteger} mutator.
	 * @param subclassInteger	The new value.
	 **/
	@XmlElement
	public void setSubclassInteger(Integer subclassInteger) {
		preset(subclassIntegerPropertyName, subclassInteger);
		this.subclassInteger = subclassInteger;
	}
}
