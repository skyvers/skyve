package modules.test.domain;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;

/**
 * Mapped Subclassed Unique Joined Strategy
 * <br/>
 * Another Extension document to test unique constraint check scopes in a hierarchy
 * 
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public class MappedSubclassedUniqueJoinedStrategy extends MappedExtensionUniqueJoinedStrategy {
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
	public static final String DOCUMENT_NAME = "MappedSubclassedUniqueJoinedStrategy";

	@Override
	@XmlTransient
	public String getBizModule() {
		return MappedSubclassedUniqueJoinedStrategy.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return MappedSubclassedUniqueJoinedStrategy.DOCUMENT_NAME;
	}

	public static MappedSubclassedUniqueJoinedStrategy newInstance() {
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
}
