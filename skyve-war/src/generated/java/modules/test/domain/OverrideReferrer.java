package modules.test.domain;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.domain.AbstractPersistentBean;

/**
 * Override Referrer
 * <br/>
 * Customer override fixture: refers to the overridden subclasses of the joined, single table and mapped hierarchies.
 * 
 * @navhas n singleSub 0..1 OverrideSingleSub
 * @navhas n mappedSub 0..1 OverrideMappedSub
 * @navhas n joinedSub 0..1 OverrideJoinedSub
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public class OverrideReferrer extends AbstractPersistentBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "test";

	/** @hidden */
	public static final String DOCUMENT_NAME = "OverrideReferrer";

	/** @hidden */
	public static final String textPropertyName = "text";

	/** @hidden */
	public static final String joinedSubPropertyName = "joinedSub";

	/** @hidden */
	public static final String singleSubPropertyName = "singleSub";

	/** @hidden */
	public static final String mappedSubPropertyName = "mappedSub";

	/**
	 * Text
	 **/
	private String text;

	/**
	 * Joined Sub
	 **/
	private OverrideJoinedSub joinedSub = null;

	/**
	 * Single Sub
	 **/
	private OverrideSingleSub singleSub = null;

	/**
	 * Mapped Sub
	 **/
	private OverrideMappedSub mappedSub = null;

	@Override
	@XmlTransient
	public String getBizModule() {
		return OverrideReferrer.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return OverrideReferrer.DOCUMENT_NAME;
	}

	public static OverrideReferrer newInstance() {
		try {
			return TestDocument.OVERRIDE_REFERRER.toDocument(CORE.getUser().getCustomer()).newInstance(CORE.getUser());
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

	/**
	 * {@link #text} accessor.
	 * @return	The value.
	 **/
	public String getText() {
		return text;
	}

	/**
	 * {@link #text} mutator.
	 * @param text	The new value.
	 **/
	@XmlElement
	public void setText(String text) {
		preset(textPropertyName, text);
		this.text = text;
	}

	/**
	 * {@link #joinedSub} accessor.
	 * @return	The value.
	 **/
	public OverrideJoinedSub getJoinedSub() {
		return joinedSub;
	}

	/**
	 * {@link #joinedSub} mutator.
	 * @param joinedSub	The new value.
	 **/
	@XmlElement
	public void setJoinedSub(OverrideJoinedSub joinedSub) {
		if (this.joinedSub != joinedSub) {
			preset(joinedSubPropertyName, joinedSub);
			this.joinedSub = joinedSub;
		}
	}

	/**
	 * {@link #singleSub} accessor.
	 * @return	The value.
	 **/
	public OverrideSingleSub getSingleSub() {
		return singleSub;
	}

	/**
	 * {@link #singleSub} mutator.
	 * @param singleSub	The new value.
	 **/
	@XmlElement
	public void setSingleSub(OverrideSingleSub singleSub) {
		if (this.singleSub != singleSub) {
			preset(singleSubPropertyName, singleSub);
			this.singleSub = singleSub;
		}
	}

	/**
	 * {@link #mappedSub} accessor.
	 * @return	The value.
	 **/
	public OverrideMappedSub getMappedSub() {
		return mappedSub;
	}

	/**
	 * {@link #mappedSub} mutator.
	 * @param mappedSub	The new value.
	 **/
	@XmlElement
	public void setMappedSub(OverrideMappedSub mappedSub) {
		if (this.mappedSub != mappedSub) {
			preset(mappedSubPropertyName, mappedSub);
			this.mappedSub = mappedSub;
		}
	}
}
