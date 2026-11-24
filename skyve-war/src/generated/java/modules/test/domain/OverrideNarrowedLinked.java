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
 * Override Narrowed Linked
 * <br/>
 * Customer override fixture: the test customer's override leaves out dropped, and it references OverrideLinkTarget, which the test2 customer overrides.
 * 
 * @navhas n target 0..1 OverrideLinkTarget
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public abstract class OverrideNarrowedLinked extends AbstractPersistentBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "test";

	/** @hidden */
	public static final String DOCUMENT_NAME = "OverrideNarrowedLinked";

	/** @hidden */
	public static final String textPropertyName = "text";

	/** @hidden */
	public static final String targetPropertyName = "target";

	/**
	 * Text
	 **/
	private String text;

	/**
	 * Target
	 **/
	private OverrideLinkTarget target = null;

	@Override
	@XmlTransient
	public String getBizModule() {
		return OverrideNarrowedLinked.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return OverrideNarrowedLinked.DOCUMENT_NAME;
	}

	public static OverrideNarrowedLinked newInstance() {
		try {
			return TestDocument.OVERRIDE_NARROWED_LINKED.toDocument(CORE.getUser().getCustomer()).newInstance(CORE.getUser());
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
	 * {@link #target} accessor.
	 * @return	The value.
	 **/
	public OverrideLinkTarget getTarget() {
		return target;
	}

	/**
	 * {@link #target} mutator.
	 * @param target	The new value.
	 **/
	@XmlElement
	public void setTarget(OverrideLinkTarget target) {
		if (this.target != target) {
			preset(targetPropertyName, target);
			this.target = target;
		}
	}
}
