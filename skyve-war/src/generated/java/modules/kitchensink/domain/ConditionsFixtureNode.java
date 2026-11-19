package modules.kitchensink.domain;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import java.util.List;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.HierarchicalBean;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.domain.AbstractPersistentBean;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;

/**
 * Conditions Fixture Node
 * <br/>
 * Hierarchical node listed by the tree grids of the Kitchen Sink conditions fixture.
 * 
 * @stereotype "persistent child"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public class ConditionsFixtureNode extends AbstractPersistentBean implements HierarchicalBean<ConditionsFixtureNode> {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "kitchensink";

	/** @hidden */
	public static final String DOCUMENT_NAME = "ConditionsFixtureNode";

	/** @hidden */
	public static final String textPropertyName = "text";

	/**
	 * Text
	 **/
	private String text;

	private String bizParentId;

	@Override
	@XmlTransient
	public String getBizModule() {
		return ConditionsFixtureNode.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return ConditionsFixtureNode.DOCUMENT_NAME;
	}

	public static ConditionsFixtureNode newInstance() {
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

	@Override
	public String getBizParentId() {
		return bizParentId;
	}

	@Override
	@XmlElement
	public void setBizParentId(String bizParentId) {
		preset(HierarchicalBean.PARENT_ID, bizParentId);
		this.bizParentId = bizParentId;
	}

	@Override
	public ConditionsFixtureNode getParent() {
		ConditionsFixtureNode result = null;

		if (bizParentId != null) {
			Persistence p = CORE.getPersistence();
			DocumentQuery q = p.newDocumentQuery(ConditionsFixtureNode.MODULE_NAME, ConditionsFixtureNode.DOCUMENT_NAME);
			q.getFilter().addEquals(Bean.DOCUMENT_ID, bizParentId);
			result = q.retrieveBean();
		}

		return result;
	}

	@Override
	@XmlTransient
	public List<ConditionsFixtureNode> getChildren() {
		Persistence p = CORE.getPersistence();
		DocumentQuery q = p.newDocumentQuery(ConditionsFixtureNode.MODULE_NAME, ConditionsFixtureNode.DOCUMENT_NAME);
		q.getFilter().addEquals(HierarchicalBean.PARENT_ID, getBizId());
		return q.beanResults();
	}
}
